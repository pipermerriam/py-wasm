import ast
import operator
import uuid
from typing import NamedTuple

import numpy

from wasm import constants
from wasm._utils.toolz import (
    groupby,
    concat,
    curry,
    complement,
)
from wasm._utils.decorators import to_tuple
from wasm._utils.memory import round_up_to_page
from wasm import datatypes
from wasm import instructions
from wasm.opcodes import (
    TEXT_TO_OPCODE,
    BinaryOpcode,
)
from . import ir
from . import lookups


#
# Utility
#
def _exactly_one(toks):
    if len(toks) != 1:
        raise Exception("INVALID")
    return toks[0]


def any_unresolved(*values):
    return any(_is_unresolved(item) for item in values)


all_resolved = complement(any_unresolved)

RESOLVED_TYPES = (
    instructions.BaseInstruction,
    datatypes.Idx,
    datatypes.GlobalType,
    datatypes.TableType,
    datatypes.MemoryType,
)


def _is_unresolved(value):
    if isinstance(value, ir.BaseUnresolved):
        return True
    elif isinstance(value, RESOLVED_TYPES):
        return False
    elif isinstance(value, tuple) and not isinstance(value, NamedTuple):
        return any(_is_unresolved(item) for item in value)
    else:
        raise Exception(f"INVALID TYPE: {value}")


def _mk_exports_and_import(idx_class, name, export_names, import_data):
    idx = idx_class(name)
    exports = tuple(
        ir.UnresolvedExport(export_name, idx)
        for export_name
        in export_names
    )
    if import_data:
        module_name, as_name = import_data
        _import = ir.UnresolvedImport(
            module_name,
            as_name,
            idx,
        )
    else:
        _import = None

    return exports, _import


#
# DEBUG
#
def assert_false(s, loc, toks):
    assert False


#
# Common Parsers
#
def concatenate_tokens(s, loc, toks):
    return tuple(concat(toks))


def parse_to_tuple(s, loc, toks):
    return tuple(toks)


@curry
def parse_simple_class(instruction_class, s, loc, toks):
    # TODO: validation that toks has a single element
    return instruction_class()


@curry
def parse_simple_var_wrapper(resolved_class, unresolved_class, s, loc, toks):
    var = _exactly_one(toks)

    if isinstance(var, ir.BaseUnresolved):
        return unresolved_class(var)
    elif isinstance(var, datatypes.Idx):
        return resolved_class(var)
    else:
        raise Exception("INVALID")


#
# Modules
#
def parse_module(s, loc, toks):
    name, sections = toks

    (
        types,
        funcs,
        tables,
        mems,
        globals,
        elem,
        data,
        start,
        imports,
        exports,
    ) = _collate_module(sections)

    module = datatypes.Module(
        version=constants.VERSION_1,
        types=types,
        funcs=funcs,
        tables=tables,
        mems=mems,
        globals=globals,
        elem=elem,
        data=data,
        start=start,
        imports=imports,
        exports=exports,
    )

    if name is None:
        return module
    else:
        assert False


def _is_function_type(value):
    return isinstance(value, (ir.UnresolvedFunctionType, ir.NamedFunctionType))


def _is_function(value):
    return isinstance(value, (datatypes.Function))


def _is_table(value):
    return isinstance(value, (datatypes.Table, ir.NamedTable))


def _is_memory(value):
    return isinstance(value, (datatypes.Memory, ir.NamedMemory))


def _is_global(value):
    return isinstance(value, (datatypes.Global, ir.NamedGlobal))


def _is_element_segment(value):
    return isinstance(value, (datatypes.ElementSegment, ir.UnresolvedElementSegment))


def _is_data_segment(value):
    return isinstance(value, (datatypes.DataSegment, ir.UnresolvedDataSegment))


def _is_start_function(value):
    return isinstance(value, (datatypes.StartFunction, ir.UnresolvedStartFunction))


def _is_import(value):
    return isinstance(value, (datatypes.Import, ir.UnresolvedImport))


def _is_export(value):
    return isinstance(value, (datatypes.Export, ir.UnresolvedExport))


# These **must** be in the same field order as the `Module` type accepts them
# as arguments.
EMPTY_SECTIONS_BY_TYPE = (
    (datatypes.FunctionType, tuple()),
    (datatypes.Function, tuple()),
    (datatypes.Table, tuple()),
    (datatypes.Memory, tuple()),
    (datatypes.Global, tuple()),
    (datatypes.ElementSegment, tuple()),
    (datatypes.DataSegment, tuple()),
    (datatypes.StartFunction, None),
    (datatypes.Import, tuple()),
    (datatypes.Export, tuple()),
)
SECTIONS_MATCH_FNS = (
    (datatypes.FunctionType, _is_function_type),
    (datatypes.Function, _is_function),
    (datatypes.Table, _is_table),
    (datatypes.Memory, _is_memory),
    (datatypes.Global, _is_global),
    (datatypes.ElementSegment, _is_element_segment),
    (datatypes.DataSegment, _is_data_segment),
    (datatypes.StartFunction, _is_start_function),
    (datatypes.Import, _is_import),
    (datatypes.Export, _is_export),
)


def _associate_section_type(value):
    for section_type, match_fn in SECTIONS_MATCH_FNS:
        if match_fn(value):
            return (section_type, value)
    else:
        raise Exception(f"No section match for {value}")


@to_tuple
def _collate_module(all_module_fields):
    fields_by_section_type = groupby(
        operator.itemgetter(0),
        map(_associate_section_type, all_module_fields)
    )
    for section_type, empty_section_value in EMPTY_SECTIONS_BY_TYPE:
        yield fields_by_section_type.get(section_type, empty_section_value)


#
# Type
#
def parse_type(s, loc, toks):
    name, function_type = toks

    if name is None:
        return function_type
    else:
        type_idx = ir.UnresolvedTypeIdx(name)
        return ir.LinkedFunctionType(type_idx, function_type)


def parse_function_type(s, loc, toks):
    params, results = toks
    return ir.UnresolvedFunctionType(params, results)


#
# Memory
#
def parse_memory(s, loc, toks):
    name, export_names, import_data, memory_type = toks
    base_memory = datatypes.Memory(memory_type)

    if name is None and (export_names or import_data):
        name = str(uuid.uuid4())
    elif name is None:
        return (), None, base_memory

    memory = ir.NamedMemory(name, base_memory)

    exports, _import = _mk_exports_and_import(
        ir.UnresolvedMemoryIdx,
        name,
        export_names,
        import_data,
    )

    return exports, _import, memory


def parse_memory_type(s, loc, toks):
    min, max = toks
    return datatypes.MemoryType(min, max)


def parse_memory_with_data(s, loc, toks):
    name, data = toks
    raw_memory_size = len(data)
    memory_size = round_up_to_page(raw_memory_size)

    base_memory = datatypes.Memory(datatypes.MemoryType(memory_size, memory_size))

    if name is None:
        name = uuid.uuid4()

    memory = ir.NamedMemory(name, base_memory)
    memory_idx = ir.UnresolvedMemoryIdx(name)
    data_segment = ir.UnresolvedDataSegment(
        memory_idx=memory_idx,
        offset=(instructions.I32Const(numpy.uint32(0)), instructions.End()),
        init=data,
    )
    return memory, data_segment


def parse_datastring(s, loc, toks):
    data = "".join(toks)
    data_bytes = data.encode('utf8')
    return data_bytes


#
# Table
#
def parse_table_with_elements(s, loc, toks):
    name, element_type, init_elements = toks
    num_elements = len(init_elements)
    limits = datatypes.Limits(num_elements, num_elements)
    base_table = datatypes.Table(datatypes.TableType(limits, element_type))
    if name is None:
        name = str(uuid.uuid4())
    table = ir.NamedTable(name, base_table)
    table_idx = ir.UnresolvedTableIdx(name)
    element_segment = ir.UnresolvedElementSegment(
        table_idx=table_idx,
        offset=(instructions.I32Const(numpy.uint32(0)), instructions.End()),
        init=init_elements,
    )
    return table, element_segment


def parse_table(s, loc, toks):
    name, export_names, import_data, table_type = toks
    base_table = datatypes.Table(table_type)
    if name is None and (export_names or import_data):
        name = str(uuid.uuid4())

    if name is None:
        return (), None, base_table

    table = ir.NamedTable(name, base_table)

    exports, _import = _mk_exports_and_import(
        ir.UnresolvedTableIdx,
        name,
        export_names,
        import_data,
    )

    return exports, _import, table


def parse_table_type(s, loc, toks):
    limits, element_type = toks
    return datatypes.TableType(limits, element_type)


def parse_elem_type(s, loc, toks):
    return datatypes.FunctionAddress


#
# Globals
#
def parse_global(s, loc, toks):
    name, global_type, init = toks
    _global = datatypes.Global(global_type, init)

    if name is None:
        return _global
    else:
        return ir.NamedGlobal(name, _global)


def parse_global_mut(s, loc, toks):
    valtype = _exactly_one(toks)
    return datatypes.GlobalType.var(valtype)


def parse_global_const(s, loc, toks):
    valtype = _exactly_one(toks)
    return datatypes.GlobalType.const(valtype)


#
# Functions
#
def parse_function(s, loc, toks):
    name, export_name, typeuse, locals, base_body = toks

    if typeuse is None:
        typeuse = ir.UnresolvedFunctionType((), ())
    if locals is None:
        locals = ()

    if base_body is None:
        body = instructions.End.as_tail()
    else:
        body = base_body + instructions.End.as_tail()

    function = ir.UnresolvedFunction(
        type=typeuse,
        locals=locals,
        body=body,
        name=name,
    )
    # TODO: this needs to return a 2-tuple of (export, function) and generate a
    # function name when none was provided)

    if export_name:
        return ir.UnresolvedExport(export_name, function)
    else:
        return function


#
# Imports
#
def parse_import(s, loc, toks):
    module_name, as_name, desc = toks
    if all_resolved(desc):
        return datatypes.Import(module_name, as_name, desc)
    else:
        return ir.UnresolvedImport(module_name, as_name, desc)


def parse_import_function(s, loc, toks):
    typeuse = _exactly_one(toks)
    if typeuse is None:
        return ir.UnresolvedFunctionType((), ())
    else:
        return typeuse


@to_tuple
def parse_folded_function_import(s, loc, toks):
    name, export_names, (module_name, as_name), typeuse = toks
    assert typeuse is not None  # TODO: verify that typeuse is indeed required

    if name is None:
        desc = typeuse
    else:
        desc = ir.LinkedFunctionType(ir.UnresolvedTypeIdx(name), typeuse)

    for name in export_names:
        yield ir.UnresolvedExport(name, desc)
    yield ir.UnresolvedImport(module_name, as_name, desc)


#
# Exports
#
def parse_export(s, loc, toks):
    name, descriptor = toks
    return datatypes.Export(name, descriptor)


#
# Data Segment
#
def parse_data_segment(s, loc, toks):
    memory_idx, offset_expr, init_bytes = toks
    is_resolved = all_resolved(memory_idx, offset_expr)

    if is_resolved:
        return datatypes.DataSegment(
            memory_idx=memory_idx,
            offset=offset_expr,
            init=init_bytes,
        )
    else:
        return ir.UnresolvedDataSegment(
            memory_idx=memory_idx,
            offset=offset_expr,
            init=init_bytes,
        )


#
# Element Segment
#
def parse_element_segment(s, loc, toks):
    table_idx, offset_expr, function_indices = toks
    is_resolved = all_resolved(table_idx, offset_expr, *function_indices)

    if is_resolved:
        return datatypes.ElementSegment(
            table_idx=table_idx,
            offset=offset_expr,
            init=function_indices,
        )
    else:
        return ir.UnresolvedElementSegment(
            table_idx=table_idx,
            offset=offset_expr,
            init=function_indices,
        )


def parse_offset_instr(s, loc, toks):
    expr = _normalize_expressions(*toks)
    return expr + instructions.End.as_tail()


#
# Start Function
#
def parse_start_function(s, loc, toks):
    function_idx = _exactly_one(toks)
    if any_unresolved(function_idx):
        return ir.UnresolvedStartFunction(function_idx)
    else:
        return datatypes.StartFunction(function_idx)


#
# Expressions
#
@to_tuple
def _normalize_expressions(*expressions):
    if isinstance(expressions, (ir.BaseUnresolved, instructions.BaseInstruction)):
        yield expressions
    else:
        for expr in expressions:
            if isinstance(expr, (ir.BaseUnresolved, instructions.BaseInstruction)):
                yield expr
            elif isinstance(expr, tuple):
                yield from _normalize_expressions(*expr)
            else:
                raise Exception("INVALID")


def parse_expr(s, loc, toks):
    return tuple(*toks) + instructions.End.as_tail()


def parse_instrs(s, loc, toks):
    return _normalize_expressions(*toks)


def parse_folded_op(s, loc, toks):
    op, *exprs = toks

    return _normalize_expressions(*exprs) + (op,)


#
# Numeric Ops
#
def parse_const_op(s, loc, toks):
    opcode_text, value = toks
    opcode = TEXT_TO_OPCODE[opcode_text]

    if opcode is BinaryOpcode.I32_CONST:
        return instructions.I32Const(numpy.uint32(value))
    elif opcode is BinaryOpcode.I64_CONST:
        return instructions.I64Const(numpy.uint64(value))
    elif opcode is BinaryOpcode.F32_CONST:
        return instructions.F32Const(numpy.float32(value))
    elif opcode is BinaryOpcode.F64_CONST:
        return instructions.F64Const(numpy.float64(value))
    else:
        raise Exception("TODO:")


@curry
def parse_opcode_text_with_lookup(opcode_class, lookups, s, loc, toks):
    opcode_text = _exactly_one(toks)
    opcode = lookups[opcode_text]
    return opcode_class.from_opcode(opcode)


parse_testop = parse_opcode_text_with_lookup(instructions.TestOp, TEXT_TO_OPCODE)
parse_unop = parse_opcode_text_with_lookup(instructions.UnOp, TEXT_TO_OPCODE)
parse_binop = parse_opcode_text_with_lookup(instructions.BinOp, TEXT_TO_OPCODE)
parse_relop = parse_opcode_text_with_lookup(instructions.RelOp, TEXT_TO_OPCODE)
parse_wrapop = parse_simple_class(instructions.Wrap)
parse_extendop = parse_opcode_text_with_lookup(instructions.Extend, lookups.EXTEND_LOOKUP)
parse_truncop = parse_opcode_text_with_lookup(instructions.Truncate, lookups.TRUNC_LOOKUP)
parse_convertop = parse_opcode_text_with_lookup(instructions.Convert, lookups.CONVERT_LOOKUP)
parse_demoteop = parse_simple_class(instructions.Demote)
parse_promoteop = parse_simple_class(instructions.Promote)
parse_reinterpretop = parse_opcode_text_with_lookup(instructions.Reinterpret, lookups.REINTERPRET_LOOKUP)  # noqa: E501


#
# Memory Ops
#
parse_memory_grow = parse_simple_class(instructions.MemoryGrow)
parse_memory_size = parse_simple_class(instructions.MemorySize)


def parse_memory_access_op(s, loc, toks):
    opcode_text, offset, align = toks
    opcode = TEXT_TO_OPCODE[opcode_text]

    offset_default, align_default = lookups.MEMORY_ARG_DEFAULTS[opcode]
    if offset is None:
        offset = offset_default
    if align is None:
        align = align_default

    memarg = instructions.MemoryArg(offset, align)
    return instructions.MemoryOp.from_opcode(opcode, memarg)


#
# Variable Ops
#
def parse_variable_op(s, loc, toks):
    opcode_text, var = toks
    instruction_class, opcode = lookups.VARIABLE_LOOKUP[opcode_text]
    if isinstance(var, ir.BaseUnresolved):
        return ir.UnresolvedVariableOp(opcode, var)
    elif isinstance(var, int):
        return instruction_class.from_opcode(opcode, var)
    else:
        raise Exception("INVALID")


#
# Parametric Ops
#
parse_drop = parse_simple_class(instructions.Drop)
parse_select = parse_simple_class(instructions.Select)


#
# Control Ops
#
parse_return = parse_simple_class(instructions.Return)
parse_nop = parse_simple_class(instructions.Nop)
parse_end = parse_simple_class(instructions.End)
parse_unreachable = parse_simple_class(instructions.Unreachable)


parse_call_op = parse_simple_var_wrapper(instructions.Call, ir.UnresolvedCall)


def parse_call_indirect_op(s, loc, toks):
    typeuse = _exactly_one(toks)
    return ir.UnresolvedCallIndirect(typeuse)


parse_br_op = parse_simple_var_wrapper(instructions.Br, ir.UnresolvedBr)
parse_br_if_op = parse_simple_var_wrapper(instructions.BrIf, ir.UnresolvedBrIf)


def parse_br_table_op(s, loc, toks):
    all_label_indices = tuple(toks)
    is_resolved = all_resolved(all_label_indices)
    *label_indices, default_idx = all_label_indices

    if is_resolved:
        return instructions.BrTable(
            label_indices=tuple(label_indices),
            default_idx=default_idx,
        )
    else:
        return ir.UnresolvedBrTable(
            label_indices=tuple(label_indices),
            default_idx=default_idx,
        )


def parse_block(s, loc, toks):
    block, end, tail_name = toks
    if tail_name is not None:
        # TODO: account for this
        assert False
    return block


def parse_folded_block(s, loc, toks):
    name, block_type, instrs = toks

    base_block = instructions.Block(block_type, instrs)
    if name is None:
        block = base_block
    else:
        block = ir.NamedBlock(name, base_block)
    return block


def parse_folded_loop(s, loc, toks):
    name, block_type, instrs = toks

    base_loop = instructions.Loop(block_type, instrs)
    if name is None:
        loop = base_loop
    else:
        loop = ir.NamedLoop(name, base_loop)
    return loop


def parse_folded_if(s, loc, toks):
    name, block_type, exprs, then_instrs, else_instrs = toks
    if else_instrs is None:
        else_instrs = instructions.End.as_tail()

    if then_instrs is None:
        then_instrs = instructions.Else.as_tail()
    else:
        then_instrs = then_instrs + instructions.Else.as_tail()

    base_if_instr = instructions.If(block_type, then_instrs, else_instrs)

    if name is not None:
        if_instr = ir.NamedIf(name, base_if_instr)
    else:
        if_instr = base_if_instr

    return exprs + (if_instr,)


def parse_blk_instrs(s, loc, toks):
    instrs = _normalize_expressions(*toks)
    return instrs + instructions.End.as_tail()


#
# TypeUse
#
def parse_typeuse_linked(s, loc, toks):
    type_idx, function_type = toks
    return ir.LinkedFunctionType(type_idx, function_type)


def parse_typeuse_only_params(s, loc, toks):
    if not toks:
        params = ()
    else:
        params = tuple(concat(toks))
    return ir.UnresolvedFunctionType(params, ())


def parse_typeuse_only_results(s, loc, toks):
    if not toks:
        results = ()
    else:
        results = tuple(concat(toks))
    return ir.UnresolvedFunctionType((), results)


#
# Locals and Params
#
def parse_named_local(s, loc, toks):
    name, valtype = toks
    return (ir.Local(valtype, name),)


def parse_bulk_locals(s, loc, toks):
    return tuple(ir.Local(valtype) for valtype in concat(toks))


def parse_named_param(s, loc, toks):
    name, valtype = toks
    return (ir.Param(valtype, name),)


def parse_bulk_params(s, loc, toks):
    return tuple(ir.Param(valtype) for valtype in concat(toks))


#
# Value Types
#
def parse_limits(s, loc, toks):
    min, max = toks
    return datatypes.Limits(min, max)


def parse_valtype(s, loc, toks):
    return datatypes.ValType.from_str(_exactly_one(toks))


#
# Indices Types
#
@curry
def parse_indices(index_class, unresolved_class, s, loc, toks):
    var = _exactly_one(toks)

    if isinstance(var, int):
        return index_class(var)
    elif isinstance(var, str):
        return unresolved_class(var)
    else:
        raise Exception("INVALID")


parse_local_idx = parse_indices(datatypes.LocalIdx, ir.UnresolvedLocalIdx)
parse_global_idx = parse_indices(datatypes.GlobalIdx, ir.UnresolvedGlobalIdx)
parse_function_idx = parse_indices(datatypes.FunctionIdx, ir.UnresolvedFunctionIdx)
parse_label_idx = parse_indices(datatypes.LabelIdx, ir.UnresolvedLabelIdx)
parse_memory_idx = parse_indices(datatypes.MemoryIdx, ir.UnresolvedMemoryIdx)
parse_table_idx = parse_indices(datatypes.TableIdx, ir.UnresolvedTableIdx)
parse_type_idx = parse_indices(datatypes.TypeIdx, ir.UnresolvedTypeIdx)


#
# Primative Value Types
#
def parse_integer_string(s, loc, toks):
    raw_num = _exactly_one(toks)
    return int(raw_num)


def parse_hex_to_int(s, loc, toks):
    hex_val = _exactly_one(toks)
    return int(hex_val, 16)


def parse_escaped_char(s, loc, toks):
    char = _exactly_one(toks)
    print("LITERAL:", repr(char))
    return ast.literal_eval(f'"{char}"')


def validate_utf_char(value):
    if value < 0xD800:
        return
    elif 0xE000 <= value < 0x110000:
        return
    else:
        raise Exception("INVALID")


def parse_escaped_unicode(s, loc, toks):
    raw_value = _exactly_one(toks)
    raw_value = raw_value[3:-1]
    value_as_int = int(raw_value, 16)

    validate_utf_char(value_as_int)

    value_as_hex = hex(value_as_int)[2:]

    if value_as_int <= 0xFFFF:
        value_as_literal_string = f'"\\u{value_as_hex.rjust(4, "0")}"'
    else:
        value_as_literal_string = f'"\\u{value_as_hex.rjust(8, "0")}"'

    return ast.literal_eval(value_as_literal_string)
