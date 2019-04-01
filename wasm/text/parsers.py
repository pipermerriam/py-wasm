from wasm._utils.toolz import (
    concat,
    curry,
    complement,
)
from wasm._utils.decorators import to_tuple
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


def _is_unresolved(value):
    if isinstance(value, ir.BaseUnresolved):
        return True
    elif isinstance(value, instructions.BaseInstruction):
        return False
    elif isinstance(value, datatypes.Idx):
        return False
    elif isinstance(value, tuple):
        return any(_is_unresolved(item) for item in value)
    else:
        raise Exception(f"INVALID TYPE: {value}")


def _assert_false(s, loc, toks):
    assert False


#
# Common Parsers
#
def concatenate_tokens(s, loc, toks):
    return tuple(concat(toks))


@curry
def parse_simple_op(instruction_class, s, loc, toks):
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

    if export_name:
        return ir.UnresolvedExport(export_name, function)
    else:
        return function


def parse_function_import(s, loc, toks):
    name, module_name, as_name, typeuse = toks
    assert typeuse is not None  # TODO: verify that typeuse is indeed required
    assert name is not None  # TODO: verify that name is indeed required
    desc = ir.LinkedFunctionType(ir.UnresolvedTypeIdx(name), typeuse)
    return ir.UnresolvedImport(module_name, as_name, desc)


#
# Exports
#
def parse_export(s, loc, toks):
    name, descriptor = toks
    return datatypes.Export(name, descriptor)


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


def parse_exprs(s, loc, toks):
    return _normalize_expressions(*toks)


def parse_instrs(s, loc, toks):
    return _normalize_expressions(*toks)


def parse_folded_op(s, loc, toks):
    op, exprs = toks

    if isinstance(exprs, (instructions.BaseInstruction, ir.BaseUnresolved)):
        return (exprs, op)
    elif isinstance(exprs, tuple):
        return exprs + (op,)
    else:
        raise Exception("INVALID")


#
# Numeric Ops
#
def parse_const_op(s, loc, toks):
    opcode_text, value = toks
    opcode = TEXT_TO_OPCODE[opcode_text]

    if opcode is BinaryOpcode.I32_CONST:
        return instructions.I32Const(value)
    elif opcode is BinaryOpcode.I64_CONST:
        return instructions.I64Const(value)
    elif opcode is BinaryOpcode.F32_CONST:
        return instructions.F32Const(value)
    elif opcode is BinaryOpcode.F64_CONST:
        return instructions.F64Const(value)
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
parse_wrapop = parse_simple_op(instructions.Wrap)
parse_extendop = parse_opcode_text_with_lookup(instructions.Extend, lookups.EXTEND_LOOKUP)
parse_truncop = parse_opcode_text_with_lookup(instructions.Truncate, lookups.TRUNC_LOOKUP)
parse_convertop = parse_opcode_text_with_lookup(instructions.Convert, lookups.CONVERT_LOOKUP)
parse_demoteop = parse_simple_op(instructions.Demote)
parse_promoteop = parse_simple_op(instructions.Promote)
parse_reinterpretop = parse_opcode_text_with_lookup(instructions.Reinterpret, lookups.REINTERPRET_LOOKUP)  # noqa: E501


#
# Memory Ops
#
parse_memory_grow = parse_simple_op(instructions.MemoryGrow)
parse_memory_size = parse_simple_op(instructions.MemorySize)


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
parse_drop = parse_simple_op(instructions.Drop)
parse_select = parse_simple_op(instructions.Select)


#
# Control Ops
#
parse_return = parse_simple_op(instructions.Return)
parse_nop = parse_simple_op(instructions.Nop)
parse_end = parse_simple_op(instructions.End)
parse_unreachable = parse_simple_op(instructions.Unreachable)


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


def parse_folded_block(s, loc, toks):
    name, block_type, instrs = toks
    if instrs is None:
        instrs = instructions.End.as_tail()
    else:
        instrs = instrs + instructions.End.as_tail()

    base_block = instructions.Block(block_type, instrs)
    if name is None:
        block = base_block
    else:
        block = ir.NamedBlock(name, base_block)
    return block


def parse_folded_loop(s, loc, toks):
    name, block_type, instrs = toks
    if instrs is None:
        instrs = instructions.End.as_tail()
    else:
        instrs = instrs + instructions.End.as_tail()

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
    else:
        else_instrs = else_instrs + instructions.End.as_tail()

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


def parse_typeuse_params_and_results(s, loc, toks):
    params, results = toks
    return ir.UnresolvedFunctionType(params, results)


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
def parse_valtype(s, loc, toks):
    return datatypes.ValType.from_str(_exactly_one(toks))


def parse_valtypes(s, loc, toks):
    return tuple(toks)


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
