from wasm._utils.toolz import (
    concat,
    curry,
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
                yield from _normalize_expressions(expr)
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


def parse_folded_block(s, loc, toks):
    name, block_type, instrs = toks
    base_block = instructions.Block(block_type, instrs)
    if name is None:
        block = base_block
    else:
        block = ir.NamedBlock(name, base_block)
    return block


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


def parse_local_idx(s, loc, toks):
    var = _exactly_one(toks)

    if isinstance(var, int):
        return datatypes.LocalIdx(var)
    elif isinstance(var, str):
        return ir.UnresolvedLocalIdx(var)
    else:
        raise Exception("INVALID")


#
# Primative Value Types
#
def parse_integer_string(s, loc, toks):
    raw_num = _exactly_one(toks)
    return int(raw_num)
