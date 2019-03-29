import functools
import operator

from pyparsing import (
    OneOrMore,
    matchPreviousExpr,
    Or,
    Forward,
    Combine,
    ZeroOrMore,
    CaselessLiteral,
    Optional,
    Literal,
    Word,
    hexnums,
    nums,
    alphanums,
)

from . import parsers


def any_literal(*literals):
    return functools.reduce(operator.xor, map(Literal, literals))


def maybe(expr, default=None):
    return Optional(ws + expr, default=default)


WHITESPACE = Word(' \n\t\r')
ws = Optional(WHITESPACE).suppress()

open = Literal('(').suppress()
close = Literal(')').suppress()

NUM = Word(nums).setParseAction(parsers.parse_integer_string)
HEXVAL = Word(hexnums, exact=2)
HEXNUM = OneOrMore(HEXVAL)

# these should not be suppressed as they are used to build text representations
# of opcodes
ZEROX = Literal('0x')
DOT = Literal('.')
UNDERSCORE = Literal('_')

HEX = ZEROX + HEXNUM

PLUS = Literal('+')
MINUS = Literal('-')

NAT = NUM ^ HEX
INT = NAT ^ Combine(PLUS + NAT) ^ Combine(MINUS + NAT)
FLOAT_AS_HEX = Combine(ZEROX + HEXNUM + DOT + Optional(HEXNUM) + Optional(CaselessLiteral('p') + NUM))  # noqa: E501
FLOAT_AS_NUM = Combine(NUM + DOT + Optional(NUM) + Optional(CaselessLiteral('e') + NUM))
FLOAT = FLOAT_AS_HEX ^ FLOAT_AS_NUM

DOLLAR = Literal('$')
namechars = alphanums + "_.+-*/\\^~=<>!?@#$%&|:'`"
NAME = Combine(Literal('$') + Word(namechars))

DOUBLE_QUOTE = Literal('"').suppress()

string_chars = Word(alphanums + "\n\t\r\\' ")
string_escaped_unicode = Literal("\\u") + HEXNUM
string_raw_hex = Literal("\\") + HEXVAL
stringbody = string_chars ^ string_raw_hex ^ string_escaped_unicode

# TODO: this doesn't fully cover the allowed characters.
string = Combine(DOUBLE_QUOTE + stringbody + DOUBLE_QUOTE)

VAR = NAT ^ NAME
VALUE = INT ^ FLOAT

I32 = Literal("i32")
I64 = Literal("i64")
F32 = Literal("f32")
F64 = Literal("f64")

VALTYPE = (I32 ^ I64 ^ F32 ^ F64).setParseAction(parsers.parse_valtype)
valtypes = (VALTYPE + ZeroOrMore(ws + VALTYPE)).setParseAction(parsers.parse_valtypes)

LOCAL = Literal('local').suppress()

local_named = (LOCAL + ws + NAME + ws + VALTYPE).setParseAction(parsers.parse_named_local)
local_bulk = (LOCAL + Optional(ws + valtypes)).setParseAction(parsers.parse_bulk_locals)  # noqa: E501

local = open + (local_named | local_bulk) + close
locals = (local + ZeroOrMore(ws + local)).setParseAction(parsers.concatenate_tokens)


PARAM = Literal("param").suppress()

param_named = (PARAM + ws + NAME + ws + VALTYPE).setParseAction(parsers.parse_named_param)
param_bulk = (PARAM + Optional(ws + valtypes)).setParseAction(parsers.parse_bulk_params)

param = open + (param_named | param_bulk) + close
params = (param + ZeroOrMore(ws + param)).setParseAction(parsers.concatenate_tokens)

RESULT = Literal("result").suppress()

result = open + RESULT + Optional(ws + valtypes) + close
results = (result + ZeroOrMore(ws + result)).setParseAction(parsers.concatenate_tokens)


# Indices Types
local_idx = VAR.copy().setParseAction(parsers.parse_local_idx)
global_idx = VAR.copy().setParseAction(parsers.parse_global_idx)
function_idx = VAR.copy().setParseAction(parsers.parse_function_idx)
label_idx = VAR.copy().setParseAction(parsers.parse_label_idx)
memory_idx = VAR.copy().setParseAction(parsers.parse_memory_idx)
table_idx = VAR.copy().setParseAction(parsers.parse_table_idx)
type_idx = VAR.copy().setParseAction(parsers.parse_type_idx)


#
# Typeuse
#
TYPE = Literal("type").suppress()

typeuse_direct = open + TYPE + ws + type_idx + close

typeuse_only_params = params.copy().setParseAction(parsers.parse_typeuse_only_params)
typeuse_only_results = results.copy().setParseAction(parsers.parse_typeuse_only_results)
typeuse_params_and_results = (params + ws + results).setParseAction(parsers.parse_typeuse_params_and_results)  # noqa: E501

typeuse_function_type = typeuse_only_results ^ typeuse_only_params ^ typeuse_params_and_results
typeuse_linked = (typeuse_direct + ws + typeuse_function_type).setParseAction(parsers.parse_typeuse_linked)  # noqa: E501

typeuse = typeuse_direct ^ typeuse_function_type ^ typeuse_linked


#
# Numeric Ops
#
INTEGER_TYPES = I32 ^ I64
FLOAT_TYPES = F32 ^ F64

INTEGER_UNOP_NAMES = any_literal("clz", "ctz", "popcnt")
INTEGER_BINOP_NAMES = any_literal(
    "add", "sub", "mul",
    "div_s", "div_u",
    "rem_s", "rem_u",
    "and", "or", "xor",
    "shl", "shr_s", "shr_u",
    "rotl", "rotr",
)
INTEGER_RELOP_NAMES = any_literal(
    "eq", "ne",
    "lt_s", "lt_u",
    "gt_s", "gt_u",
    "le_s", "le_u",
    "ge_s", "ge_u",
)

FLOAT_UNOP_NAMES = any_literal("abs", "neg", "ceil", "floor", "trunc", "nearest", "sqrt")
FLOAT_BINOP_NAMES = any_literal("add", "sub", "mul", "div", "min", "max", "copysign")
FLOAT_RELOP_NAMES = any_literal("eq", "ne", "lt", "gt", "le", "ge")

INTEGER_UNOP = Combine(INTEGER_TYPES + DOT + INTEGER_UNOP_NAMES)
INTEGER_BINOP = Combine(INTEGER_TYPES + DOT + INTEGER_BINOP_NAMES)
INTEGER_RELOP = Combine(INTEGER_TYPES + DOT + INTEGER_RELOP_NAMES)

FLOAT_UNOP = Combine(FLOAT_TYPES + DOT + FLOAT_UNOP_NAMES)
FLOAT_BINOP = Combine(FLOAT_TYPES + DOT + FLOAT_BINOP_NAMES)
FLOAT_RELOP = Combine(FLOAT_TYPES + DOT + FLOAT_RELOP_NAMES)

const_op_keyword = Combine(VALTYPE + DOT + Literal("const"))
const_op = (const_op_keyword + ws + VALUE).setParseAction(parsers.parse_const_op)

unop = (INTEGER_UNOP ^ FLOAT_UNOP).setParseAction(parsers.parse_unop)
binop = (INTEGER_BINOP ^ FLOAT_BINOP).setParseAction(parsers.parse_binop)
relop = (INTEGER_RELOP ^ FLOAT_RELOP).setParseAction(parsers.parse_relop)
testop = Combine(INTEGER_TYPES + Literal(".eqz")).setParseAction(parsers.parse_testop)

SIGN = Literal('s') ^ Literal('u')

wrapop = Combine(I32 + Literal(".wrap_") + I64).setParseAction(parsers.parse_wrapop)
extendop = Combine(I64 + Literal(".extend_") + I32 + UNDERSCORE + SIGN).setParseAction(parsers.parse_extendop)  # noqa: E501

truncop = Combine(INTEGER_TYPES + Literal(".trunc_") + FLOAT_TYPES + UNDERSCORE + SIGN).setParseAction(parsers.parse_truncop)  # noqa: E501
convertop = Combine(FLOAT_TYPES + Literal(".convert_") + INTEGER_TYPES + UNDERSCORE + SIGN).setParseAction(parsers.parse_convertop)  # noqa: E501

demoteop = Combine(F32 + ".demote_" + F64).setParseAction(parsers.parse_demoteop)
promoteop = Combine(F64 + ".promote_" + F32).setParseAction(parsers.parse_promoteop)

reinterpretop = Or((
    Combine(I32 + Literal(".reinterpret_") + F32),
    Combine(I64 + Literal(".reinterpret_") + F64),
    Combine(F32 + Literal(".reinterpret_") + I32),
    Combine(F64 + Literal(".reinterpret_") + I64),
)).setParseAction(parsers.parse_reinterpretop)

numeric_op = Or((
    const_op,
    testop,
    unop,
    relop,
    binop,
    wrapop,
    extendop,
    truncop,
    convertop,
    demoteop,
    promoteop,
    reinterpretop,
))


#
# Memory Ops
#
offset = Literal("offset=").suppress() + NAT

align_value = any_literal("1", "2", "4", "8", "16", "32")
align = Combine(Literal("align=").suppress() + align_value)

memory_arg = maybe(offset) + maybe(align)

memory_bitsize = any_literal("8", "16", "32")

LOAD_INTEGER_PREFIX = Combine(INTEGER_TYPES + Literal(".load"))
LOAD_FLOAT_PREFIX = Combine(FLOAT_TYPES + Literal(".load"))

memory_load_integer_op = Combine(LOAD_INTEGER_PREFIX + Optional(memory_bitsize + UNDERSCORE + SIGN))  # noqa: E501
memory_load_float_op = LOAD_FLOAT_PREFIX

memory_load_op = memory_load_integer_op ^ memory_load_float_op

STORE_INTEGER_PREFIX = Combine(INTEGER_TYPES + Literal(".store"))
STORE_FLOAT_PREFIX = Combine(FLOAT_TYPES + Literal(".store"))

memory_store_integer_op = Combine(STORE_INTEGER_PREFIX + Optional(memory_bitsize))
memory_store_float_op = STORE_FLOAT_PREFIX

memory_store_op = memory_store_float_op ^ memory_store_integer_op

memory_access_op = ((memory_load_op ^ memory_store_op) + memory_arg).setParseAction(parsers.parse_memory_access_op)  # noqa: E501

memory_size_op = Literal("memory.size").setParseAction(parsers.parse_memory_size)
memory_grow_op = Literal("memory.grow").setParseAction(parsers.parse_memory_grow)

memory_op = memory_access_op ^ memory_size_op ^ memory_grow_op


#
# Variable Ops
#
LOCAL_GET = Literal("local.get")
LOCAL_SET = Literal("local.set")
LOCAL_TEE = Literal("local.tee")
GLOBAL_GET = Literal("global.get")
GLOBAL_SET = Literal("global.set")

local_variable_op = (LOCAL_GET ^ LOCAL_SET ^ LOCAL_TEE) + ws + local_idx
global_variable_op = (GLOBAL_GET ^ GLOBAL_SET) + ws + global_idx

variable_op = (local_variable_op ^ global_variable_op).setParseAction(parsers.parse_variable_op)


#
# Parametric Ops
#
drop_op = Literal("drop").setParseAction(parsers.parse_drop)
select_op = Literal("select").setParseAction(parsers.parse_select)

parametric_op = drop_op ^ select_op


#
# Control Ops
#
return_op = Literal("return").setParseAction(parsers.parse_return)
nop_op = Literal("nop").setParseAction(parsers.parse_nop)
unreachable_op = Literal("unreachable").setParseAction(parsers.parse_unreachable)
end_op = Literal("end").setParseAction(parsers.parse_end)

instrs = Forward()
expr = Forward()
exprs = Forward()

CALL = Literal("call").suppress()
CALL_INDIRECT = Literal("call_indirect").suppress()

call_op = (CALL + ws + function_idx).setParseAction(parsers.parse_call_op)
call_indirect_op = (CALL_INDIRECT + ws + typeuse).setParseAction(parsers.parse_call_indirect_op)

BR_IF = Literal("br_if").suppress()
BR_TABLE = Literal("br_table").suppress()
BR = Literal("br").suppress()

br_if_op = (BR_IF + ws + label_idx).setParseAction(parsers.parse_br_if_op)
br_table_op = (BR_TABLE + OneOrMore(ws + label_idx)).setParseAction(parsers.parse_br_table_op)
br_op = (BR + ws + label_idx).setParseAction(parsers.parse_br_op)


#
# Block Control Ops
#
blk_name = Optional(ws + NAME, default=None)
blk_type = Optional(ws + results, default=())
blk_instrs = Optional(ws + instrs, default=None)
blk_body = blk_name + blk_type + blk_instrs

BLOCK = Literal("block").suppress()

folded_block_op = (BLOCK + blk_body).setParseAction(parsers.parse_folded_block)
block_instr = folded_block_op + ws + end_op + matchPreviousExpr(BLOCK + blk_name)


LOOP = Literal("loop").suppress()

folded_loop_op = (LOOP + blk_body).setParseAction(parsers.parse_folded_loop)
loop_instr = folded_loop_op + ws + end_op + matchPreviousExpr(LOOP + blk_name)


IF = Literal("if").suppress()
THEN = Literal("then").suppress()
ELSE = Literal("else").suppress()

folded_then = open + THEN + blk_instrs + close
folded_else = open + ELSE + blk_instrs + close
folded_if_op = (IF + blk_name + blk_type + maybe(exprs, default=()) + ws + folded_then + maybe(folded_else)).setParseAction(parsers.parse_folded_if)  # noqa: E501

else_inline = ELSE + matchPreviousExpr(IF + blk_name) + blk_instrs
if_instr = IF + blk_body + maybe(else_inline) + ws + end_op + matchPreviousExpr(IF + blk_name)  # noqa: E501

control_op = return_op ^ nop_op ^ unreachable_op ^ end_op ^ call_op ^ call_indirect_op ^ br_op ^ br_if_op ^ br_table_op  # noqa: E501

op = numeric_op ^ memory_op ^ variable_op ^ parametric_op ^ control_op

folded_op = (op + ws + exprs).setParseAction(parsers.parse_folded_op)

instr = op ^ block_instr ^ loop_instr ^ if_instr ^ expr
instrs << (instr + ZeroOrMore(ws + instr)).setParseAction(parsers.parse_instrs)

expr << open + (op ^ folded_op ^ folded_block_op ^ folded_loop_op ^ folded_if_op) + close
exprs << (expr + ZeroOrMore(ws + expr)).setParseAction(parsers.parse_exprs)

EXPORT = Literal("export").suppress()
FUNC = Literal("func").suppress()
GLOBAL = Literal("global").suppress()
TABLE = Literal("table").suppress()
MEMORY = Literal("memory").suppress()

export_function = FUNC + ws + function_idx
export_global = GLOBAL + ws + global_idx
export_table = TABLE + ws + table_idx
export_memory = MEMORY + ws + memory_idx

export_kind = open + (export_function ^ export_global ^ export_table ^ export_memory) + close
export = (open + EXPORT + ws + string + ws + export_kind + close).setParseAction(parsers.parse_export)  # noqa: E501
exports = export + ZeroOrMore(ws + export)

IMPORT = Literal("import").suppress()

export_inline = open + EXPORT + ws + string + close
import_inline = open + IMPORT + OneOrMore(ws + string) + close

function = open + FUNC + maybe(name) + maybe(export) + typeuse + maybe(locals) + maybe(instrs) + close  # noqa: E501
function_imports = open + FUNC + maybe(name) + ws + import_inline + maybe(typeuse) + close

"""
( func <name>? <func_type> <local>* <instr>* )
( func <name>? ( export <string> ) <...> )                         ;; = (export <string> (func <N>)) (func <name>? <...>)
( func <name>? ( import <string> <string> ) <func_type>)
