import functools
import operator

from pyparsing import (
    StringStart,
    StringEnd,
    LineEnd,
    Regex,
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

from wasm.datatypes import MemoryIdx, TableIdx

from . import parsers


def any_literal(*literals):
    return functools.reduce(operator.xor, map(Literal, literals))


def maybe(thing, default=None):
    return Optional(ws + thing, default=default)


def many(thing):
    return thing + ZeroOrMore(ws + thing)


linechar = Regex(r'[\u0000-\uD7FF\U0000E000-\U0010FFFF]+')
line_comment = Literal(";;").suppress() + linechar + LineEnd()
block_char = Regex(
    r'('
    r'[\u0000-\u0027]|'
    # exclude 0x28 "("
    r'[\u0029-\u003A]|'
    # exclude 0x3b ";"
    r'[\u003C-\uD7FF]|'
    # dunno why by \U000E0000-\U000E0019 matches ";"
    r'[\U0000E001A-\U0010FFFF]'
    r')+'
)
ALLOWED_SEMICOLON = Combine(Literal(";") + ~Literal(")"))
ALLOWED_OPEN_PAREN = Combine(Literal("(") + ~Literal(";"))
block_comment = Forward()
block_comment << Literal("(;").suppress() + ZeroOrMore(block_char ^ ALLOWED_SEMICOLON ^ ALLOWED_OPEN_PAREN ^ block_comment) + Literal(";)").suppress()  # noqa: E501
comment = line_comment ^ block_comment

WHITESPACE = Word(' \n\t\r')
ws = Optional(WHITESPACE ^ comment).suppress()

open = Literal('(').suppress()
close = Literal(')').suppress()

# these should not be suppressed as they are used to build text representations
# of opcodes
DOT = Literal('.')
UNDERSCORE = Literal('_')

# Suppressable
ZEROX = Literal('0x').suppress()

# a supressed underscore
LODASH = UNDERSCORE.copy().suppress()

HEXDIGIT = Word(hexnums, exact=1)
HEXNUM = Combine(HEXDIGIT + ZeroOrMore(Optional(LODASH) + HEXDIGIT))

HEX = (ZEROX + HEXNUM).setParseAction(parsers.parse_hex_to_int)

EQUALS = Literal("=").suppress()

PLUS = Literal('+')
MINUS = Literal('-')

NAN = Literal('nan')
INF = Literal('inf')

COLON = Literal(':')

DIGIT = Word(nums, exact=1)
NUM = Combine(DIGIT + ZeroOrMore(Optional(LODASH) + DIGIT)).setParseAction(parsers.parse_integer_string)  # noqa: E501

#
# Keywords
#
RESULT = Literal("result").suppress()
PARAM = Literal("param").suppress()
LOCAL = Literal('local').suppress()

CALL = Literal("call").suppress()
CALL_INDIRECT = Literal("call_indirect").suppress()

BR_IF = Literal("br_if").suppress()
BR_TABLE = Literal("br_table").suppress()
BR = Literal("br").suppress()

BLOCK = Literal("block").suppress()

LOOP = Literal("loop").suppress()

IF = Literal("if").suppress()
THEN = Literal("then").suppress()
ELSE = Literal("else").suppress()

FUNCREF = Literal("funcref").suppress()
MUT = Literal("mut").suppress()

OFFSET = Literal("offset").suppress()
ALIGN = Literal("align").suppress()

TYPE = Literal("type").suppress()
FUNC = Literal("func").suppress()
TABLE = Literal("table").suppress()
MEMORY = Literal("memory").suppress()
GLOBAL = Literal("global").suppress()
ELEM = Literal("elem").suppress()
DATA = Literal("data").suppress()
START = Literal("start").suppress()
IMPORT = Literal("import").suppress()
EXPORT = Literal("export").suppress()

MODULE = Literal("module").suppress()


#
# Value Primatives
#
SIGN = Optional(PLUS ^ MINUS)


#
# Integers
#
NAT = NUM ^ HEX
INT = NAT ^ Combine(SIGN + NAT)


#
# FLoats
#
FRAC = DIGIT + ZeroOrMore(Optional(LODASH) + DIGIT)
HEXFRAC = HEXDIGIT + ZeroOrMore(Optional(LODASH) + HEXDIGIT)

EXP_E = CaselessLiteral('e')
EXP_P = CaselessLiteral('p')

FLOAT_1 = Combine(NUM + DOT + FRAC)
FLOAT_2 = Combine(NUM + EXP_P + SIGN + NUM)
FLOAT_3 = Combine(NUM + DOT + FRAC + EXP_E + SIGN + NUM)


HEXFLOAT_1 = Combine(ZEROX + HEXNUM + DOT + HEXFRAC)
HEXFLOAT_2 = Combine(ZEROX + HEXNUM + EXP_P + SIGN + NUM)
HEXFLOAT_3 = Combine(ZEROX + HEXNUM + DOT + HEXFRAC + EXP_P + SIGN + NUM)

NUM_FLOAT = FLOAT_1 ^ FLOAT_2 ^ FLOAT_3
HEXFLOAT = HEXFLOAT_1 ^ HEXFLOAT_2 ^ HEXFLOAT_3

FLOAT = Combine(SIGN + (NUM_FLOAT ^ HEXFLOAT ^ INF ^ NAN ^ Combine(NAN + COLON + ZEROX + HEXNUM)))


#
# Strings
#
DOLLAR = Literal('$')
idchars = alphanums + "_.+-*/\\^~=<>!?@#$%&|:'`"
SYMBOL_ID = Combine(Literal('$') + Word(idchars))
ID = NAT ^ SYMBOL_ID

DOUBLE_QUOTE = Literal('"').suppress()

base_string_chars = (
    r'['
    r'\u0020\u0021'
    # exclude \u0022: doublequote
    r'\u0023-\u0026'
    # exclude \u0027: singlequote
    r'\u0028-\u007e'
    # exclude \u007f: excluded by spec
    r'\u0080-\uD7FF'
    r'\U0000E000-\U0010FFFF'
    r']+'
)

string_chars = Regex(base_string_chars)
string_chars_then_escape = Regex(
    base_string_chars + (
        # Make the preceeding character matching non-greedy
        r'?'
        # terminate at any of the forward slash escape
        r'(?='
        r'(\\[a-fA-F0-9]{2})|'
        r'(\\n|\\t|\\r|\\\\|\\\'|\\")|'
        r'(\\u\{[a-fA-F0-9]+\})'
        r')'
    )
)
string_escaped_values = any_literal(
    r"\n",
    r"\t",
    r"\r",
    r"\\",
    r"\'",
    r'\"',
).setParseAction(parsers.parse_escaped_char)
string_escaped_unicode = Combine(Literal("\\u{") + HEXNUM + Literal('}')).setParseAction(parsers.parse_escaped_unicode)  # noqa: E501
string_escaped_hex_char = Combine(Literal("\\") + HEXDIGIT + HEXDIGIT).setParseAction(parsers.parse_escaped_char)  # noqa: E501

any_escaped_char = OneOrMore(string_escaped_values ^ string_escaped_unicode ^ string_escaped_hex_char)  # noqa: E501

stringbody = any_escaped_char | string_chars_then_escape | string_chars

STRING = Combine(DOUBLE_QUOTE + ZeroOrMore(stringbody) + DOUBLE_QUOTE)

VAR = NAT ^ SYMBOL_ID
VALUE = INT ^ FLOAT
I32 = Literal("i32")
I64 = Literal("i64")
F32 = Literal("f32")
F64 = Literal("f64")

VALTYPE = (I32 ^ I64 ^ F32 ^ F64).setParseAction(parsers.parse_valtype)
valtypes = (VALTYPE + ZeroOrMore(ws + VALTYPE)).setParseAction(parsers.parse_to_tuple)


#
# Locals, Params, Results
#
local_named = (LOCAL + ws + ID + ws + VALTYPE).setParseAction(parsers.parse_named_local)
local_bulk = (LOCAL + Optional(ws + valtypes)).setParseAction(parsers.parse_bulk_locals)  # noqa: E501

local = open + (local_named | local_bulk) + close
locals = many(local).setParseAction(parsers.concatenate_tokens)


param_named = (PARAM + ws + ID + ws + VALTYPE).setParseAction(parsers.parse_named_param)
param_bulk = (PARAM + Optional(ws + valtypes)).setParseAction(parsers.parse_bulk_params)

param = open + (param_named | param_bulk) + close
params = many(param).setParseAction(parsers.concatenate_tokens)


result = open + RESULT + Optional(ws + valtypes) + close
results = many(result).setParseAction(parsers.concatenate_tokens)


#
# Indices Types
#
local_idx = VAR.copy().setParseAction(parsers.parse_local_idx)
global_idx = VAR.copy().setParseAction(parsers.parse_global_idx)
function_idx = VAR.copy().setParseAction(parsers.parse_function_idx)
label_idx = VAR.copy().setParseAction(parsers.parse_label_idx)
memory_idx = VAR.copy().setParseAction(parsers.parse_memory_idx)
table_idx = VAR.copy().setParseAction(parsers.parse_table_idx)
type_idx = VAR.copy().setParseAction(parsers.parse_type_idx)

function_indices = many(function_idx).setParseAction(parsers.parse_to_tuple)


#
# Typeuse
#
typeuse_direct = open + TYPE + ws + type_idx + close

typeuse_only_params = params.copy().setParseAction(parsers.parse_typeuse_only_params)
typeuse_only_results = results.copy().setParseAction(parsers.parse_typeuse_only_results)
typeuse_params_and_results = (params + ws + results).setParseAction(parsers.parse_function_type)  # noqa: E501

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

SIGNED = Literal('s') ^ Literal('u')

wrapop = Combine(I32 + Literal(".wrap_") + I64).setParseAction(parsers.parse_wrapop)
extendop = Combine(I64 + Literal(".extend_") + I32 + UNDERSCORE + SIGNED).setParseAction(parsers.parse_extendop)  # noqa: E501

truncop = Combine(INTEGER_TYPES + Literal(".trunc_") + FLOAT_TYPES + UNDERSCORE + SIGNED).setParseAction(parsers.parse_truncop)  # noqa: E501
convertop = Combine(FLOAT_TYPES + Literal(".convert_") + INTEGER_TYPES + UNDERSCORE + SIGNED).setParseAction(parsers.parse_convertop)  # noqa: E501

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
offset = Combine(OFFSET + EQUALS).suppress() + NAT

align_value = any_literal("1", "2", "4", "8", "16", "32")
align = Combine(ALIGN + EQUALS) + align_value

memory_arg = maybe(offset) + maybe(align)

memory_bitsize = any_literal("8", "16", "32")

LOAD_INTEGER_PREFIX = Combine(INTEGER_TYPES + Literal(".load"))
LOAD_FLOAT_PREFIX = Combine(FLOAT_TYPES + Literal(".load"))

memory_load_integer_op = Combine(LOAD_INTEGER_PREFIX + Optional(memory_bitsize + UNDERSCORE + SIGNED))  # noqa: E501
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


call_op = (CALL + ws + function_idx).setParseAction(parsers.parse_call_op)
call_indirect_op = (CALL_INDIRECT + ws + typeuse).setParseAction(parsers.parse_call_indirect_op)


br_if_op = (BR_IF + ws + label_idx).setParseAction(parsers.parse_br_if_op)
br_table_op = (BR_TABLE + OneOrMore(ws + label_idx)).setParseAction(parsers.parse_br_table_op)
br_op = (BR + ws + label_idx).setParseAction(parsers.parse_br_op)


#
# Block Control Ops
#
blk_name = maybe(SYMBOL_ID)
blk_type = maybe(results, default=())
blk_instrs = maybe(instrs, default=()).setParseAction(parsers.parse_blk_instrs)
blk_body = blk_name + blk_type + blk_instrs

folded_block_op = (BLOCK + blk_body).setParseAction(parsers.parse_folded_block)
block_instr = (folded_block_op + ws + end_op + maybe(matchPreviousExpr(BLOCK + blk_name))).setParseAction(parsers.parse_block)  # noqa: E501


folded_loop_op = (LOOP + blk_body).setParseAction(parsers.parse_folded_loop)
loop_instr = folded_loop_op + ws + end_op + maybe(matchPreviousExpr(LOOP + blk_name))

folded_instr = Forward()

folded_then = open + THEN + maybe(instrs) + close
folded_else = open + ELSE + blk_instrs + close
folded_if_op = (IF + blk_name + blk_type + maybe(folded_instr) + ws + folded_then + maybe(folded_else)).setParseAction(parsers.parse_folded_if)  # noqa: E501

else_inline = ELSE + maybe(matchPreviousExpr(IF + blk_name)) + blk_instrs
if_instr = IF + blk_body + maybe(else_inline) + ws + end_op + maybe(matchPreviousExpr(IF + blk_name))  # noqa: E501

control_op = return_op ^ nop_op ^ unreachable_op ^ call_op ^ call_indirect_op ^ br_op ^ br_if_op ^ br_table_op  # noqa: E501

op = numeric_op ^ memory_op ^ variable_op ^ parametric_op ^ control_op

folded_instrs = many(folded_instr)
folded_op = (op + maybe(folded_instrs, default=())).setParseAction(parsers.parse_folded_op)

folded_instr << open + (folded_op ^ folded_block_op ^ folded_loop_op ^ folded_if_op) + close

instr = op ^ block_instr ^ loop_instr ^ if_instr ^ folded_instr
instrs << many(instr).setParseAction(parsers.parse_instrs)
expr = instrs.copy().setParseAction(parsers.parse_expr)


#
# Individual "type" components
#
limits = (NAT + maybe(NAT)).setParseAction(parsers.parse_limits)

function_type = (open + FUNC + maybe(params, ()) + maybe(results, ()) + close).setParseAction(parsers.parse_function_type)  # noqa: E501

global_const = VALTYPE.copy().setParseAction(parsers.parse_valtype, parsers.parse_global_const)
global_mut = (open + MUT + ws + VALTYPE + close).setParseAction(parsers.parse_global_mut)
global_type = global_const ^ global_mut

elem_type = FUNCREF.setParseAction(parsers.parse_elem_type)
table_type = (limits + ws + elem_type).setParseAction(parsers.parse_table_type)

memory_type = limits.copy().setParseAction(parsers.parse_memory_type)

#
# Exports
#
export_function = FUNC + ws + function_idx
export_global = GLOBAL + ws + global_idx
export_table = TABLE + ws + table_idx
export_memory = MEMORY + ws + memory_idx

export_kind = open + (export_function ^ export_global ^ export_table ^ export_memory) + close
export = (open + EXPORT + ws + STRING + ws + export_kind + close).setParseAction(parsers.parse_export)  # noqa: E501
exports = many(export)


#
# Imports
#
import_function = (FUNC + maybe(typeuse)).setParseAction(parsers.parse_import_function)
import_global = GLOBAL + ws + global_type
import_table = TABLE + ws + table_type
import_memory = MEMORY + ws + memory_type

import_kind = open + (import_function ^ import_global ^ import_table ^ import_memory) + close
import_ = (open + IMPORT + ws + STRING + ws + STRING + ws + import_kind + close).setParseAction(parsers.parse_import)  # noqa: E501


#
# Inlineable Import/Export
#
export_inline = open + EXPORT + ws + STRING + close
import_inline = (open + IMPORT + ws + STRING + ws + STRING + close).setParseAction(parsers.parse_to_tuple)  # noqa: E501

inline_exports = (export_inline + ZeroOrMore(ws + export_inline)).setParseAction(parsers.parse_to_tuple)  # noqa: E501


#
# Type
#
type = (open + TYPE + maybe(SYMBOL_ID) + ws + function_type).setParseAction(parsers.parse_type)


#
# Functions
#
function = (open + FUNC + maybe(SYMBOL_ID) + maybe(export_inline) + maybe(typeuse) + maybe(locals) + maybe(instrs) + close).setParseAction(parsers.parse_function)  # noqa: E501
folded_function_import = (open + FUNC + maybe(SYMBOL_ID) + maybe(inline_exports, ()) + ws + import_inline + maybe(typeuse) + close).setParseAction(parsers.parse_folded_function_import)  # noqa: E501


#
# Globals
#
global_ = (open + GLOBAL + maybe(SYMBOL_ID) + ws + global_type + ws + expr + close).setParseAction(parsers.parse_global)  # noqa: E501


elements_inline = open + ELEM + function_indices + close

#
# Tables
#
table = (open + TABLE + maybe(SYMBOL_ID) + maybe(inline_exports, ()) + maybe(import_inline) + ws + table_type + close).setParseAction(parsers.parse_table)  # noqa: E501
table_with_elements = (open + TABLE + maybe(SYMBOL_ID) + ws + elem_type + ws + elements_inline + close).setParseAction(parsers.parse_table_with_elements)  # noqa: E501


datastring = many(STRING).setParseAction(parsers.parse_datastring)
data_inline = open + DATA + maybe(datastring, b'') + close

#
# Memory
#
memory = (open + MEMORY + maybe(SYMBOL_ID) + maybe(inline_exports, ()) + maybe(import_inline) + ws + memory_type + close).setParseAction(parsers.parse_memory)  # noqa: E501
memory_with_data = (open + MEMORY + maybe(SYMBOL_ID) + ws + data_inline + close).setParseAction(parsers.parse_memory_with_data)  # noqa: E501


#
# Start Function
#
start = (open + START + ws + function_idx + close).setParseAction(parsers.parse_start_function)


#
# Element Segments
#
offset_expr = open + OFFSET + ws + expr + close
offset_instr = instr.copy().setParseAction(parsers.parse_offset_instr)

element_segment = (open + ELEM + maybe(table_idx, TableIdx(0)) + ws + (offset_expr ^ offset_instr) + maybe(function_indices, ())).setParseAction(parsers.parse_element_segment)  # noqa: E501


#
# Data Segment
#
data_segment = (open + DATA + maybe(memory_idx, MemoryIdx(0)) + ws + (offset_expr ^ offset_instr) + maybe(datastring, b'')).setParseAction(parsers.parse_data_segment)  # noqa: E501


#
# Modules
#
module_field = type ^ import_ ^ function ^ table ^ memory ^ global_ ^ export ^ start ^ element_segment ^ data_segment  # noqa: E501
module_fields = many(module_field)

module = (open + MODULE + maybe(SYMBOL_ID) + maybe(module_fields, ()) + close).setParseAction(parsers.parse_module)  # noqa: E501
top_level_module = StringStart() + module_fields + StringEnd()
