from lark import Lark

from .transformer import WasmTransformer

GRAMMAR = r"""
_WHITESPACE: (" " | /\n/ | /\t/)+

DIGIT: /[0-9]/
NUM: DIGIT+

_CLOSE: _WHITESPACE? ")"
_OPEN: "(" _WHITESPACE?

HEX: /[0-9a-fA-F]/
HEXNUM: HEX+

NAT: NUM | "0x" HEXNUM
INT: NAT | "+" NAT | "-" NAT
FLOAT_HEX: "0x" HEXNUM "." HEXNUM? (("p" | "P") NUM)?
FLOAT_NUM: NUM "." NUM? (("e" | "E") NUM)?
FLOAT: FLOAT_NUM | FLOAT_HEX

LETTER: /[a-zA-Z]/
CHAR: LETTER | DIGIT

NAME_CHAR: LETTER | DIGIT
    | "_" | "." | "+" | "-" | "*" | "/" | "\\"
    | "^" | "~" | "=" | "<" | ">" | "!" | "?"
    | "@" | "#" | "$" | "%" | "&" | "|" | ":"
    | "'" | "`"
NAME: "$" NAME_CHAR+

STRING_CHAR: CHAR | /\n/ | /\t/ | /\\/ | "'" | "\\\"" | "\\" HEX HEX | /\\u/ HEX+
STRING: "\"" STRING_CHAR* "\""

VAR: NAT | NAME

_ws:     _WHITESPACE
_close:  _CLOSE
_open:   _OPEN

nat:    NAT
int:    INT
float:  FLOAT
?name:   NAME
string: STRING

_value: int | float
_var: nat | name
_vars: _var (_ws _var)*

I32: "i32"
I64: "i64"
F32: "f32"
F64: "f64"

i32: I32
i64: I64
f32: F32
f64: F64

VALTYPE: I32 | I64 | F32 | F64

valtype: VALTYPE
_valtypes_tail: _ws valtype
valtypes: valtype _valtypes_tail*

INTEGER_TYPES: I32 | I64
FLOAT_TYPES:   F32 | F64

local_named: "local" _ws name _ws valtype
local_bare : "local" _ws        valtypes

?local: _open (local_named | local_bare) _close
_locals_tail: _ws local
locals: local _locals_tail*

_RESULT: "result"

?result: _open _RESULT (_ws valtypes)? _close
_results_tail: _ws result
results: result _results_tail*

_PARAM: "param"

param_named: _PARAM _ws name _ws valtype
param_bare : _PARAM (_ws        valtypes)?

?param: _open (param_named | param_bare) _close
_params_tail: _ws param
params: param _params_tail*

SIGN: "s" | "u"

sign: SIGN

INTEGER_UNOP_NAMES: "clz" | "ctz" | "popcnt"
INTEGER_BINOP_NAMES: "add" | "sub" | "mul"
    | "div_" SIGN | "rem_" SIGN
    | "and" | "or" | "xor"
    | "shl" | "shr_s" | "shr_u" | "rotl" | "rotr"
INTEGER_RELOP_NAMES: "eq" | "ne" | "lt_" SIGN | "gt_" SIGN | "le_" SIGN | "ge_" SIGN

FLOAT_UNOP_NAMES : "abs" | "neg" | "ceil" | "floor" | "trunc" | "nearest" | "sqrt"
FLOAT_BINOP_NAMES: "add" | "sub" | "mul" | "div" | "min" | "max" | "copysign"
FLOAT_RELOP_NAMES: "eq" | "ne" | "lt" | "gt" | "le" | "ge"

INTEGER_UNOP : INTEGER_TYPES "." INTEGER_UNOP_NAMES
INTEGER_BINOP: INTEGER_TYPES "." INTEGER_BINOP_NAMES
INTEGER_RELOP: INTEGER_TYPES "." INTEGER_RELOP_NAMES

FLOAT_UNOP : FLOAT_TYPES "." FLOAT_UNOP_NAMES
FLOAT_BINOP: FLOAT_TYPES "." FLOAT_BINOP_NAMES
FLOAT_RELOP: FLOAT_TYPES "." FLOAT_RELOP_NAMES

CONSTOP_PREFIXE: VALTYPE ".const"

TESTOP.2: INTEGER_TYPES ".eqz"

testop: TESTOP
unop : INTEGER_UNOP | FLOAT_UNOP
binop: INTEGER_BINOP | FLOAT_BINOP
relop: INTEGER_RELOP | FLOAT_RELOP

WRAPOP  : I32 ".wrap_" I64
_EXTENDOP: I64 ".extend_" I32 "_"

wrapop: WRAPOP
extendop: _EXTENDOP sign

TRUNCOP: INTEGER_TYPES ".trunc_" FLOAT_TYPES "_" SIGN
CONVERTOP: FLOAT_TYPES ".convert_" INTEGER_TYPES "_" SIGN

truncop: TRUNCOP
convertop: CONVERTOP

DEMOTEOP : F32 ".demote_" F64
PROMOTEOP: F64 ".promote_" F32

demoteop: DEMOTEOP
promoteop: PROMOTEOP

REINTERPRETOP: I32 ".reinterpret_" F32 | I64 ".reinterpret_" F64
    | F32 ".reinterpret_" I32 | F64 ".reinterpret_" I64

reinterpretop: REINTERPRETOP

?const_op: CONSTOP_PREFIXE _WHITESPACE _value

?numeric_op: const_op | testop
    | unop | relop | binop
    | wrapop | extendop
    | truncop | convertop
    | demoteop | promoteop
    | reinterpretop

_OFFSET_PREFIX: "offset="
_ALIGN_PREFIX: "align="
ALIGN_VALUE: "1" | "2" | "4" | "8" | "16" | "32"

offset: _OFFSET_PREFIX nat
align_value: ALIGN_VALUE
align: _ALIGN_PREFIX align_value

?memory_arg: (_ws offset)? (_ws align)?

MEMORY_ACCESS_BITSIZE: "8" | "16" | "32"

memory_access_bitsize: MEMORY_ACCESS_BITSIZE

MEMORY_LOAD_INTEGER_PREFIX: INTEGER_TYPES ".load"
MEMORY_LOAD_FLOAT_PREFIX: FLOAT_TYPES ".load"

?memory_load_integer_op: MEMORY_LOAD_INTEGER_PREFIX (memory_access_bitsize "_" SIGN memory_arg)?
?memory_load_float_op: MEMORY_LOAD_FLOAT_PREFIX memory_arg

?memory_load_op: memory_load_float_op | memory_load_integer_op

MEMORY_STORE_INTEGER_PREFIX: INTEGER_TYPES ".store"
MEMORY_STORE_FLOAT_PREFIX: FLOAT_TYPES ".store"

?memory_store_integer_op: MEMORY_STORE_INTEGER_PREFIX (memory_access_bitsize memory_arg)?
?memory_store_float_op: MEMORY_STORE_FLOAT_PREFIX memory_arg

?memory_store_op: memory_store_float_op | memory_store_integer_op

memory_access_op: memory_load_op | memory_store_op

MEMORY_SIZE_OP: "memory.size"
MEMORY_GROW_OP: "memory.grow"

memory_size_op: MEMORY_SIZE_OP
memory_grow_op: MEMORY_GROW_OP

?memory_op: memory_access_op
    | memory_size_op
    | memory_grow_op


LOCAL_GET: "local.get"
LOCAL_SET: "local.set"
LOCAL_TEE: "local.tee"
GLOBAL_GET: "global.get"
GLOBAL_SET: "global.set"

localidx: _var
globalidx: _var

_local_variable_op:  (LOCAL_GET | LOCAL_SET | LOCAL_TEE) _ws localidx
_global_variable_op: (GLOBAL_GET | GLOBAL_SET) _ws globalidx

variable_op: _local_variable_op | _global_variable_op

DROP: "drop"
SELECT: "select"

drop_op: DROP
select_op: SELECT

?parametric_op: drop_op | select_op

_TYPE: "type"

typeidx: _var

typeuse_params_and_results: param _ws results
typeuse_params_only: params
typeuse_results_only: results
_typeuse_direct: _open _TYPE _ws typeidx _close
?typeuse: _typeuse_direct | typeuse_params_only | typeuse_params_and_results | typeuse_results_only

FUNC: "func"

func_type: _open FUNC _ws typeuse _close

_CALL:          "call"
_CALL_INDIRECT: "call_indirect"
_BR_IF:         "br_if"
_BR_TABLE:      "br_table"
_BR:            "br"
_RETURN:        "return"
_UNREACHABLE:   "unreachable"
_NOP:           "nop"
_END:           "end"

call_op: _CALL _ws typeidx
call_indirect_op: _CALL_INDIRECT _ws typeuse

br_if_op:    _BR_IF    _ws _var
br_table_op: _BR_TABLE _ws _vars
br_op:       _BR       _ws _var

return_op:      _RETURN
unreachable_op: _UNREACHABLE
nop_op:         _NOP
end_op:         _END

?control_op: unreachable_op | nop_op | return_op
    | br_if_op | br_table_op | br_op
    | call_op | call_indirect_op

?op: numeric_op | memory_op | variable_op | parametric_op | control_op

?expr: _open op _close
_exprs_tail: _ws expr
?exprs: expr _exprs_tail*

?start: exprs | locals | results | params
"""

parser = Lark(GRAMMAR, parser="lalr", transformer=WasmTransformer())
