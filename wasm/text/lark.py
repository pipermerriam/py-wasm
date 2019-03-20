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

?result: _open "result" (_ws valtypes)? _close
_results_tail: _ws result
results: result _results_tail*

param_named: "param" _ws name _ws valtype
param_bare : "param" (_ws        valtypes)?

?param: _open (param_named | param_bare) _close
_params_tail: _ws param
params: param _params_tail*

SIGN: "s" | "u"

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

UNOP : INTEGER_UNOP | FLOAT_UNOP
BINOP: INTEGER_BINOP | FLOAT_BINOP
RELOP: INTEGER_RELOP | FLOAT_RELOP

TESTOP: INTEGER_TYPES ".eqz"

WRAPOP  : I32 ".wrap_" I64
EXTENDOP: I64 ".extend_" I32 "_" SIGN

TRUNCOP: INTEGER_TYPES ".trunc_" FLOAT_TYPES "_" SIGN
CONVERTOP: FLOAT_TYPES ".convert_" INTEGER_TYPES "_" SIGN

DEMOTEOP : F32 ".demote_" F64
PROMOTEOP: F64 ".promote_" F32

REINTERPRETOP: I32 ".reinterpret_" F32 | I64 ".reinterpret_" F64
    | F32 ".reinterpret_" I32 | F64 ".reinterpret_" I64

const_op: CONSTOP_PREFIXE _WHITESPACE _value

numeric_op: const_op | TESTOP
    | UNOP | RELOP | BINOP
    | WRAPOP | EXTENDOP
    | TRUNCOP | CONVERTOP
    | DEMOTEOP | PROMOTEOP
    | REINTERPRETOP

op: numeric_op

?expr: _open op _close
_exprs_tail: _ws expr
exprs: expr _exprs_tail*

?start: exprs | locals | results | params
"""

parser = Lark(GRAMMAR, parser="lalr", transformer=WasmTransformer())
