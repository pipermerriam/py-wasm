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
_STRING_QUOTE: "\""

VAR: NAT | NAME

_ws:     _WHITESPACE
_close:  _CLOSE
_open:   _OPEN

nat:    NAT
int:    INT
float:  FLOAT
?name:   NAME
string: _STRING_QUOTE STRING_CHAR* _STRING_QUOTE

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

_DROP: "drop"
_SELECT: "select"

drop_op: _DROP
select_op: _SELECT

?parametric_op: drop_op | select_op

_TYPE: "type"

typeidx: _var

_typeuse_direct: _open _TYPE _ws typeidx _close
typeuse_params_and_results: (_ws param)* (_ws result)*
typeuse: _ws _typeuse_direct | typeuse_params_and_results

_FUNC: "func"

?func_type: _open _FUNC typeuse _close

_CALL:          "call"
_CALL_INDIRECT: "call_indirect"
_BR_IF:         "br_if"
_BR_TABLE:      "br_table"
_BR:            "br"
_RETURN:        "return"
_UNREACHABLE:   "unreachable"
_NOP:           "nop"
_END:           "end"

funcidx: _var

call_op: _CALL _ws funcidx
call_indirect_op: _CALL_INDIRECT typeuse

labelidx: _var

br_if_op:    _BR_IF    _ws labelidx
br_table_op: _BR_TABLE (_ws labelidx)+
br_op:       _BR       _ws labelidx

return_op:      _RETURN
unreachable_op: _UNREACHABLE
nop_op:         _NOP
end_op:         _END

?control_op: unreachable_op | nop_op | return_op
    | br_if_op | br_table_op | br_op
    | call_op | call_indirect_op

?op: numeric_op | memory_op | variable_op | parametric_op | control_op

block_name: _ws name
?block_type: _ws result
block_instrs: (_ws instr)*
block_tail_with_result: block_type+ block_instrs
block_tail_no_result: block_instrs

_BLOCK: "block"

block_body_named: _BLOCK block_name (block_tail_no_result | block_tail_with_result)
block_body_anon: _BLOCK (block_tail_no_result | block_tail_with_result)
?block_body: block_body_named | block_body_anon
block_instr_named: block_body_named _ws end_op block_name?
block_instr_anon: block_body _ws end_op
block_instr: block_instr_named | block_instr_anon

_LOOP: "loop"

loop_body_named: _LOOP block_name (block_tail_no_result | block_tail_with_result)
loop_body_anon: _LOOP (block_tail_no_result | block_tail_with_result)
?loop_body: loop_body_named | loop_body_anon
loop_instr_named: loop_body_named _ws end_op block_name?
loop_instr_anon: loop_body _ws end_op
loop_instr: loop_instr_named | loop_instr_anon

_IF: "if"
_THEN: "then"
_ELSE: "else"

folded_else: _open _ELSE block_instrs _close
folded_then: _open _THEN block_instrs _close

then_tail: _ws folded_then (_ws folded_else)?

folded_if_tail_with_result: block_type+ (_ws expr)+ then_tail
folded_if_tail_no_result: (_ws expr)+ then_tail
folded_if_tail: folded_if_tail_no_result | folded_if_tail_with_result
folded_if_named: _IF block_name folded_if_tail
folded_if_anon: _IF folded_if_tail
?folded_if: folded_if_named | folded_if_anon

else_tail_named: _ws _ELSE block_name block_instrs
else_tail_anon: _ws _ELSE block_instrs

if_body_named: _IF block_name (block_tail_no_result | block_tail_with_result)
?if_body_anon: _IF (block_tail_no_result | block_tail_with_result)

if_instr_named: if_body_named else_tail_named? _ws end_op block_name?
if_instr_anon: if_body_anon else_tail_anon? _ws end_op

if_instr: if_instr_named | if_instr_anon

instr: op | block_instr | loop_instr | if_instr | expr
_instrs_tail: _ws instr
instrs: instr _instrs_tail*

folded_op: op _ws exprs
?inlined_folded_op: folded_op

expr: _open (op | inlined_folded_op | block_body | loop_body | folded_if) _close
_exprs_tail: _ws expr
?exprs: expr _exprs_tail*

_EXPORT: "export"

tableidx: _var
memoryidx: _var

_GLOBAL: "global"
_TABLE: "table"
_MEMORY: "memory"

?exfunc: _open _FUNC _ws funcidx _close
?exglobal: _open _GLOBAL _ws globalidx _close
?extable: _open _TABLE _ws tableidx _close
?exmemory: _open _MEMORY _ws memoryidx _close
export: _open _EXPORT _ws string _ws (exfunc | exglobal | extable | exmemory) _close
_exports_tail: _ws export
?exports: export _exports_tail*

?start: exprs
"""
