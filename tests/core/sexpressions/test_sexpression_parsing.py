import numpy

from wasm import constants
from wasm._utils.decorators import to_tuple
from wasm._utils.toolz import cons, concatv
from wasm.datatypes import (
    Import,
    Memory,
    MemoryType,
    Table,
    Limits,
    ValType,
    GlobalIdx,
    LabelIdx,
    FunctionIdx,
    Export,
    LocalIdx,
    TableIdx,
    MemoryIdx,
    GlobalType,
    Global,
    TableType,
    FunctionAddress,
)
from wasm.opcodes import (
    BinaryOpcode,
)
from wasm.instructions.parametric import (
    Drop,
    Select,
)
from wasm.instructions.variable import (
    GlobalOp,
    LocalOp,
)
from wasm.instructions.memory import (
    MemoryArg,
    MemoryGrow,
    MemoryOp,
    MemorySize,
)
from wasm.instructions.control import (
    Else,
    If,
    Return,
    Nop,
    Unreachable,
    End,
    Loop,
    Block,
    Br,
    BrIf,
    BrTable,
    Call,
)
from wasm.instructions.numeric import (
    I32Const,
    I64Const,
    F32Const,
    F64Const,
    UnOp,
    BinOp,
    RelOp,
    TestOp as _TestOp,
    Convert,
    Demote,
    Extend,
    Promote,
    Reinterpret,
    Truncate,
    Wrap,
)
from wasm.text import grammar
from wasm.text.ir import (
    UnresolvedDataSegment,
    NamedMemory,
    NamedGlobal,
    UnresolvedImport,
    UnresolvedExport,
    UnresolvedGlobalIdx,
    Local,
    NamedBlock,
    NamedIf,
    NamedLoop,
    NamedTable,
    Param,
    UnresolvedBr,
    UnresolvedBrIf,
    UnresolvedBrTable,
    UnresolvedCall,
    UnresolvedCallIndirect,
    UnresolvedFunctionIdx,
    UnresolvedTableIdx,
    UnresolvedMemoryIdx,
    UnresolvedFunctionType,
    UnresolvedLabelIdx,
    UnresolvedTypeIdx,
    UnresolvedLocalIdx,
    UnresolvedVariableOp,
    UnresolvedFunction,
    LinkedFunctionType,
)


def _default_cmp_fn(result, expected):
    assert len(result) == 1
    actual = result[0]
    assert actual == expected


def cmp_table_with_elements(result, expected):
    assert len(result) == 1
    named_table, element_segment = result[0]
    table, offset, init = expected
    assert isinstance(element_segment.table_idx, UnresolvedTableIdx)
    assert named_table.name == element_segment.table_idx.name
    assert named_table.table == table
    assert element_segment.offset == offset
    assert element_segment.init == init


@to_tuple
def with_parser(parser, *tests, cmp_fn=_default_cmp_fn):
    for test in tests:
        yield tuple(cons(parser, test)) + (cmp_fn,)


UNOPS = tuple(op for op in BinaryOpcode if op.is_unop)
RELOPS = tuple(op for op in BinaryOpcode if op.is_relop)
BINOPS = tuple(op for op in BinaryOpcode if op.is_binop)
TESTOPS = tuple(op for op in BinaryOpcode if op.is_testop)
OP_PAIRS = (
    (UNOPS, UnOp),
    (RELOPS, RelOp),
    (BINOPS, BinOp),
    (TESTOPS, _TestOp),
)


i32 = ValType.i32
i64 = ValType.i64
f32 = ValType.f32
f64 = ValType.f64

pi32 = Param(i32)
pi64 = Param(i64)
pf32 = Param(f32)
pf64 = Param(f64)

li32 = Local(i32)
li64 = Local(i64)
lf32 = Local(f32)
lf64 = Local(f64)

NOP = Nop()
END = End()
END_TAIL = End.as_tail()
ELSE = Else()
ELSE_TAIL = Else.as_tail()
UNREACHABLE = Unreachable()
LOCAL_ZERO_OP = LocalOp.from_opcode(BinaryOpcode.GET_LOCAL, LocalIdx(0))
BASIC_IF_INSTR = If(result_type=(), instructions=ELSE_TAIL, else_instructions=END_TAIL)
NAMED_BASIC_IF_INSTR = NamedIf('$l', BASIC_IF_INSTR)
NOP_IF_INSTR = If(result_type=(), instructions=(NOP, ELSE), else_instructions=END_TAIL)
NOP_IF_WITH_NOP_ELSE_INSTR = If(result_type=(), instructions=(NOP, ELSE), else_instructions=(NOP, END))  # noqa: E501
I32_CONST_0 = I32Const(numpy.uint32(0))
I32_CONST_1 = I32Const(numpy.uint32(1))
I32_CONST_2 = I32Const(numpy.uint32(2))
I32_CONST_3 = I32Const(numpy.uint32(3))
I32_CONST_7 = I32Const(numpy.uint32(7))
RETURN = Return()

EMPTY_FUNC_TYPE = UnresolvedFunctionType((), ())

COMPLEX_FUNCTION_SEXPR = """(func $complex
    (param i32 f32) (param $x i64) (param) (param i32)
    (result) (result i32) (result)
    (local f32) (local $y i32) (local i64 i32) (local) (local f64 i32)
    (unreachable) (unreachable)
  )
"""
COMPLEX_FUNCTION = UnresolvedFunction(
    type=UnresolvedFunctionType(
        params=(pi32, pf32, Param(i64, '$x'), pi32),
        results=(i32,),
    ),
    locals=(lf32, Local(i32, '$y'), li64, li32, lf64, li32),
    body=(UNREACHABLE, UNREACHABLE, END),
    name='$complex',
)

SEXPRESSION_TESTS = tuple(concatv(
    with_parser(
        grammar.STRING,
        ('"a"', "a"),
        ('"a_b"', "a_b"),
    ),
    with_parser(
        grammar.comment,
        (';;this is a line comment', "this is a line comment"),
        ('(;this is a block comment;)', "this is a block comment"),
        ('(;this is a\nblock comment;)', "this is a\nblock comment"),
    ),
    with_parser(
        grammar.limits,
        ('0', Limits(0, None)),
        ('0 10', Limits(0, 10)),
    ),
    with_parser(
        grammar.locals,
        # unnamed
        ('(local)', ()),
        ('(local) (local)', ()),
        ('(local i32)', (li32,)),
        ('(local i64)', (li64,)),
        ('(local f32)', (lf32,)),
        ('(local f64)', (lf64,)),
        ('(local i32) (local i64)', (li32, li64)),
        ('(local i32) (local) (local i64)', (li32, li64)),
        ('(local i32 i64)', (li32, li64)),
        ('(local f32 f64)', (lf32, lf64)),
        ('(local f32 f64 i32 i64)', (lf32, lf64, li32, li64)),
        # # named
        ('(local $i i32)', (Local(i32, '$i'),)),
        # # multi
        ('(local f32 f64)\n(local $i i32)', (lf32, lf64, Local(i32, '$i'))),
    ),
    with_parser(
        grammar.results,
        # simple
        ('(result)', ()),
        ('(result i32)', (i32,)),
        ('(result i32 i64)', (i32, i64)),
        # many
        ('(result i32) (result i64)', (i32, i64)),
    ),
    with_parser(
        grammar.params,
        # simple
        ('(param)', ()),
        ('(param i32)', (pi32,)),
        # multiple
        ('(param i32 i64)', (pi32, pi64,)),
        ('(param) (param)', ()),
        ('(param i32) (param i64)', (pi32, pi64,)),
        # named
        ('(param $x i32)', (Param(i32, '$x'),)),
    ),
    with_parser(
        grammar.typeuse,
        ('(result)', UnresolvedFunctionType((), ())),
        ('(param i64)', UnresolvedFunctionType((pi64,), ())),
        ('(param i64) (result i32)', UnresolvedFunctionType((pi64,), (i32,))),
        (
            '(param i64) (param) (param f64 i32 i64)',
            UnresolvedFunctionType((pi64, pf64, pi32, pi64), ()),
        ),
        (
            '(param) (param i64) (param) (param f64 i32 i64) (param) (param) (result) (result i32) (result) (result)',  # noqa: E501
            UnresolvedFunctionType((pi64, pf64, pi32, pi64), (i32,)),
        ),
        (
            '(type $check)',
            UnresolvedTypeIdx('$check'),
        ),
        (
            '(type $over-i64) (param i64) (result i64)',
            LinkedFunctionType(UnresolvedTypeIdx('$over-i64'), UnresolvedFunctionType((pi64,), (i64,))),  # noqa: E501
        ),
    ),
    with_parser(
        grammar.instr,
        ('i32.const 1234', I32Const(1234)),
        ('i64.const 1234', I64Const(1234)),
        ('f32.const 1234', F32Const(1234)),
        ('f64.const 1234', F64Const(1234)),
    ),
    with_parser(
        grammar.instr,
        *tuple(
            (op.text, instruction.from_opcode(op))
            for ops, instruction in OP_PAIRS
            for op in ops
        ),
    ),
    with_parser(
        grammar.instr,
        ('i32.wrap_i64', Wrap()),
        ('i64.extend_i32_s', Extend.from_opcode(BinaryOpcode.I64_EXTEND_S_I32)),
        ('i64.extend_i32_u', Extend.from_opcode(BinaryOpcode.I64_EXTEND_U_I32)),
    ),
    with_parser(
        grammar.instr,
        ('i32.trunc_f32_s', Truncate.from_opcode(BinaryOpcode.I32_TRUNC_S_F32)),
        ('i32.trunc_f32_u', Truncate.from_opcode(BinaryOpcode.I32_TRUNC_U_F64)),
        ('i32.trunc_f64_s', Truncate.from_opcode(BinaryOpcode.I32_TRUNC_S_F32)),
        ('i32.trunc_f64_u', Truncate.from_opcode(BinaryOpcode.I32_TRUNC_U_F64)),
        ('i64.trunc_f32_s', Truncate.from_opcode(BinaryOpcode.I64_TRUNC_S_F32)),
        ('i64.trunc_f32_u', Truncate.from_opcode(BinaryOpcode.I64_TRUNC_U_F64)),
        ('i64.trunc_f64_s', Truncate.from_opcode(BinaryOpcode.I64_TRUNC_S_F32)),
        ('i64.trunc_f64_u', Truncate.from_opcode(BinaryOpcode.I64_TRUNC_U_F64)),
    ),
    with_parser(
        grammar.instr,
        ('f32.convert_i32_s', Convert.from_opcode(BinaryOpcode.F32_CONVERT_S_I32)),
        ('f32.convert_i32_u', Convert.from_opcode(BinaryOpcode.F32_CONVERT_U_I32)),
        ('f32.convert_i64_s', Convert.from_opcode(BinaryOpcode.F32_CONVERT_S_I64)),
        ('f32.convert_i64_u', Convert.from_opcode(BinaryOpcode.F32_CONVERT_U_I64)),
        ('f64.convert_i32_s', Convert.from_opcode(BinaryOpcode.F64_CONVERT_S_I32)),
        ('f64.convert_i32_u', Convert.from_opcode(BinaryOpcode.F64_CONVERT_U_I32)),
        ('f64.convert_i64_s', Convert.from_opcode(BinaryOpcode.F64_CONVERT_S_I64)),
        ('f64.convert_i64_u', Convert.from_opcode(BinaryOpcode.F64_CONVERT_U_I64)),
    ),
    with_parser(
        grammar.instr,
        ('f32.demote_f64', Demote()),
        ('f64.promote_f32', Promote()),
    ),
    with_parser(
        grammar.instr,
        ('i32.reinterpret_f32', Reinterpret.from_opcode(BinaryOpcode.I32_REINTERPRET_F32)),
        ('i64.reinterpret_f64', Reinterpret.from_opcode(BinaryOpcode.I64_REINTERPRET_F64)),
        ('f32.reinterpret_i32', Reinterpret.from_opcode(BinaryOpcode.F32_REINTERPRET_I32)),
        ('f64.reinterpret_i64', Reinterpret.from_opcode(BinaryOpcode.F64_REINTERPRET_I64)),
    ),
    with_parser(
        grammar.instr,
        ('drop', Drop()),
        ('select', Select()),
    ),
    with_parser(
        grammar.instr,
        ('local.get $i', UnresolvedVariableOp(BinaryOpcode.GET_LOCAL, UnresolvedLocalIdx('$i'))),
        ('local.get 1', LocalOp.from_opcode(BinaryOpcode.GET_LOCAL, LocalIdx(1))),
        ('local.set $i', UnresolvedVariableOp(BinaryOpcode.SET_LOCAL, UnresolvedLocalIdx('$i'))),
        ('local.set 1', LocalOp.from_opcode(BinaryOpcode.SET_LOCAL, LocalIdx(1))),
        ('local.tee $i', UnresolvedVariableOp(BinaryOpcode.TEE_LOCAL, UnresolvedLocalIdx('$i'))),
        ('local.tee 1', LocalOp.from_opcode(BinaryOpcode.TEE_LOCAL, LocalIdx(1))),
        ('global.get $i', UnresolvedVariableOp(BinaryOpcode.GET_GLOBAL, UnresolvedGlobalIdx('$i'))),  # noqa: E501
        ('global.get 1', GlobalOp.from_opcode(BinaryOpcode.GET_GLOBAL, GlobalIdx(1))),
        ('global.set $i', UnresolvedVariableOp(BinaryOpcode.SET_GLOBAL, UnresolvedGlobalIdx('$i'))),  # noqa: E501
        ('global.set 1', GlobalOp.from_opcode(BinaryOpcode.SET_GLOBAL, GlobalIdx(1))),
    ),
    with_parser(
        grammar.instr,
        ('i32.load', MemoryOp.from_opcode(BinaryOpcode.I32_LOAD, MemoryArg(0, 4))),
        ('i64.load', MemoryOp.from_opcode(BinaryOpcode.I64_LOAD, MemoryArg(0, 8))),
        ('f32.load', MemoryOp.from_opcode(BinaryOpcode.F32_LOAD, MemoryArg(0, 4))),
        ('f64.load', MemoryOp.from_opcode(BinaryOpcode.F64_LOAD, MemoryArg(0, 8))),
        ('i32.load8_s', MemoryOp.from_opcode(BinaryOpcode.I32_LOAD8_S, MemoryArg(0, 1))),
        ('i32.load8_u', MemoryOp.from_opcode(BinaryOpcode.I32_LOAD8_U, MemoryArg(0, 1))),
        ('i32.load16_s', MemoryOp.from_opcode(BinaryOpcode.I32_LOAD16_S, MemoryArg(0, 2))),
        ('i32.load16_u', MemoryOp.from_opcode(BinaryOpcode.I32_LOAD16_U, MemoryArg(0, 2))),
        ('i64.load8_s', MemoryOp.from_opcode(BinaryOpcode.I64_LOAD8_S, MemoryArg(0, 1))),
        ('i64.load8_u', MemoryOp.from_opcode(BinaryOpcode.I64_LOAD8_U, MemoryArg(0, 1))),
        ('i64.load16_s', MemoryOp.from_opcode(BinaryOpcode.I64_LOAD16_S, MemoryArg(0, 2))),
        ('i64.load16_u', MemoryOp.from_opcode(BinaryOpcode.I64_LOAD16_U, MemoryArg(0, 2))),
        ('i64.load32_s', MemoryOp.from_opcode(BinaryOpcode.I64_LOAD32_S, MemoryArg(0, 4))),
        ('i64.load32_u', MemoryOp.from_opcode(BinaryOpcode.I64_LOAD32_U, MemoryArg(0, 4))),
        ('i32.store', MemoryOp.from_opcode(BinaryOpcode.I32_STORE, MemoryArg(0, 4))),
        ('i64.store', MemoryOp.from_opcode(BinaryOpcode.I64_STORE, MemoryArg(0, 8))),
        ('f32.store', MemoryOp.from_opcode(BinaryOpcode.F32_STORE, MemoryArg(0, 4))),
        ('f64.store', MemoryOp.from_opcode(BinaryOpcode.F64_STORE, MemoryArg(0, 8))),
        ('i32.store8', MemoryOp.from_opcode(BinaryOpcode.I32_STORE8, MemoryArg(0, 1))),
        ('i32.store16', MemoryOp.from_opcode(BinaryOpcode.I32_STORE16, MemoryArg(0, 2))),
        ('i64.store8', MemoryOp.from_opcode(BinaryOpcode.I64_STORE8, MemoryArg(0, 1))),
        ('i64.store16', MemoryOp.from_opcode(BinaryOpcode.I64_STORE16, MemoryArg(0, 2))),
        ('i64.store32', MemoryOp.from_opcode(BinaryOpcode.I64_STORE32, MemoryArg(0, 4))),
        # TODO: tests that include offsets and alignments
    ),
    with_parser(
        grammar.instr,
        ('memory.size', MemorySize()),
        ('memory.grow', MemoryGrow()),
    ),
    with_parser(
        grammar.instr,
        ('return', RETURN),
        ('(return (i32.const 1))', (I32_CONST_1, RETURN)),
    ),
    with_parser(
        grammar.instr,
        ('nop', Nop()),
        ('unreachable', UNREACHABLE),
    ),
    with_parser(
        grammar.instr,
        ('call $func-name', UnresolvedCall(UnresolvedFunctionIdx('$func-name'))),
        ('call 1', Call(FunctionIdx(1))),
    ),
    with_parser(
        grammar.instr,
        ('call_indirect (result)', UnresolvedCallIndirect(UnresolvedFunctionType((), ()))),
        (
            'call_indirect (param i64)',
            UnresolvedCallIndirect(UnresolvedFunctionType((pi64,), ())),
        ),
        (
            'call_indirect (param i64) (result i32)',
            UnresolvedCallIndirect(UnresolvedFunctionType((pi64,), (i32,))),
        ),
        (
            'call_indirect (param i64) (param) (param f64 i32 i64)',
            UnresolvedCallIndirect(UnresolvedFunctionType((pi64, pf64, pi32, pi64), ())),
        ),
        (
            'call_indirect (param) (param i64) (param) (param f64 i32 i64) (param) (param) (result) (result i32) (result) (result)',  # noqa: E501
            UnresolvedCallIndirect(UnresolvedFunctionType((pi64, pf64, pi32, pi64), (i32,))),
        ),
        (
            'call_indirect (type $check)',
            UnresolvedCallIndirect(UnresolvedTypeIdx('$check')),
        ),
        (
            'call_indirect (type $over-i64) (param i64) (result i64)',
            UnresolvedCallIndirect(LinkedFunctionType(UnresolvedTypeIdx('$over-i64'), UnresolvedFunctionType((pi64,), (i64,)))),  # noqa: E501
        ),
    ),
    with_parser(
        grammar.instr,
        ('br 0', Br(LabelIdx(0))),
        ('br $i', UnresolvedBr(UnresolvedLabelIdx('$i'))),
        ('br_if 0', BrIf(LabelIdx(0))),
        ('br_if $i', UnresolvedBrIf(UnresolvedLabelIdx('$i'))),
        ('br_table 1 2 3', BrTable((LabelIdx(1), LabelIdx(2)), LabelIdx(3))),
        (
            'br_table 1 2 $default',
            UnresolvedBrTable((LabelIdx(1), LabelIdx(2)), UnresolvedLabelIdx('$default')),
        ),
        (
            'br_table 1 2 $three 4',
            UnresolvedBrTable(
                (LabelIdx(1), LabelIdx(2), UnresolvedLabelIdx('$three')),
                LabelIdx(4),
            ),
        ),
    ),
    with_parser(
        grammar.instr,
        # blocks
        ('block end', Block((), End.as_tail())),
        ('block $blk end', NamedBlock('$blk', Block((), End.as_tail()))),
        ('block nop end', Block((), (Nop(), END))),
        ('block nop end', Block((), (Nop(), END))),
    ),
    with_parser(
        grammar.folded_instr,
        # folded blocks
        ('(block)', Block((), End.as_tail())),
        ('(block $blk)', NamedBlock('$blk', Block((), End.as_tail()))),
        ('(block (nop))', Block((), (Nop(), END))),
        ('(block nop)', Block((), (Nop(), END))),
        (
            '(block (result i32) (i32.const 7))',
            Block((i32,), (I32_CONST_7, END)),
        ),
        (
            '(block (call $dummy))',
            Block((), (UnresolvedCall(UnresolvedFunctionIdx('$dummy')), END)),
        ),
        (
            '(block (result i32) (i32.ctz (return (i32.const 1))))',
            Block(
                (i32,),
                (
                    I32_CONST_1,
                    RETURN,
                    UnOp.from_opcode(BinaryOpcode.I32_CTZ),
                    END,
                )
            ),
        ),
    ),
    # with_parser(
    #     grammar.instr,
    #     TODO: non-folded loops
    # ),
    with_parser(
        grammar.folded_instr,
        # folded loops
        ('(loop)', Loop((), End.as_tail())),
        ('(loop $l)', NamedLoop('$l', Loop((), END_TAIL))),
        ('(loop (nop))', Loop((), (Nop(), END))),
        (
            '(loop (result i32) (i32.const 7))',
            Loop((i32,), (I32_CONST_7, END)),
        ),
        (
            '(loop (call $dummy))',
            Loop((), (UnresolvedCall(UnresolvedFunctionIdx('$dummy')), END)),
        ),
    ),
    # with_parser(
    #     grammar.instr,
    #     TODO: non-folded if
    # ),
    with_parser(
        grammar.folded_instr,
        # folded if
        ('(if (local.get 0) (then))', (LOCAL_ZERO_OP, BASIC_IF_INSTR)),
        ('(if (local.get 0) (then) (else))', (LOCAL_ZERO_OP, BASIC_IF_INSTR)),
        ('(if $l (local.get 0) (then))', (LOCAL_ZERO_OP, NAMED_BASIC_IF_INSTR)),
        ('(if $l (local.get 0) (then) (else))', (LOCAL_ZERO_OP, NAMED_BASIC_IF_INSTR)),
        ('(if (local.get 0) (then (nop)))', (LOCAL_ZERO_OP, NOP_IF_INSTR)),
        ('(if (local.get 0) (then (nop)) (else (nop)))', (LOCAL_ZERO_OP, NOP_IF_WITH_NOP_ELSE_INSTR)),  # noqa: E501
    ),
    with_parser(
        # folded instructions
        grammar.instr,
        (
            '(i32.add (local.get 1) (i32.const 2))',
            (
                LocalOp.from_opcode(BinaryOpcode.GET_LOCAL, LocalIdx(1)),
                I32_CONST_2,
                BinOp.from_opcode(BinaryOpcode.I32_ADD),
            ),
        ),
        (
            '(i32.mul (i32.add (local.get 1) (i32.const 2)) (i32.const 3))',
            (
                LocalOp.from_opcode(BinaryOpcode.GET_LOCAL, LocalIdx(1)),
                I32_CONST_2,
                BinOp.from_opcode(BinaryOpcode.I32_ADD),
                I32_CONST_3,
                BinOp.from_opcode(BinaryOpcode.I32_MUL),
            ),
        ),
    ),
    with_parser(
        grammar.export,
        ('(export "a" (func 0))', Export("a", FunctionIdx(0))),
        ('(export "a" (func $a))', UnresolvedExport("a", UnresolvedFunctionIdx("$a"))),
        ('(export "a" (global 0))', Export("a", GlobalIdx(0))),
        ('(export "a" (global $a))', UnresolvedExport("a", UnresolvedGlobalIdx("$a"))),
        ('(export "a" (table 0))', Export("a", TableIdx(0))),
        ('(export "a" (table $a))', UnresolvedExport("a", UnresolvedTableIdx("$a"))),
        ('(export "a" (memory 0))', Export("a", MemoryIdx(0))),
        ('(export "a" (memory $a))', UnresolvedExport("a", UnresolvedMemoryIdx("$a"))),
    ),
    with_parser(
        grammar.import_,
        ('(import "test" "func" (func))', UnresolvedImport("test", "func", UnresolvedFunctionType((), ()))),  # noqa: E501
        ('(import "test" "func" (func (param i32)))', UnresolvedImport("test", "func", UnresolvedFunctionType((pi32,), ()))),  # noqa: E501
        ('(import "test" "g" (global i32))', Import("test", "g", GlobalType.const(i32))),
        ('(import "test" "t" (table 10 20 funcref))', Import("test", "t", TableType(Limits(10, 20), FunctionAddress))),  # noqa: E501
        ('(import "test" "m" (memory 1 2))', Import("test", "m", MemoryType(1, 2))),
    ),
    with_parser(
        grammar.function,
        # function declarations
        ('(func)', UnresolvedFunction(type=EMPTY_FUNC_TYPE, locals=(), body=END_TAIL)),
        ('(func $f)', UnresolvedFunction(type=EMPTY_FUNC_TYPE, locals=(), body=END_TAIL, name='$f')),  # noqa: E501
        (COMPLEX_FUNCTION_SEXPR, COMPLEX_FUNCTION),
        ('(func (local))', UnresolvedFunction(type=EMPTY_FUNC_TYPE, locals=(), body=END_TAIL)),
        # function declarations w/inline exports
        # TODO: extract to separate test and declaration
        ('(func (export "f"))', UnresolvedExport("f", UnresolvedFunction(type=EMPTY_FUNC_TYPE, locals=(), body=END_TAIL))),  # noqa: E501
        ('(func (export "f") (type $a))', UnresolvedExport("f", UnresolvedFunction(type=UnresolvedTypeIdx('$a'), locals=(), body=END_TAIL))),  # noqa: E501
    ),
    with_parser(
        grammar.folded_function_import,
        # function imports
        (
            '(func $a (import "m" "f") (param i32))',
            (
                UnresolvedImport(module_name="m", as_name="f", desc=LinkedFunctionType(UnresolvedTypeIdx('$a'), UnresolvedFunctionType((pi32,), ()))),  # noqa: E501
            ),
        ),
        (
            '(func (export "e") (import "m" "f") (param i32))',
            (
                UnresolvedExport("e", UnresolvedFunctionType((pi32,), ())),
                UnresolvedImport("m", "f", UnresolvedFunctionType((pi32,), ())),
            ),
        ),
        (
            '(func $p (export "e") (import "m" "f") (param i32))',
            (
                UnresolvedExport("e", LinkedFunctionType(UnresolvedTypeIdx('$p'), UnresolvedFunctionType((pi32,), ()))),  # noqa: E501
                UnresolvedImport("m", "f", LinkedFunctionType(UnresolvedTypeIdx('$p'), UnresolvedFunctionType((pi32,), ()))),  # noqa: E501
            ),
        ),
        (
            '(func (export "e1") (export "e2") (import "m" "f") (param i32))',
            (
                UnresolvedExport("e1", UnresolvedFunctionType((pi32,), ())),
                UnresolvedExport("e2", UnresolvedFunctionType((pi32,), ())),
                UnresolvedImport("m", "f", UnresolvedFunctionType((pi32,), ())),
            ),
        ),
    ),
    with_parser(
        grammar._global,
        ('(global $a i32 (i32.const 2))', NamedGlobal('$a', Global(GlobalType.const(i32), (I32_CONST_2, END)))),  # noqa: E501
        ('(global $a (mut i32) (i32.const 2))', NamedGlobal('$a', Global(GlobalType.var(i32), (I32_CONST_2, END)))),  # noqa: E501
        ('(global (;1;) i32 (i32.const 3))', Global(GlobalType.const(i32), (I32_CONST_3, END))),
    ),
    with_parser(
        grammar.table_type,
        ('0 funcref', TableType(Limits(0, None), FunctionAddress)),
    ),
    with_parser(
        grammar.table,
        ('(table 0 funcref)', ((), None, Table(TableType(Limits(0, None), FunctionAddress)),)),
        ('(table $t 10 funcref)', ((), None, NamedTable('$t', Table(TableType(Limits(10, None), FunctionAddress)))),),  # noqa: E501
        (
            '(table $t (export "a") 0 funcref)',
            (
                (UnresolvedExport('a', UnresolvedTableIdx('$t')),),
                None,
                NamedTable('$t', Table(TableType(Limits(0, None), FunctionAddress))),  # noqa: E501
            ),
        ),
        (
            '(table $t (import "test" "table") 10 20 funcref)',
            (
                (),
                UnresolvedImport("test", "table", UnresolvedTableIdx('$t')),
                NamedTable('$t', Table(TableType(Limits(10, 20), FunctionAddress))),  # noqa: E501
            ),
        )
    ),
    with_parser(
        grammar.table_with_elements,
        (
            '(table funcref (elem $func))',
            (
                Table(TableType(Limits(1, 1), FunctionAddress)),
                (I32_CONST_0, END),
                (UnresolvedFunctionIdx('$func'),),
            ),
        ),
        cmp_fn=cmp_table_with_elements,
    ),
    with_parser(
        grammar.datastring,
        ('""', b''),
        ('"arst"', b'arst'),
        ('"arst" "tsra"', b'arsttsra'),
    ),
    with_parser(
        grammar.data_inline,
        ('(data)', b''),
        ('(data "arst")', b'arst'),
        ('(data "arst" "tsra")', b'arsttsra'),
    ),
    with_parser(
        grammar.memory,
        ('(memory 0 0)', ((), None, Memory(MemoryType(0, 0)))),
        ('(memory 0 1)', ((), None, Memory(MemoryType(0, 1)))),
        ('(memory 1 256)', ((), None, Memory(MemoryType(1, 256)))),
        ('(memory 0 65536)', ((), None, Memory(MemoryType(0, 65536)))),
        ('(memory 1)', ((), None, Memory(MemoryType(1)))),
        ('(memory $m 1)', ((), None, NamedMemory('$m', Memory(MemoryType(1))))),
        (
            '(memory $m (export "a") 0)',
            (
                (UnresolvedExport("a", UnresolvedMemoryIdx('$m')),),
                None,
                NamedMemory('$m', Memory(MemoryType(0)))
            ),
        ),
    ),
    with_parser(
        grammar.memory_with_data,
        (
            '(memory $m (data))',
            (
                NamedMemory('$m', Memory(MemoryType(0, 0))),
                UnresolvedDataSegment(
                    memory_idx=UnresolvedMemoryIdx('$m'),
                    offset=(I32_CONST_0, END),
                    init=b'',
                ),
            ),
        ),
        (
            '(memory $m (data "unicorns"))',
            (
                NamedMemory('$m', Memory(MemoryType(constants.PAGE_SIZE_64K, constants.PAGE_SIZE_64K))),  # noqa: E501
                UnresolvedDataSegment(
                    memory_idx=UnresolvedMemoryIdx('$m'),
                    offset=(I32_CONST_0, END),
                    init=b'unicorns',
                ),
            ),
        ),
    ),
))


def pytest_generate_tests(metafunc):
    metafunc.parametrize('parser,sexpr,expected,cmp_fn', SEXPRESSION_TESTS)


def test_sexpression_parsing(parser, sexpr, expected, cmp_fn):
    result = parser.parseString(sexpr)
    cmp_fn(result, expected)
