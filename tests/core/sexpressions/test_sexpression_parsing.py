import numpy

from wasm._utils.decorators import to_tuple
from wasm._utils.toolz import memoize, cons, concatv
from wasm.datatypes import (
    ValType,
    GlobalIdx,
    LabelIdx,
    FunctionIdx,
    Export,
    LocalIdx,
    TableIdx,
    MemoryIdx,
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
    UnresolvedImport,
    UnresolvedExport,
    UnresolvedGlobalIdx,
    Local,
    NamedBlock,
    NamedIf,
    NamedLoop,
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


@to_tuple
def with_parser(parser, *tests):
    for test in tests:
        yield tuple(cons(parser, test))


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
        grammar.expr,
        ('(i32.const 1234)', I32Const(1234)),
        ('(i64.const 1234)', I64Const(1234)),
        ('(f32.const 1234)', F32Const(1234)),
        ('(f64.const 1234)', F64Const(1234)),
    ),
    with_parser(
        grammar.expr,
        *tuple(
            (f'({op.text})', instruction.from_opcode(op))
            for ops, instruction in OP_PAIRS
            for op in ops
        ),
    ),
    with_parser(
        grammar.expr,
        ('(i32.wrap_i64)', Wrap()),
        ('(i64.extend_i32_s)', Extend.from_opcode(BinaryOpcode.I64_EXTEND_S_I32)),
        ('(i64.extend_i32_u)', Extend.from_opcode(BinaryOpcode.I64_EXTEND_U_I32)),
    ),
    with_parser(
        grammar.expr,
        ('(i32.trunc_f32_s)', Truncate.from_opcode(BinaryOpcode.I32_TRUNC_S_F32)),
        ('(i32.trunc_f32_u)', Truncate.from_opcode(BinaryOpcode.I32_TRUNC_U_F64)),
        ('(i32.trunc_f64_s)', Truncate.from_opcode(BinaryOpcode.I32_TRUNC_S_F32)),
        ('(i32.trunc_f64_u)', Truncate.from_opcode(BinaryOpcode.I32_TRUNC_U_F64)),
        ('(i64.trunc_f32_s)', Truncate.from_opcode(BinaryOpcode.I64_TRUNC_S_F32)),
        ('(i64.trunc_f32_u)', Truncate.from_opcode(BinaryOpcode.I64_TRUNC_U_F64)),
        ('(i64.trunc_f64_s)', Truncate.from_opcode(BinaryOpcode.I64_TRUNC_S_F32)),
        ('(i64.trunc_f64_u)', Truncate.from_opcode(BinaryOpcode.I64_TRUNC_U_F64)),
    ),
    with_parser(
        grammar.expr,
        ('(f32.convert_i32_s)', Convert.from_opcode(BinaryOpcode.F32_CONVERT_S_I32)),
        ('(f32.convert_i32_u)', Convert.from_opcode(BinaryOpcode.F32_CONVERT_U_I32)),
        ('(f32.convert_i64_s)', Convert.from_opcode(BinaryOpcode.F32_CONVERT_S_I64)),
        ('(f32.convert_i64_u)', Convert.from_opcode(BinaryOpcode.F32_CONVERT_U_I64)),
        ('(f64.convert_i32_s)', Convert.from_opcode(BinaryOpcode.F64_CONVERT_S_I32)),
        ('(f64.convert_i32_u)', Convert.from_opcode(BinaryOpcode.F64_CONVERT_U_I32)),
        ('(f64.convert_i64_s)', Convert.from_opcode(BinaryOpcode.F64_CONVERT_S_I64)),
        ('(f64.convert_i64_u)', Convert.from_opcode(BinaryOpcode.F64_CONVERT_U_I64)),
    ),
    with_parser(
        grammar.expr,
        ('(f32.demote_f64)', Demote()),
        ('(f64.promote_f32)', Promote()),
    ),
    with_parser(
        grammar.expr,
        ('(i32.reinterpret_f32)', Reinterpret.from_opcode(BinaryOpcode.I32_REINTERPRET_F32)),
        ('(i64.reinterpret_f64)', Reinterpret.from_opcode(BinaryOpcode.I64_REINTERPRET_F64)),
        ('(f32.reinterpret_i32)', Reinterpret.from_opcode(BinaryOpcode.F32_REINTERPRET_I32)),
        ('(f64.reinterpret_i64)', Reinterpret.from_opcode(BinaryOpcode.F64_REINTERPRET_I64)),
    ),
    with_parser(
        grammar.expr,
        ('(drop)', Drop()),
        ('(select)', Select()),
    ),
    with_parser(
        grammar.expr,
        ('(local.get $i)', UnresolvedVariableOp(BinaryOpcode.GET_LOCAL, UnresolvedLocalIdx('$i'))),
        ('(local.get 1)', LocalOp.from_opcode(BinaryOpcode.GET_LOCAL, LocalIdx(1))),
        ('(local.set $i)', UnresolvedVariableOp(BinaryOpcode.SET_LOCAL, UnresolvedLocalIdx('$i'))),
        ('(local.set 1)', LocalOp.from_opcode(BinaryOpcode.SET_LOCAL, LocalIdx(1))),
        ('(local.tee $i)', UnresolvedVariableOp(BinaryOpcode.TEE_LOCAL, UnresolvedLocalIdx('$i'))),
        ('(local.tee 1)', LocalOp.from_opcode(BinaryOpcode.TEE_LOCAL, LocalIdx(1))),
        ('(global.get $i)', UnresolvedVariableOp(BinaryOpcode.GET_GLOBAL, UnresolvedGlobalIdx('$i'))),  # noqa: E501
        ('(global.get 1)', GlobalOp.from_opcode(BinaryOpcode.GET_GLOBAL, GlobalIdx(1))),
        ('(global.set $i)', UnresolvedVariableOp(BinaryOpcode.SET_GLOBAL, UnresolvedGlobalIdx('$i'))),  # noqa: E501
        ('(global.set 1)', GlobalOp.from_opcode(BinaryOpcode.SET_GLOBAL, GlobalIdx(1))),
    ),
    with_parser(
        grammar.expr,
        ('(i32.load)', MemoryOp.from_opcode(BinaryOpcode.I32_LOAD, MemoryArg(0, 4))),
        ('(i64.load)', MemoryOp.from_opcode(BinaryOpcode.I64_LOAD, MemoryArg(0, 8))),
        ('(f32.load)', MemoryOp.from_opcode(BinaryOpcode.F32_LOAD, MemoryArg(0, 4))),
        ('(f64.load)', MemoryOp.from_opcode(BinaryOpcode.F64_LOAD, MemoryArg(0, 8))),
        ('(i32.load8_s)', MemoryOp.from_opcode(BinaryOpcode.I32_LOAD8_S, MemoryArg(0, 1))),
        ('(i32.load8_u)', MemoryOp.from_opcode(BinaryOpcode.I32_LOAD8_U, MemoryArg(0, 1))),
        ('(i32.load16_s)', MemoryOp.from_opcode(BinaryOpcode.I32_LOAD16_S, MemoryArg(0, 2))),
        ('(i32.load16_u)', MemoryOp.from_opcode(BinaryOpcode.I32_LOAD16_U, MemoryArg(0, 2))),
        ('(i64.load8_s)', MemoryOp.from_opcode(BinaryOpcode.I64_LOAD8_S, MemoryArg(0, 1))),
        ('(i64.load8_u)', MemoryOp.from_opcode(BinaryOpcode.I64_LOAD8_U, MemoryArg(0, 1))),
        ('(i64.load16_s)', MemoryOp.from_opcode(BinaryOpcode.I64_LOAD16_S, MemoryArg(0, 2))),
        ('(i64.load16_u)', MemoryOp.from_opcode(BinaryOpcode.I64_LOAD16_U, MemoryArg(0, 2))),
        ('(i64.load32_s)', MemoryOp.from_opcode(BinaryOpcode.I64_LOAD32_S, MemoryArg(0, 4))),
        ('(i64.load32_u)', MemoryOp.from_opcode(BinaryOpcode.I64_LOAD32_U, MemoryArg(0, 4))),
        ('(i32.store)', MemoryOp.from_opcode(BinaryOpcode.I32_STORE, MemoryArg(0, 4))),
        ('(i64.store)', MemoryOp.from_opcode(BinaryOpcode.I64_STORE, MemoryArg(0, 8))),
        ('(f32.store)', MemoryOp.from_opcode(BinaryOpcode.F32_STORE, MemoryArg(0, 4))),
        ('(f64.store)', MemoryOp.from_opcode(BinaryOpcode.F64_STORE, MemoryArg(0, 8))),
        ('(i32.store8)', MemoryOp.from_opcode(BinaryOpcode.I32_STORE8, MemoryArg(0, 1))),
        ('(i32.store16)', MemoryOp.from_opcode(BinaryOpcode.I32_STORE16, MemoryArg(0, 2))),
        ('(i64.store8)', MemoryOp.from_opcode(BinaryOpcode.I64_STORE8, MemoryArg(0, 1))),
        ('(i64.store16)', MemoryOp.from_opcode(BinaryOpcode.I64_STORE16, MemoryArg(0, 2))),
        ('(i64.store32)', MemoryOp.from_opcode(BinaryOpcode.I64_STORE32, MemoryArg(0, 4))),
        # TODO: tests that include offsets and alignments
    ),
    with_parser(
        grammar.expr,
        ('(memory.size)', MemorySize()),
        ('(memory.grow)', MemoryGrow()),
    ),
    with_parser(
        grammar.expr,
        ('(return)', RETURN),
        ('(return (i32.const 1))', (I32_CONST_1, RETURN)),
    ),
    with_parser(
        grammar.expr,
        ('(nop)', Nop()),
    ),
    with_parser(
        grammar.expr,
        ('(unreachable)', UNREACHABLE),
    ),
    with_parser(
        grammar.expr,
        ('(call $func-name)', UnresolvedCall(UnresolvedFunctionIdx('$func-name'))),
        ('(call 1)', Call(FunctionIdx(1))),
    ),
    with_parser(
        grammar.expr,
        ('(call_indirect (result))', UnresolvedCallIndirect(UnresolvedFunctionType((), ()))),
        (
            '(call_indirect (param i64))',
            UnresolvedCallIndirect(UnresolvedFunctionType((pi64,), ())),
        ),
        (
            '(call_indirect (param i64) (result i32))',
            UnresolvedCallIndirect(UnresolvedFunctionType((pi64,), (i32,))),
        ),
        (
            '(call_indirect (param i64) (param) (param f64 i32 i64))',
            UnresolvedCallIndirect(UnresolvedFunctionType((pi64, pf64, pi32, pi64), ())),
        ),
        (
            '(call_indirect (param) (param i64) (param) (param f64 i32 i64) (param) (param) (result) (result i32) (result) (result))',  # noqa: E501
            UnresolvedCallIndirect(UnresolvedFunctionType((pi64, pf64, pi32, pi64), (i32,))),
        ),
        (
            '(call_indirect (type $check))',
            UnresolvedCallIndirect(UnresolvedTypeIdx('$check')),
        ),
        (
            '(call_indirect (type $over-i64) (param i64) (result i64))',
            UnresolvedCallIndirect(LinkedFunctionType(UnresolvedTypeIdx('$over-i64'), UnresolvedFunctionType((pi64,), (i64,)))),  # noqa: E501
        ),
    ),
    with_parser(
        grammar.expr,
        # folded
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
    with_parser(
        grammar.expr,
        ('(br 0)', Br(LabelIdx(0))),
        ('(br $i)', UnresolvedBr(UnresolvedLabelIdx('$i'))),
        ('(br_if 0)', BrIf(LabelIdx(0))),
        ('(br_if $i)', UnresolvedBrIf(UnresolvedLabelIdx('$i'))),
        ('(br_table 1 2 3)', BrTable((LabelIdx(1), LabelIdx(2)), LabelIdx(3))),
        (
            '(br_table 1 2 $default)',
            UnresolvedBrTable((LabelIdx(1), LabelIdx(2)), UnresolvedLabelIdx('$default')),
        ),
        (
            '(br_table 1 2 $three 4)',
            UnresolvedBrTable(
                (LabelIdx(1), LabelIdx(2), UnresolvedLabelIdx('$three')),
                LabelIdx(4),
            ),
        ),
    ),
    with_parser(
        grammar.expr,
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
    with_parser(
        grammar.exprs,
        ('(if (local.get 0) (then))', (LOCAL_ZERO_OP, BASIC_IF_INSTR)),
        ('(if (local.get 0) (then) (else))', (LOCAL_ZERO_OP, BASIC_IF_INSTR)),
        ('(if $l (local.get 0) (then))', (LOCAL_ZERO_OP, NAMED_BASIC_IF_INSTR)),
        ('(if $l (local.get 0) (then) (else))', (LOCAL_ZERO_OP, NAMED_BASIC_IF_INSTR)),
        ('(if (local.get 0) (then (nop)))', (LOCAL_ZERO_OP, NOP_IF_INSTR)),
        ('(if (local.get 0) (then (nop)) (else (nop)))', (LOCAL_ZERO_OP, NOP_IF_WITH_NOP_ELSE_INSTR)),  # noqa: E501
    ),
    with_parser(
        # Test folded instructions
        grammar.exprs,
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
        grammar.function,
        # function declarations
        ('(func)', UnresolvedFunction(type=EMPTY_FUNC_TYPE, locals=(), body=END_TAIL)),
        ('(func $f)', UnresolvedFunction(type=EMPTY_FUNC_TYPE, locals=(), body=END_TAIL, name='$f')),  # noqa: E501
        (COMPLEX_FUNCTION_SEXPR, COMPLEX_FUNCTION),
        ('(func (local))', UnresolvedFunction(type=EMPTY_FUNC_TYPE, locals=(), body=END_TAIL)),
        # function declarations w/inline exports
        ('(func (export "f"))', UnresolvedExport("f", UnresolvedFunction(type=EMPTY_FUNC_TYPE, locals=(), body=END_TAIL))),  # noqa: E501
        ('(func (export "f") (type $a))', UnresolvedExport("f", UnresolvedFunction(type=UnresolvedTypeIdx('$a'), locals=(), body=END_TAIL))),  # noqa: E501
    ),
    with_parser(
        grammar.folded_function_import,
        # function imports
        (
            '(func $a (import "spectest" "$a") (param i32))',
            UnresolvedImport(module_name="spectest", as_name="$a", desc=LinkedFunctionType(UnresolvedTypeIdx('$a'), UnresolvedFunctionType((pi32,), ()))),  # noqa: E501
        ),
        # (
        #     '(func (export "p1") (import "spectest" "print_i32") (param i32))',
        #     TODO,
        # ),
        # (
        #     '(func $p (export "p2") (import "spectest" "print_i32") (param i32))',
        #     TODO
        # ),
    ),
))


def pytest_generate_tests(metafunc):
    metafunc.parametrize('parser,sexpr,expected', SEXPRESSION_TESTS)


def test_sexpression_parsing(parser, sexpr, expected):
    result = parser.parseString(sexpr)
    assert len(result) == 1
    actual = result[0]
    assert actual == expected
