import numpy

from lark import Lark

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
from wasm.text import (
    parser as default_parser,
)
from wasm.text.lark import (
    GRAMMAR,
)
from wasm.text.transformer import (
    WasmTransformer,
)
from wasm.text.ir import (
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
)


@memoize
def _get_parser(start_rule):
    if start_rule == 'start':
        return default_parser
    else:
        return Lark(
            GRAMMAR,
            parser="lalr",
            transformer=WasmTransformer(),
            start=start_rule,
        )


@to_tuple
def with_start(start, *tests):
    for test in tests:
        yield tuple(cons(start, test))


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
pf64 = Param(f64)

li32 = Local(i32)
li64 = Local(i64)
lf32 = Local(f32)
lf64 = Local(f64)

NOP = Nop()
END = End()
END_TAIL = End.as_tail()
UNREACHABLE = Unreachable()
LOCAL_ZERO_OP = LocalOp.from_opcode(BinaryOpcode.GET_LOCAL, LocalIdx(0))
BASIC_IF_INSTR = If(result_type=(), instructions=(END,), else_instructions=())
NAMED_BASIC_IF_INSTR = NamedIf('$l', BASIC_IF_INSTR)
NOP_IF_INSTR = If(result_type=(), instructions=(NOP, END), else_instructions=())
NOP_IF_WITH_NOP_ELSE_INSTR = If(result_type=(), instructions=(NOP,), else_instructions=(NOP, END))
I32_CONST_1 = I32Const(numpy.uint32(1))
I32_CONST_2 = I32Const(numpy.uint32(2))
I32_CONST_3 = I32Const(numpy.uint32(3))
I32_CONST_7 = I32Const(numpy.uint32(7))
RETURN = Return()


SEXPRESSION_TESTS = tuple(concatv(
    with_start(
        'exprs',
        ("(return)", RETURN),
        ("(return (i32.const 1))", (I32_CONST_1, RETURN)),
    ),
    with_start(
        'exprs',
        ("(nop)", Nop()),
    ),
    with_start(
        'exprs',
        ("(unreachable)", UNREACHABLE),
    ),
    with_start(
        'exprs',
        ("(loop)", Loop((), End.as_tail())),
        ("(loop $l)", NamedLoop('$l', Loop((), End.as_tail()))),
        ("(loop (nop))", Loop((), (Nop(),))),
        (
            "(loop (result i32) (i32.const 7))",
            Loop((i32,), (I32_CONST_7,)),
        ),
        (
            "(loop (call $dummy))",
            Loop((), (UnresolvedCall(UnresolvedFunctionIdx('$dummy')),)),
        ),
    ),
    with_start(
        'locals',
        # unnamed
        ('(local i32)', (li32,)),
        ('(local i64)', (li64,)),
        ('(local f32)', (lf32,)),
        ('(local f64)', (lf64,)),
        ('(local i32 i64)', (li32, li64)),
        ('(local f32 f64)', (lf32, lf64)),
        ('(local f32 f64 i32 i64)', (lf32, lf64, li32, li64)),
        # # named
        ('(local $i i32)', (Local(i32, '$i'),)),
        # # multi
        ('(local f32 f64)\n(local $i i32)', (lf32, lf64, Local(i32, '$i'))),
    ),
    with_start(
        'exprs',
        # folded
        ("(block)", Block((), End.as_tail())),
        ("(block $blk)", NamedBlock('$blk', Block((), End.as_tail()))),
        ("(block (nop))", Block((), (Nop(),))),
        (
            "(block (result i32) (i32.const 7))",
            Block((i32,), (I32_CONST_7,)),
        ),
        (
            "(block (call $dummy))",
            Block((), (UnresolvedCall(UnresolvedFunctionIdx('$dummy')),)),
        ),
        (
            "(block (result i32) (i32.ctz (return (i32.const 1))))",
            Block(
                (i32,),
                (
                    I32_CONST_1,
                    RETURN,
                    UnOp.from_opcode(BinaryOpcode.I32_CTZ),
                )
            ),
        ),
    ),
    with_start(
        'exprs',
        ("(br 0)", Br(LabelIdx(0))),
        ("(br $i)", UnresolvedBr(UnresolvedLabelIdx('$i'))),
        ("(br_if 0)", BrIf(LabelIdx(0))),
        ("(br_if $i)", UnresolvedBrIf(UnresolvedLabelIdx('$i'))),
        ("(br_table 1 2 3)", BrTable((LabelIdx(1), LabelIdx(2)), LabelIdx(3))),
        (
            "(br_table 1 2 $default)",
            UnresolvedBrTable((LabelIdx(1), LabelIdx(2)), UnresolvedLabelIdx('$default')),
        ),
        (
            "(br_table 1 2 $three 4)",
            UnresolvedBrTable(
                (LabelIdx(1), LabelIdx(2), UnresolvedLabelIdx('$three')),
                LabelIdx(4),
            ),
        ),
    ),
    with_start(
        'exprs',
        ("(call $func-name)", UnresolvedCall(UnresolvedFunctionIdx('$func-name'))),
        ("(call 1)", Call(FunctionIdx(1))),
    ),
    with_start(
        'exprs',
        ("(call_indirect (result))", UnresolvedCallIndirect(UnresolvedFunctionType((), ()))),
        (
            "(call_indirect (param i64))",
            UnresolvedCallIndirect(UnresolvedFunctionType((pi64,), ())),
        ),
        (
            "(call_indirect (param i64) (result i32))",
            UnresolvedCallIndirect(UnresolvedFunctionType((pi64,), (i32,))),
        ),
        (
            "(call_indirect (param i64) (param) (param f64 i32 i64))",
            UnresolvedCallIndirect(UnresolvedFunctionType((pi64, pf64, pi32, pi64), ())),
        ),
        (
            "(call_indirect (param) (param i64) (param) (param f64 i32 i64) (param) (param) (result) (result i32) (result) (result))",  # noqa: E501
            UnresolvedCallIndirect(UnresolvedFunctionType((pi64, pf64, pi32, pi64), (i32,))),
        ),
        (
            "(call_indirect (type $check))",
            UnresolvedCallIndirect(UnresolvedTypeIdx('$check')),
        ),
    ),
    with_start(
        'exports',
        ('(export "a" (func 0))', Export("a", FunctionIdx(0))),
        ('(export "a" (func $a))', UnresolvedExport("a", UnresolvedFunctionIdx("$a"))),
        ('(export "a" (global 0))', Export("a", GlobalIdx(0))),
        ('(export "a" (global $a))', UnresolvedExport("a", UnresolvedGlobalIdx("$a"))),
        ('(export "a" (table 0))', Export("a", TableIdx(0))),
        ('(export "a" (table $a))', UnresolvedExport("a", UnresolvedTableIdx("$a"))),
        ('(export "a" (memory 0))', Export("a", MemoryIdx(0))),
        ('(export "a" (memory $a))', UnresolvedExport("a", UnresolvedMemoryIdx("$a"))),
    ),
    with_start(
        'exprs',
        (
            "(i32.add (local.get 1) (i32.const 2))",
            (
                LocalOp.from_opcode(BinaryOpcode.GET_LOCAL, LocalIdx(1)),
                I32_CONST_2,
                BinOp.from_opcode(BinaryOpcode.I32_ADD),
            ),
        ),
        (
            "(i32.mul (i32.add (local.get 1) (i32.const 2)) (i32.const 3))",
            (
                LocalOp.from_opcode(BinaryOpcode.GET_LOCAL, LocalIdx(1)),
                I32_CONST_2,
                BinOp.from_opcode(BinaryOpcode.I32_ADD),
                I32_CONST_3,
                BinOp.from_opcode(BinaryOpcode.I32_MUL),
            ),
        ),
    ),
    with_start(
        'func_type',
        ("(func (param))", UnresolvedFunctionType((), ())),
        ("(func (param) (param))", UnresolvedFunctionType((), ())),
        ("(func (param i32))", UnresolvedFunctionType((Param(i32),), ())),
        ("(func (param $x i32))", UnresolvedFunctionType((Param(i32, '$x'),), ())),
        (
            "(func (param i32 f64 i64))",
            UnresolvedFunctionType((Param(i32), Param(f64), Param(i64)), ()),
        ),
        ("(func (param i32) (param f64))", UnresolvedFunctionType((Param(i32), Param(f64)), ())),
        (
            "(func (param i32 f32) (param $x i64) (param) (param i32 f64))",
            UnresolvedFunctionType(
                (Param(i32), Param(f32), Param(i64, '$x'), Param(i32), Param(f64)),
                (),
            ),
        ),
    ),
    with_start(
        'exprs',
        ("(if (local.get 0) (then))", (LOCAL_ZERO_OP, BASIC_IF_INSTR)),
        ("(if (local.get 0) (then) (else))", (LOCAL_ZERO_OP, BASIC_IF_INSTR)),
        ("(if $l (local.get 0) (then))", (LOCAL_ZERO_OP, NAMED_BASIC_IF_INSTR)),
        ("(if $l (local.get 0) (then) (else))", (LOCAL_ZERO_OP, NAMED_BASIC_IF_INSTR)),
        ("(if (local.get 0) (then (nop)))", (LOCAL_ZERO_OP, NOP_IF_INSTR)),
        ("(if (local.get 0) (then (nop)) (else (nop)))", (LOCAL_ZERO_OP, NOP_IF_WITH_NOP_ELSE_INSTR)),  # noqa: E501
    ),
    with_start(
        'exprs',
        ('(i32.const 1234)', I32Const(1234)),
        ('(i64.const 1234)', I64Const(1234)),
        ('(f32.const 1234)', F32Const(1234)),
        ('(f64.const 1234)', F64Const(1234)),
    ),
    with_start(
        'exprs',
        *tuple(
            (f'({op.text})', instruction.from_opcode(op))
            for ops, instruction in OP_PAIRS
            for op in ops
        ),
    ),
    with_start(
        'exprs',
        ('(i32.wrap_i64)', Wrap()),
        ('(i64.extend_i32_s)', Extend.from_opcode(BinaryOpcode.I64_EXTEND_S_I32)),
        ('(i64.extend_i32_u)', Extend.from_opcode(BinaryOpcode.I64_EXTEND_U_I32)),
    ),
    with_start(
        'exprs',
        ('(i32.trunc_f32_s)', Truncate.from_opcode(BinaryOpcode.I32_TRUNC_S_F32)),
        ('(i32.trunc_f32_u)', Truncate.from_opcode(BinaryOpcode.I32_TRUNC_U_F64)),
        ('(i32.trunc_f64_s)', Truncate.from_opcode(BinaryOpcode.I32_TRUNC_S_F32)),
        ('(i32.trunc_f64_u)', Truncate.from_opcode(BinaryOpcode.I32_TRUNC_U_F64)),
        ('(i64.trunc_f32_s)', Truncate.from_opcode(BinaryOpcode.I64_TRUNC_S_F32)),
        ('(i64.trunc_f32_u)', Truncate.from_opcode(BinaryOpcode.I64_TRUNC_U_F64)),
        ('(i64.trunc_f64_s)', Truncate.from_opcode(BinaryOpcode.I64_TRUNC_S_F32)),
        ('(i64.trunc_f64_u)', Truncate.from_opcode(BinaryOpcode.I64_TRUNC_U_F64)),
    ),
    with_start(
        'exprs',
        ('(f32.convert_i32_s)', Convert.from_opcode(BinaryOpcode.F32_CONVERT_S_I32)),
        ('(f32.convert_i32_u)', Convert.from_opcode(BinaryOpcode.F32_CONVERT_U_I32)),
        ('(f32.convert_i64_s)', Convert.from_opcode(BinaryOpcode.F32_CONVERT_S_I64)),
        ('(f32.convert_i64_u)', Convert.from_opcode(BinaryOpcode.F32_CONVERT_U_I64)),
        ('(f64.convert_i32_s)', Convert.from_opcode(BinaryOpcode.F64_CONVERT_S_I32)),
        ('(f64.convert_i32_u)', Convert.from_opcode(BinaryOpcode.F64_CONVERT_U_I32)),
        ('(f64.convert_i64_s)', Convert.from_opcode(BinaryOpcode.F64_CONVERT_S_I64)),
        ('(f64.convert_i64_u)', Convert.from_opcode(BinaryOpcode.F64_CONVERT_U_I64)),
    ),
    with_start(
        'exprs',
        ('(f32.demote_f64)', Demote()),
        ('(f64.promote_f32)', Promote()),
    ),
    with_start(
        'exprs',
        ('(i32.reinterpret_f32)', Reinterpret.from_opcode(BinaryOpcode.I32_REINTERPRET_F32)),
        ('(i64.reinterpret_f64)', Reinterpret.from_opcode(BinaryOpcode.I64_REINTERPRET_F64)),
        ('(f32.reinterpret_i32)', Reinterpret.from_opcode(BinaryOpcode.F32_REINTERPRET_I32)),
        ('(f64.reinterpret_i64)', Reinterpret.from_opcode(BinaryOpcode.F64_REINTERPRET_I64)),
    ),
    with_start(
        'exprs',
        ("(drop)", Drop()),
        ("(select)", Select()),
    ),
    with_start(
        'params',
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
    with_start(
        'results',
        # simple
        ('(result)', ()),
        ('(result i32)', (i32,)),
        ('(result i32 i64)', (i32, i64)),
        # many
        ('(result i32) (result i64)', (i32, i64)),
    ),
    with_start(
        'exprs',
        ("(local.get $i)", UnresolvedVariableOp(BinaryOpcode.GET_LOCAL, UnresolvedLocalIdx('$i'))),
        ("(local.get 1)", LocalOp.from_opcode(BinaryOpcode.GET_LOCAL, LocalIdx(1))),
        ("(local.set $i)", UnresolvedVariableOp(BinaryOpcode.SET_LOCAL, UnresolvedLocalIdx('$i'))),
        ("(local.set 1)", LocalOp.from_opcode(BinaryOpcode.SET_LOCAL, LocalIdx(1))),
        ("(local.tee $i)", UnresolvedVariableOp(BinaryOpcode.TEE_LOCAL, UnresolvedLocalIdx('$i'))),
        ("(local.tee 1)", LocalOp.from_opcode(BinaryOpcode.TEE_LOCAL, LocalIdx(1))),
        ("(global.get $i)", UnresolvedVariableOp(BinaryOpcode.GET_GLOBAL, UnresolvedGlobalIdx('$i'))),  # noqa: E501
        ("(global.get 1)", GlobalOp.from_opcode(BinaryOpcode.GET_GLOBAL, GlobalIdx(1))),
        ("(global.set $i)", UnresolvedVariableOp(BinaryOpcode.SET_GLOBAL, UnresolvedGlobalIdx('$i'))),  # noqa: E501
        ("(global.set 1)", GlobalOp.from_opcode(BinaryOpcode.SET_GLOBAL, GlobalIdx(1))),
    ),
    with_start(
        'exprs',
        ("(i32.load)", MemoryOp.from_opcode(BinaryOpcode.I32_LOAD, MemoryArg(0, 4))),
        ("(i64.load)", MemoryOp.from_opcode(BinaryOpcode.I64_LOAD, MemoryArg(0, 8))),
        ("(f32.load)", MemoryOp.from_opcode(BinaryOpcode.F32_LOAD, MemoryArg(0, 4))),
        ("(f64.load)", MemoryOp.from_opcode(BinaryOpcode.F64_LOAD, MemoryArg(0, 8))),
        ("(i32.load8_s)", MemoryOp.from_opcode(BinaryOpcode.I32_LOAD8_S, MemoryArg(0, 1))),
        ("(i32.load8_u)", MemoryOp.from_opcode(BinaryOpcode.I32_LOAD8_U, MemoryArg(0, 1))),
        ("(i32.load16_s)", MemoryOp.from_opcode(BinaryOpcode.I32_LOAD16_S, MemoryArg(0, 2))),
        ("(i32.load16_u)", MemoryOp.from_opcode(BinaryOpcode.I32_LOAD16_U, MemoryArg(0, 2))),
        ("(i64.load8_s)", MemoryOp.from_opcode(BinaryOpcode.I64_LOAD8_S, MemoryArg(0, 1))),
        ("(i64.load8_u)", MemoryOp.from_opcode(BinaryOpcode.I64_LOAD8_U, MemoryArg(0, 1))),
        ("(i64.load16_s)", MemoryOp.from_opcode(BinaryOpcode.I64_LOAD16_S, MemoryArg(0, 2))),
        ("(i64.load16_u)", MemoryOp.from_opcode(BinaryOpcode.I64_LOAD16_U, MemoryArg(0, 2))),
        ("(i64.load32_s)", MemoryOp.from_opcode(BinaryOpcode.I64_LOAD32_S, MemoryArg(0, 4))),
        ("(i64.load32_u)", MemoryOp.from_opcode(BinaryOpcode.I64_LOAD32_U, MemoryArg(0, 4))),
        ("(i32.store)", MemoryOp.from_opcode(BinaryOpcode.I32_STORE, MemoryArg(0, 4))),
        ("(i64.store)", MemoryOp.from_opcode(BinaryOpcode.I64_STORE, MemoryArg(0, 8))),
        ("(f32.store)", MemoryOp.from_opcode(BinaryOpcode.F32_STORE, MemoryArg(0, 4))),
        ("(f64.store)", MemoryOp.from_opcode(BinaryOpcode.F64_STORE, MemoryArg(0, 8))),
        ("(i32.store8)", MemoryOp.from_opcode(BinaryOpcode.I32_STORE8, MemoryArg(0, 1))),
        ("(i32.store16)", MemoryOp.from_opcode(BinaryOpcode.I32_STORE16, MemoryArg(0, 2))),
        ("(i64.store8)", MemoryOp.from_opcode(BinaryOpcode.I64_STORE8, MemoryArg(0, 1))),
        ("(i64.store16)", MemoryOp.from_opcode(BinaryOpcode.I64_STORE16, MemoryArg(0, 2))),
        ("(i64.store32)", MemoryOp.from_opcode(BinaryOpcode.I64_STORE32, MemoryArg(0, 4))),
    ),
    with_start(
        'exprs',
        ("(memory.size)", MemorySize()),
        ("(memory.grow)", MemoryGrow()),
    ),
    with_start(
        'func',
        ("(func)", UnresolvedFunction(type=None, locals=(), body=END_TAIL)),
    ),
))


def load_sexpression_tests(test_params):
    for start_name, *params in test_params:
        parser = _get_parser(start_name)
        yield tuple(cons(parser.parse, params))


def pytest_generate_tests(metafunc):
    test_params = load_sexpression_tests(SEXPRESSION_TESTS)
    metafunc.parametrize('parse,sexpr,expected', test_params)


def test_sexpression_parsing(parse, sexpr, expected):
    actual = parse(sexpr)
    assert actual == expected
