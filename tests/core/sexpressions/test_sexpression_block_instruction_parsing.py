import pytest

import numpy

from wasm.text import parse
from wasm.datatypes import ValType
from wasm.opcodes import BinaryOpcode
from wasm.instructions.control import (
    Block,
    End,
    Nop,
    Return,
)
from wasm.instructions.numeric import (
    UnOp,
    I32Const,
)
from wasm.text.ir import NamedBlock, UnresolvedCall, UnresolvedFunctionIdx


i32 = ValType.i32


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        # folded
        ("(block)", Block((), End.as_tail())),
        ("(block $blk)", NamedBlock('$blk', Block((), End.as_tail()))),
        ("(block (nop))", Block((), (Nop(),))),
        (
            "(block (result i32) (i32.const 7))",
            Block((i32,), (I32Const(numpy.uint32(7)),)),
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
                    I32Const(numpy.uint32(1)),
                    Return(),
                    UnOp.from_opcode(BinaryOpcode.I32_CTZ),
                )
            ),
        ),
    ),
)
def test_sexpression_block_instructions_parsing(sexpr, expected):
    actual, = parse(sexpr)
    assert actual == expected
