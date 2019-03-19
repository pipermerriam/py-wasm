import pytest

import numpy

from wasm.text import parse
from wasm.datatypes import LocalIdx
from wasm.opcodes import BinaryOpcode
from wasm.instructions.variable import (
    LocalOp,
)
from wasm.instructions.numeric import (
    BinOp,
    I32Const,
)


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        (
            "(i32.add (local.get 1) (i32.const 2))",
            (
                LocalOp.from_opcode(BinaryOpcode.GET_LOCAL, LocalIdx(1)),
                I32Const(numpy.uint32(2)),
                BinOp.from_opcode(BinaryOpcode.I32_ADD),
            ),
        ),
        (
            "(i32.mul (i32.add (local.get 1) (i32.const 2)) (i32.const 3))",
            (
                LocalOp.from_opcode(BinaryOpcode.GET_LOCAL, LocalIdx(1)),
                I32Const(numpy.uint32(2)),
                BinOp.from_opcode(BinaryOpcode.I32_ADD),
                I32Const(numpy.uint32(3)),
                BinOp.from_opcode(BinaryOpcode.I32_MUL),
            ),
        ),
    ),
)
def test_sexpression_block_instructions_parsing(sexpr, expected):
    actual = parse(sexpr)
    assert actual == expected
