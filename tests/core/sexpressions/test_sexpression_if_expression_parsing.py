import pytest

from wasm.text import parse
from wasm.datatypes import LocalIdx
from wasm.opcodes import BinaryOpcode
from wasm.instructions.control import (
    End,
    If,
)
from wasm.instructions.variable import (
    LocalOp,
)


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        # folded
        (
            "(if (local.get 0) (then))",
            (
                LocalOp.from_opcode(BinaryOpcode.GET_LOCAL, LocalIdx(0)),
                If(
                    result_type=(),
                    instructions=(),
                    else_instructions=(),
                ),
            ),
        ),
    ),
)
def test_sexpression_block_instructions_parsing(sexpr, expected):
    actual = parse(sexpr)
    assert actual == expected
