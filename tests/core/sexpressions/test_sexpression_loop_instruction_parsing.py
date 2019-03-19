
import pytest

from wasm.text import parse
from wasm.instructions.control import (
    Block,
    End,
    Nop,
)
from wasm.text.ir import NamedBlock


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        ("(block)", Block((), End.as_tail())),
        ("(block $blk)", NamedBlock('$blk', Block((), End.as_tail()))),
        ("(block (nop))", Block((), (Nop(),))),
    ),
)
def test_sexpression_block_instructions_parsing(sexpr, expected):
    actual, = parse(sexpr)
    assert actual == expected
