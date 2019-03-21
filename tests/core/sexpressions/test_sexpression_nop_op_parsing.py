import pytest

from wasm.text.lark import parser
from wasm.instructions.control import (
    Nop
)


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        ("(nop)", Nop()),
    ),
)
def test_sexpression_nop_instructions_parsing(sexpr, expected):
    actual = parser.parse(sexpr)
    assert actual == expected
