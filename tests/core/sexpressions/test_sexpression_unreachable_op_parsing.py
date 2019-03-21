import pytest

from wasm.text.lark import parser
from wasm.instructions.control import (
    Unreachable,
)


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        ("(unreachable)", Unreachable()),
    ),
)
def test_sexpression_unreachable_instructions_parsing(sexpr, expected):
    actual = parser.parse(sexpr)
    assert actual == expected
