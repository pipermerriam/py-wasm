import pytest

from wasm.instructions.parametric import (
    Drop,
    Select,
)
from wasm.text import (
    parse,
)


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        ("(drop)", Drop()),
        ("(select)", Select()),
    ),
)
def test_sexpression_parametric_instruction_parsing(sexpr, expected):
    actual = parse(sexpr)
    assert actual == expected
