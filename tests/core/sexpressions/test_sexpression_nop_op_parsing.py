import pytest

from wasm.instructions.control import (
    Nop,
)


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        ("(nop)", Nop()),
    ),
)
def test_sexpression_nop_instructions_parsing(sexpr, expected, parse):
    actual = parse(sexpr)
    assert actual == expected
