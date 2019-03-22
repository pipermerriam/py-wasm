import pytest

from wasm.datatypes import (
    ValType,
)

i32 = ValType.i32
i64 = ValType.i64

PARSE_START = 'results'


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        # simple
        ('(result)', ()),
        ('(result i32)', (i32,)),
        ('(result i32 i64)', (i32, i64)),
        # many
        ('(result i32) (result i64)', (i32, i64)),
    ),
)
def test_sexpression_results_parsing(sexpr, expected, parse):
    actual = parse(sexpr)
    assert actual == expected
