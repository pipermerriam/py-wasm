import pytest

from wasm.datatypes import (
    Export,
    FunctionIdx,
)
from wasm.text.ir import (
    UnresolvedExport,
    UnresolvedFunctionIdx,
)

PARSE_START = 'exports'


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        # unnamed
        ('(export "a" (func 0))', Export("a", FunctionIdx(0))),
        ('(export "a" (func $a))', UnresolvedExport("a", UnresolvedFunctionIdx("$a"))),
    ),
)
def test_sexpression_exports_parsing(sexpr, expected, parse):
    actual = parse(sexpr)
    assert type(actual) is type(expected)
    assert actual == expected
