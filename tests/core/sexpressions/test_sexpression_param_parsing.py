import pytest

from wasm.text import GRAMMAR
from wasm.datatypes import ValType
from wasm.text.ir import Param


i32 = ValType.i32
i64 = ValType.i64
f32 = ValType.f32
f64 = ValType.f64


grammar = GRAMMAR['params']


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        # simple
        ('(param)', ()),
        ('(param i32)', (Param(i32),)),
        # multiple
        ('(param i32 i64)', (Param(i32), Param(i64),)),
        ('(param) (param)', ()),
        ('(param i32) (param i64)', (Param(i32), Param(i64),)),
        # named
        ('(param $x i32)', (Param(i32, '$x'),)),
    ),
)
def test_sexpression_param_parsing(sexpr, expected, parse):
    actual = parse(sexpr)
    assert actual == expected
