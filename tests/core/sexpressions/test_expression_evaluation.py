import pytest

import numpy

from wasm.instructions import (
    End,
    I32Const,
)
from wasm.tools.eval import wasm_eval

END_TAIL = End.as_tail()
CONST_0_EXPR = (I32Const(numpy.uint32(0)), End())
CONST_1_EXPR = (I32Const(numpy.uint32(1)), End())


@pytest.mark.parametrize(
    'expression,expected',
    (
        (CONST_0_EXPR, numpy.uint32(0)),
        (CONST_1_EXPR, numpy.uint32(1)),
    )
)
def test_wasm_eval(expression, expected):
    actual = wasm_eval(expression)

    assert isinstance(actual, type(expected))
    assert actual == expected
