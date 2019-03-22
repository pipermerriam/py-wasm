import numpy
import pytest

from wasm.datatypes import (
    ValType,
)
from wasm.instructions.control import (
    End,
    Loop,
    Nop,
)
from wasm.instructions.numeric import (
    I32Const,
)
from wasm.text import (
    parse,
)
from wasm.text.ir import (
    NamedLoop,
    UnresolvedCall,
    UnresolvedFunctionIdx,
)

i32 = ValType.i32


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        ("(loop)", Loop((), End.as_tail())),
        ("(loop $l)", NamedLoop('$l', Loop((), End.as_tail()))),
        ("(loop (nop))", Loop((), (Nop(),))),
        (
            "(loop (result i32) (i32.const 7))",
            Loop((i32,), (I32Const(numpy.uint32(7)),)),
        ),
        (
            "(loop (call $dummy))",
            Loop((), (UnresolvedCall(UnresolvedFunctionIdx('$dummy')),)),
        ),
    ),
)
def test_sexpression_loop_instructions_parsing(sexpr, expected):
    actual = parse(sexpr)
    assert actual == expected
