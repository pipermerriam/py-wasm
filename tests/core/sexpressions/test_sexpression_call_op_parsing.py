import pytest

from wasm.text.lark import parser
from wasm.instructions.control import (
    Call,
)
from wasm.datatypes import (
    ValType,
    FunctionIdx,
)
from wasm.text.ir import (
    Param,
    UnresolvedFunctionType,
    UnresolvedCallIndirect,
    UnresolvedTypeIdx,
    UnresolvedFunctionIdx,
    UnresolvedCall,
)

i32 = ValType.i32
i64 = ValType.i64
f64 = ValType.f64

pi32 = Param(i32)
pi64 = Param(i64)
pf64 = Param(f64)


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        ("(call_indirect (result))", UnresolvedCallIndirect(UnresolvedFunctionType((), ()))),
        (
            "(call_indirect (param i64))",
            UnresolvedCallIndirect(UnresolvedFunctionType((pi64,), ())),
        ),
        (
            "(call_indirect (param i64) (result i32))",
            UnresolvedCallIndirect(UnresolvedFunctionType((pi64,), (i32,))),
        ),
        (
            "(call_indirect (param i64) (param) (param f64 i32 i64))",
            UnresolvedCallIndirect(UnresolvedFunctionType((pi64, pf64, pi32, pi64), ())),
        ),
        (
            "(call_indirect (type $check))",
            UnresolvedCallIndirect(UnresolvedTypeIdx('$check')),
        ),
    ),
)
def test_sexpression_call_indirect_instruction_parsing(sexpr, expected):
    actual = parser.parse(sexpr)
    assert actual == expected


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        ("(call $func-name)", UnresolvedCall(UnresolvedFunctionIdx('$func-name'))),
        ("(call 1)", Call(FunctionIdx(1))),
    ),
)
def test_sexpression_call_instruction_parsing(sexpr, expected):
    actual = parser.parse(sexpr)
    assert actual == expected
