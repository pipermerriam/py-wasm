import pytest

from wasm.datatypes import (
    GlobalIdx,
    LocalIdx,
)
from wasm.instructions.variable import (
    GlobalOp,
    LocalOp,
)
from wasm.opcodes import (
    BinaryOpcode,
)
from wasm.text.ir import (
    UnresolvedGlobalIdx,
    UnresolvedLocalIdx,
    UnresolvedVariableOp,
)


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        ("(local.get $i)", UnresolvedVariableOp(BinaryOpcode.GET_LOCAL, UnresolvedLocalIdx('$i'))),
        ("(local.get 1)", LocalOp.from_opcode(BinaryOpcode.GET_LOCAL, LocalIdx(1))),
        ("(local.set $i)", UnresolvedVariableOp(BinaryOpcode.SET_LOCAL, UnresolvedLocalIdx('$i'))),
        ("(local.set 1)", LocalOp.from_opcode(BinaryOpcode.SET_LOCAL, LocalIdx(1))),
        ("(local.tee $i)", UnresolvedVariableOp(BinaryOpcode.TEE_LOCAL, UnresolvedLocalIdx('$i'))),
        ("(local.tee 1)", LocalOp.from_opcode(BinaryOpcode.TEE_LOCAL, LocalIdx(1))),
        ("(global.get $i)", UnresolvedVariableOp(BinaryOpcode.GET_GLOBAL, UnresolvedGlobalIdx('$i'))),  # noqa: E501
        ("(global.get 1)", GlobalOp.from_opcode(BinaryOpcode.GET_GLOBAL, GlobalIdx(1))),
        ("(global.set $i)", UnresolvedVariableOp(BinaryOpcode.SET_GLOBAL, UnresolvedGlobalIdx('$i'))),  # noqa: E501
        ("(global.set 1)", GlobalOp.from_opcode(BinaryOpcode.SET_GLOBAL, GlobalIdx(1))),
    ),
)
def test_sexpression_local_variable_parsing(sexpr, expected, parse):
    actual = parse(sexpr)
    assert actual == expected
