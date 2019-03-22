import pytest

from wasm.datatypes import (
    LocalIdx,
)
from wasm.instructions.control import (
    End,
    If,
    Nop,
)
from wasm.instructions.variable import (
    LocalOp,
)
from wasm.opcodes import (
    BinaryOpcode,
)
from wasm.text.ir import (
    NamedIf,
)

NOP = Nop()
END = End()
LOCAL_ZERO_OP = LocalOp.from_opcode(BinaryOpcode.GET_LOCAL, LocalIdx(0))
BASIC_IF_INSTR = If(result_type=(), instructions=(END,), else_instructions=())
NAMED_BASIC_IF_INSTR = NamedIf('$l', BASIC_IF_INSTR)
NOP_IF_INSTR = If(result_type=(), instructions=(NOP, END), else_instructions=())
NOP_IF_WITH_NOP_ELSE_INSTR = If(result_type=(), instructions=(NOP,), else_instructions=(NOP, END))


@pytest.mark.parametrize(
    'sexpr,expected',
    (
        # folded
        ("(if (local.get 0) (then))", (LOCAL_ZERO_OP, BASIC_IF_INSTR)),
        ("(if (local.get 0) (then) (else))", (LOCAL_ZERO_OP, BASIC_IF_INSTR)),
        ("(if $l (local.get 0) (then))", (LOCAL_ZERO_OP, NAMED_BASIC_IF_INSTR)),
        ("(if $l (local.get 0) (then) (else))", (LOCAL_ZERO_OP, NAMED_BASIC_IF_INSTR)),
        ("(if (local.get 0) (then (nop)))", (LOCAL_ZERO_OP, NOP_IF_INSTR)),
        ("(if (local.get 0) (then (nop)) (else (nop)))", (LOCAL_ZERO_OP, NOP_IF_WITH_NOP_ELSE_INSTR)),  # noqa: E501
    ),
)
def test_sexpression_block_instructions_parsing(sexpr, expected, parse):
    actual = parse(sexpr)
    assert actual == expected
