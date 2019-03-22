from typing import (
    TYPE_CHECKING,
    Any,
    NamedTuple,
    Sequence,
    Tuple,
    cast,
)

from wasm._utils.interned import (
    Interned,
)
from wasm.datatypes import (
    FunctionIdx,
    LabelIdx,
    TypeIdx,
    ValType,
)
from wasm.opcodes import (
    BinaryOpcode,
)

from .base import (
    BaseInstruction,
    SimpleOp,
    register,
)

if TYPE_CHECKING:
    from wasm.instructions import (  # noqa: F401
        Instruction,
    )


def stringify_instructions(instructions: Sequence['Instruction']) -> str:
    return ' > '.join(map(str, instructions))


class BaseBlockInstruction:
    def __init__(self,
                 result_type: Tuple[ValType, ...],
                 instructions: Tuple[BaseInstruction, ...]) -> None:
        self.result_type = result_type
        self.instructions = instructions

    def __str__(self) -> str:
        rt = f"({','.join((str(v) for v in self.result_type))})"
        return f"{self.opcode.text}[rt={rt},expr={stringify_instructions(self.instructions)}]"

    def __repr__(self) -> str:
        return f"<{self.opcode.text}: {str(self)}>"

    def __eq__(self, other: Any) -> bool:
        if type(self) is not type(other):
            return False
        elif self.result_type != other.result_type:
            return False
        elif self.instructions != other.instructions:
            return False
        else:
            return True


@register
class Block(BaseBlockInstruction):
    opcode = BinaryOpcode.BLOCK

    @classmethod
    def wrap(cls,
             result_type: Tuple[ValType, ...],
             instructions: Tuple[BaseInstruction, ...]
             ) -> Tuple[BaseInstruction, ...]:
        return cast(Tuple[BaseInstruction, ...], (
            cls(
                result_type,
                instructions,
            ),
        ))

    @classmethod
    def wrap_with_end(cls,
                      result_type: Tuple[ValType, ...],
                      instructions: Tuple[BaseInstruction, ...]
                      ) -> Tuple[BaseInstruction, ...]:
        expression = cls(result_type, instructions)
        return cast(Tuple[BaseInstruction, ...], expression + End.as_tail())


@register
class Br(Interned):
    opcode = BinaryOpcode.BR

    def __init__(self, label_idx: LabelIdx) -> None:
        self.label_idx = label_idx

    def __str__(self) -> str:
        return f"{self.opcode.text}[{self.label_idx}]"


@register
class BrTable(Interned):
    opcode = BinaryOpcode.BR_TABLE

    def __init__(self,
                 label_indices: Tuple[LabelIdx, ...],
                 default_idx: LabelIdx) -> None:
        self.label_indices = label_indices
        self.default_idx = default_idx

    @property
    def opcode(self) -> BinaryOpcode:
        return BinaryOpcode.BR_TABLE

    def __str__(self) -> str:
        return (
            f"{self.opcode.text}["
            f"labels={':'.join((str(l) for l in self.label_indices))},"
            f"default={self.default_idx}]"
        )

    def __repr__(self) -> str:
        return f"<BrTable: {str(self)}>"

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, BrTable):
            return False
        elif self.default_idx != self.default_idx:
            return False
        elif self.label_indices != other.label_indices:
            return False
        else:
            return True


@register
class BrIf(Interned):
    opcode = BinaryOpcode.BR_IF

    def __init__(self, label_idx: LabelIdx) -> None:
        self.label_idx = label_idx

    def __str__(self) -> str:
        return f"{self.opcode.text}[{self.label_idx}]"


@register
class Loop(BaseBlockInstruction):
    opcode = BinaryOpcode.LOOP


@register
class If:
    opcode = BinaryOpcode.IF

    def __init__(self,
                 result_type: Tuple[ValType, ...],
                 instructions: Tuple[BaseInstruction, ...],
                 else_instructions: Tuple[BaseInstruction, ...]) -> None:
        self.result_type = result_type
        self.instructions = instructions
        self.else_instructions = else_instructions

    def __str__(self) -> str:
        rt = f"({','.join((v.value for v in self.result_type))})"
        if self.else_instructions:
            return (
                f"{self.opcode.text}["
                f"rt={rt},"
                f"main={stringify_instructions(self.instructions)},"
                f"else={stringify_instructions(self.else_instructions)}]"
            )
        else:
            return f"{self.opcode.text}[rt={rt},main={self.instructions}]"

    def __repr__(self) -> str:
        return f"<IF: {str(self)}>"

    def __eq__(self, other: Any) -> bool:
        if type(self) is not type(other):
            return False
        elif self.result_type != other.result_type:
            return False
        elif self.instructions != other.instructions:
            return False
        elif self.else_instructions != other.else_instructions:
            return False
        else:
            return True


@register
class Else(SimpleOp):
    opcode = BinaryOpcode.ELSE

    @classmethod
    def as_tail(cls) -> Tuple['BaseInstruction', ...]:
        return cast(Tuple['BaseInstruction', ...], (cls(),))


@register
class End(SimpleOp):
    opcode = BinaryOpcode.END

    @classmethod
    def as_tail(cls) -> Tuple['BaseInstruction', ...]:
        return cast(Tuple['BaseInstruction', ...], (cls(),))


@register
class CallIndirect(Interned):
    opcode = BinaryOpcode.CALL_INDIRECT

    def __init__(self, type_idx: TypeIdx) -> None:
        self.type_idx = type_idx

    def __str__(self) -> str:
        return f"{self.opcode.text}[{self.type_idx}]"


@register
class Nop(SimpleOp):
    opcode = BinaryOpcode.NOP


@register
class Unreachable(SimpleOp):
    opcode = BinaryOpcode.UNREACHABLE


@register
class Call(Interned):
    opcode = BinaryOpcode.CALL

    def __init__(self, function_idx: FunctionIdx) -> None:
        self.function_idx = function_idx

    def __str__(self) -> str:
        return f"{self.opcode.text}[{self.function_idx}]"


@register
class Return(SimpleOp):
    opcode = BinaryOpcode.RETURN
