from abc import ABC
from typing import (
    Any,
    Type,
    TypeVar,
)

from wasm.opcodes import (
    BinaryOpcode,
)


class BaseInstruction(ABC):
    """
    Abstract base class that all instruction classes are registered with to
    allow for isinstance checks.
    """
    opcode: BinaryOpcode


TInstruction = TypeVar("TInstruction")


def register(cls: Type[TInstruction]) -> Type[TInstruction]:
    """
    Class decorator which registeres the class with the `BaseInstruction` base
    class.
    """
    BaseInstruction.register(cls)
    return cls


class SimpleOp:
    """
    Base class for opcodes which don't have any arguments or state whos
    instances can be reused.
    """
    opcode: BinaryOpcode

    def __str__(self) -> str:
        return self.opcode.text

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__}: {str(self)}>"

    def __eq__(self, other: Any) -> bool:
        return type(self) is type(other)
