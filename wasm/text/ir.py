from abc import ABC
from typing import (
    NamedTuple,
    Optional,
    Tuple,
    Type,
    TypeVar,
    Union,
)

from wasm.datatypes import (
    LabelIdx,
    ValType,
)
from wasm.instructions.control import (
    Block,
    If,
    Loop,
)
from wasm.opcodes import (
    BinaryOpcode,
)


class BaseUnresolved(ABC):
    pass


TInstruction = TypeVar("TInstruction")


def register(cls: Type[TInstruction]) -> Type[TInstruction]:
    """
    Class decorator which registeres the class with the `BaseUnresolved` base
    class.
    """
    BaseUnresolved.register(cls)
    return cls


@register
class Local(NamedTuple):
    valtype: ValType
    name: Optional[str] = None


@register
class Param(NamedTuple):
    valtype: ValType
    name: Optional[str] = None


@register
class UnresolvedFunctionType(NamedTuple):
    params: Tuple[Param, ...]
    results: Tuple[ValType, ...]


@register
class UnresolvedTypeIdx(NamedTuple):
    name: str


@register
class UnresolvedLabelIdx(NamedTuple):
    name: str


@register
class UnresolvedLocalIdx(NamedTuple):
    name: str


@register
class UnresolvedGlobalIdx(NamedTuple):
    name: str


@register
class UnresolvedFunctionIdx(NamedTuple):
    name: str


@register
class UnresolvedTableIdx(NamedTuple):
    name: str


@register
class UnresolvedMemoryIdx(NamedTuple):
    name: str


@register
class UnresolvedVariableOp(NamedTuple):
    opcode: BinaryOpcode
    name: UnresolvedLocalIdx


@register
class UnresolvedCall(NamedTuple):
    func_idx: UnresolvedFunctionIdx


@register
class UnresolvedCallIndirect(NamedTuple):
    type_idx: Union[UnresolvedFunctionType, UnresolvedTypeIdx]


@register
class UnresolvedBr(NamedTuple):
    label_idx: UnresolvedLabelIdx


@register
class UnresolvedBrIf(NamedTuple):
    label_idx: UnresolvedLabelIdx


@register
class UnresolvedBrTable(NamedTuple):
    label_indices: Tuple[Union[UnresolvedLabelIdx, LabelIdx], ...]
    default_idx: Union[UnresolvedLabelIdx, LabelIdx]


@register
class NamedBlock(NamedTuple):
    name: str
    block: Block


@register
class NamedLoop(NamedTuple):
    name: str
    loop: Loop


@register
class NamedIf(NamedTuple):
    name: str
    loop: If


@register
class UnresolvedExport(NamedTuple):
    name: str
    function_idx: UnresolvedFunctionIdx
