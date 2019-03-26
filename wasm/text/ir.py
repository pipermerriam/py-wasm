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
    Function,
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


class UnresolvedInstruction(BaseUnresolved):
    pass


TClass = TypeVar("TClass")


def register(cls: Type[TClass]) -> Type[TClass]:
    """
    Class decorator which registeres the class with the `BaseUnresolved` base
    class.
    """
    BaseUnresolved.register(cls)
    return cls


def register_instruction(cls: Type[TClass]) -> Type[TClass]:
    """
    Class decorator which registeres the class with the `BaseUnresolved` base
    class.
    """
    UnresolvedInstruction.register(cls)
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


@register_instruction
@register
class UnresolvedVariableOp(NamedTuple):
    opcode: BinaryOpcode
    name: UnresolvedLocalIdx


@register_instruction
@register
class UnresolvedCall(NamedTuple):
    func_idx: UnresolvedFunctionIdx


@register_instruction
@register
class UnresolvedCallIndirect(NamedTuple):
    type_idx: Union[UnresolvedFunctionType, UnresolvedTypeIdx]


@register_instruction
@register
class UnresolvedBr(NamedTuple):
    label_idx: UnresolvedLabelIdx


@register_instruction
@register
class UnresolvedBrIf(NamedTuple):
    label_idx: UnresolvedLabelIdx


@register_instruction
@register
class UnresolvedBrTable(NamedTuple):
    label_indices: Tuple[Union[UnresolvedLabelIdx, LabelIdx], ...]
    default_idx: Union[UnresolvedLabelIdx, LabelIdx]


@register_instruction
@register
class NamedBlock(NamedTuple):
    name: str
    block: Block


@register_instruction
@register
class NamedLoop(NamedTuple):
    name: str
    loop: Loop


@register_instruction
@register
class NamedIf(NamedTuple):
    name: str
    loop: If


@register
class UnresolvedExport(NamedTuple):
    name: str
    function_idx: UnresolvedFunctionIdx


@register
class UnresolvedFunction(NamedTuple):
    type: Union[None, UnresolvedTypeIdx, UnresolvedFunctionType]
    locals: Tuple[ValType, ...]
    body: Tuple['BaseInstruction', ...]


@register
class NamedFunction(NamedTuple):
    name: str
    function: Function
