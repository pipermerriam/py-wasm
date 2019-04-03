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
    Table,
    TypeIdx,
    FunctionIdx,
    LabelIdx,
    ValType,
    Global,
    Memory,
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
class LinkedFunctionType(NamedTuple):
    # TODO: either this should be converted to be a `NamedFunctionType` and
    # inline the name, or all of the `NamedThing` classes should be converted
    # to follow this pattern (the latter might be more appropriate).
    type_idx: Union[UnresolvedTypeIdx, TypeIdx]
    function_type: UnresolvedFunctionType


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
class UnresolvedFunction(NamedTuple):
    type: Union[None, UnresolvedTypeIdx, UnresolvedFunctionType]
    locals: Tuple[ValType, ...]
    body: Tuple['BaseInstruction', ...]
    name: Optional[str] = None


@register
class UnresolvedExport(NamedTuple):
    name: str
    desc: Union[UnresolvedFunctionIdx, UnresolvedGlobalIdx, UnresolvedMemoryIdx, UnresolvedTableIdx]


@register
class UnresolvedImport(NamedTuple):
    module_name: str
    as_name: str
    desc: Union[
        LinkedFunctionType,  # TODO: UnresolvedTypeIdx? also
        UnresolvedGlobalIdx,
        UnresolvedMemoryIdx,
        UnresolvedTableIdx,
    ]


@register
class NamedGlobal(NamedTuple):
    name: str
    global_: Global


@register
class NamedTable(NamedTuple):
    name: str
    table: Table


@register
class UnresolvedElementSegment(NamedTuple):
    table_idx: Union[NamedTable, UnresolvedTableIdx]
    offset: Tuple['BaseInstruction', ...]
    init: Tuple[FunctionIdx, ...]


@register
class NamedMemory(NamedTuple):
    name: str
    table: Memory


@register
class UnresolvedDataSegment(NamedTuple):
    memory_idx: UnresolvedMemoryIdx
    offset: Tuple['BaseInstruction', ...]
    init: bytes


@register
class UnresolvedStartFunction(NamedTuple):
    function_idx: UnresolvedFunctionIdx
