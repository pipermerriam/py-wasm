from typing import Tuple

from wasm import constants
from wasm.datatypes import (
    Module,
    Store,
)
from wasm.typing import (
    TValue,
)
from wasm.instructions import BaseInstruction
from wasm.execution import (
    Configuration,
    Frame,
)


def wasm_eval(expression: Tuple[BaseInstruction, ...],
              ) -> Tuple[TValue, ...]:
    store = Store()
    module = Module(constants.VERSION_1, (), (), (), (), (), (), (), (), (), ())

    frame = Frame(
        module=module,
        locals=[],
        instructions=expression,
        arity=1,
    )
    config = Configuration(store)
    config.push_frame(frame)

    results = config.execute()
    if len(results) != 1:
        raise Exception("INVALID")
    return results[0]
