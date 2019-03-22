from lark import Lark

from .lark import (
    GRAMMAR,
)
from .transformer import (
    WasmTransformer,
)

parser = Lark(GRAMMAR, parser="lalr", transformer=WasmTransformer())
parse = parser.parse
