from lark import Lark

from .transformer import WasmTransformer
from .lark import (
    GRAMMAR,
)


parser = Lark(GRAMMAR, parser="lalr", transformer=WasmTransformer())
parse = parser.parse
