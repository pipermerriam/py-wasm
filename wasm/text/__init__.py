from lark import Lark

from .lark import (
    GRAMMAR,
)

from .transformer import (
    WasmTransformer,
)

default_parser = Lark(GRAMMAR, parser="lalr")
default_transformer = WasmTransformer()


def parse(text, parser=None, transformer=None):
    if parser is None:
        parser = default_parser
    if transformer is None:
        transformer = default_transformer

    tree = parser.parse(text)
    result = transformer.transform(tree)
    return result
