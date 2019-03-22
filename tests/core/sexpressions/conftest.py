import pytest

from lark import Lark

from wasm.text import parser
from wasm.text.transformer import WasmTransformer
from wasm.text.lark import (
    GRAMMAR,
)


@pytest.fixture(scope="session")
def base_parse(request):
    return parser.parse


@pytest.fixture(scope="module")
def parse(request, base_parse):
    if hasattr(request.module, 'PARSE_START'):
        parser = Lark(
            GRAMMAR,
            parser="lalr",
            transformer=WasmTransformer(),
            start=request.module.PARSE_START,
        )
        return parser.parse
    else:
        return base_parse
