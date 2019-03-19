import pytest


from wasm.text import NodeVisitor


@pytest.fixture
def parse(request):
    class Visitor(NodeVisitor):
        grammar = request.module.grammar

    visitor = Visitor()
    return visitor.parse
