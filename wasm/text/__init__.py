from .visitor import (
    NodeVisitor,
)
from .grammar import (  # noqa: F401
    GRAMMAR,
)


visitor = NodeVisitor()
parse = visitor.parse
