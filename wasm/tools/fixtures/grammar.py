from pyparsing import (
    Literal,
    NoMatch,
    Or,
)

from wasm.text.grammar import (
    expr,
    STRING,
    SYMBOL_ID,
    many,
    maybe,
    open,
    close,
    ws,
    datastring,
)

from . import parsers


REGISTER = Literal("register").suppress()

MODULE = Literal("module").suppress()

BINARY = Literal("binary").suppress()
QUOTED = Literal("quote").suppress()

INVOKE = Literal("invoke").suppress()
GET = Literal("get").suppress()

ASSERT_RETURN = Literal("assert_return").suppress()
ASSERT_RETURN_CANONICAL_NAN = Literal("assert_return_canonical_nan").suppress()
ASSERT_RETURN_ARITHMETIC_NAN = Literal("assert_return_arithmetic_nan").suppress()
ASSERT_TRAP = Literal("assert_trap").suppress()
ASSERT_MALFORMED = Literal("assert_malformed").suppress()
ASSERT_INVALID = Literal("assert_invalid").suppress()
ASSERT_UNLINKABLE = Literal("assert_unlinkable").suppress()

register = (open + REGISTER + ws + STRING + maybe(SYMBOL_ID) + close).setParseAction(parsers.parse_register)  # noqa: E501

utf8string = many(STRING).setParseAction(parsers.parse_utf8)

module_binary = (open + MODULE + maybe(SYMBOL_ID) + ws + BINARY + maybe(datastring, b'') + close).setParseAction(parsers.parse_binary_module)  # noqa: E501
module_quoted = (open + MODULE + maybe(SYMBOL_ID) + ws + QUOTED + maybe(utf8string, '') + close).setParseAction(parsers.parse_quote_module)  # noqa: E501
module = module_binary ^ module_quoted

exprs = many(open + expr + close).setParseAction(parsers.parse_exprs)

invoke = (open + INVOKE + maybe(SYMBOL_ID) + ws + STRING + maybe(exprs, ()) + close).setParseAction(parsers.parse_invoke)  # noqa: E501
get = (open + GET + maybe(SYMBOL_ID) + ws + STRING + close).setParseAction(parsers.parse_get)

action = invoke ^ get

assert_return = (open + ASSERT_RETURN + ws + action + maybe(exprs, ()) + close).setParseAction(parsers.parse_assert_return)  # noqa: E501
assert_return_canonical_nan = (open + ASSERT_RETURN_CANONICAL_NAN + ws + action + close).setParseAction(parsers.parse_assert_return_canonical_nan)  # noqa: E501
assert_return_arithmetic_nan = open + ASSERT_RETURN_ARITHMETIC_NAN + ws + action + close
assert_malformed = open + ASSERT_MALFORMED + ws + module + ws + STRING + close
assert_invalid = open + ASSERT_INVALID + ws + module + ws + STRING + close
assert_unlinkable = open + ASSERT_UNLINKABLE + ws + module + ws + STRING + close

assert_trap_on_action = open + ASSERT_TRAP + ws + action + ws + STRING + close
assert_trap_on_instantiation = open + ASSERT_TRAP + ws + module + ws + STRING + close

assert_trap = assert_trap_on_action ^ assert_trap_on_instantiation

assertion = Or((
    assert_return,
    assert_return_canonical_nan,
    assert_return_arithmetic_nan,
    assert_trap,
    assert_malformed,
    assert_invalid,
    assert_unlinkable,
))

meta = NoMatch()

cmd = module ^ register ^ action ^ assertion ^ meta
