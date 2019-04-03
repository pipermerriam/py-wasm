import numpy

from wasm._utils.decorators import to_tuple
from wasm._utils.toolz import cons, concatv
from wasm.datatypes import (
    ValType,
)
from wasm.tools.fixtures import grammar
from wasm.tools.fixtures.datatypes import (
    Expected,
    AssertReturnCommand,
    AssertReturnCanonicalNan,
    Argument,
    Get,
    Invoke,
    Register,
    BinaryModule,
    QuoteModule,
    canonical_nan,
)


@to_tuple
def with_parser(parser, *tests):
    for test in tests:
        yield tuple(cons(parser, test))


EMPTY_MODULE_BYTES = b"\00asm\01\00\00\00"
EMPTY_BINARY_MODULE = BinaryModule(line=1, name=None, data=EMPTY_MODULE_BYTES)
EMPTY_NAMED_BINARY_MODULE = BinaryModule(line=1, name='$M', data=EMPTY_MODULE_BYTES)

EMPTY_QUOTED_MODULE = QuoteModule(line=1, name=None, data="(module)")
EMPTY_NAMED_QUOTED_MODULE = QuoteModule(line=1, name='$M', data="(module)")

ARG_I32_0 = Argument(ValType.i32, numpy.uint32(0))
ARG_I32_1 = Argument(ValType.i32, numpy.uint32(1))

ARG_F32_NAN = Argument(ValType.f32, ValType.f32.nan)

EXP_I32_0 = Expected(ValType.i32, numpy.uint32(0))
EXP_I32_1 = Expected(ValType.i32, numpy.uint32(1))

EXP_F32_NAN = Expected(ValType.f32, canonical_nan)

SCRIPTS_SEXPRESSION_TESTS = tuple(concatv(
    with_parser(
        grammar.register,
        ('(register "m")', Register(1, None, "m")),
        ('(register "m" $m)', Register(1, "$m", "m")),
    ),
    with_parser(
        grammar.module,
        (r'(module binary "\00asm\01\00\00\00")', EMPTY_BINARY_MODULE),
        (r'(module binary "\00asm" "\01\00\00\00")', EMPTY_BINARY_MODULE),
        (r'(module $M binary "\00asm\01\00\00\00")', EMPTY_NAMED_BINARY_MODULE),
        (r'(module $M binary "\00asm" "\01\00\00\00")', EMPTY_NAMED_BINARY_MODULE),
        ('(module quote "(module)")', EMPTY_QUOTED_MODULE),
        ('(module $M quote "(module)")', EMPTY_NAMED_QUOTED_MODULE),
    ),
    with_parser(
        grammar.action,
        ('(invoke "empty")', Invoke(1, module_name=None, function_name="empty", args=())),
        ('(invoke $M "empty")', Invoke(1, module_name='$M', function_name="empty", args=())),
        ('(invoke "call" (i32.const 0))', Invoke(1, module_name=None, function_name="call", args=(ARG_I32_0,))),  # noqa: E501
        (
            '(invoke "call" (i32.const 0) (i32.const 1))',
            Invoke(1, module_name=None, function_name="call", args=(ARG_I32_0, ARG_I32_1))
        ),
        ('(get "e")', Get(1, module_name=None, global_name="e")),
        ('(get $Global "e")', Get(1, module_name='$Global', global_name="e")),
    ),
    with_parser(
        grammar.assertion,
        (
            '(assert_return (invoke "f" (i32.const 0)) (i32.const 1))',
            AssertReturnCommand(
                line=1,
                action=Invoke(1, None, "f", (ARG_I32_0,)),
                expected=(EXP_I32_1,),
            ),
        ),
        (
            '(assert_return_canonical_nan (invoke "f" (f32.const nan)))',
            AssertReturnCanonicalNan(
                line=1,
                action=Invoke(1, None, "f", (ARG_I32_0,)),
                expected=(EXP_F32_NAN,),
            )
        ),
    )
))


def pytest_generate_tests(metafunc):
    metafunc.parametrize('parser,sexpr,expected', SCRIPTS_SEXPRESSION_TESTS)


def test_sexpression_parsing(parser, sexpr, expected):
    result = parser.parseString(sexpr)
    assert len(result) == 1
    actual = result[0]
    assert actual == expected
