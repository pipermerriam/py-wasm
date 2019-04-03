from typing import Iterable, Tuple

import numpy

from pyparsing import (
    lineno,
)

from wasm._utils.decorators import to_tuple
from wasm.typing import TValue
from wasm.datatypes import ValType
from wasm.tools.fixtures.datatypes import (
    canonical_nan,
    AssertReturnCommand,
    AssertReturnCanonicalNan,
    Expected,
    Argument,
    Get,
    BinaryModule,
    QuoteModule,
    Register,
    Invoke,
)
from wasm.tools.eval import wasm_eval


#
# Utils
#
def _get_valtype(value: TValue) -> ValType:
    if isinstance(value, numpy.uint32):
        return ValType.i32
    elif isinstance(value, numpy.uint64):
        return ValType.i64
    elif isinstance(value, numpy.float32):
        return ValType.f32
    elif isinstance(value, numpy.float64):
        return ValType.f64
    else:
        raise Exception("INVALID")


def _evaluate_exprs(exprs) -> Iterable[Tuple[ValType, TValue]]:
    for expression in exprs:
        value = wasm_eval(expression)
        valtype = _get_valtype(value)
        yield (valtype, value)


@to_tuple
def _evaluate_args(exprs) -> Iterable[Argument]:
    for valtype, value in _evaluate_exprs(exprs):
        yield Argument(valtype, value)


@to_tuple
def _evaluate_expected(exprs) -> Iterable[Expected]:
    for valtype, value in _evaluate_exprs(exprs):
        yield Expected(valtype, value)


#
# Value Types
#
def parse_utf8(s, loc, toks):
    data = "".join(toks)
    return data


def parse_exprs(s, lok, toks):
    return tuple(toks)


#
# Module Registration
#
def parse_register(s, loc, toks):
    line = lineno(loc, s)
    as_name, name = toks

    return Register(
        line=line,
        name=name,
        as_=as_name,
    )


#
# Modules
#
def parse_binary_module(s, loc, toks):
    line = lineno(loc, s)
    name, data = toks

    return BinaryModule(
        line=line,
        name=name,
        data=data,
    )


def parse_quote_module(s, loc, toks):
    line = lineno(loc, s)
    name, data = toks

    return QuoteModule(
        line=line,
        name=name,
        data=data,
    )


#
# Actions
#
def parse_invoke(s, loc, toks):
    line = lineno(loc, s)
    module_name, function_name, exprs = toks
    args = _evaluate_args(exprs)

    return Invoke(
        line=line,
        module_name=module_name,
        function_name=function_name,
        args=args
    )


def parse_get(s, loc, toks):
    line = lineno(loc, s)
    module_name, global_name = toks

    return Get(
        line=line,
        module_name=module_name,
        global_name=global_name,
    )


#
# Assertions
#
def parse_assert_return(s, loc, toks):
    line = lineno(loc, s)
    action, exprs = toks
    expected = _evaluate_expected(exprs)

    return AssertReturnCommand(
        line=line,
        action=action,
        expected=expected,
    )


def parse_assert_return_canonical_nan(s, loc, toks):
    line = lineno(loc, s)
    action, exprs = toks
    computed_expected = _evaluate_expected(exprs)

    assert numpy.isnan(computed_expected[0].value)
    expected = tuple(Expected(exp.valtype, canonical_nan) for exp in computed_expected)

    return AssertReturnCanonicalNan(
        line=line,
        action=action,
        expected=expected,
    )
