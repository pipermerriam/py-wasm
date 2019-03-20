from lark import Transformer, v_args

from wasm._utils.toolz import (
    concat,
)
from wasm.opcodes import TEXT_TO_OPCODE, BinaryOpcode
from wasm.instructions.numeric import (
    I32Const,
    I64Const,
    F32Const,
    F64Const,
)

from .ir import (
    Local,
    Param,
)

from wasm.datatypes import ValType


class WasmTransformer(Transformer):
    #
    # Expressions
    #
    def op(self, args):
        assert False

    #
    # Numeric Ops
    #
    @v_args(inline=True)
    def const_op(self, opcode_text, value):
        opcode = TEXT_TO_OPCODE[opcode_text.value]

        if opcode is BinaryOpcode.I32_CONST:
            return I32Const(value)
        elif opcode is BinaryOpcode.I64_CONST:
            return I64Const(value)
        elif opcode is BinaryOpcode.F32_CONST:
            return F32Const(value)
        elif opcode is BinaryOpcode.F64_CONST:
            return F64Const(value)
        else:
            raise Exception("TODO:")

    #
    # Params
    #
    def params(self, args):
        return tuple(concat(args))

    def param_named(self, args):
        name, valtype = args
        return (Param(valtype, name),)

    def param_bare(self, valtypes):
        return tuple(Param(valtype) for valtype in concat(valtypes))

    #
    # Results
    #
    def result(self, args):
        return tuple(args)

    def results(self, args):
        return tuple(concat(args))

    #
    # Locals
    #
    def locals(self, args):
        return tuple(concat(args))

    def local_named(self, args):
        name, valtype = args
        return (Local(valtype, name),)

    def local_bare(self, valtypes):
        return tuple(Local(valtype) for valtype in concat(valtypes))

    #
    # ValType values
    #
    @v_args(inline=True)
    def valtype(self, valtype_token):
        return ValType.from_str(valtype_token.value)

    def valtypes(self, args):
        return tuple(args)

    #
    # Basic values
    #
    def num(self, args):
        assert False

    @v_args(inline=True)
    def int(self, str_value):
        return int(str_value)

    def nat(self, args):
        assert False

    def float(self, args):
        assert False
