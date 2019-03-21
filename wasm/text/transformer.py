from lark import Transformer, v_args, Tree, Token

from wasm._utils.toolz import (
    concat,
)
from wasm.opcodes import TEXT_TO_OPCODE, BinaryOpcode
from wasm.datatypes import (
    ValType,
    TypeIdx,
    LocalIdx,
    GlobalIdx,
)
from wasm.instructions.control import (
    Unreachable,
    Return,
    Nop,
)
from wasm.instructions.variable import (
    LocalOp,
    GlobalOp,
)
from wasm.instructions.memory import (
    MemoryArg,
    MemoryOp,
    MemoryGrow,
    MemorySize,
)
from wasm.instructions.numeric import (
    I32Const,
    I64Const,
    F32Const,
    F64Const,
    UnOp,
    RelOp,
    BinOp,
    TestOp,
    Wrap,
    Extend,
    Promote,
    Demote,
    Truncate,
    Convert,
    Reinterpret,
)

from .ir import (
    BaseUnresolved,
    Local,
    Param,
    UnresolvedGlobalIdx,
    UnresolvedLocalIdx,
    UnresolvedVariableOp,
    UnresolvedCallIndirect,
    UnresolvedTypeIdx,
    UnresolvedFunctionType,
)


VARIABLE_LOOKUP = {
    'local.get': (LocalOp, BinaryOpcode.GET_LOCAL),
    'local.set': (LocalOp, BinaryOpcode.SET_LOCAL),
    'local.tee': (LocalOp, BinaryOpcode.TEE_LOCAL),
    'global.get': (GlobalOp, BinaryOpcode.GET_GLOBAL),
    'global.set': (GlobalOp, BinaryOpcode.SET_GLOBAL),
}
TRUNC_LOOKUP = {
    'i32.trunc_f32_s': BinaryOpcode.I32_TRUNC_S_F32,
    'i32.trunc_f32_u': BinaryOpcode.I32_TRUNC_U_F64,
    'i32.trunc_f64_s': BinaryOpcode.I32_TRUNC_S_F32,
    'i32.trunc_f64_u': BinaryOpcode.I32_TRUNC_U_F64,
    'i64.trunc_f32_s': BinaryOpcode.I64_TRUNC_S_F32,
    'i64.trunc_f32_u': BinaryOpcode.I64_TRUNC_U_F64,
    'i64.trunc_f64_s': BinaryOpcode.I64_TRUNC_S_F32,
    'i64.trunc_f64_u': BinaryOpcode.I64_TRUNC_U_F64,
}
CONVERT_LOOKUP = {
    'f32.convert_i32_s': BinaryOpcode.F32_CONVERT_S_I32,
    'f32.convert_i32_u': BinaryOpcode.F32_CONVERT_U_I32,
    'f32.convert_i64_s': BinaryOpcode.F32_CONVERT_S_I64,
    'f32.convert_i64_u': BinaryOpcode.F32_CONVERT_U_I64,
    'f64.convert_i32_s': BinaryOpcode.F64_CONVERT_S_I32,
    'f64.convert_i32_u': BinaryOpcode.F64_CONVERT_U_I32,
    'f64.convert_i64_s': BinaryOpcode.F64_CONVERT_S_I64,
    'f64.convert_i64_u': BinaryOpcode.F64_CONVERT_U_I64,
}
REINTERPRET_LOOKUP = {
    'i32.reinterpret_f32': BinaryOpcode.I32_REINTERPRET_F32,
    'i64.reinterpret_f64': BinaryOpcode.I64_REINTERPRET_F64,
    'f32.reinterpret_i32': BinaryOpcode.F32_REINTERPRET_I32,
    'f64.reinterpret_i64': BinaryOpcode.F64_REINTERPRET_I64,
}
MEMORY_ARG_DEFAULTS = {
    BinaryOpcode.I32_LOAD: MemoryArg(0, 4),
    BinaryOpcode.I64_LOAD: MemoryArg(0, 8),
    BinaryOpcode.F32_LOAD: MemoryArg(0, 4),
    BinaryOpcode.F64_LOAD: MemoryArg(0, 8),
    BinaryOpcode.I32_LOAD8_S: MemoryArg(0, 1),
    BinaryOpcode.I32_LOAD8_U: MemoryArg(0, 1),
    BinaryOpcode.I32_LOAD16_S: MemoryArg(0, 2),
    BinaryOpcode.I32_LOAD16_U: MemoryArg(0, 2),
    BinaryOpcode.I64_LOAD8_S: MemoryArg(0, 1),
    BinaryOpcode.I64_LOAD8_U: MemoryArg(0, 1),
    BinaryOpcode.I64_LOAD16_S: MemoryArg(0, 2),
    BinaryOpcode.I64_LOAD16_U: MemoryArg(0, 2),
    BinaryOpcode.I64_LOAD32_S: MemoryArg(0, 4),
    BinaryOpcode.I64_LOAD32_U: MemoryArg(0, 4),
    BinaryOpcode.I32_STORE: MemoryArg(0, 4),
    BinaryOpcode.I64_STORE: MemoryArg(0, 8),
    BinaryOpcode.F32_STORE: MemoryArg(0, 4),
    BinaryOpcode.F64_STORE: MemoryArg(0, 8),
    BinaryOpcode.I32_STORE8: MemoryArg(0, 1),
    BinaryOpcode.I32_STORE16: MemoryArg(0, 2),
    BinaryOpcode.I64_STORE8: MemoryArg(0, 1),
    BinaryOpcode.I64_STORE16: MemoryArg(0, 2),
    BinaryOpcode.I64_STORE32: MemoryArg(0, 4),
}


class WasmTransformer(Transformer):
    #
    # Expressions
    #
    @v_args(inline=True)
    def exprs(self, args):
        return tuple(args.children)

    #
    # Instructions
    #
    #
    # Control Ops
    #
    def call_op(self, args):
        assert False

    @v_args(inline=True)
    def call_indirect_op(self, typeuse):
        if isinstance(typeuse, UnresolvedTypeIdx):
            return UnresolvedCallIndirect(typeuse)
        elif isinstance(typeuse, tuple) and len(typeuse) == 2:
            params, results = typeuse
            func_type = UnresolvedFunctionType(params, results)
            return UnresolvedCallIndirect(func_type)
        else:
            raise Exception("INVALID")

    @v_args(inline=True)
    def unreachable_op(self):
        return Unreachable()

    @v_args(inline=True)
    def nop_op(self):
        return Nop()

    @v_args(inline=True)
    def return_op(self):
        return Return()

    @v_args(inline=True)
    def typeuse_params_only(self, params):
        return params, ()

    @v_args(inline=True)
    def typeuse_results_only(self, results):
        return (), results

    @v_args(inline=True)
    def typeuse_params_and_results(self, params, results=None):
        if results is None:
            results = ()

        return params, results

    #
    # Variable Ops
    #
    @v_args(inline=True)
    def variable_op(self, opcode_token, var):
        (instruction_class, opcode) = VARIABLE_LOOKUP[opcode_token.value]
        if isinstance(var, BaseUnresolved):
            return UnresolvedVariableOp(opcode, var)
        elif isinstance(var, int):
            # TODO: cast the integer value to the proper Index class (GlobalIdx, LocalIdx)
            return instruction_class.from_opcode(opcode, var)
        else:
            raise Exception("INVALID")

    #
    # Memory Ops
    #
    @v_args(inline=True)
    def memory_size_op(self, arg):
        return MemorySize()

    @v_args(inline=True)
    def memory_grow_op(self, arg):
        return MemoryGrow()

    @v_args(inline=True)
    def memory_access_op(self, arg):
        if isinstance(arg, Token):
            opcode_text = arg.value
            opcode = TEXT_TO_OPCODE[opcode_text]
            memarg = None
        elif isinstance(arg, Tree) and len(arg.children) == 2:
            token, memarg = arg.children
            opcode_text = token.value
            opcode = TEXT_TO_OPCODE[opcode_text]
        elif isinstance(arg, Tree) and len(arg.children) == 4:
            opcode_token, bit_size, sign_token, memarg = arg.children
            opcode_text = f"{opcode_token.value}{bit_size}_{sign_token.value}"
            opcode = TEXT_TO_OPCODE[opcode_text]
        elif isinstance(arg, Tree) and len(arg.children) == 3:
            opcode_token, bit_size, memarg = arg.children
            opcode_text = f"{opcode_token.value}{bit_size}"
            opcode = TEXT_TO_OPCODE[opcode_text]
        else:
            assert False

        if memarg is None:
            memarg = MEMORY_ARG_DEFAULTS[opcode]
        else:
            assert isinstance(memarg, MemoryArg)
        return MemoryOp.from_opcode(opcode, memarg)

    def memory_arg(self, args):
        if not args:
            return None
        else:
            assert False

    @v_args(inline=True)
    def memory_access_bitsize(self, bit_size):
        return int(bit_size)

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

    @v_args(inline=True)
    def testop(self, token):
        opcode = TEXT_TO_OPCODE[token.value]
        return TestOp.from_opcode(opcode)

    @v_args(inline=True)
    def unop(self, token):
        opcode = TEXT_TO_OPCODE[token.value]
        return UnOp.from_opcode(opcode)

    @v_args(inline=True)
    def binop(self, token):
        opcode = TEXT_TO_OPCODE[token.value]
        return BinOp.from_opcode(opcode)

    @v_args(inline=True)
    def relop(self, token):
        opcode = TEXT_TO_OPCODE[token.value]
        return RelOp.from_opcode(opcode)

    @v_args(inline=True)
    def wrapop(self, token):
        return Wrap()

    @v_args(inline=True)
    def extendop(self, signed):
        if signed is True:
            return Extend.from_opcode(BinaryOpcode.I64_EXTEND_S_I32)
        elif signed is False:
            return Extend.from_opcode(BinaryOpcode.I64_EXTEND_U_I32)
        else:
            raise Exception("INVALID")

    @v_args(inline=True)
    def truncop(self, args):
        opcode = TRUNC_LOOKUP[args.value]
        return Truncate.from_opcode(opcode)

    @v_args(inline=True)
    def convertop(self, token):
        opcode = CONVERT_LOOKUP[token.value]
        return Convert.from_opcode(opcode)

    @v_args(inline=True)
    def demoteop(self, token):
        return Demote()

    @v_args(inline=True)
    def promoteop(self, token):
        return Promote()

    @v_args(inline=True)
    def reinterpretop(self, token):
        opcode = REINTERPRET_LOOKUP[token.value]
        return Reinterpret.from_opcode(opcode)

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
    # Index values
    #
    @v_args(inline=True)
    def typeidx(self, token):
        if isinstance(token.value, str):
            return UnresolvedTypeIdx(token.value)
        elif isinstance(token.value, int):
            return TypeIdx(token.value)
        else:
            raise Exception("INVALID")

    @v_args(inline=True)
    def localidx(self, var):
        if isinstance(var, int):
            return LocalIdx(var)
        elif isinstance(var, str):
            return UnresolvedLocalIdx(var)
        else:
            raise Exception("INVALID")

    @v_args(inline=True)
    def globalidx(self, var):
        if isinstance(var, int):
            return GlobalIdx(var)
        elif isinstance(var, str):
            return UnresolvedGlobalIdx(var)
        else:
            raise Exception("INVALID")

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
    @v_args(inline=True)
    def sign(self, token):
        if token.value == 's':
            return True
        elif token.value == 'u':
            return False
        else:
            raise Exception("INVALID")

    def num(self, args):
        assert False

    @v_args(inline=True)
    def int(self, token):
        return int(token.value)

    @v_args(inline=True)
    def nat(self, token):
        if token.value.isdigit():
            return int(token.value)
        else:
            assert False

    def float(self, args):
        assert False
