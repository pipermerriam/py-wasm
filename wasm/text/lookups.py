from wasm.instructions.variable import (
    GlobalOp,
    LocalOp,
)
from wasm.opcodes import (
    BinaryOpcode,
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
EXTEND_LOOKUP = {
    'i64.extend_i32_s': BinaryOpcode.I64_EXTEND_S_I32,
    'i64.extend_i32_u': BinaryOpcode.I64_EXTEND_U_I32,
}
MEMORY_ARG_DEFAULTS = {
    BinaryOpcode.I32_LOAD: (0, 4),
    BinaryOpcode.I64_LOAD: (0, 8),
    BinaryOpcode.F32_LOAD: (0, 4),
    BinaryOpcode.F64_LOAD: (0, 8),
    BinaryOpcode.I32_LOAD8_S: (0, 1),
    BinaryOpcode.I32_LOAD8_U: (0, 1),
    BinaryOpcode.I32_LOAD16_S: (0, 2),
    BinaryOpcode.I32_LOAD16_U: (0, 2),
    BinaryOpcode.I64_LOAD8_S: (0, 1),
    BinaryOpcode.I64_LOAD8_U: (0, 1),
    BinaryOpcode.I64_LOAD16_S: (0, 2),
    BinaryOpcode.I64_LOAD16_U: (0, 2),
    BinaryOpcode.I64_LOAD32_S: (0, 4),
    BinaryOpcode.I64_LOAD32_U: (0, 4),
    BinaryOpcode.I32_STORE: (0, 4),
    BinaryOpcode.I64_STORE: (0, 8),
    BinaryOpcode.F32_STORE: (0, 4),
    BinaryOpcode.F64_STORE: (0, 8),
    BinaryOpcode.I32_STORE8: (0, 1),
    BinaryOpcode.I32_STORE16: (0, 2),
    BinaryOpcode.I64_STORE8: (0, 1),
    BinaryOpcode.I64_STORE16: (0, 2),
    BinaryOpcode.I64_STORE32: (0, 4),
}
