require_extension('F');
require_fp;
softfloat_roundingMode = RM;
WRITE_FRD(f32_mulAdd(f32(FRS1), f32(FRS2), f32(FRS3 ^ (uint32_t)INT32_MIN)).v);
set_fp_exceptions;
