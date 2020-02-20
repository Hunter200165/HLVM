unit HLVM.JIT.Constants;

interface

{$Define HLVM_JIT_SecureConstants True}

const
  HLVM_MAX_LOCAL_VARIABLES = 256;
  HLVM_REGISTER_SIZE = 256;
  HLVM_POINTED_REGISTER_SIZE = 256;
  HLVM_SCOPE_EXPANSION_SIZE = 8;
  HLVM_BREAKS_EXPANSION_SIZE = 8;
  HLVM_FORS_EXPANSION_SIZE = 8;

{$if Defined(HLVM_JIT_SecureConstants) and not (HLVM_REGISTER_SIZE = HLVM_POINTED_REGISTER_SIZE)}
  {$message error 'Cannot have non-identical values! For security reasons'}
{$endif}

implementation

end.
