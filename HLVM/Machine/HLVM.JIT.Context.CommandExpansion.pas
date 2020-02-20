unit HLVM.JIT.Context.CommandExpansion;

interface

{ JIT commands pallete expansion }

const
  JIT_Command_PushLocal = 200;
  JIT_Command_Break = 210;
  JIT_Command_Continue = 211;
  JIT_Command_ForCheckAsc = 212;
  JIT_Command_ForCheckDes = 213;

  JIT_Command_JMP_NULL = 214;
  JIT_Command_JC_NULL = 215;
  JIT_Command_JNC_NULL = 216;

  JIT_Command_PushKnownLocal = 217;
  JIT_Command_PushKnownGlobal = 218;
  JIT_Command_PushGlobal = 219;
  JIT_Command_PushKnownLocalForce = 220;

  JIT_Command_ForSetStartAsc = 221;
  JIT_Command_ForSetStartDes = 222;

  JIT_Command_PushHighestVariable = 223;
  JIT_Command_DisposeHighestVariable = 224;

  JIT_Command_Container = 225;

  { KadJIT v1.6 BeHeading upgrade }
  JIT_Command_Heading_SetLocalHeapSize = 226;
  JIT_Command_Heading_SetGlobalHeapSize = 227;
  JIT_Command_Heading_CacheGlobal = 228;
  JIT_Command_Heading_CacheHighest = 229;

const
  ScoperFlag_Var_ForcedModifier  = 1;
  ScoperFlag_Var_PushKnownLocal  = 2;
  ScoperFlag_Var_PushKnownGlobal = 4;
  ScoperFlag_Var_PushGlobalCache = 8;
  ScoperFlag_Var_PushRawGlobal   = 16;
  ScoperFlag_Var_PushHighest     = 32;

const
  JIT_Header_NULL = 0;

  JIT_Header_LocalHeapSize = 10;
  JIT_Header_GlobalCacheSize = 11;
  JIT_Header_CacheGlobal = 12;

implementation

end.
