unit HLVM.Static.Command;

interface

const
  (* TYPES OF ADDRESSED *)
  Type_Value = 0;
  Type_Variable = 1;
  Type_LanguageConstant = 2;

//(*const *)
//type HLVM_COMMAND = (
//  (* STACK OPERATIONS *)
//    { Pushes value to stack }
//    Command_PushVal,
//    { Pushes variable to stack }
//    Command_PushVar,
//    { Popes value from stack }
//    Command_PopVal,
//    { Popes variable from stack }
//    Command_PopVar,
//    { Clears stack }
//    Command_ClearStack,
//    { Moves Stack Pointer on top of the Stack }
//    Command_MoveStackPointer,
//    { Restores Stack Pointer to the previous position }
//    Command_RestoreStackPointer,
//    { Pushes string to a Stack }
//    Command_PushString,
//    { Indexes A with B }
//    Command_Index,
//    { Pushes numberic value }
//    Command_PushInteger,
//    { Pushes float value }
//    Command_PushReal,
//    { Pushes nil value }
//    Command_PushNil,
//    { Pushes true value }
//    Command_PushTrue,
//    { Pushes false value }
//    Command_PushFalse,
//
//  (* MOVE COMMANDS *)
//    { Moves from stack to variable }
//    Command_MOV   ,
//    { Moves value by address from stack to variable }
//    Command_MOVA  ,
//
//  (* ARITHMETICS *)
//    { ADD: Pop 2 (A, B) -> Push A + B; }
//    Command_ArADD ,
//    { SUB: Pop 2 (A, B) -> Push A - B; }
//    Command_ArSUB ,
//    { MUL: Pop 2 (A, B) -> Push A * B; }
//    Command_ArMUL ,
//    { DIV: Pop 2 (A, B) -> Push A / B; }
//    Command_ArDIV ,
//    { AND: Pop 2 (A, B) -> Push A AND B; }
//    Command_LgAND ,
//    Command_BtAND ,
//    { OR:  Pop 2 (A, B) -> Push A OR  B; }
//    Command_LgOR  ,
//    Command_BtOR ,
//    { XOR: Pop 2 (A, B) -> Push A XOR B; }
//    Command_LgXOR ,
//    Command_BtXOR ,
//    { NOT: Pop 1 (A) -> Push NOT A; }
//    Command_LgNOT ,
//    Command_BtNOT ,
//
//  (* LABELS *)
//    { LABEL <Mark>: Marks position for Jumping; Only valid in byte-code - later AoT compiler resolves all jumps }
//    Command_LABEL ,
//    { JMP <Mark>:   Performs Jumping onto marked position }
//    Command_JMP   ,
//    { JC (Jump-If-Carry) <Mark> : Jumps, if EF is True }
//    Command_JC    ,
//    { JNC (Jump-If-Not-Carry) <Mark> : Jumps, if EF is False }
//    Command_JNC   ,
//    { CheckStack : Checks top value of stack and then sets EF (EF = True, if value is Trueful and False if opposite) }
//    Command_CheckStack ,
//
//  (* STACK COMMANDS *)
//    { Get Address: Pop 1 (Variable) -> Push @Variable }
//    Command_GetAddr ,
//    { Calls the function with given arguments }
//    Command_Call ,
//    { Calls the function with given arguments, BUT returns only one value on the Stack. }
//    Command_SimpleCall ,
//    { Length operator }
//    Command_GetLength ,
//    { Interpolation operator }
//    Command_DoInterpolation ,
//    { Is Operator }
//    Command_OperatorIs ,
//    { As operator }
//    Command_OperatorAs ,
//    { Equals operator }
//    Command_Equals ,
//    { Greater than operator }
//    Command_GreaterThan ,
//    { Greater or equal operator }
//    Command_GreaterEqual ,
//    { Pushes long arguments }
//    Command_PushLongArguments ,
//    { Pushes shortified arguments }
//    Command_PushShortArguments ,
//    { Assigns to value from stack the value from stack: Stack -> Var; Stack -> Value; Assign Value to Var... }
//    Command_Assign ,
//    { Performs unary minus of value on top of the Stack }
//    Command_UnaryMinus ,
//
//    { Creates table from the values on the Stack }
//    Command_CreateTable ,
//    { Force to push local variable }
//    Command_PushLocal ,
//
//    { Call as async }
//    Command_CallAsync ,
//    { Duplicates value on top of the stack }
//    Command_Duplicate ,
//
//    { Opens new local scope }
//    Command_OpenScope ,
//    { Destroy last local scope }
//    Command_CloseScope ,
//
//    { Break command (for loops) }
//    Command_BreakLoop ,
//    { Continue command (for loops) }
//    Command_ContinueLoop ,
//
//    { Register elements in table using runtime indexing }
//    Command_CreateRunTimeTable ,
//
//    { Set the break and continue points }
//    Command_SetBreak ,
//    Command_SetContinue ,
//
//    { Command, that checks last value of Stack by checking its type }
//    { If type mismatch -> HLVM will throw exception on the Stack }
//    Command_RestrictType ,
//
//    { Check Stack without poping the last value }
//    Command_CheckStackInvisible ,
//
//    { Assign without any blocks }
//    Command_StrongAssign ,
//
//    { Mark variable as loop'ed and restrict changes! }
//    Command_MarkAsLoopVar ,
//
//    { AoT will understand that =D }
//    Command_CreateFunction ,
//
//    { Assign using pointers }
//    Command_AssignPointer = 131;
//
//    { Consumes arguments from Stack }
//    Command_ConsumeArguments = 132;
//
//    { Returns }
//    Command_Return = 133;
//
//    { In Operator }
//    Command_IsIn = 134;
//
//    { Internal Machine's: Pushes function with mark }
//    Command_Internal_PushFunction = 135;
//
//    Command_CBoolAnd = 136;
//    Command_CBoolOr  = 137;
//    Command_CBoolNot = 138;
//    Command_CBitAnd = 139;
//    Command_CBitOr  = 140;
//    Command_CBitNot = 141;
//    Command_CBitXor = 142;
//    Command_CBitShl = 143;
//    Command_CBitShr = 144;
//
//    Command_BitShiftLeft  = 145;
//    Command_BitShiftRight = 146;
//
//    Command_AssignLocalReference = 147;
//
//    Command_ForSetStart = 148;
//    Command_ForSetFinish = 149;
//    Command_ForSetType = 150;
//    Command_ForCheck = 151;
//    Command_ForSetVar = 152;
//    Command_ForSetStep = 153;
//
//    Command_NewIndex = 154;
//
//    Command_NoOperation = 155;

const  (* STACK OPERATIONS *)
    { Pushes value to stack }
    Command_PushVal = 0;
    { Pushes variable to stack }
    Command_PushVar = 1;
    { Popes value from stack }
    Command_PopVal = 2;
    { Popes variable from stack }
    Command_PopVar = 3;
    { Clears stack }
    Command_ClearStack = 4;
    { Moves Stack Pointer on top of the Stack }
    Command_MoveStackPointer = 5;
    { Restores Stack Pointer to the previous position }
    Command_RestoreStackPointer = 6;
    { Pushes string to a Stack }
    Command_PushString = 7;
    { Indexes A with B }
    Command_Index = 8;
    { Pushes numberic value }
    Command_PushInteger = 9;
    { Pushes float value }
    Command_PushReal = 20;
    { Pushes nil value }
    Command_PushNil = 21;
    { Pushes true value }
    Command_PushTrue = 22;
    { Pushes false value }
    Command_PushFalse = 23;

  (* MOVE COMMANDS *)
    { Moves from stack to variable }
    Command_MOV   = 10;
    { Moves value by address from stack to variable }
    Command_MOVA  = 11;

  (* ARITHMETICS *)
    { ADD: Pop 2 (A, B) -> Push A + B; }
    Command_ArADD = 31;
    { SUB: Pop 2 (A, B) -> Push A - B; }
    Command_ArSUB = 32;
    { MUL: Pop 2 (A, B) -> Push A * B; }
    Command_ArMUL = 33;
    { DIV: Pop 2 (A, B) -> Push A / B; }
    Command_ArDIV = 34;
    { AND: Pop 2 (A, B) -> Push A AND B; }
    Command_LgAND = 35;
    Command_BtAND = 38;
    { OR:  Pop 2 (A, B) -> Push A OR  B; }
    Command_LgOR  = 36;
    Command_BtOR  = 39;
    { XOR: Pop 2 (A, B) -> Push A XOR B; }
    Command_LgXOR = 37;
    Command_BtXOR = 40;
    { NOT: Pop 1 (A) -> Push NOT A; }
    Command_LgNOT = 30;
    Command_BtNOT = 41;
    { TERNARY: }
    Command_Ternary = 42;

  (* LABELS *)
    { LABEL <Mark>: Marks position for Jumping; Only valid in byte-code - later AoT compiler resolves all jumps }
    Command_LABEL = 50;
    { JMP <Mark>:   Performs Jumping onto marked position }
    Command_JMP   = 51;
    { JC (Jump-If-Carry) <Mark> : Jumps, if EF is True }
    Command_JC    = 52;
    { JNC (Jump-If-Not-Carry) <Mark> : Jumps, if EF is False }
    Command_JNC   = 53;
    { CheckStack : Checks top value of stack and then sets EF (EF = True, if value is Trueful and False if opposite) }
    Command_CheckStack = 54;

  (* STACK COMMANDS *)
    { Get Address: Pop 1 (Variable) -> Push @Variable }
    Command_GetAddr = 101;
    { Calls the function with given arguments }
    Command_Call = 102;
    { Calls the function with given arguments, BUT returns only one value on the Stack. }
    Command_SimpleCall = 103;
    { Length operator }
    Command_GetLength = 104;
    { Interpolation operator }
    Command_DoInterpolation = 105;
    { Is Operator }
    Command_OperatorIs = 106;
    { As operator }
    Command_OperatorAs = 107;
    { Equals operator }
    Command_Equals = 108;
    { Greater than operator }
    Command_GreaterThan = 109;
    { Greater or equal operator }
    Command_GreaterEqual = 110;
    { Pushes long arguments }
    Command_PushLongArguments = 111;
    { Pushes shortified arguments }
    Command_PushShortArguments = 112;
    { Assigns to value from stack the value from stack: Stack -> Var; Stack -> Value; Assign Value to Var... }
    Command_Assign = 113;
    { Performs unary minus of value on top of the Stack }
    Command_UnaryMinus = 114;

    { Creates table from the values on the Stack }
    Command_CreateTable = 115;
    { Force to push local variable }
    Command_PushLocal = 116;

    { Call as async }
    Command_CallAsync = 117;
    { Duplicates value on top of the stack }
    Command_Duplicate = 118;

    { Opens new local scope }
    Command_OpenScope = 119;
    { Destroy last local scope }
    Command_CloseScope = 120;

    { Break command (for loops) }
    Command_BreakLoop = 121;
    { Continue command (for loops) }
    Command_ContinueLoop = 122;

    { Register elements in table using runtime indexing }
    Command_CreateRunTimeTable = 123;

    { Set the break and continue points }
    Command_SetBreak = 124;
    Command_SetContinue = 125;

    { Command, that checks last value of Stack by checking its type }
    { If type mismatch -> HLVM will throw exception on the Stack }
    Command_RestrictType = 126;

    { Check Stack without poping the last value }
    Command_CheckStackInvisible = 127;

    { Assign without any blocks }
    Command_StrongAssign = 128;

    { Mark variable as loop'ed and restrict changes! }
    Command_MarkAsLoopVar = 129;

    { AoT will understand that =D }
    Command_CreateFunction = 130;

    { Assign using pointers }
    Command_AssignPointer = 131;

    { Consumes arguments from Stack }
    Command_ConsumeArguments = 132;

    { Returns }
    Command_Return = 133;

    { In Operator }
    Command_IsIn = 134;

    { Internal Machine's: Pushes function with mark }
    Command_Internal_PushFunction = 135;

    Command_CBoolAnd = 136;
    Command_CBoolOr  = 137;
    Command_CBoolNot = 138;
    Command_CBitAnd = 139;
    Command_CBitOr  = 140;
    Command_CBitNot = 141;
    Command_CBitXor = 142;
    Command_CBitShl = 143;
    Command_CBitShr = 144;

    Command_BitShiftLeft  = 145;
    Command_BitShiftRight = 146;

    Command_AssignLocalReference = 147;

    Command_ForSetStart = 148;
    Command_ForSetFinish = 149;
    Command_ForSetType = 150;
    Command_ForCheck = 151;
    Command_ForSetVar = 152;
    Command_ForSetStep = 153;

    Command_NewIndex = 154;

    Command_NoOperation = 155;

    Command_MarkAsNative = 156;
    Command_DoMonumentalPointer = 157;

    Command_SetExcept = 158;
    Command_SetFinally = 159;

    Command_CloseTry = 160;
    Command_ReRaise = 161;

    Command_AssignFromMemory = 162;

implementation

end.

