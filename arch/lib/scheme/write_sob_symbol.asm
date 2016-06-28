/* scheme/write_sob_symbol.asm
 * Take a pointer to a Scheme symbol object, and 
 * prints (to stdout) the string representation
 * of that object.
 * 
 * Programmer: Matan Paz, 2015
 */

 WRITE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  MOV(R0, FPARG(0));
  MOV(R0, INDD(R0, 1));   
  MOV(R1, INDD(R0, 1));
  MOV(R2, R0);
  ADD(R2, IMM(2));
 L_WSSym_LOOP:
  CMP(R1, IMM(0));
  JUMP_EQ(L_WSSym_EXIT);
  CMP(IND(R2), '\n');
  JUMP_EQ(L_WSSym_NEWLINE);
  CMP(IND(R2), '\t');
  JUMP_EQ(L_WSSym_TAB);
  CMP(IND(R2), '\f');
  JUMP_EQ(L_WSSym_PAGE);
  CMP(IND(R2), '\r');
  JUMP_EQ(L_WSSym_RETURN);
  CMP(IND(R2), '\\');
  JUMP_EQ(L_WSSym_BACKSLASH);
  CMP(IND(R2), '\"');
  JUMP_EQ(L_WSSym_DQUOTE);
  CMP(IND(R2), ' ');
  JUMP_LT(L_WSSym_OCT_CHAR);
  PUSH(IND(R2));
  CALL(PUTCHAR);
  DROP(1);
  JUMP(L_WSSym_LOOP_CONT);
 L_WSSym_DQUOTE:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('\"'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSSym_LOOP_CONT);
 L_WSSym_BACKSLASH:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSSym_LOOP_CONT);
 L_WSSym_RETURN:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('r'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSSym_LOOP_CONT);
 L_WSSym_PAGE:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('f'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSSym_LOOP_CONT);
 L_WSSym_TAB:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('t'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSSym_LOOP_CONT);  
 L_WSSym_NEWLINE:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('n'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSSym_LOOP_CONT);
 L_WSSym_OCT_CHAR:
  MOV(R0, IND(R2));
  MOV(R3, R0);
  REM(R3, IMM(8));
  PUSH(R3);
  DIV(R0, IMM(8));
  MOV(R3, R0);
  REM(R3, IMM(8));
  PUSH(R3);
  DIV(R0, IMM(8));
  REM(R0, IMM(8));
  PUSH(R0);
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  DROP(1);
  CALL(WRITE_INTEGER);
  DROP(1);
  CALL(WRITE_INTEGER);
  DROP(1);
  CALL(WRITE_INTEGER);
  DROP(1);
 L_WSSym_LOOP_CONT:
  INCR(R2);
  DECR(R1);
  JUMP(L_WSSym_LOOP);
 L_WSSym_EXIT:
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;
