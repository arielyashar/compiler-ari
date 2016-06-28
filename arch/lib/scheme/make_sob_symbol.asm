/* scheme/make_sob_string.asm
 * Takes pointer to string on the stack. Places in R0 the address
 * of a newly-allocated pointer to a Scheme symbol.
 * 
 * Programmer: Matan&Hodai, 2015
 */

 MAKE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(2));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0,0),T_SYMBOL);
  MOV(INDD(R0, 1), FPARG(0));
  POP(FP);
  RETURN;

