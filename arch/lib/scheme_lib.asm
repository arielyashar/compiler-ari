/* scheme_lib.asm
 * compiler scheme lib functions
 * 
 * Programmer: Hodai & Matan, 2015
 */


/* the cons procedure */
/**********************/

L_prim_cons:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(FPARG(3)); // The cdr
	PUSH(FPARG(2)); // The car
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	POP(FP);
	RETURN;
	

/* the car procedure */
/*********************/

L_prim_car:
	PUSH(FP);
	MOV(FP, SP);
	//PUSH(FPARG(2)) // The pair
	// TODO - add type cheking
	MOV(R0, FPARG(2));
	MOV(R0, INDD(R0, 1));
	POP(FP);
	RETURN;
	

/* the cdr procedure */
/*********************/
L_prim_cdr:
	PUSH(FP);
	MOV(FP, SP);
	//PUSH(FPARG(2)) // The pair
	// TODO - add type cheking
	MOV(R0, FPARG(2));
	MOV(R0, INDD(R0, 2));
	POP(FP);
	RETURN;
	
	 
/* the apply procedure */
/***********************/
L_prim_apply:
	// apply always implemented as tail call
	PUSH(FP);
	MOV(FP, SP);
	
	MOV(R1 , FPARG(3));  // The list
	MOV(R2 , FPARG(2));  // The closure
	MOV(R3 , FPARG(-1)); // save the return address
	POP(FP);			// restor old fp
	DROP(5);			// overite the old frame

	// push all the arguments
	MOV(R0 , IMM(0));	// initial counter for n
	MOV(R4 , R1);		// initial argument list iterator
L_prim_apply_args_count:
	CMP(IND(R4) , IMM(T_NIL));
	JUMP_EQ(L_prim_apply_args_count_end);	// brake the loop if reach to nill
	ADD(R0 , IMM(1));						// counter++
	MOV(R4 , INDD(R4 , 2));  				// move to the next pair in the list 
	JUMP(L_prim_apply_args_count);
L_prim_apply_args_count_end:	
	// now R0 hold the n (args count)
	ADD(SP , R0);					// alocate memory in stac to the arguments
	PUSH(R0);						// after argumenrs we have n
	MOV(R0 , SP);
	SUB(R0 , IMM(2));				// R0 hold the addres of the first argument in stack
	
	PUSH(INDD(R2 , 1)); 			// push th env
	PUSH(R3);						// push the return address
	
	// now copy the arguments in reverse order
L_prim_apply_args:
	CMP(IND(R1) , IMM(T_NIL));
	JUMP_EQ(L_prim_apply_args_end);	// brake the loop if reach to nill
	MOV(STACK(R0) , INDD(R1 , 1)); 	// push the element as argument
	SUB(R0 , IMM(1));
	MOV(R1 , INDD(R1 , 2));  		// move to the next pair in the list 
	JUMP(L_prim_apply_args);

L_prim_apply_args_end:
	MOV(R0 , INDD(R2, 2));		// the code
	JUMPA(R0);
	
	
/* the exit procerure */
/**********************/
L_prim_exit:
	HALT;
	

/* the display procerure */
/*************************/
L_prim_display:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0 , FPARG(2));   // the object to print
	CMP(IND(R0) , T_STRING);
	JUMP_EQ(L_prim_display_is_string);
	PUSH(R0);	
	CALL(WRITE_SOB);
	JUMP(L_prim_display_end);
L_prim_display_is_string:
	ADD(R0 , 2);	// the string start TODO - represent also as null terminated string
	PUSH(R0);
	CALL(WRITE);
L_prim_display_end:
	DROP(1);
	POP(FP);
	RETURN;
	

/* the eq? procedure */
/*********************/
L_prim_eq:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);

	MOV(R0,IMM(sob_true));		// initialize ans to true
	MOV(R1,FPARG(2));
	MOV(R2,FPARG(3));
	CMP(INDD(R1,0),INDD(R2,0));
	JUMP_NE(L_prim_eq_false);	// if tags are different return false

	CMP(INDD(R1,0),T_STRING);	// handle complex types
	JUMP_NE(L_prim_eq_simple);
	CMP(INDD(R1,0),T_PAIR);
	JUMP_NE(L_prim_eq_simple);
	CMP(INDD(R1,0),T_VECTOR);
	JUMP_NE(L_prim_eq_simple);
	CMP(R1,R2);					// in case of complex type - compare addresses
	JUMP_NE(L_prim_eq_false);	// if addresses are different return false
	JUMP(L_prim_eq_end);		// else true

L_prim_eq_simple:				// in case of simple type - determine type
	CMP(INDD(R1,0),T_VOID);	// T_VOID
	JUMP_EQ(L_prim_eq_void);
	CMP(INDD(R1,0),T_NIL);	// T_NIL
	JUMP_EQ(L_prim_eq_nil);
	CMP(INDD(R1,0),T_BOOL);	//T_BOOL
	JUMP_EQ(L_prim_eq_bool);
	CMP(INDD(R1,0),T_CHAR);	//T_CHAR
	JUMP_EQ(L_prim_eq_char);
	CMP(INDD(R1,0),T_INTEGER);	//T_INTEGER
	JUMP_EQ(L_prim_eq_int);
	CMP(INDD(R1,0),T_SYMBOL);	//T_SYMBOL
	JUMP_EQ(L_prim_eq_sym);
	CMP(INDD(R1,0),T_CLOSURE);	//T_CLOSURE
	JUMP_EQ(L_prim_eq_clos);

L_prim_eq_void:
L_prim_eq_nil:
	// no comparison is needed, tag is also value
	JUMP(L_prim_eq_end);

L_prim_eq_bool:
L_prim_eq_char:
L_prim_eq_int:
L_prim_eq_sym:
	// need to compare only 1 cell
	CMP(INDD(R1,1),INDD(R2,1));
	JUMP_NE(L_prim_eq_false);
	JUMP(L_prim_eq_end);

L_prim_eq_clos:
	// need to compare 2 cells
	CMP(INDD(R1,1),INDD(R2,1));
	JUMP_NE(L_prim_eq_false);
	JUMP(L_prim_eq_end);
	CMP(INDD(R1,2),INDD(R2,2));
	JUMP_NE(L_prim_eq_false);
	JUMP(L_prim_eq_end);

L_prim_eq_false:
	MOV(R0,sob_false);

L_prim_eq_end:
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;


/* the list->string procedure */
/*********************/

L_prim_lst2str:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	PUSH(R4);
	PUSH(R5);
	MOV(R1,FPARG(2));		// R1 points at list's first pair 
	MOV(R2,IMM(0));			// R2 will use to store list's length

L_prim_lst2str_count:
	CMP(IND(R1),T_NIL);
	JUMP_EQ(L_prim_lst2str_end_count);
	INCR(R2);
	MOV(R1,INDD(R1,2));
	JUMP(L_prim_lst2str_count);

L_prim_lst2str_end_count:	// R2 holds num of chars
	MOV(R3,R2);
	ADD(R3,IMM(2));			// R3 holds num of cells in newly created string
	PUSH(R3);
	CALL(MALLOC);
	DROP(1);				// R0 points at newly created string

	MOV(R1,FPARG(2));
	MOV(INDD(R0,0),T_STRING);
	MOV(INDD(R0,1),R2);
	MOV(R4,IMM(2));
L_prim_lst2str_copy:
	CMP(R4,R3);
	JUMP_EQ(L_prim_lst2str_end_copy);
	MOV(R5,INDD(R1,1));
	MOV(R5,INDD(R5,1));		// R5 holds current char value
	MOV(INDD(R0,R4),R5);
	INCR(R4);
	MOV(R1,INDD(R1,2));
	JUMP(L_prim_lst2str_copy);

L_prim_lst2str_end_copy:
	POP(R5);
	POP(R4);
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;

		
/* the string->symbol procedure */
/*********************/

L_prim_str2sym:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);

	MOV(R0,FPARG(2));		// R0 points at beginning of given string
	MOV(R1, IMM(0));		// R1 points at first link in sym list (will point after incr inside the loop)
	//TODO: Do we wanna check for string's correctness? (length>0 for example)

L_prim_str2sym_loop:
	INCR(R1);
	MOV(R1,IND(R1));
	CMP(R1, IMM(0));		// check if we're done iterating the list
	JUMP_EQ(L_prim_str2sym_not_found);
	MOV(R2,IND(R1));		// R2 points at tested string
	MOV(R3,INDD(R0,1));		// R3 holds given string's length
	INCR(R3);				// in order to compare lengths as well
	
L_prim_str2sym_char_cmp:
	CMP(INDD(R0,R3),INDD(R2,R3));
	JUMP_NE(L_prim_str2sym_loop);
	DECR(R3);
	CMP(R3, IMM(0));
	JUMP_GE(L_prim_str2sym_char_cmp);

L_prim_str2sym_found:
	PUSH(R2)	// R2 points at matching string found
	CALL(MAKE_SOB_SYMBOL);
	DROP(1);
	JUMP(L_prim_str2sym_end);

L_prim_str2sym_not_found:
	PUSH(R0);
	CALL(MAKE_SOB_SYMBOL);
	DROP(1);

L_prim_str2sym_end:
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;


/* the symbol->string procedure */
/*********************/

L_prim_sym2str:
	PUSH(FP);
	MOV(FP, SP);
	
	MOV(R0,FPARG(2));
	MOV(R0,INDD(R0,1));

	POP(FP);
	RETURN;


/* the integer->char procedure */
/*********************/

L_prim_int2char:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);

	MOV(R1,FPARG(2));
	MOV(R1,INDD(R1,1));	// R1 holds int value

	PUSH(IMM(2));
	CALL(MALLOC);
	DROP(1);

	MOV(INDD(R0,0),T_CHAR);
	MOV(INDD(R0,1),R1);

	POP(R1);
	POP(FP);
	RETURN;


/* the char->integer procedure */
/*********************/

L_prim_char2int:
	PUSH(FP);
	MOV(FP,SP);
	PUSH(R1);

	MOV(R1,FPARG(2));
	MOV(R1,INDD(R1,1));	// R1 holds char ascii

	PUSH(IMM(2));
	CALL(MALLOC);
	DROP(1);

	MOV(INDD(R0,0),T_INTEGER);
	MOV(INDD(R0,1),R1);

	POP(R1);
	POP(FP);
	RETURN;

	
/* the make-vector procedure */
/*********************/

L_prim_make_vector:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);

	MOV(R1,FPARG(2));		
	MOV(R1,INDD(R1,1));			// R1 holds num of obj in vec
	MOV(R2,R1);
	ADD(R2,IMM(2));				// R2 holds num of cells for vec
	MOV(R3,FPARG(3));			// R3 points at vec obj

	PUSH(R2);
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R0,0),T_VECTOR);
	MOV(INDD(R0,1),R1);
	DECR(R2);					// R2 holds vector-wise index of obj

L_prim_make_vector_loop:
	CMP(R2,IMM(1));
	JUMP_EQ(L_prim_make_vector_end);
	MOV(INDD(R0,R2),R3);
	DECR(R2);
	JUMP(L_prim_make_vector_loop);

L_prim_make_vector_end:
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;


/* the + procedure */
/*********************/

L_prim_plus:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	PUSH(R4);
	MOV(R1,FPARG(1));		// R1 holds num of elements

	PUSH(IMM(2));
	CALL(MALLOC);
	DROP(1);				// R0 points at newly created integer
	
	MOV(R4,IMM(0));			// R4 will hold sum
	MOV(R3,IMM(2));			// R3 holds FPARG-index of 1st arg
L_prim_plus_loop:
	CMP(R1,IMM(0));			// iterate for R1 times
	JUMP_EQ(L_prim_plus_end);
	MOV(R2,FPARG(R3));
	ADD(R4,INDD(R2,1))
	DECR(R1);
	INCR(R3);
	JUMP(L_prim_plus_loop);

L_prim_plus_end:
	MOV(INDD(R0,0),T_INTEGER);
	MOV(INDD(R0,1),R4);

	POP(R4);
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;

/* the * procedure */
/*********************/

L_prim_mul:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	PUSH(R4);
	MOV(R1,FPARG(1));		// R1 holds num of elements

	PUSH(IMM(2));
	CALL(MALLOC);
	DROP(1);				// R0 points at newly created integer
	
	MOV(R4,IMM(1));			// R4 will hold result
	MOV(R3,IMM(2));			// R3 holds FPARG-index of 1st arg
L_prim_mul_loop:
	CMP(R1,IMM(0));			// iterate for R1 times
	JUMP_EQ(L_prim_mul_end);
	MOV(R2,FPARG(R3));
	MUL(R4,INDD(R2,1))
	DECR(R1);
	INCR(R3);
	JUMP(L_prim_mul_loop);

L_prim_mul_end:
	MOV(INDD(R0,0),T_INTEGER);
	MOV(INDD(R0,1),R4);

	POP(R4);
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;

	
/* the - procedure */
/*********************/

L_prim_minus:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	PUSH(R4);
	MOV(R1,FPARG(1));		// R1 holds num of elements

	PUSH(IMM(2));
	CALL(MALLOC);
	DROP(1);				// R0 points at newly created integer
	
	CMP(R1, IMM(1));		// check if there's 1 arg or more
	JUMP_GT(L_prim_minus_multiple);
	MOV(R4,IMM(0));			// result starts from 0
	MOV(R3,IMM(2));			// R3 holds FPARG-index of 1st arg
	JUMP(L_prim_minus_loop);

L_prim_minus_multiple:
	MOV(R2,FPARG(2));
	MOV(R4,INDD(R2,1));		// result starts from 1st arg
	DECR(R1);				// num of args to iterate decreases by 1
	MOV(R3,IMM(3));			// R3 holds FPARG-index of 2nd arg

L_prim_minus_loop:
	CMP(R1,IMM(0));			// iterate for R1 times
	JUMP_EQ(L_prim_minus_end);
	MOV(R2,FPARG(R3));
	SUB(R4,INDD(R2,1))
	DECR(R1);
	INCR(R3);
	JUMP(L_prim_minus_loop);

L_prim_minus_end:
	MOV(INDD(R0,1),R4);
	MOV(INDD(R0,0),T_INTEGER);

	POP(R4);
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;


/* the div procedure */
/*********************/

L_prim_div:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	PUSH(R4);
	MOV(R1,FPARG(1));		// R1 holds num of elements

	PUSH(IMM(2));
	CALL(MALLOC);
	DROP(1);				// R0 points at newly created integer
	
	CMP(R1, IMM(1));		// check if there's 1 arg or more
	JUMP_GT(L_prim_div_multiple);
	MOV(R4,IMM(1));			// result starts from 1
	MOV(R3,IMM(2));			// R3 holds FPARG-index of 1st arg
	JUMP(L_prim_div_loop);

L_prim_div_multiple:
	MOV(R2,FPARG(2));
	MOV(R4,INDD(R2,1));		// result starts from 1st arg
	DECR(R1);				// num of args to iterate decreases by 1
	MOV(R3,IMM(3));			// R3 holds FPARG-index of 2nd arg

L_prim_div_loop:
	CMP(R1,IMM(0));			// iterate for R1 times
	JUMP_EQ(L_prim_div_end);
	MOV(R2,FPARG(R3));
	DIV(R4,INDD(R2,1))
	DECR(R1);
	INCR(R3);
	JUMP(L_prim_div_loop);

L_prim_div_end:
	MOV(INDD(R0,0),T_INTEGER);
	MOV(INDD(R0,1),R4);

	POP(R4);
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;


	
/* the less-than procedure */
/*********************/

L_prim_less:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	PUSH(R4);
	PUSH(R5);

	MOV(R1,FPARG(1));		// R1 holds num of elements
	DECR(R1);				// num of iterations is 1 less than args num
	MOV(R2,IMM(2));			// R2 holds index of "previous"
	MOV(R3,IMM(3));			// R3 holds index of "current"

L_prim_less_loop:
	CMP(R1,IMM(0));			// iterate for R1 times
	JUMP_EQ(L_prim_less_true);
	MOV(R4,FPARG(R2));		// R4 holds "previous"
	MOV(R5,FPARG(R3));		// R4 holds "current"
	CMP(INDD(R4,1),INDD(R5,1));
	JUMP_GE(L_prim_less_false);
	DECR(R1);				// iterations counter--
	INCR(R2);				// indices++
	INCR(R3);
	JUMP(L_prim_less_loop);

L_prim_less_true:
	MOV(R0,sob_true);
	JUMP(L_prim_less_end);

L_prim_less_false:
	MOV(R0,sob_false);

L_prim_less_end:
	POP(R5);
	POP(R4);
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;


/* the greater-than procedure */
/*********************/

L_prim_greater:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	PUSH(R4);
	PUSH(R5);
	
	MOV(R1,FPARG(1));		// R1 holds num of elements
	DECR(R1);				// num of iterations is 1 less than args num
	MOV(R2,IMM(2));			// R2 holds index of "previous"
	MOV(R3,IMM(3));			// R3 holds index of "current"

L_prim_greater_loop:
	CMP(R1,IMM(0));			// iterate for R1 times
	JUMP_EQ(L_prim_greater_true);
	MOV(R4,FPARG(R2));		// R4 holds "previous"
	MOV(R5,FPARG(R3));		// R4 holds "current"
	CMP(INDD(R4,1),INDD(R5,1));
	JUMP_LE(L_prim_greater_false);
	DECR(R1);				// iterations counter--
	INCR(R2);				// indices++
	INCR(R3);
	JUMP(L_prim_greater_loop);

L_prim_greater_true:
	MOV(R0,sob_true);
	JUMP(L_prim_greater_end);

L_prim_greater_false:
	MOV(R0,sob_false);

L_prim_greater_end:
	POP(R5);
	POP(R4);
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;

	
/* the equal procedure */
/*********************/

L_prim_equal:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	PUSH(R4);
	PUSH(R5);

	MOV(R1,FPARG(1));		// R1 holds num of elements
	DECR(R1);				// num of iterations is 1 less than args num
	MOV(R2,IMM(2));			// R2 holds index of "previous"
	MOV(R3,IMM(3));			// R3 holds index of "current"

L_prim_equal_loop:
	CMP(R1,IMM(0));			// iterate for R1 times
	JUMP_EQ(L_prim_equal_true);
	MOV(R4,FPARG(R2));		// R4 holds "previous"
	MOV(R5,FPARG(R3));		// R4 holds "current"
	CMP(INDD(R4,1),INDD(R5,1));
	JUMP_NE(L_prim_equal_false);
	DECR(R1);				// iterations counter--
	INCR(R2);				// indices++
	INCR(R3);
	JUMP(L_prim_equal_loop);

L_prim_equal_true:
	MOV(R0,sob_true);
	JUMP(L_prim_equal_end);

L_prim_equal_false:
	MOV(R0,sob_false);

L_prim_equal_end:
	POP(R5);
	POP(R4);
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;

		
/* the set-car! procedure */
/*********************/

L_prim_set_car:
	PUSH(FP);
	MOV(FP, SP);

	MOV(R0,FPARG(2));
	MOV(INDD(R0,1),FPARG(3));
	MOV(R0,sob_void);

	POP(FP);
	RETURN;


/* the set-cdr! procedure */
/*********************/

L_prim_set_cdr:
	PUSH(FP);
	MOV(FP, SP);

	MOV(R0,FPARG(2));
	MOV(INDD(R0,2),FPARG(3));
	MOV(R0,sob_void);

	POP(FP);
	RETURN;


/* the sob-length procedure */	//uses for str-len and vec-len
/*********************/

L_prim_sob_length:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	
	MOV(R1,FPARG(2));
	MOV(R1,INDD(R1,1));		// R1 holds string length
	
	PUSH(IMM(2));
	CALL(MALLOC);
	DROP(1);				// R0 holds newly-created int

	MOV(INDD(R0,0),T_INTEGER);
	MOV(INDD(R0,1),R1);

	POP(R1);
	POP(FP);
	RETURN;


	
/* the string-ref procedure */
/*********************/

L_prim_string_ref:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);

	MOV(R1,FPARG(3));		
	MOV(R1,INDD(R1,1));		// R1 holds "real" index of requested char
	ADD(R1,IMM(2));			// R1 holds "scheme-string-wise" index of requested char
	MOV(R2,FPARG(2));		// R2 points at string
	MOV(R2,INDD(R2,R1));	// R2 holds requested char ascii

	PUSH(IMM(2));
	CALL(MALLOC);
	DROP(1);				// R0 holds newly created char

	MOV(INDD(R0,0),T_CHAR);
	MOV(INDD(R0,1),R2);

	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;

	
/* the string-set! procedure */
/*********************/

L_prim_string_set:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);

	MOV(R1,FPARG(3));		
	MOV(R1,INDD(R1,1));		// R1 holds "real" index of old char
	ADD(R1,IMM(2));			// R1 holds "scheme-string-wise" index of old char
	MOV(R2,FPARG(4));		// R2 points at new char
	MOV(R2,INDD(R2,1));		// R2 holds new char ascii
	MOV(R0,FPARG(2));		// R0 points at string
	MOV(INDD(R0,R1),R2);	// set char

	MOV(R0,IMM(sob_void));		// return void (manual says "unspecified")

	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;

	
/* the vector-ref procedure */
/*********************/

L_prim_vector_ref:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);

	MOV(R1,FPARG(3));		
	MOV(R1,INDD(R1,1));		// R1 holds "real" index of requested obj
	ADD(R1,IMM(2));			// R1 holds "scheme-vector-wise" index of requested obj
	MOV(R2,FPARG(2));		// R2 points at vector

	MOV(R0,INDD(R2,R1));

	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;

	
/* the vector-set! procedure */
/*********************/

L_prim_vector_set:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);

	MOV(R1,FPARG(3));		
	MOV(R1,INDD(R1,1));		// R1 holds "real" index of old obj
	ADD(R1,IMM(2));			// R1 holds "scheme-object-wise" index of old obj
	MOV(R2,FPARG(4));		// R2 points at new obj
	MOV(R0,FPARG(2));		// R0 points at vector
	MOV(INDD(R0,R1),R2);	// set obj

	MOV(R0,IMM(sob_void));		// return void (manual says "unspecified")

	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;


/* predicates */
/**************/

#define make_predicate(lable, ltrue, lend , tag) \
lable:\
	PUSH(FP);\
	MOV(FP, SP);\
	MOV(R0, FPARG(2)); \
	CMP(IND(R0) , IMM(tag));\
	JUMP_EQ(ltrue);\
	MOV(R0, IMM(sob_false));\
	JUMP(lend);\
ltrue:\
	MOV(R0, IMM(sob_true));\
lend:\
	POP(FP);\
	RETURN;\
	
make_predicate(L_pred_bool , L_pred_bool_true, L_pred_bool_end , T_BOOL);
make_predicate(L_pred_symbol , L_pred_symbol_true, L_pred_symbol_end , T_SYMBOL);
make_predicate(L_pred_procedure , L_pred_procedure_true, L_pred_procedure_end , T_CLOSURE);
make_predicate(L_pred_integer , L_pred_integer_true, L_pred_integer_end , T_INTEGER);
make_predicate(L_pred_void , L_pred_void_true, L_pred_void_end , T_VOID);
make_predicate(L_pred_nil , L_pred_nil_true, L_pred_nil_end , T_NIL);
make_predicate(L_pred_char , L_pred_char_true, L_pred_char_end , T_CHAR);
make_predicate(L_pred_string , L_pred_string_true, L_pred_string_end , T_STRING);
make_predicate(L_pred_pair , L_pred_pair_true, L_pred_pair_end , T_PAIR);
make_predicate(L_pred_vector , L_pred_vector_true, L_pred_vector_end , T_VECTOR);
	
	
	
	
	
	
	
	
	
	
