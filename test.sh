#!/bin/bash

test_file()
{
 	echo "generate code for $1"
 	rm $1.c
	echo "(compile \"$1\" \"$1.c\") " |  petite ./formatter.scm ./compiler.scm ./final.scm;
	echo "compiling $1.c ..."
	
	if gcc -I ./arch/ $1.c -o $1.out > logcc.log; then
		echo "gcc compile sucsses"
	else
		echo "gcc fail"
	fi	
}

test_all() 
{
	# Loop until all parameters are used up
	for afile in tests/*.csm
	do
		test_file $afile
	done
}

# check input arguments
if [ "$1" == "" ]; then
    echo "please suply at least 1 argument"
    exit
fi

if [ "$1" == "all" ]; then
    test_all
else
	# Loop until all parameters are used up
	while [ "$1" != "" ]; do
		test_file $1
		
		# Shift all the parameters down by one
		shift
	done
fi





