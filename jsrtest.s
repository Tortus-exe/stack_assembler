	jmp start
function:
	push 2
	store 1
	wrpc

start:
	push 3
	push 5
	iadd
	jsr function
	load 1
	iadd
	iprint
