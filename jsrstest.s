	jmp start
function:
	push 3
	push 5
	iadd
	store 0
	wrpc

start:
	push 5
	push function
	jsrs
	load 0
	iadd
	iprint