	push 3
	push 5
loop:
	push 1
	isub
	iprint
	push 0
	bne loop
	pop
loop2:
	push 1
	isub
	iprint
	push 0
	bne loop2
	iprint