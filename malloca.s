	push 40
	malloca
	dup
	store 0
	push 0
	store 1
	push 1
	store 2
	push 1
init_loop:
	pop
	load 1
	load 2
	writea
	push 4
	iadd
	dup
	store 1
	load 2
	push 1
	iadd
	store 2
	push 40
	bne init_loop
	pop
	push 1
	push 2
	lsl
	indexa
	iprint
	pop
	freea
; 5655b6f0
