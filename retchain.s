	push 0x1
	store 5
	push newloc
	push data
	push 4
	iadd
	pusha
	push data
	push 2
	iadd
	pusha
	push data
	pusha
	wrpc
newloc:
	load 5
	iprint
	halt

data:
.db	>>add_3,>add_3
.db	>>mul_8,>mul_8
.db	>>sub_2,>sub_2

add_3:
	load 5
	push 0x3
	iadd
	store 5
	wrpc

mul_8:
	load 5
	push 0x8
	imul
	store 5
	wrpc

sub_2:
	load 5
	push 0x2
	isub
	store 5
	wrpc

