	jmp main

data:
.db	>>add_3,>add_3,
.db	>>mul_8,>mul_8,
.db	>>sub_2,>sub_2,

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

jmp_fnc:
	store 0xfe
	pusha
	jsrs
	load 0xfe
	wrpc

main:
	push data
	push 4
	iadd
	push data
	push 2
	iadd
	push data
	push 1
	store 5
	jsr jmp_fnc
	jsr jmp_fnc
	jsr jmp_fnc
	load 5
	iprint