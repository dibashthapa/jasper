	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 14, 0	sdk_version 14, 5
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #128
	.cfi_def_cfa_offset 128
	stp	x29, x30, [sp, #112]            ; 16-byte Folded Spill
	add	x29, sp, #112
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	mov	w8, #0
	str	w8, [sp, #56]                   ; 4-byte Folded Spill
	stur	wzr, [x29, #-4]
	adrp	x8, l___const.main.a@PAGE
	add	x8, x8, l___const.main.a@PAGEOFF
	ldr	w9, [x8]
	sub	x14, x29, #12
	stur	w9, [x29, #-12]
	ldrh	w8, [x8, #4]
	sturh	w8, [x29, #-8]
	adrp	x8, l___const.main.b@PAGE
	add	x8, x8, l___const.main.b@PAGEOFF
	ldr	w9, [x8]
	sub	x13, x29, #20
	stur	w9, [x29, #-20]
	ldrh	w8, [x8, #4]
	sturh	w8, [x29, #-16]
	adrp	x8, l___const.main.c@PAGE
	add	x8, x8, l___const.main.c@PAGEOFF
	ldr	w9, [x8]
	sub	x12, x29, #28
	stur	w9, [x29, #-28]
	ldrh	w8, [x8, #4]
	sturh	w8, [x29, #-24]
	adrp	x8, l___const.main.d@PAGE
	add	x8, x8, l___const.main.d@PAGEOFF
	ldr	w9, [x8]
	sub	x11, x29, #36
	stur	w9, [x29, #-36]
	ldrh	w8, [x8, #4]
	sturh	w8, [x29, #-32]
	adrp	x8, l___const.main.e@PAGE
	add	x8, x8, l___const.main.e@PAGEOFF
	ldr	w9, [x8]
	sub	x10, x29, #44
	stur	w9, [x29, #-44]
	ldrh	w8, [x8, #4]
	sturh	w8, [x29, #-40]
	adrp	x9, l___const.main.f@PAGE
	add	x9, x9, l___const.main.f@PAGEOFF
	ldr	w15, [x9]
	sub	x8, x29, #52
	stur	w15, [x29, #-52]
	ldrh	w9, [x9, #4]
	sturh	w9, [x29, #-48]
	mov	x9, sp
	str	x14, [x9]
	str	x13, [x9, #8]
	str	x12, [x9, #16]
	str	x11, [x9, #24]
	str	x10, [x9, #32]
	str	x8, [x9, #40]
	adrp	x0, l_.str@PAGE
	add	x0, x0, l_.str@PAGEOFF
	bl	_printf
	ldr	w0, [sp, #56]                   ; 4-byte Folded Reload
	ldp	x29, x30, [sp, #112]            ; 16-byte Folded Reload
	add	sp, sp, #128
	ret
	.cfi_endproc
                                        ; -- End function
	.section	__TEXT,__cstring,cstring_literals
l___const.main.a:                       ; @__const.main.a
	.asciz	"Hello"

l___const.main.b:                       ; @__const.main.b
	.asciz	"Hello"

l___const.main.c:                       ; @__const.main.c
	.asciz	"Hello"

l___const.main.d:                       ; @__const.main.d
	.asciz	"Hello"

l___const.main.e:                       ; @__const.main.e
	.asciz	"Hello"

l___const.main.f:                       ; @__const.main.f
	.asciz	"Hello"

l_.str:                                 ; @.str
	.asciz	"%s %s %s %s %s %s"

.subsections_via_symbols
