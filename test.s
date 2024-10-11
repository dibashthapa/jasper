	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 14, 0	sdk_version 14, 5
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:
	stp	x28, x27, [sp, #-32]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w27, -24
	.cfi_offset w28, -32
	sub	sp, sp, #480
	adrp	x8, ___stack_chk_guard@GOTPAGE
	ldr	x8, [x8, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x8, [x8]
	stur	x8, [x29, #-24]
	mov	w1, #0
	str	w1, [sp, #28]                   ; 4-byte Folded Spill
	str	wzr, [sp, #68]
	sub	x0, x29, #224
	str	x0, [sp, #48]                   ; 8-byte Folded Spill
	mov	x2, #200
	str	x2, [sp, #32]                   ; 8-byte Folded Spill
	bl	_memset
	ldr	w1, [sp, #28]                   ; 4-byte Folded Reload
	ldr	x2, [sp, #32]                   ; 8-byte Folded Reload
	mov	w8, #72
	sturb	w8, [x29, #-224]
	mov	w8, #101
	sturb	w8, [x29, #-223]
	mov	w8, #108
	str	w8, [sp, #44]                   ; 4-byte Folded Spill
	sturb	w8, [x29, #-222]
	sturb	w8, [x29, #-221]
	mov	w8, #111
	str	w8, [sp, #40]                   ; 4-byte Folded Spill
	sturb	w8, [x29, #-220]
	add	x0, sp, #72
	str	x0, [sp, #56]                   ; 8-byte Folded Spill
	bl	_memset
	ldr	w11, [sp, #40]                  ; 4-byte Folded Reload
	ldr	w9, [sp, #44]                   ; 4-byte Folded Reload
	ldr	x10, [sp, #48]                  ; 8-byte Folded Reload
	ldr	x8, [sp, #56]                   ; 8-byte Folded Reload
	mov	w12, #119
	strb	w12, [sp, #72]
	strb	w11, [sp, #73]
	mov	w11, #114
	strb	w11, [sp, #74]
	strb	w9, [sp, #75]
	mov	w9, #100
	strb	w9, [sp, #76]
	sturb	w9, [x29, #-204]
	mov	x9, sp
	str	x10, [x9]
	str	x8, [x9, #8]
	adrp	x0, l_.str@PAGE
	add	x0, x0, l_.str@PAGEOFF
	bl	_printf
	ldur	x9, [x29, #-24]
	adrp	x8, ___stack_chk_guard@GOTPAGE
	ldr	x8, [x8, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x8, [x8]
	subs	x8, x8, x9
	cset	w8, eq
	tbnz	w8, #0, LBB0_2
	b	LBB0_1
LBB0_1:
	bl	___stack_chk_fail
LBB0_2:
	mov	w0, #0
	add	sp, sp, #480
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x28, x27, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.section	__TEXT,__cstring,cstring_literals
l_.str:                                 ; @.str
	.asciz	"c is %s and b is %s"

.subsections_via_symbols
