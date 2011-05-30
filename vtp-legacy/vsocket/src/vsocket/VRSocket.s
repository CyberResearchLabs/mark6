	.section __TEXT,__text,regular,pure_instructions
	.section __TEXT,__picsymbolstub1,symbol_stubs,pure_instructions,32
.lcomm __ZSt8__ioinit,1,0
	.globl __mtu
.data
	.align 2
__mtu:
	.space 4
.lcomm _sender,4,2
	.globl _XEND_FUNC
	.align 2
_XEND_FUNC:
	.space 4
	.globl _set_so_reuseaddr
	.align 2
_set_so_reuseaddr:
	.long	1
	.globl __mtu
	.align 2
__mtu:
	.space 4
	.globl _XEND_FUNC
	.align 2
_XEND_FUNC:
	.space 4
.literal4
	.align 2
_DEFAULT_SOCK_BUF_SIZE:
	.long	9000
.data
.mod_init_func
	.align 2
	.long	__GLOBAL__I__mtu
.data
.mod_term_func
	.align 2
	.long	__GLOBAL__D__mtu
.data
.constructor
.data
.destructor
	.align 1
