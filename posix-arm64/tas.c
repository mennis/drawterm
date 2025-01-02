#include "u.h"
#include "libc.h"

int
tas(long *x)
{
	int     v, t, i = 1;

	__asm__ (
		"1:	ldxr	%w0, [%2]\n"
		"	strex	%1, %w3, [%2]\n"
		"	teq	%w1, #0\n"
		"	bne	1b"
		: "=&r" (v), "=&r" (t)
		: "r" (x), "r" (i)
		: "cc");

	switch(v) {
	case 0:
	case 1:
		return v;
	default:
		print("canlock: corrupted 0x%lux\n", v);
		return 1;
	}
}

