#include "u.h"
#include "libc.h"

int
tas(long *x)
{
	int     v, t, i = 1;
#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)
#pragma message("ARM Architecture Version: " TOSTRING(__ARM_ARCH))
#if __ARM_ARCH == 5
	// ARMv5: Use SWP instruction
	__asm__(
		"swp  %0, %1, [%2]"
		: "=&r" (v)
		: "r" (1), "r" (x)
		: "memory"
	);
#elif __ARM_ARCH >= 8
	// ARMv8: Use LDXR/STXR for atomic operations
   __asm__(
		"1: ldxr    %w0, [%2] \n"       // Load-exclusive, store value at %2 (x) into %w0 (v)
		"   stxr    %w1, %w3, [%2] \n"  // Attempt to store %w3 (i) to %2 (x), set %w1 (t) to success/fail
		"   cbnz    %w1, 1b \n"        // If %w1 (t) is non-zero, retry by branching to label 1
		: "=&r" (v), "=&r" (t)         // Output operands: v = loaded value, t = store result
		: "r" (x), "r" (i)             // Input operands: x = memory address, i = value to store
		: "memory"
   );
#else // AArch32 (32-bit)
	__asm__ (
		"1:	ldrex	%w0, [%2]\n"
		"	strex	%w1, %w3, [%2]\n"
		"	teq	%w1, #0\n"
		"	bne	1b"
		: "=&r" (v), "=&r" (t)
		: "r" (x), "r" (i)
		: "cc");
#endif
	switch(v) {
	case 0:
	case 1:
		return v;
	default:
		print("canlock: corrupted 0x%lux\n", v);
		return 1;
	}
}

