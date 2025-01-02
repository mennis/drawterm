ROOT=..
include ../Make.config
LIB=libgui.a

OFILES=\
	alloc.$O\
	cload.$O\
	draw.$O\
	load.$O\
	mouseswap.$O\
	main.$O\
	screen.$O

check:
	echo $(OSVERS)
	echo $(OFILES)

default: $(LIB)
$(LIB): $(OFILES)
	$(AR) r $(LIB) $(OFILES)
	$(RANLIB) $(LIB)

%.$O: %.c
	$(CC) $(CFLAGS) $*.c

screen.$O: screen.m
	$(CC) $(CFLAGS) -fobjc-arc -o $*.$O $*.m

%.$O: %.m
	$(CC) $(CFLAGS) $*.m
