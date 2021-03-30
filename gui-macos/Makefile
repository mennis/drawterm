ROOT=..
include ../Make.config
LIB=libgui.a

OFILES=\
	alloc.$O\
	cload.$O\
	draw.$O\
	load.$O\
	mouseswap.$O\
	screen.$O\
	srv.$O\
	main.$O

default: $(LIB)
$(LIB): $(OFILES)
	$(AR) r $(LIB) $(OFILES)
	$(RANLIB) $(LIB)

%.$O: %.c
	$(CC) $(CFLAGS) -o $@ $<

screen.$O: screen.m
	$(CC) $(CFLAGS) -fobjc-arc -o screen.$O screen.m

%.$O: %.m
	$(CC) $(CFLAGS) -fobjc-arc -o $@ $<
