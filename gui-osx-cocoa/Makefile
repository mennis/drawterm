ROOT=..
include ../Make.config
LIB=libgui.a

WSYSOFILES=\
	alloc.$O\
	cload.$O\
	draw.$O\
	load.$O\
	mouseswap.$O\
	main-cocoa.$O

OSVERS=$(shell ../mkwsysrules.sh)

ifeq ($(OSVERS), 14)
	OFILES=$(WSYSOFILES) cocoa-screen-metal-objc.$O
else
	OFILES=$(WSYSOFILES) cocoa-screen.$O
endif

check:
	echo $(OSVERS)
	echo $(OFILES)

default: $(LIB)
$(LIB): $(OFILES)
	$(AR) r $(LIB) $(OFILES)
	$(RANLIB) $(LIB)

%.$O: %.c
	$(CC) $(CFLAGS) $*.c

cocoa-screen-metal-objc.$O: cocoa-screen-metal.m
	$(CC) $(CFLAGS) -fobjc-arc -o cocoa-screen-metal-objc.$O cocoa-screen-metal.m

%.$O: %.m
	$(CC) $(CFLAGS) $*.m
