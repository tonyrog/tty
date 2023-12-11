#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

CFLAGS+=-std=c99 -Wall -O3
LDFLAGS+=-shared

OBJS = 
LIBS = -ltermcap

all: ../priv/tty_drv.so

../priv/tty_drv.so: tty_drv.o
	$(CC) $(LDFLAGS) tty_drv.o $(LIBS) -o ../priv/tty_drv.so

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d
