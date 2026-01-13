CFLAGS=-std=c11 -g -fno-common -Wall -Wno-switch

SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

# Stage 1
i686-utix-chibicc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(OBJS): chibicc.h

# Stage 2
stage2/i686-utix-chibicc: $(OBJS:%=stage2/%)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

stage2/%.o: i686-utix-chibicc %.c
	@mkdir -p $(@D)
	./i686-utix-chibicc -c -o $@ $<

# Misc
clean:
	rm -rf chibicc stage2 *.o

.PHONY: clean
