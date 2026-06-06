CC = gcc
CFLAGS = -c -O2 `pkg-config --cflags freetype2`

ftx.o: ftx.c
	$(CC) $(CFLAGS) $< -o $@

ftx.a: ftx.o
	ar rcs libftx.a ftx.o

.PHONY: clean
clean:
	rm -rf *.o

all: ftx.a
