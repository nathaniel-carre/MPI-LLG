CFLAGS=-Wall -pedantic -std=c99 -g -fsanitize=address

build: TP7.c dicts.c
	gcc $(CFLAGS) -o TP7 dicts.c TP7.c -lm

run: TP7
	./TP7

all: build run


