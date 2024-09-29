CFLAGS=-Wall -pedantic -std=c99 -fsanitize=address

build: TP4.c utilitaire.c
	gcc $(CFLAGS) -o TP4 utilitaire.c TP4.c -lm

run: TP4
	./TP4

all: build run