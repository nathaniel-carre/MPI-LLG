CFLAGS=-Wall -Wextra -pedantic -std=c99 -fsanitize=address,undefined -g

build: base.c arbre.c main.c
	gcc $(CFLAGS) -o main base.c arbre.c main.c -lm
