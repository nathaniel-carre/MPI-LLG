main: *.c
	gcc -o main.exe -Wall *.c -lm

safe: *.c
	gcc -o main.exe -g -Wall -Wextra -fsanitize=address *.c -lm

# Pour le jury, non demandé aux candidats
full: *.c
	gcc -o main.exe -g -Wall -Wextra -std=c99 -pedantic -Wvla -fsanitize=address,undefined *.c -lm

clean:
	rm -f *.o *.exe
