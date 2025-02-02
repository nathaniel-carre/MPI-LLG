#include <stdio.h>


int pgcd(int a, int b){
    if (b >= a){
        int stockage_val = a;
        a = b;
        b = stockage_val;
    }
    while (a >= b){
        a = a - b;
    }
    for (int i = a; i >= 1; i++){
        if (b%i == a%i && b%i == 0){
            return i;
        }
    }
    return 0;
}

int main(void)
{
    printf("%d\n", pgcd(59,13));
}