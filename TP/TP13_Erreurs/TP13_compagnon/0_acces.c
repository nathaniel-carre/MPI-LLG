#include <stdio.h>

int somme(int* tab, int n)
{
    int s = 0;
    for (int i = 0; i <= n; i++)
    {
        s += tab[i];
    }
    return s;
}

int main(void)
{
    int tableau[4] = {1, 2, 3, 4};    
    printf("%d\n", somme(tableau,4));
    return 0;
}