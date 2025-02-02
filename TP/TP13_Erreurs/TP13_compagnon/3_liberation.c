#include <stdio.h>
#include <stdlib.h>

int* tab_n(int n)
{
    int* t = (int*) malloc(sizeof(int)*n);
    for (int i = 0; i < n; i++)
    {
        t[i] = n;
    }
    return t;
}

int vaut_3(int n)
{
    int* tab = tab_n(n);
    if (tab[0] = 3) { return 1;}
    else { return 0;}
    free(tab);
}

int main(void)
{
    printf("%d\n", vaut_3(1));
    printf("%d\n", vaut_3(3));
    printf("%d\n", vaut_3(42));
}