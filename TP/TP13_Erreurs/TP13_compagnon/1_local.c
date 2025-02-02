#include <stdio.h>

int* minimum(int a, int b)
{
    if(a < b)
    {
        return &a;
    }
    else
    {
        return &b;
    }
}

int main(void)
{
    int *p = minimum(3, 4);
    printf("%d\n", *p);
    return 0;
}