#include <string.h>
#include <stdlib.h>
#include <stdio.h>

// On réimplémente la fonction strcat
char* concatener(char* s1, char* s2)
{
    int n1 = strlen(s1);
    int n2 = strlen(s2);
    char* s = (char*) malloc(sizeof(char)*(n1+n2));
    for (int i = 0; i < n1; i++)
    {
        s[i] = s1[i];
    }
    for (int i = n1; i < n1+n2; i++)
    {
        s[i] = s2[i];
    }
    return s;
}

int main(void)
{
    char s1[] = "ceci est une jolie chaine mais elle s'arrete ";
    char s2[] = "un peu trop tot donc on la rallonge un peu, c'est mieux !";
    char* s = concatener(s1,s2);
    printf("%s, longueur = %ld\n", s, strlen(s));
    free(s);
}