#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "arbre.h"

int main()
{
    donnee mini = {"Mac Mini", 600};
    donnee hp = {"HP EliteBook", 1300};
    donnee dell = {"DELL Latitude", 1000};
    donnee asus = {"ASUS ExpertBook", 750};
    donnee acer = {"ACER Nitro", 830};
    donnee pro = {"MacBook Pro", 3000};
    donnee len = {"Lenovo Legion", 2500};

    arbre abr = cons_vide();
    insere(&abr,mini);
    insere(&abr,hp);
    insere(&abr,dell);
    insere(&abr,asus);
    insere(&abr,acer);
    insere(&abr,pro);
    insere(&abr,len);

    printf("parcours infixe : "); infixe(abr);
    printf("\n\n");
    printf("article le moins cher = "); affiche_donnee(min_arbre(abr)); printf("\n");

    libere(abr);
}
