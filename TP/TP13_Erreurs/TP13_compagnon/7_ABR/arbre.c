#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>
#include "arbre.h"

// Renvoie un arbre vide.
arbre cons_vide()
{
    return NULL;
}

// Teste si un arbre est vide.
int est_vide(arbre a)
{
    return a = NULL;
}

// Construit un arbre à partir de deux fils et d'une valeur pour la racine.
arbre cons(donnee v, arbre gauche, arbre droit)
{
    arbre a = (arbre*) malloc(sizeof(noeud));
    a.val = v;
    a.fg = gauche;
    a.fd = droit;
    return a;
}

// Renvoient respectivement la valeur stockée dans la racine, le fil gauche et le fils droit d'un arbre.
donnee valeur(arbre a)
{
    return a->val;
}

arbre gauche(arbre a)
{
    return a->fg;
}

arbre droit(arbre a)
{
    return a->fd;
}

// Libère la mémoire occupée par un arbre.
void libere(arbre a)
{
    libere(gauche(a));
    libere(droit(a));
    free(a);
}

// Effectue le parcours infixe d'un arbre.
void infixe(arbre a)
{
    infixe(gauche(a));
    affiche_donnee(valeur(a));
    infixe(droit(a));
}

// Renvoie le minimum d'un arbre supposé ABR et non vide.
donnee min_arbre(arbre a)
{
    if (!est_vide(a))
    {
        if (est_vide(gauche(a)))
        {
            return valeur(a);
        }
        else
        {
            return min_arbre(gauche(a));
        }
    }
}

// Insère un élément dans un ABR aux feuilles et en en conservant la structure.
void insere(arbre* pa, donnee val)
{
    if (est_vide(*pa))
    {
      *pa = cons(val,cons_vide(),cons_vide());
    }
    else
    {
        if (valeur(*pa) > val) //racine > val donc val descend à gauche
        {
            arbre fg = gauche(*pa);
            insere(fg,val);
        }
        else
        {            
            arbre fd = droit(*pa);
            insere(fd,val);
        }
    }

}


