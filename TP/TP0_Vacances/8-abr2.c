#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

struct ABR {
    int x;
    struct ABR* g;
    struct ABR* d;
};

typedef struct ABR abr;

bool recherche(abr* A, int x){
    // Un classique des ABR. On s'arrête sur le pointeur NULL.
    if (A == NULL) return false;
    if (A->x == x) return true;
    if (x < A->x) return recherche(A->g, x);
    return recherche(A->d, x);
}

abr* creer_noeud(int x){
    // Crée un unique nœud sans enfant.
    abr* noeud = malloc(sizeof(abr));
    noeud->x = x;
    noeud->g = NULL;
    noeud->d = NULL;
}

void insertion(abr* A, int x){
    // On compare à la racine pour savoir de quel côté insérer. Si ce côté
    // est vide, on le remplace par un nouveau nœud. Sinon, on fait un
    // appel récursif.
    if (x < A->x) {
        if (A->g == NULL)
            A->g = creer_noeud(x);
        else
            insertion(A->g, x);
    } else {
        if (A->d == NULL)
            A->d = creer_noeud(x);
        else
            insertion(A->d, x);
    }
}

void liberer_abr(abr* A){
    // Fonction de libération de la mémoire.
    if (A == NULL) return;
    liberer_abr(A->g);
    liberer_abr(A->d);
    free(A);
}

void infixe(abr* A){
    // Fonction qui affiche le parcours infixe d'un ABR.
    if (A == NULL) return;
    infixe(A->g);
    printf("%d, ", A->x);
    infixe(A->d);
}

int main(void){
    int tab[8] = {2, 5, 6, 8, 1, 18, 2, 4};
    abr* A = creer_noeud(7);
    for (int i=0; i<8; i++){
        insertion(A, tab[i]);
        infixe(A);
        printf("\n");
    }
    assert(recherche(A, 2));
    assert(recherche(A, 18));
    assert(!recherche(A, 3));
    liberer_abr(A);
    return EXIT_SUCCESS;
}