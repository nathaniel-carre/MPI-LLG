#include "liste.h"

// Renvoie un pointeur vers la liste qui contient les éléments
// contenus dans la liste a plus la paire (n,d).
liste* ajoute(liste* a, int n, double d) {
    liste* nouv_liste = malloc(sizeof(liste));
    nouv_liste->voisin = n;
    nouv_liste->distance = d;
    nouv_liste->suivant = a;
    return nouv_liste;
}

int taille_liste(liste* a) {
    if (a == NULL){
        return 0;
    } else {
        return 1 + taille_liste(a->suivant);
    }
}

void liberer_liste(liste* a){
    if (a != NULL){
        liberer_liste(a->suivant);
        free(a);
    }
}