#ifndef LISTE_H
#define LISTE_H

#include <stdlib.h>

struct liste_voisin {
    int voisin;
    double distance;
    struct liste_voisin* suivant;
};
typedef struct liste_voisin liste;

liste* ajoute(liste*, int, double);
int taille_liste(liste*);
void liberer_liste(liste*);

#endif
