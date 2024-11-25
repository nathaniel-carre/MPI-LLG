#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "dicts.h"

struct TTT {
    int n;
    int k;
    int* grille;
};
 
typedef struct TTT ttt;

ttt init_jeu(int k, int n);

int* repartition(ttt jeu);

int joueur_courant(ttt jeu);

void jouer_coup(ttt jeu, int lgn, int cln);

bool alignement(ttt jeu, int i, int di, int joueur);

bool gagnant(ttt jeu, int joueur);

int encodage(ttt jeu);

int attracteur(ttt jeu, dict* D);

int strategie_optimale(ttt jeu, dict* D);

void afficher(ttt jeu);

void jouer_partie(int k, int n);

int main(void){
    return 0;
}