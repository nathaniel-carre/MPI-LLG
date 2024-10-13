#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

//#include "dicts.h"

struct AFD {
    int Q;
    int Sigma;
    int q0;
    bool* finaux;
    int** delta;
};

typedef struct AFD afd;

void liberer_afd(afd A){
    // À modifier
}

afd init_afd(int Q, int Sigma, int q0){
    afd A;
    // À modifier
    return A;
}

void ajout_transition_afd(afd A, int q, char a, int p){
    // À modifier
}

int delta_etoile_afd(afd A, int q, char* u){
    // À modifier    
    return 0;
}

bool reconnu_afd(afd A, char* u){
    // À modifier    
    return false;
}

/*
struct AFND {
    int Q;
    int Sigma;
    bool* initiaux;
    bool* finaux;
    liste*** Delta;
};

typedef struct AFND afnd;

void liberer_afnd(afnd B){
    // Libère la mémoire d'un AFND.
    free(B.initiaux);
    free(B.finaux);
    for (int i=0; i<B.Q; i++){
        for (int j=0; j<B.Sigma; j++){
            liberer_liste(B.Delta[i][j]);
        }
        free(B.Delta[i]);
    }
    free(B.Delta);
}

afnd init_afnd(int Q, int Sigma){
    // Initialise un AFND.
    afnd B;
    B.Q = Q;
    B.Sigma = Sigma;
    bool* finaux = malloc(Q * sizeof(*finaux));
    bool* initiaux = malloc(Q * sizeof(*initiaux));
    liste*** Delta = malloc(Q * sizeof(*Delta));
    for (int q=0; q<Q; q++){
        finaux[q] = false;
        initiaux[q] = false;
        liste** tab = malloc(Sigma * sizeof(*tab));
        for (int a=0; a<Sigma; a++){
            tab[a] = NULL;
        }
        Delta[q] = tab;
    }
    B.finaux = finaux;
    B.initiaux = initiaux;
    B.Delta = Delta;
    return B;
}

void ajout_transition_afnd(afnd B, int q, char a, int p){
    // À modifier
}

bool* Delta_etats(afnd B, bool* X, char a){
    // À modifier    
    return NULL;
}

bool* Delta_etoile_afnd(afnd B, bool* X, char* u){
    // À modifier    
    return NULL;
}

bool reconnu_afnd(afnd B, char* u){
    // À modifier
    return false;
}

int etats_vers_entier(bool* X, int Q){
    // À modifier
    return 0;
}

bool* entier_vers_etats(int x, int Q){
    // À modifier
    return NULL;
}

afd determiniser(afnd B){
    afd A;
    // À modifier
    return A;
}

dict accessibles(afnd B){
    dict D;
    // À modifier
    return D;
}

afd determiniser2(afnd B){
    afd A;
    // À modifier
    return A;
}
*/

int main(void){
    //afd A1 = init_afd(4, 2, 0);
    //A1.finaux[3] = true;
    //ajout_transition_afd(A1, 0, 'b', 0); ajout_transition_afd(A1, 0, 'a', 1);
    //ajout_transition_afd(A1, 1, 'a', 2); ajout_transition_afd(A1, 1, 'b', 0);
    //ajout_transition_afd(A1, 2, 'a', 2); ajout_transition_afd(A1, 2, 'b', 3);
    //ajout_transition_afd(A1, 3, 'a', 1); ajout_transition_afd(A1, 3, 'b', 0);
    
    char u[] = "abbabbabaab";
    char v[] = "baababbbbba";
    char w[] = "aaabababb";

    //liberer_afd(A1);

    //afnd B1 = init_afnd(6, 2);
    //B1.initiaux[0] = true; B1.initiaux[3] = true;
    //B1.finaux[2] = true; B1.finaux[5] = true;
    //ajout_transition_afnd(B1, 0, 'a', 0); ajout_transition_afnd(B1, 0, 'b', 0);
    //ajout_transition_afnd(B1, 0, 'a', 1); ajout_transition_afnd(B1, 1, 'b', 2);
    //ajout_transition_afnd(B1, 3, 'b', 4); ajout_transition_afnd(B1, 4, 'a', 5);
    //ajout_transition_afnd(B1, 5, 'a', 5); ajout_transition_afnd(B1, 5, 'b', 5);
    
    //liberer_afnd(B1);
    return 0;
}