#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <math.h>

#include "utilitaire.h"

int* creer_tab(int k, int val){
    // À compléter
    return NULL;
}

int cardinal_couplage(graphe G, int* C){
    // À compléter
    return 0;
}

liste* chemin_alternant(graphe G, int* C, bool* vus, int x){
    // À compléter
    return NULL;
}

void augmenter(int* C, liste* sigma){
    // À compléter
}

int* couplage_maximum(graphe G){
    // À compléter
    return NULL;
}

int* bfs_alternant(graphe G, int* C, int* dist){
    // À compléter
    return NULL;
}

liste* dfs_alternant(graphe G, int* C, int* dist, bool* vus, int y){
    // À compléter
    return NULL;
}

int* hopcroft_karp(graphe G){
    // À compléter
    return NULL;
}

double poids_couplage(graphe G, double** M, int* C){
    // À compléter
    return 0;
}

int** floyd_warshall(double** M, int n){
    // À compléter
    return NULL;
}

liste* plus_court_chemin(double** M, int n, int s, int t){
    // À compléter
    return NULL;
}

double** construire_GC(graphe G, double** M, int* C){
    // À compléter
    return NULL;
}

int* couplage_maximum_poids_minimum(graphe G, double** M){
    // À compléter
    return NULL;
}

int main(void){
    graphe G1 = generer_graphe(6, 6, 3, 11);

    for (int s=0; s<6; s++){
        printf("Sommet %d : ", s);
        afficher_liste(G1.adj[s]);
    }

    double** M1 = creer_ponderations(G1, 30, 11);

    graphe G2 = generer_graphe(100, 100, 5, 1);
    double** M2 = creer_ponderations(G2, 100, 1);

    graphe G3 = generer_graphe(10000, 10000, 10, 42);
    
    /*
    
    
    À compléter
    
    
    */


    liberer_matrice_double(M1, 12);
    liberer_graphe(G1);

    liberer_matrice_double(M2, 200);
    liberer_graphe(G2);

    liberer_graphe(G3);
    return 0;
}