#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <math.h>

#include "utilitaire.h"

int* creer_tab(int k, int val){
    int* tab = malloc(k * sizeof(int));
    for (int i=0; i<k; i++) tab[i] = val;
    return tab;
}

int cardinal_couplage(graphe G, int* C){
    // On se contente de parcourir les sommets de X.
    int card = 0;
    for (int x=0; x<G.n; x++){
        if (C[x] != -1) card++;
    }
    return card;
}

liste* chemin_alternant(graphe G, int* C, bool* vus, int x){
    if (vus[x]) return NULL;
    vus[x] = true;
    liste* lst = G.adj[x];
    while (lst != NULL){
        int y = lst->val;
        lst = lst->succ;
        // Si le voisin y de x en cours d'étude est libre, on termine, s'il
        // est relié à x par une arête de C, on le saute, sinon on cherche
        // un chemin alternant depuis le voisin par C de y.
        if (C[y] == -1) return cons(x, cons(y, NULL));
        if (C[y] == x) continue;
        int u = C[y];
        liste* sigma_u = chemin_alternant(G, C, vus, u);
        if (sigma_u != NULL) return cons(x, cons(y, sigma_u));
    }
    return NULL;
}

void augmenter(int* C, liste* sigma){
    liste* lst = sigma;
    // On traite les sommets deux par deux.
    while (lst != NULL){
        int x = lst->val;
        int y = lst->succ->val;
        lst = lst->succ->succ;
        C[x] = y;
        C[y] = x;
    }
}

int* couplage_maximum(graphe G){
    int* C = creer_tab(G.n + G.p, -1);
    bool fini = false;
    while (!fini){
        fini = true;
        bool* vus = malloc((G.n + G.p) * sizeof(bool));
        for (int s=0; s<G.n + G.p; s++) vus[s] = false;
        // On cherche un chemin alternant depuis chaque sommet libre
        // de X. Si on en trouve un, on augmente C et on repasse dans
        // la boucle while.
        for (int x=0; x<G.n; x++){
            if (C[x] != -1) continue;
            liste* sigma = chemin_alternant(G, C, vus, x);
            if (sigma == NULL) continue;
            fini = false;
            augmenter(C, sigma);
            liberer_liste(sigma);
            break;
        }
        free(vus);
    }
    return C;
}

int* bfs_alternant(graphe G, int* C, int* dist){
    int* ordre_bfs = creer_tab(G.n + G.p, -1);
    for (int s=0; s<G.n + G.p; s++) dist[s] = -1;
    // L'indice debut correspond au prochain sommet à sortir de la file,
    // l'indice fin au premier emplacement libre dans la file.
    int debut = 0, fin = 0;
    for (int x = 0; x<G.n; x++){
        if (C[x] == -1){
            ordre_bfs[fin] = x;
            fin++;
            dist[x] = 0;
        }
    }
    // On implémente un BFS classique, en vérifiant que les chemins sont
    // bien alternants.
    while (debut < fin){
        int s = ordre_bfs[debut];
        debut++;
        liste* lst = G.adj[s];
        while (lst != NULL){
            int t = lst->val;
            lst = lst->succ;
            if (dist[t] != -1) continue;
            // On doit emprunter une arête de C si et seulement si s est dans Y.
            if ((s < G.n && C[s] != t) || (s >= G.n && C[s] == t)){
                dist[t] = dist[s] + 1;
                ordre_bfs[fin] = t;
                fin++;
            }
        }
    }
    return ordre_bfs;
}

liste* dfs_alternant(graphe G, int* C, int* dist, bool* vus, int y){
    // Même principe que la fonction chemin_alternant, sauf qu'on vérifie
    // que la distance diminue. Attention, on part d'un sommet de Y ici.
    if (vus[y]) return NULL;
    vus[y] = true;
    liste* lst = G.adj[y];
    while (lst != NULL){
        int x = lst->val;
        lst = lst->succ;
        if (C[x] == -1) return cons(y, cons(x, NULL));
        if (C[x] == y || dist[x] != dist[y] - 1) continue;
        int u = C[x];
        liste* sigma_u = dfs_alternant(G, C, dist, vus, u);
        if (sigma_u != NULL) return cons(y, cons(x, sigma_u));
    }
    return NULL;
}

int* hopcroft_karp(graphe G){
    int* C = creer_tab(G.n + G.p, -1);
    bool fini = false;
    while (!fini){
        fini = true;
        int* dist = creer_tab(G.n + G.p, -1); 
        int* ordre_bfs = bfs_alternant(G, C, dist);
        bool* vus = malloc((G.n + G.p) * sizeof(bool));
        for (int s=0; s<G.n + G.p; s++) vus[s] = false;
        // On augmente le couplage au fur et à mesure, le tableau vus
        // nous assure que les chemins suivants seront bien disjoints.
        int taille_min = G.n + G.p;
        for (int i=0; i<G.n + G.p; i++){
            int x = ordre_bfs[i];
            // On arrête dès que les chemins ne sont plus de taille minimale.
            if (x < 0 || dist[x] > taille_min) break;
            if (x < G.n || C[x] != -1) continue;
            liste* sigma = dfs_alternant(G, C, dist, vus, x);
            if (sigma == NULL) continue;
            taille_min = dist[x];
            fini = false;
            augmenter(C, sigma);
            liberer_liste(sigma);
        }
        free(dist);
        free(ordre_bfs);
        free(vus);
    }
    return C;
}

double poids_couplage(graphe G, double** M, int* C){
    double poids = 0;
    for (int x=0; x<G.n; x++){
        if (C[x] != -1) poids += M[x][C[x]];
    }
    return poids;
}

int** floyd_warshall(double** M, int n){
    // Algorithme classique au programme.
    int** pred = creer_matrice_int(n, -1);
    for (int i=0; i<n; i++){
        for (int j=0; j<n; j++){
            if (M[i][j] < INFINITY) pred[i][j] = i;
        }
    }
    for (int k=0; k<n; k++){
        for (int i=0; i<n; i++){
            for (int j=0; j<n; j++){
                double poids = M[i][k] + M[k][j];
                if (poids < M[i][j]){
                    M[i][j] = poids;
                    pred[i][j] = pred[k][j];
                }
            }
        }
    }
    return pred;
}

liste* plus_court_chemin(double** M, int n, int s, int t){
    // On part de la fin et on reconstruit le chemin en utilisant
    // la matrice pred.
    int** pred = floyd_warshall(M, n);
    liste* sigma = NULL;
    int u = pred[s][t];
    while (u != -1 && u != s){
        sigma = cons(u, sigma);
        u = pred[s][u];
    }
    liberer_matrice_int(pred, n);
    return sigma;
}

double** construire_GC(graphe G, double** M, int* C){
    // Les deux nouveaux sommets sont rajoutés à la fin.
    double** GC = creer_matrice_double(G.n + G.p + 2, INFINITY);
    int x = G.n + G.p;
    int y = x + 1;
    for (int s=0; s<G.n; s++){
        if (C[s] == -1) GC[x][s] = 0;
        GC[s][s] = 0;
        for (int t=G.n; t<G.n + G.p; t++){
            if (C[t] == -1) GC[t][y] = 0;
            if (C[s] != t){
                GC[s][t] = M[s][t];
            } else {
                GC[t][s] = -M[s][t];
            }
        }
    }
    return GC;
}

int* couplage_maximum_poids_minimum(graphe G, double** M){
    int* C = creer_tab(G.n + G.p, -1);
    bool fini = false;
    while (!fini){
        fini = true;
        double** GC = construire_GC(G, M, C);
        liste* sigma = plus_court_chemin(GC, G.n + G.p + 2, G.n + G.p, G.n + G.p + 1);
        if (sigma != NULL) {
            fini = false;
            augmenter(C, sigma);
            liberer_liste(sigma);
        }
        liberer_matrice_double(GC, G.n + G.p + 2);
    }
    return C;
}

int main(void){
    graphe G1 = generer_graphe(6, 6, 3, 11);
    double** M1 = creer_ponderations(G1, 30, 11);

    graphe G2 = generer_graphe(100, 100, 5, 1);
    double** M2 = creer_ponderations(G2, 100, 1);

    graphe G3 = generer_graphe(10000, 10000, 10, 42);
    
    int C1[12] = {-1, 10, 6, 11, -1, 7, 2, 5, -1, -1, 1, 3};
    printf("Cardinal de C1 : %d\n", cardinal_couplage(G1, C1));

    int* C1bis = couplage_maximum(G1);
    int* C2 = couplage_maximum(G2);
    int* C3 = couplage_maximum(G3);
    printf("Couplage maximum de G1 : \n");
    afficher_tab(C1bis, 12);
    printf("Cardinal d'un couplage maximum de G1 : %d\n", cardinal_couplage(G1, C1bis));
    printf("Cardinal d'un couplage maximum de G2 : %d\n", cardinal_couplage(G2, C2));
    printf("Cardinal d'un couplage maximum de G3 : %d\n", cardinal_couplage(G3, C3));
    free(C1bis);
    free(C2);
    free(C3);

    int dist[12];
    int* ordre_bfs = bfs_alternant(G1, C1, dist);
    printf("Distances dans un BFS alternant dans G1, pour C1 :\n");
    afficher_tab(dist, 12);
    printf("Ordre d'un BFS alternant dans G1, pour C1 :\n");
    afficher_tab(ordre_bfs, 12);
    free(ordre_bfs);

    C1bis = hopcroft_karp(G1);
    C2 = hopcroft_karp(G2);
    C3 = hopcroft_karp(G3);
    printf("Couplage maximum de G1 : \n");
    afficher_tab(C1bis, 12);
    printf("Cardinal d'un couplage maximum de G1 : %d\n", cardinal_couplage(G1, C1bis));
    printf("Cardinal d'un couplage maximum de G2 : %d\n", cardinal_couplage(G2, C2));
    printf("Cardinal d'un couplage maximum de G3 : %d\n", cardinal_couplage(G3, C3));
    free(C1bis);
    free(C2);
    free(C3);

    printf("Temps de calcul de l'algo du cours sur G2 : %lf\n", chronometre(couplage_maximum, G2));
    printf("Temps de calcul de Hopcroft-Karp sur G2 : %lf\n", chronometre(hopcroft_karp, G2));

    printf("Temps de calcul de l'algo du cours sur G3 : %lf\n", chronometre(couplage_maximum, G3));
    printf("Temps de calcul de Hopcroft-Karp sur G3 : %lf\n", chronometre(hopcroft_karp, G3));

    C1bis = couplage_maximum_poids_minimum(G1, M1);
    C2 = couplage_maximum_poids_minimum(G2, M2);
    printf("Poids minimum d'un couplage maximum de G1 : %lf\n", poids_couplage(G1, M1, C1bis));
    printf("Poids minimum d'un couplage maximum de G2 : %lf\n", poids_couplage(G2, M2, C2));
    free(C1bis);
    free(C2);

    liberer_matrice_double(M1, 12);
    liberer_graphe(G1);

    liberer_matrice_double(M2, 200);
    liberer_graphe(G2);

    liberer_graphe(G3);
    return 0;
}