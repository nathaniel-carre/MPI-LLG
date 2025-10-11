#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "utilitaire.h"

int* creer_tab(int k, int val){
    int* tab = malloc(k * sizeof(int));
    for (int i=0; i<k; i++) tab[i] = val;
    return tab;
}

int cardinal_couplage(graphe G, int* C){
    // En parcourant tous les sommets, on compte
    // deux fois chaque arête de C.
    int card = 0;
    for (int s=0; s<G.n; s++){
        if (C[s] != -1) card++;
    }
    return card / 2;
}

void dfs(graphe G, int t, int u, int* pred){
    // On met à jour le prédécesseur, puis on relance un
    // parcours depuis les voisins de t non vus.
    pred[t] = u;
    for (liste* lst=G.adj[t]; lst!=NULL; lst=lst->succ){
        int v = lst->val;
        if (pred[v] == -2) dfs(G, v, t, pred);
    }
}

int* accessibles(graphe G, int s){
    // On crée un tableau de -2, et on lance un parcours
    // depuis la source s.
    int* pred = creer_tab(G.n, -2);
    dfs(G, s, -1, pred);
    return pred;
}

int calcul_nX(graphe G){
    // Il suffit de trouver la première arête d'un sommet
    // vers un sommet plus petit, les sommets de X étant
    // supposés avant ceux de Y.
    for (int s=0; s<G.n; s++){
        if (G.adj[s] != NULL && G.adj[s]->val < s) return s;
    }
    return 0;
}

graphe graphe_augmentation(graphe G, int nX, int* C){
    // On crée un graphe avec deux sommets de plus, et on parcourt
    // chaque sommet. Pour les sommets libres, on ajoute une arête
    // depuis la source ou vers le puits (selon qu'on est dans X ou Y),
    // et on parcourt les listes d'adjacence pour ajouter les arêtes
    // depuis X ou depuis Y selon si l'arête est dans le couplage.
    graphe GA = creer_graphe(G.n + 2);
    for (int s=0; s<G.n; s++){
        if (s<nX && C[s] == -1) GA.adj[G.n] = cons(s, GA.adj[G.n]);
        if (s >= nX && C[s] == -1) GA.adj[s] = cons(G.n + 1, GA.adj[s]);
        for (liste* lst=G.adj[s]; lst!=NULL; lst=lst->succ){
            int t = lst->val;
            if ((s<nX && C[s] != t)||(s>=nX && C[s] == t)) GA.adj[s] = cons(t, GA.adj[s]);
        }
    }
    return GA;
}

liste* chemin_augmentant(graphe G, int nX, int* C){
    // On calcule le graphe d'augmentation et le tableau
    // des prédécesseurs depuis le sommet source, puis, en
    // partant du prédécesseur du sommet puits (s'il existe),
    // on reconstitue le chemin en passant au prédécesseur à 
    // chaque étape.
    graphe GA = graphe_augmentation(G, nX, C);
    int* pred = accessibles(GA, G.n);
    liste* sigma = NULL;
    int cur = pred[G.n + 1];
    while (cur != -2 && cur != G.n){
        sigma = cons(cur, sigma);
        cur = pred[cur];
    }
    liberer_graphe(GA);
    free(pred);
    return sigma;
}

void augmenter(int* C, liste* sigma){
    // On traite une arête sur deux. Il n'y a pas besoin de « retirer »
    // les arêtes de C, car avec la représentation choisie, l'ajout des
    // arêtes hors de C écrase les arêtes de C.
    for (liste* lst = sigma; lst != NULL; lst = lst->succ->succ){
        C[lst->val] = lst->succ->val;
        C[lst->succ->val] = lst->val;
    }
}

int* couplage_maximum(graphe G){
    // On commence par calculer |X|. Ensuite, en partant d'un ensemble vide C,
    // tant qu'il existe un chemin augmentant, on augmente C.
    int nX = calcul_nX(G);
    int* C = creer_tab(G.n, -1);
    bool flag = true;
    while (flag){
        liste* sigma = chemin_augmentant(G, nX, C);
        flag = (sigma != NULL);
        if (flag) augmenter(C, sigma);
        liberer_liste(sigma);
    }
    return C;
}

int* bfs_alternant(graphe G, int nX, int* C, int* dist){
    int* ordre_bfs = creer_tab(G.n, -1);
    for (int s=0; s<G.n; s++) dist[s] = -1;
    // L'indice debut correspond au prochain sommet à sortir de la file,
    // l'indice fin au premier emplacement libre dans la file.
    int debut = 0, fin = 0;
    for (int x = 0; x<nX; x++){
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
        for (liste* lst=G.adj[s]; lst!=NULL; lst=lst->succ){
            int t = lst->val;
            if (dist[t] != -1) continue;
            // On doit emprunter une arête de C si et seulement si s est dans Y.
            if ((s < nX && C[s] != t) || (s >= nX && C[s] == t)){
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
    for (liste* lst=G.adj[y]; lst!=NULL; lst=lst->succ){
        int x = lst->val;
        if (C[x] == -1) return cons(y, cons(x, NULL));
        if (C[x] == y || dist[x] != dist[y] - 1) continue;
        int u = C[x];
        liste* sigma_u = dfs_alternant(G, C, dist, vus, u);
        if (sigma_u != NULL) return cons(y, cons(x, sigma_u));
    }
    return NULL;
}

int* hopcroft_karp(graphe G){
    int nX = calcul_nX(G);
    int* C = creer_tab(G.n, -1);
    bool fini = false;
    while (!fini){
        fini = true;
        int* dist = creer_tab(G.n, -1); 
        int* ordre_bfs = bfs_alternant(G, nX, C, dist);
        bool* vus = malloc((G.n) * sizeof(bool));
        for (int s=0; s<G.n; s++) vus[s] = false;
        // On augmente le couplage au fur et à mesure, le tableau vus
        // nous assure que les chemins suivants seront bien disjoints.
        int taille_min = G.n;
        for (int i=0; i<G.n; i++){
            int s = ordre_bfs[i];
            // On arrête dès que les chemins ne sont plus de taille minimale.
            if (s < 0 || dist[s] > taille_min) break;
            if (s < nX || C[s] != -1) continue;
            liste* sigma = dfs_alternant(G, C, dist, vus, s);
            if (sigma == NULL) continue;
            taille_min = dist[s];
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

int main(void){
    graphe G1 = generer_biparti(6, 6, 3, 11);

    for (int s=0; s<6; s++){
        printf("Sommet %d : ", s);
        afficher_liste(G1.adj[s]);
    }

    graphe G2 = generer_biparti(100, 100, 5, 1);
    graphe G3 = generer_biparti(5000, 5000, 10, 42);

    int C1[12] = {-1, 10, 6, 11, -1, 7, 2, 5, -1, -1, 1, 3};
    printf("Cardinal de C1 : %d\n", cardinal_couplage(G1, C1));

    int* pred = accessibles(G1, 0);
    printf("Tableau de prédécesseurs depuis 0 dans G1 : ");
    afficher_tab(pred, 12);
    free(pred);

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
    int* ordre_bfs = bfs_alternant(G1, 6, C1, dist);
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

    liberer_graphe(G1);
    liberer_graphe(G2);
    liberer_graphe(G3);
    return 0;
}