#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

typedef struct Graphe {
    int n;
    bool** mat;
} graphe;

void dfs(graphe G, int* vus, int s){
    // DFS récursif, avec le tableau des sommets vus en argument
    // supplémentaire.
    if (vus[s]) return;
    vus[s] = true;
    for (int t=0; t<G.n; t++){
        if (G.mat[s][t]) dfs(G, vus, t);
    }
}

bool connexe(graphe G){
    // On crée un tableau des sommets vus, on lance un parcours depuis
    // le sommet 0, puis on vérifie que tous les sommets sont vus.
    int* vus = malloc(G.n * sizeof(int));
    for (int s=0; s<G.n; s++) vus[s] = false;
    dfs(G, vus, 0);
    for (int s=0; s<G.n; s++){
        if (!vus[s]){
            free(vus);
            return false;
        }
    }
    free(vus);
    return true;
}

int main(void){
    // Graphe non connexe
    bool mat[7][7] = {
        {0, 0, 0, 0, 1, 0, 1},
        {0, 0, 0, 0, 1, 0, 0},
        {0, 0, 0, 1, 0, 0, 0},
        {0, 0, 1, 0, 0, 0, 0},
        {1, 1, 0, 0, 0, 1, 1},
        {0, 0, 0, 0, 1, 0, 0},
        {1, 0, 0, 0, 1, 0, 0}
    };
    bool* ptr_mat[7] = {
        mat[0], mat[1], mat[2], mat[3], mat[4], mat[5], mat[6]
    };
    graphe G = {.n = 7, .mat = ptr_mat};
    assert (!connexe(G));

    // On ajoute une arête.
    mat[3][5] = true;
    mat[5][3] = true;
    assert(connexe(G));
    return EXIT_SUCCESS;
}