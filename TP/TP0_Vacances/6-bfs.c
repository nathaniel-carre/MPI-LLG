#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

struct Graphe {
    int n;
    int* deg;
    int** adj;
};

int* bfs(struct Graphe G, int s0){
    // On initialise le tableau des distances, et on alloue la file.
    // L'indice deb est l'indice du prochain élément à défiler, et
    // fin est l'indice de l'emplacement du prochain élément à enfiler.
    // Tant que la file est non vide, on défile un élément, et on parcourt
    // ses voisins pas encore vus, qu'on ajoute à la file, et pour lesquels
    // on met à jour là distance.
    int* dist = malloc(G.n * sizeof(int));
    int* file = malloc(G.n * sizeof(int));
    for (int s=0; s<G.n; s++) dist[s] = -1;
    dist[s0] = 0;
    file[0] = s0;
    int deb = 0;
    int fin = 1;
    while (deb < fin){
        int s = file[deb];
        deb++;
        for (int i=0; i<G.deg[s]; i++){
            int t = G.adj[s][i];
            if (dist[t] == -1){
                dist[t] = 1 + dist[s];
                file[fin] = t;
                fin++;
            }
        }
    }
    free(file);
    return dist;
}

void print_array(int* tab, int n){
    // Fonction utilitaire qui affiche un tableau.
    for (int i=0; i<n; i++){
        printf("%d, ", tab[i]);
    }
    printf("\n");
}

int main(void){
    // Graphe non connexe
    int deg[7] = {2, 1, 1, 1, 4, 1, 2};
    int adj0[] = {4, 6};
    int adj1[] = {4};
    int adj2[] = {3};
    int adj3[] = {2};
    int adj4[] = {0, 1, 5, 6};
    int adj5[] = {4};
    int adj6[] = {0, 4};
    int* adj[7] = {adj0, adj1, adj2, adj3, adj4, adj5, adj6};
    struct Graphe G = {.n = 7, .deg = deg, .adj = adj};
    int* dist = bfs(G, 6);
    print_array(dist, 7);
    free(dist);
    return EXIT_SUCCESS;
}