#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

struct graph_s {
    int n;
    int degree[100];
    int voisins[100][10];
};

typedef struct graph_s graph;

int degre_max(graph* g, bool* S_prime){
    // À modifier
    return 0;
}

bool* accessibles(graph* g, int s){
    // À modifier
    return NULL;
}

int degre_etoile(graph* g, int s){
    // À modifier
    return 0;
}


int main(void){
    // Encodage du graphe donné en exemple.
    graph g0 = {
        .n = 9, 
        .degree = {0, 2, 1, 2, 2, 3, 1, 1, 0},
        .voisins = {{}, {0, 4}, {4}, {0, 4}, {6, 7}, {2, 4, 8}, {7}, {8}, {}}
    };

    return EXIT_SUCCESS;
}