#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <math.h>
#include <limits.h>

#include "utilitaire.h"

static unsigned long int next = 1;

int rand(void){
    // Renvoie un entier selon une suite pseudo-aléatoire.
    next = next * 1103515245 + 12345;
    return (unsigned int)(next/65536) % 2147483647;
}

void srand(unsigned int seed){
    // Permet d'initialiser la graine du générateur pseudo-aléatoire.
    next = seed;
}

liste* cons(int tete, liste* queue){
    // Construit une nouvelle liste en prenant en argument une tête
    // (un entier) et une queue (une autre liste).
    liste* lst = malloc(sizeof(liste));
    lst->val = tete;
    lst->succ = queue;
    return lst;
}

void liberer_liste(liste* lst){
    // Libère la mémoire occupée par une liste.
    if (lst != NULL){
        liberer_liste(lst->succ);
    }
    free(lst);
}

void afficher_liste(liste* lst){
    // Affiche les éléments d'une liste.
    printf("[");
    while (lst != NULL){
        printf("%d ", lst->val);
        lst = lst->succ;
    }
    printf("]\n");
}

graphe creer_graphe(int n, int p){
    // Crée un graphe biparti sans arête étant donné |X| et |Y|.
    liste** adj = malloc((n + p) * sizeof(liste*));
    for (int s=0; s<n + p; s++) adj[s] = NULL;
    graphe G = {.n = n, .p = p, .adj = adj};
    return G;
}

void liberer_graphe(graphe G){
    // Libère la mémoire occupée par le tableau de listes d'adjacence
    // d'un graphe.
    for (int s=0; s<G.n + G.p; s++) liberer_liste(G.adj[s]);
    free(G.adj);
}

void ajouter_arete(graphe G, int s, int t){
    // Ajoute une arête dans un graphe.
    G.adj[s] = cons(t, G.adj[s]);
    G.adj[t] = cons(s, G.adj[t]);
}

graphe generer_graphe(int n, int p, int deg_max, int graine){
    // Crée un graphe biparti aléatoirement, étant donné |X|, 
    // |Y|, le degré maximal d'un sommet de X et une graine.
    srand(graine);
    graphe G = creer_graphe(n, p);
    int* tab = malloc(p * sizeof(int));
    for (int i=0; i<p; i++) tab[i] = n + i;
    for (int s=0; s<n; s++){
        int deg = rand() % (deg_max + 1);
        for (int i=0; i<deg; i++){
            int j = rand() % (p - i);
            int t = tab[j];
            tab[j] = tab[p - i - 1];
            tab[p - i - 1] = t;
            ajouter_arete(G, s, t);
        }
    }
    free(tab);
    return G;
}

void afficher_tab(int* tab, int n){
    // Affiche un tableau d'entiers.
    printf("{");
    for (int i=0; i<n - 1; i++) printf("%d, ", tab[i]);
    if (n > 0) printf("%d", tab[n - 1]);
    printf("}\n");
}

double** creer_matrice_double(int n, double val){
    // Crée une matrice de doubles de taille n × n, contenant
    // uniquement la valeur val.
    double** M = malloc(n * sizeof(double*));
    for (int i=0; i<n; i++){
        M[i] = malloc(n * sizeof(double));
        for (int j=0; j<n; j++) M[i][j] = val;
    }
    return M;
}

void liberer_matrice_double(double** M, int n){
    // Libère la mémoire occupée par une matrice de doubles.
    for (int i=0; i<n; i++){
        free(M[i]);
    }
    free(M);
}

double** creer_ponderations(graphe G, double poids_max, int graine){
    // Crée une matrice de pondérations aléatoires pour un graphe
    // biparti étant donné un poids maximal d'arête.
    srand(graine);
    double** M = creer_matrice_double(G.n + G.p, INFINITY);
    for (int s=0; s<G.n; s++){
        liste* lst = G.adj[s];
        while (lst != NULL){
            int t = lst->val;
            lst = lst->succ;
            double poids = (((double) rand()) / RAND_MAX) * poids_max;
            M[s][t] = poids;
            M[t][s] = poids;
        }
    }
    return M;
}

int** creer_matrice_int(int n, int val){
    // Crée une matrice d'entiers de taille n × n, contenant
    // uniquement la valeur val.
    int** M = malloc(n * sizeof(int*));
    for (int i=0; i<n; i++){
        M[i] = malloc(n * sizeof(int));
        for (int j=0; j<n; j++) M[i][j] = val;
    }
    return M;
}

void liberer_matrice_int(int** M, int n){
    // Libère la mémoire occupée par une matrice d'entiers.
    for (int i=0; i<n; i++){
        free(M[i]);
    }
    free(M);
}

double chronometre(int* f(graphe), graphe arg){
    // Renvoie le temps d'exécution en secondes pour calculer f(arg),
    // f étant une fonction qui prend en argument un graphe et renvoie
    // un tableau d'entiers.
    struct timeval start, stop;
    gettimeofday(&start, NULL);
    int* tab = f(arg); 
    gettimeofday(&stop, NULL);
    free(tab);
    return (stop.tv_sec - start.tv_sec) + 1e-6 * (stop.tv_usec - start.tv_usec);
}