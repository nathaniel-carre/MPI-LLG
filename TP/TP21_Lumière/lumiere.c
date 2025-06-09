/*-----------------------------------------------------------*/
/* DÉBUT DU CODE DE BASE. NE PAS MODIFIER.                   */
/*-----------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>


/* Pointeur global réservé pour le tri. Ne pas utiliser. */
int* ordre_global_reserve = NULL;

/* Fonction de comparaison pour le tri. Ne pas utiliser. */
int comp_ordre_glob(int* u, int* v) {
    int c;
    if (ordre_global_reserve[*u] < ordre_global_reserve[*v]) {
        c = -1;
    } else if (ordre_global_reserve[*u] > ordre_global_reserve[*v]) {
        c = 1;
    } else if (*u < *v) {
        c = -1;
    } else if (*v < *u) {
        c = 1;
    } else {
        c = 0;
    }
    return c;
}


/* Les fonctions qui suivent peuvent être utilisées dans vos programmes
   si c'est utile. */

/* Prend en entrée un tableau a de n entiers,
   et un tableau ordre.
   Si ordre contient m éléments, les éléments de a doivent être
   entre 0 et m-1.
   Trie a dans l'ordre croissant pour l'ordre "<<" défini par
   "p << q lorsque ordre[p] < ordre[q], ou (ordre[p] = ordre[q] et p < q)"
   où "<" est l'ordre habituel sur les entiers.
   La fonction modifie directement le tableau a,
   plutôt que de renvoyer le tableau trié. */
/* Complexité en temps : O(n*log(n)) */
void trie_tableau(int* a, int n, int* ordre) {
    ordre_global_reserve = ordre;
    qsort(a, n, sizeof(int), (int (*) (const void *, const void*)) &comp_ordre_glob);
    ordre_global_reserve = NULL;
    return;
}


/* Affiche une matrice a de taille n * n */
void affiche_matrice(int** a, int n) {
    printf("/");          
    for (int j = 0; j < n; j++) {
        printf("  ");
    }
    printf(" \\\n");
    for (int i = 0; i < n; i++) {
        printf("|");
        for (int j = 0; j < n; j++) {
            printf(" %d", a[i][j]);
        }
        printf(" |\n");
    }
    printf("\\");
    for (int j = 0; j < n; j++) {
        printf("  ");
    }
    printf(" /\n");
    return;
}


/* On stockera dans Un la table des u_n précalculés */
int* Un;

/* On précalculera u_n jusqu'à n = max_n - 1 */
const int max_n = 3000000;


/*-----------------------------------------------------------*/
/* FIN DU CODE DE BASE.                                      */
/*-----------------------------------------------------------*/

// Remplit le tableau des Un
void calcul_un(int u0){
    Un[0] = u0;
    for (int i=1; i<max_n; i++){
        Un[i] = (569 * Un[i - 1]) % 2424259;
    }
}

// Alloue une matrice n×n, sans l'initialiser
int** mat_alloc(int n){
    int** A = malloc(n * sizeof(*A));
    for (int i=0; i<n; i++){
        A[i] = malloc(n * sizeof(int));
    }
    return A;
}

// Crée la matrice A_nmp
int** matrice(int n, int m, int p){
    int** A = mat_alloc(n);
    for (int i=0; i<n; i++){
        for (int j=i; j<n; j++){
            int k = n * n + 89 * m + 71 * p + n * i + j;
            if ((Un[k] % p) < m) {
                A[i][j] = 1;
                A[j][i] = 1;
            }
            else {
                A[i][j] = 0;
                A[j][i] = 0;
            }
        }
    }
    return A;
}

// Libère la mémoire occupée par une matrice
void libere_matrice(int** A, int n){
    for (int i=0; i<n; i++){
        free(A[i]);
    }
    free(A);
}

// Calcule la somme de contrôle de la question 7
int somme_controle(int** A, int n){
    int s = 0;
    for (int i=0; i<n; i++){
        for (int j=0; j<n; j++){
            s = (s + (n * i + j) * A[i][j]) % 10000;
        }
    }
    return s;
}

void test_q7(int n, int m, int p){
    int** A = matrice(n, m, p);
    printf("%d\n", somme_controle(A, n));
    libere_matrice(A, n);
}

void q7(void){
    test_q7(3, 1, 2);
    test_q7(20, 3, 4);
    test_q7(100, 3, 4);
    test_q7(1000, 1, 1000);
}

int largeur_bande(int** A, int n){
    int delta = 0;
    for (int i=0; i<n; i++){
        for (int j=0; j<n; j++){
            if (abs(j - i) > delta && A[i][j] == 1) delta = abs(j - i);
        }
    }
    return delta;
}

void test_q8(int n, int m, int p){
    int** A = matrice(n, m, p);
    printf("%d\n", largeur_bande(A, n));
    libere_matrice(A, n);
}

void q8(void){
    test_q8(10, 1, 10);
    test_q8(100, 1, 100);
    test_q8(200, 1, 1000);
}

// On crée une structure de graphe par listes d'adjacence pour faire le parcours.
// Un graphe est donné par son nombre de sommets, son tableau des degrés et un tableau
// de tableaux, chaque sous-tableau correspondant aux voisins d'un sommet. On crée les
// sous-tableaux de taille n, mais on ne remplit pas toutes les cases.
struct Graphe {
    int n;
    int* degres;
    int** adj;
};

typedef struct Graphe graphe;

// Convertit la matrice en graphe, en triant les voisins selon l'ordre sur les sommet
// indiqué par le sujet.
graphe convertir(int** A, int n){
    graphe G = {.n = n, .degres = malloc(n * sizeof(int)), .adj = malloc(n * sizeof(int*))};
    for (int s=0; s<n; s++){
        G.adj[s] = malloc(n * sizeof(int));
        G.degres[s] = 0;
        for (int t=0; t<n; t++){
            if (A[s][t] == 1){
                G.adj[s][G.degres[s]] = t;
                G.degres[s]++;
            }
        }
    }
    for (int s=0; s<n; s++){        
        trie_tableau(G.adj[s], G.degres[s], G.degres);
    }
    return G;
}

// Libère la mémoire occupé par le graphe
void libere_graphe(graphe G){
    for (int s=0; s<G.n; s++) free(G.adj[s]);
    free(G.adj);
    free(G.degres);
}

// Applique l'algorithme de parcours en largeur. La file est donnée par un tableau et
// deux indices de début et de fin. Pour éviter d'avoir à reparcourir les n sommets pour
// en trouver un non vu, on garde en mémoire un tableau des sommets à voir, initialement
// trié selon l'ordre donné par le sujet, qu'on parcourt linéairement, et on se sert du 
// tableau vus pour déterminer si un sommet a déjà été vu ou non.
int* parcours(graphe G){
    bool* vus = malloc(G.n * sizeof(bool));
    int* file = malloc(G.n * sizeof(int));
    int deb = 0; int fin = 0;
    int* a_voir = malloc(G.n * sizeof(int));
    for (int s=0; s<G.n; s++) {
        a_voir[s] = s;
        vus[s] = false;
    }
    trie_tableau(a_voir, G.n, G.degres);
    int courant = 0;
    while (courant < G.n){
        int s = a_voir[courant];        
        courant++;
        if (vus[s]) continue;
        vus[s] = true;
        file[fin] = s;
        fin++;
        while (deb < fin){
            int t = file[deb];
            deb++;
            for (int i=0; i<G.degres[t]; i++){
                int u = G.adj[t][i];
                if (!vus[u]){
                    vus[u] = true;
                    file[fin] = u;
                    fin++;
                }
            }
        }
    }
    free(vus);
    free(a_voir);
    return file;
}

// Calcule la matrice R(M)
int** RM(int** A, int n){
    graphe G = convertir(A, n);
    int* sigma = parcours(G);
    int** B = mat_alloc(n);
    for (int i=0; i<n; i++){
        for (int j=0; j<n; j++){
            B[i][j] = A[sigma[i]][sigma[j]];
        }
    }
    libere_graphe(G);
    free(sigma);
    return B;
}

void test_q9(int n, int m, int p){
    int** A = matrice(n, m, p);
    int** B = RM(A, n);
    printf("%d\n", somme_controle(B, n));
    libere_matrice(A, n);
    libere_matrice(B, n);
}

void q9(void){
    test_q9(10, 1, 10);
    test_q9(100, 1, 100);
    test_q9(1000, 1, 1000);
    test_q9(1000, 1, 10);
}

int main(void) {

    /* allocation d'un tableau de max_n entier pour stocker u_n */
    Un = malloc(max_n * sizeof(int));
    if (!Un) { printf("Erreur :  allocation Un\n"); exit(1); }

    int u0 = 1571;
    calcul_un(u0);

    q7();
    q8();
    q9();
    return 0;
}
