struct Liste {
    int val; // Valeur du maillon
    struct Liste* succ; // Pointeur vers le maillon suivant
};

typedef struct Liste liste;

struct Graphe {
    // Implémente un graphe biparti G = (X U Y, A)
    int n; // Taille de X
    int p; // Taille de Y
    liste** adj; // Tableau de listes d'adjacence
};

typedef struct Graphe graphe;

liste* cons(int, liste*);

void liberer_liste(liste*);

graphe creer_graphe(int, int);

void liberer_graphe(graphe);

void ajouter_arete(graphe, int, int);

graphe generer_graphe(int, int, int, int);

void afficher_tab(int*, int);

double** creer_matrice_double(int, double);

void liberer_matrice_double(double**, int);

double** creer_ponderations(graphe, double, int);

int** creer_matrice_int(int, int);

void liberer_matrice_int(int**, int);

double chronometre(int* f(graphe), graphe arg);