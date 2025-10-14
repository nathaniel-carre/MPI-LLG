#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "dicts.h"

struct AFD {
    int Q;
    int Sigma;
    int q0;
    bool* finaux;
    int** delta;
};

typedef struct AFD afd;

// On libère le tableau des états finaux, chaque tableau de
// transitions, et le tableau contenant les tableaux de transitions.
void liberer_afd(afd A){
    free(A.finaux);
    for (int q=0; q<A.Q; q++){
        free(A.delta[q]);
    }
    free(A.delta);
}

// On initialise tout correctement, avec des -1 dans les tableaux.
afd init_afd(int Q, int Sigma, int q0){
    afd A;
    A.Q = Q;
    A.Sigma = Sigma;
    A.q0 = q0;
    bool* finaux = malloc(Q * sizeof(*finaux));
    int** delta = malloc(Q * sizeof(*delta));
    for (int q=0; q<Q; q++){
        finaux[q] = false;
        int* tab = malloc(Sigma * sizeof(*tab));
        for (int a=0; a<Sigma; a++){
            tab[a] = -1;
        }
        delta[q] = tab;
    }
    A.finaux = finaux;
    A.delta = delta;
    return A;
}

// Il faut juste faire le décalage d'indice.
void ajout_transition_afd(afd A, int q, char a, int p){
    A.delta[q][a - 97] = p;
}

// On lit chaque lettre et on calcule l'image. On s'arrête
// lorsqu'on a terminé le mot (caractère de fin de chaîne)
// ou qu'on a rencontré un blocage (état -1).
int delta_etoile_afd(afd A, int q, char* u){
    int etat = q;
    int i = 0;
    while (u[i] != '\0' && etat != -1){
        etat = A.delta[etat][u[i] - 97];
        i++;
    }
    return etat;
}

// On vérifie si δ*(q0, u) est défini et s'il ∈ F ou non.
bool reconnu_afd(afd A, char* u){
    int q = delta_etoile_afd(A, A.q0, u);
    return q != -1 && A.finaux[q];
}

struct AFND {
    int Q;
    int Sigma;
    bool* initiaux;
    bool* finaux;
    liste*** Delta;
};

typedef struct AFND afnd;

void liberer_afnd(afnd B){
    //Libère la mémoire d'un AFND.
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
    //Initialise un AFND.
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

// Comme précédemment, sauf qu'on modifie la liste chaînée.
void ajout_transition_afnd(afnd B, int q, char a, int p){
    B.Delta[q][a-97] = cons(p, B.Delta[q][a-97]);
}

// On crée un tableau Y de false. Pour chaque état q de B tel que
// X[q] vaut true, on parcourt ses voisins par la transition en
// lisant a, et on met à true leur valeur dans le tableau Y, qu'on
// renvoie.
// Complexité : dans le pire cas, si X = Q, on parcourt les listes
// d'adjacence de chaque état, qui sont de taille au plus |Q|.
// Au total, O(|Q|²).
bool* Delta_etats(afnd B, bool* X, char a){
    bool* Y = malloc(B.Q * sizeof(*Y));
    for (int q=0; q<B.Q; q++){
        Y[q] = false;
    }
    for (int q=0; q<B.Q; q++){
        if (X[q]){
            liste* lst = B.Delta[q][a-97];
            while (lst != NULL){
                Y[lst->val] = true;
                lst = lst->suivant;
            }
        }
    }
    return Y;
}

// On commence par créer une copie de X (car on va faire des
// libérations de mémoire au fur et à mesure, et qu'on ne souhaite
// pas libérer le tableau donné en argument). Ensuite, on calcule
// chaque image par une lettre en fonction de la précédente. On 
// pense à créer un tableau temporaire pour bien libérer la mémoire.
bool* Delta_etoile_afnd(afnd B, bool* X, char* u){
    bool* Y = malloc(B.Q * sizeof(*Y));
    for (int q=0; q<B.Q; q++){
        Y[q] = X[q];
    }
    int i = 0;
    while (u[i] != '\0'){
        bool* tmp = Y;
        Y = Delta_etats(B, Y, u[i]);
        free(tmp);
        i++;
    }
    return Y;
}

// Après avoir calculé Δ*(I, u), on parcourt chaque état de B et
// on vérifie s'il en existe un dans Δ*(I, u) et dans F.
bool reconnu_afnd(afnd B, char* u){
    bool* X = Delta_etoile_afnd(B, B.initiaux, u);
    for (int q=0; q<B.Q; q++){
        if (X[q] && B.finaux[q]){
            free(X);
            return true;
        }
    }
    free(X);
    return false;
}

// On encode un tableau de booléen X comme l'entier x dont la
// représentation binaire sur Q bits correspond à X (en associant
// les false aux 0 et les true aux 1), le bit de poids fort étant
// à gauche (donc à l'indice 0). On utilise ici l'évaluation de
// Horner, qui permet d'éviter de calculer des puissances de 2.
// Complexité : O(|Q|).
int etats_vers_entier(bool* X, int Q){
    int x = 0;
    for (int i=0; i<Q; i++){
        x *= 2;
        if (X[i]) x += 1;
    }
    return x;
}

// On crée un tableau, et on remplit chaque case en fonction de
// la parité de x, qu'on divise par 2 à chaque itération. On fait
// attention à remplir le tableau par la fin pour que ce soit bien
// la réciproque de la fonction précédente.
// Complexité : O(|Q|).
bool* entier_vers_etats(int x, int Q){
    bool* X = malloc(Q * sizeof(*X));
    for (int i=0; i<Q; i++){
        X[Q - i - 1] = x%2 == 1;
        x /= 2;
    }
    return X;
}

// L'idée est de calculer toutes les parties comme des entiers. On 
// utilise de l'arithmétique bit à bit : x & F correspond au ET bit 
// à bit. Cette valeur est non nulle si les deux entiers possède un 
// bit en commun qui vaut 1 (ce qui correspond ici à la définition 
// d'un état final et évite de refaire le parcours de tableau). On 
// calcule alors les images par la fonction Δ et on convertit en 
// entier pour créer la transition dans l'automate déterminisé.
afd determiniser(afnd B){
    int taille = 1 << B.Q; // Calcule 2 puissance |Q|
    int q0 = etats_vers_entier(B.initiaux, B.Q);
    int F = etats_vers_entier(B.finaux, B.Q);
    afd A = init_afd(taille, B.Sigma, q0);
    for (int x=1; x<taille; x++){
        A.finaux[x] = (x & F) > 0;
        bool* X = entier_vers_etats(x, B.Q);
        for (int a=0; a<B.Sigma; a++){
            bool* Y = Delta_etats(B, X, a+97);
            int y = etats_vers_entier(Y, B.Q);
            A.delta[x][a] = y;
            free(Y);
        }
        free(X);
    }
    return A;
}

// On utilise ici un parcours de graphe en profondeur, écrit de
// manière récursive pour éviter d'avoir à créer une structure de
// file ou d'utiliser les listes comme structure de pile. La fonction
// DFS prend en argument l'automate, un pointeur vers un dictionnaire
// (nécessaire car on va le modifier), un entier correspondant à l'état
// courant et un pointeur vers un entier correspondant au prochain
// numéro d'un état dans l'AFD qu'on souhaite construire, et fait le
// parcours en créant les associations dans le dictionnaire.
// Complexité : pour chaque sommet accessible de l'AFD, on calcule
// l'image pour chaque lettre de l'alphabet. Ainsi, si la partie 
// accessible contient n états, le total est en O(|Σ|n|Q|²).
void DFS(afnd B, dict* D, int x, int* v){
    if (member(*D, x)) return;
    add(D, x, *v);
    ++*v;
    bool* X = entier_vers_etats(x, B.Q);
    for (int a=0; a<B.Sigma; a++){
        bool* Y = Delta_etats(B, X, a+97);
        int y = etats_vers_entier(Y, B.Q);
        free(Y);
        DFS(B, D, y, v);
    }
    free(X);
}

// Dès lors, la fonction accessibles consiste juste à faire un
// parcours depuis l'ensemble des états initiaux et à renvoyer
// le dictionnaire ainsi obtenu.
// Complexité : comme DFS, soit O(|Σ|n|Q|²).
dict accessibles(afnd B){
    int x0 = etats_vers_entier(B.initiaux, B.Q);
    dict D = create();
    int v = 0;
    DFS(B, &D, x0, &v);
    return D;
}

// On détermine l'ensemble des parties accessibles et on crée un 
// automate déterministe de cette taille. Ensuite, on détermine 
// la liste des états, et on crée les transitions en calculant 
// leurs images (qui sont nécessairement des états accessibles). 
// On utilise le dictionnaire D pour la renumérotation des états 
// comme des valeurs consécutives partant de 0.
// Complexité : comme accessibles, soit O(|Σ|n|Q|²).
afd determiniser2(afnd B){
    dict D = accessibles(B);
    afd A = init_afd(size(D), B.Sigma, 0);
    liste* lst = key_list(D);
    liste* tmp = lst;
    int F = etats_vers_entier(B.finaux, B.Q);
    while (lst != NULL){
        int x = lst->val;
        lst = lst->suivant;
        A.finaux[get(D, x)] = (x & F) > 0;
        bool* X = entier_vers_etats(x, B.Q);
        for (int a=0; a<B.Sigma; a++){
            bool* Y = Delta_etats(B, X, a+97);
            int y = etats_vers_entier(Y, B.Q);
            A.delta[get(D, x)][a] = get(D, y);
            free(Y);
        }
        free(X);
    }
    liberer_liste(tmp);
    dict_free(D);
    return A;
}

int main(void){
    afd A1 = init_afd(4, 2, 0);
    A1.finaux[3] = true;
    ajout_transition_afd(A1, 0, 'b', 0); ajout_transition_afd(A1, 0, 'a', 1);
    ajout_transition_afd(A1, 1, 'a', 2); ajout_transition_afd(A1, 1, 'b', 0);
    ajout_transition_afd(A1, 2, 'a', 2); ajout_transition_afd(A1, 2, 'b', 3);
    ajout_transition_afd(A1, 3, 'a', 1); ajout_transition_afd(A1, 3, 'b', 0);
    afd A2 = init_afd(4, 2, 0);
    A2.finaux[3] = true;
    ajout_transition_afd(A2, 0, 'a', 1); 
    ajout_transition_afd(A2, 1, 'a', 2); ajout_transition_afd(A2, 1, 'b', 2);
    ajout_transition_afd(A2, 2, 'a', 3); ajout_transition_afd(A2, 2, 'b', 3);
    ajout_transition_afd(A2, 3, 'a', 1); ajout_transition_afd(A2, 3, 'b', 1);
    char u[] = "abbabbabaab";
    char v[] = "baababbbbba";
    char w[] = "aaabababb";
    printf("%s", u); printf(" est-il reconnu par A1 ? %d\n", reconnu_afd(A1, u));
    printf("%s", v); printf(" est-il reconnu par A1 ? %d\n", reconnu_afd(A1, v));
    printf("%s", w); printf(" est-il reconnu par A1 ? %d\n", reconnu_afd(A1, w));
    printf("%s", u); printf(" est-il reconnu par A2 ? %d\n", reconnu_afd(A2, u));
    printf("%s", v); printf(" est-il reconnu par A2 ? %d\n", reconnu_afd(A2, v));
    printf("%s", w); printf(" est-il reconnu par A2 ? %d\n", reconnu_afd(A2, w));
    liberer_afd(A1);
    liberer_afd(A2);
    afnd B1 = init_afnd(6, 2);
    B1.initiaux[0] = true; B1.initiaux[3] = true;
    B1.finaux[2] = true; B1.finaux[5] = true;
    ajout_transition_afnd(B1, 0, 'a', 0); ajout_transition_afnd(B1, 0, 'b', 0);
    ajout_transition_afnd(B1, 0, 'a', 1); ajout_transition_afnd(B1, 1, 'b', 2);
    ajout_transition_afnd(B1, 3, 'b', 4); ajout_transition_afnd(B1, 4, 'a', 5);
    ajout_transition_afnd(B1, 5, 'a', 5); ajout_transition_afnd(B1, 5, 'b', 5);
    afnd B2 = init_afnd(4, 2);
    B2.initiaux[0] = true; B2.finaux[3] = true;
    ajout_transition_afnd(B2, 0, 'a', 0); ajout_transition_afnd(B2, 0, 'b', 0);
    ajout_transition_afnd(B2, 0, 'a', 1); 
    ajout_transition_afnd(B2, 1, 'a', 2); ajout_transition_afnd(B2, 1, 'b', 2); 
    ajout_transition_afnd(B2, 2, 'a', 3); ajout_transition_afnd(B2, 2, 'b', 3); 
    printf("%s", u); printf(" est-il reconnu par B1 ? %d\n", reconnu_afnd(B1, u));
    printf("%s", v); printf(" est-il reconnu par B1 ? %d\n", reconnu_afnd(B1, v));
    printf("%s", w); printf(" est-il reconnu par B1 ? %d\n", reconnu_afnd(B1, w));
    printf("%s", u); printf(" est-il reconnu par B2 ? %d\n", reconnu_afnd(B2, u));
    printf("%s", v); printf(" est-il reconnu par B2 ? %d\n", reconnu_afnd(B2, v));
    printf("%s", w); printf(" est-il reconnu par B2 ? %d\n", reconnu_afnd(B2, w));
    afd A3 = determiniser(B1);
    printf("%s", u); printf(" est-il reconnu par A3 ? %d\n", reconnu_afd(A3, u));
    printf("%s", v); printf(" est-il reconnu par A3 ? %d\n", reconnu_afd(A3, v));
    printf("%s", w); printf(" est-il reconnu par A3 ? %d\n", reconnu_afd(A3, w));
    printf("taille A3 : %d\n", A3.Q);
    liberer_afd(A3);
    afd A4 = determiniser2(B2);
    printf("%s", u); printf(" est-il reconnu par A4 ? %d\n", reconnu_afd(A4, u));
    printf("%s", v); printf(" est-il reconnu par A4 ? %d\n", reconnu_afd(A4, v));
    printf("%s", w); printf(" est-il reconnu par A4 ? %d\n", reconnu_afd(A4, w));
    printf("taille A4 : %d\n", A4.Q);
    liberer_afd(A4);
    liberer_afnd(B1);    
    liberer_afnd(B2);
    return 0;
}