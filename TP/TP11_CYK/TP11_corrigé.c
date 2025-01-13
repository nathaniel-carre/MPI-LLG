#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

struct Regle {
    char X;
    char* alpha;
};

typedef struct Regle regle;

struct Grammaire {
    int taille_V;
    int nb_prod;
    regle* Prod;
};

typedef struct Grammaire grammaire;

bool terminal(char c){
    return 97 <= (int) c && (int) c <= 122;
}

bool variable(char c){
    return 65 <= (int) c && (int) c <= 90;
}

grammaire creer_grammaire(int taille_V, char* tab_X, char** tab_alpha){
    grammaire G;
    G.taille_V = taille_V;
    int n = strlen(tab_X);
    G.nb_prod = n;
    G.Prod = malloc(n * sizeof(regle));
    for (int i=0; i<n; i++){
        regle r;
        r.X = tab_X[i];
        r.alpha = tab_alpha[i];
        G.Prod[i] = r;
    }
    return G;
}

void liberer_grammaire(grammaire G){
    free(G.Prod);
}

bool regle_chomsky(regle r){
    int n = strlen(r.alpha);
    return ((n == 0 && r.X == 'A') ||
            (n == 1 && terminal(r.alpha[0])) || 
            (n == 2 && variable(r.alpha[0]) && variable(r.alpha[1])
                    && r.alpha[0] != 'A' && r.alpha[1] != 'A'));
}

bool est_FNC(grammaire G){
    for (int p=0; p<G.nb_prod; p++){
        if (!regle_chomsky(G.Prod[p])) return false;
    }
    return true;
}

int indice(char X){
    return (int) X - (int) 'A';
}

// Fonction pour initialiser une matrice d'ensembles, chaque
// ensemble étant un tableau de booléens.
bool*** init_tab_X(int n, int m){
    bool*** tab_X = malloc(n * sizeof(*tab_X));
    for (int i=0; i<n; i++){
        tab_X[i] = malloc(n * sizeof(bool*));
        for (int j=0; j<n; j++){
            tab_X[i][j] = malloc(m * sizeof(bool));
            for (int v=0; v<m; v++){
                tab_X[i][j][v] = false;
            }
        }
    }
    return tab_X;
}

// Fonction pour libérer la mémoire d'une matrice d'ensembles.
void liberer_tab_X(bool*** tab_X, int n){
    for (int i=0; i<n; i++){
        for (int j=0; j<n; j++){
            free(tab_X[i][j]);
        }
        free(tab_X[i]);
    }
    free(tab_X);
}

bool CYK(grammaire G, char* u){
    int n = strlen(u);
    if (n == 0){
        for (int p=0; p<G.nb_prod; p++){
            if (G.Prod[p].X == 'A' && strlen(G.Prod[p].alpha) == 0){
                return true;
            }
        }
        return false;
    }
    bool*** tab_X = init_tab_X(n, G.taille_V);
    for (int i=0; i<n; i++){
        for (int p=0; p<G.nb_prod; p++){
            int t = strlen(G.Prod[p].alpha);            
            if (t == 1 && G.Prod[p].alpha[0] == u[i]){
                tab_X[i][i][indice(G.Prod[p].X)] = true;
            }
        }   
    }
    for (int d=1; d<n; d++){
        for (int i=0; i<n-d; i++){
            int j = i + d;
            for (int k = i; k<j; k++){
                for (int p=0; p<G.nb_prod; p++){
                    int t = strlen(G.Prod[p].alpha);
                    if (t == 2){
                        char Y = G.Prod[p].alpha[0];
                        char Z = G.Prod[p].alpha[1];
                        if (tab_X[i][k][indice(Y)] && tab_X[k+1][j][indice(Z)]){
                            tab_X[i][j][indice(G.Prod[p].X)] = true;
                        }
                    }
                }
            }
        }
    }
    bool b = tab_X[0][n-1][0];
    liberer_tab_X(tab_X, n);
    return b;
}

/*
La dernière partie est laissée au travail en autonomie.
*/

int main(void){
    char* tab_alpha0[3] = {"BbB", "Ba", ""};
    grammaire G0 = creer_grammaire(2, "ABB", tab_alpha0);
    printf("%d\n", est_FNC(G0));
    
    char* tab_alpha1[9] = {"BC", "CB", "DB", "b", "BE", "a", "b", "BC", "a"};
    grammaire G1 = creer_grammaire(5, "AAAABBCDE", tab_alpha1);
    printf("%d\n", est_FNC(G1));

    char* tab_alpha2[14] = {"FC", "FD", "FE", "FG", "", "FC", "FD", 
                            "FE", "FG", "BD", "GB", "BG", "a", "b"};
    grammaire G2 = creer_grammaire(7, "AAAAABBBBCDEFG", tab_alpha2);
    printf("%d\n", est_FNC(G2));

    char* tab_alpha3[8] = {"BC", "DE", "aBC", "", "b", "cAc", "dDE", ""};
    grammaire G3 = creer_grammaire(5, "ABCCDDEE", tab_alpha3);
    printf("%d\n", est_FNC(G3));

    printf("%d\n", CYK(G1, "aaaba"));
    printf("%d\n", CYK(G1, "aabab"));
    printf("%d\n", CYK(G1, "aaaaa"));

    printf("%d\n", CYK(G2, "abaabb"));
    printf("%d\n", CYK(G2, "bbaaba"));
    printf("%d\n", CYK(G2, "ababab"));

    printf("%d\n", CYK(G1, ""));
    printf("%d\n", CYK(G2, ""));

    liberer_grammaire(G0);
    liberer_grammaire(G1);
    liberer_grammaire(G2);
    liberer_grammaire(G3);
    return 0;
}