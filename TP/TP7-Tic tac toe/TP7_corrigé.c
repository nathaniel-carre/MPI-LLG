#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "dicts.h"

struct TTT {
    int n;
    int k;
    int* grille;
};

typedef struct TTT ttt;

// Exercice 1

// Q1. Si k > n, il n'est pas possible d'aligner k cases. Toutes les parties
// sont donc des matchs nuls.

// Q2. Le premier joueur joue dans le coin (0, 0). Dès lors, quel que soit le
// coup du deuxième joueur, le premier joueur pourra jouer soit en (1, 0), soit
// en (0, 1) et gagner la partie.

// Q3. En jouant en (1, 1), le joueur 1 ouvre une possibilité de gagner dans 3 
// directions : diagonal, horizontal et vertical. Lors de son deuxième coup, le 
// joueur 1 joue en (2, 2), (1, 2) ou (2, 1), correspondant à une direction où 
// le joueur 2 n'a pas joué. Dès lors, le joueur 1 a deux coups gagnants, de part 
// et d'autre de l'alignement de deux pions, et le joueur 2 ne peut en bloquer 
// qu'un des deux, donc le joueur 1 gagne lors de son troisième coup.

// On alloue la grille en un tableau unidimensionnel.
ttt init_jeu(int k, int n){
    int* grille = malloc((n * n) * sizeof(*grille));
    for (int i=0; i<n*n; i++){
        grille[i] = 0;
    }
    ttt jeu = {.n = n, .k = k, .grille = grille};
    return jeu;
}

// On crée un tableau de taille 3, on parcourt ensuite la grille
// et on remplit le tableau pour chaque valeur rencontrée dans la grille.
int* repartition(ttt jeu){
    int n = jeu.n;
    int* repart = malloc(3 * sizeof(int));
    for (int i=0; i<3; i++) repart[i] = 0;
    for (int i=0; i<n*n; i++){
        int val = jeu.grille[i];
        assert (0 <= val && val <= 2);
        repart[val] += 1;
    }
    return repart;
}

// On détermine la répartition des cases. S'il ne reste aucune case
// libre, on renvoie zéro. Sinon, si les deux joueurs ont le même nombre
// de cases occupées, c'est au premier joueur de jouer, sinon au deuxième.
int joueur_courant(ttt jeu){
    int* repart = repartition(jeu);
    if (repart[0] == 0){
        free(repart);
        return 0;
    }
    bool b = repart[1] == repart[2];
    free(repart);
    return b?1:2;
}

// On vérifie que le coup est possible et on modifie la case avec le
// joueur courant.
void jouer_coup(ttt jeu, int lgn, int cln){
    int i = lgn * jeu.n + cln;
    if (jeu.grille[i] != 0){
        printf("Coup impossible\n");
    } else {
        jeu.grille[i] = joueur_courant(jeu);
    }
}

// On calcule, en fonction du différentiel d'indices, le différentiel de
// ligne (dl) et de colonne (dc) à ajouter à chaque itération pour passer
// à la prochaine case de l'alignement. À chaque itération, on vérifie qu'on
// ne sort pas de la grille, puis que la case contient bien le symbole du
// joueur. On s'arrête si on a bien trouvé un alignement de k cases.
bool alignement(ttt jeu, int i, int di, int joueur){
    int k = jeu.k, n = jeu.n;
    int cln = i % n, lgn = i / n;
    int dc = ((di + 1) % n) - 1, dl = (di + 1) / n;
    for (int j=0; j<k; j++){
        if (cln < 0 || cln >= n || lgn < 0 || lgn >= n){
            return false;
        }
        i = lgn * n + cln;
        if (jeu.grille[i] != joueur){
            return false;
        }
        cln += dc; lgn += dl;
    }
    return true;
}

// Pour chaque case, on vérifie s'il existe un alignement dans chacune des
// 4 directions possibles. Notons qu'on aurait pu éviter la vérification à 
// partir de certaines cases, mais cela aurait complexifié l'écriture de la
// fonction.
bool gagnant(ttt jeu, int joueur){
    int n = jeu.n;
    int tabdi[4] = {1, n - 1, n, n + 1};
    for (int i=0; i<n*n; i++){
        for (int j=0; j<4; j++){
            if (alignement(jeu, i, tabdi[j], joueur)){
                return true;
            }
        }
    }
    return false;
}

// On utilise une représentation où le chiffre de poids fort est celui à
// l'indice 0.
int encodage(ttt jeu){
    int cle = 0, n = jeu.n;
    for (int i=0; i<n*n; i++){
        cle = 3 * cle + jeu.grille[i];
    }
    return cle;
}

// On commence par vérifier qu'on n'a pas déjà calculé l'attracteur de
// la position courante. Si ce n'est pas le cas, on traite les cas de base,
// à savoir ceux où l'un des joueurs a gagné, ou lorsque la grille est
// remplie sans gagnant. Si ce n'est pas le cas, on calcule, pour chaque
// coup possible, l'attracteur du voisin. On distingue alors selon le
// joueur courant et les voisins. On pense à annuler les coups joués.
int attracteur(ttt jeu, dict* d){
    int cle = encodage(jeu);
    int n = jeu.n, joueur = joueur_courant(jeu);
    if (!member(*d, cle)){
        if (gagnant(jeu, 1)) {
            add(d, cle, 1);
        } else if (gagnant(jeu, 2)) {
            add(d, cle, 2);
        } else if (joueur == 0) {
            add(d, cle, 0);
        } else {
            int tab[3] = {0, 0, 0};
            for (int i=0; i<n*n; i++){
                if (jeu.grille[i] == 0){
                    jeu.grille[i] = joueur;
                    int att = attracteur(jeu, d);
                    jeu.grille[i] = 0;
                    tab[att] += 1;
                    if (att == joueur){
                        break;
                    }                                 
                }
            }
            if (tab[joueur] != 0) {
                add(d, cle, joueur);
            } else if (tab[0] != 0) {
                add(d, cle, 0);
            } else {
                add(d, cle, 3 - joueur);
            }
        }      
    }
    return get(*d, cle);
}

// On commence par calculer l'attracteur de la position courante (la valeur
// est mémoïsée dans le dictionnaire). Ensuite, on cherche un coup voisin qui
// a la même valeur d'attracteur pour jouer ce coup. On est censé renvoyer une
// valeur dans la boucle (sinon il y a un problème quelque part).
int strategie_optimale(ttt jeu, dict* d){
    int att = attracteur(jeu, d);
    printf("attr : %d\n", att);
    int n = jeu.n, joueur = joueur_courant(jeu);
    for (int i=0; i<n*n; i++){
        if (jeu.grille[i] == 0){
            jeu.grille[i] = joueur;
            int att2 = attracteur(jeu, d);
            jeu.grille[i] = 0;
            if (att == att2){
                return i;
            }
        }
    }
    assert(false);
}

// Affichage d'une ligne séparatrice.
void afficher_ligne_sep(int n){
    printf("\n ");
    for (int i=0; i<n; i++){
        printf("+-");
    }
    printf("+\n");
}

// On affiche le jeu ligne par ligne, avec les lignes séparatrices.
void afficher(ttt jeu){
    int n = jeu.n;
    char tab[3] = {' ', 'X', 'O'};
    printf(" ");
    for (int cln=0; cln<n; cln++){
        // affichage des numéros de colonnes
        printf(" %d", cln);
    }
    afficher_ligne_sep(n);
    for (int lgn=0; lgn<n; lgn++){
        printf("%d|", lgn);
        for (int cln=0; cln<n; cln++){
            int i = n * lgn + cln;
            printf("%c|", tab[jeu.grille[i]]);
        }
        afficher_ligne_sep(n);
    }
    printf("\n");
}

// On commence par créer le jeu et à demander si le joueur humain veut
// commencer. On garde en mémoire le numéro du joueur ordinateur.
// Ensuite, la boucle while permet de faire alterner les joueurs, en
// faisant jouer automatiquement l'ordinateur en utilisant la stratégie
// optimale, ou en demandant les coups à jouer pour le joueur humain.
// Une fois la partie terminée, on détermine si c'est un match nul ou
// si un joueur a gagné. On pense à libérer la mémoire avant la fin.
void jouer_partie(int k, int n){
    ttt jeu = init_jeu(k, n);
    char c;
    int IA, cln, lgn;
    while (true){
        printf("Voulez-vous commencer ? (o/n) ");
        scanf("%c", &c);
        if (c == 'o'){
            IA = 2;
            break;
        } else if (c == 'n'){
            IA = 1;
            break;
        }
    }
    dict d = create();
    int joueur = 1;
    while (joueur != 0 && !gagnant(jeu, 1) && !gagnant(jeu, 2)){
        afficher(jeu);
        if (joueur == IA){
            int i = strategie_optimale(jeu, &d);
            cln = i % n;
            lgn = i / n;
            printf("L'IA joue ligne %d, colonne %d\n", lgn, cln);
            jouer_coup(jeu, lgn, cln);
        } else {        
            printf("C'est a vous de jouer\n");
            printf("Saisir la ligne : ");
            scanf("%d", &lgn);
            printf("Saisir la colonne : ");
            scanf("%d", &cln);
            if (cln < 0 || cln >= jeu.n || lgn < 0 || lgn >= jeu.n){
                printf("Ces coordonnees ne sont pas possibles.\n");
            } else {
                jouer_coup(jeu, lgn, cln);
            }
        }
        joueur = joueur_courant(jeu);
    }
    afficher(jeu);
    if (gagnant(jeu, IA)){
        printf("L'IA a gagne !\n");
    } else if (gagnant(jeu, 3 - IA)){
        printf("Vous avez gagne !\n");
    } else {
        printf("C'est un match nul !\n");
    }
    free(jeu.grille);
    dict_free(d);
}

int main(void){
    jouer_partie(3, 4);
    return 0;
}