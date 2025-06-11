
/******************************************************/
/* Concours commun INP                                */
/* https://www.concours-commun-inp.fr                 */
/* CC BY-NC-SA, Novembre 2023                         */
/* https://creativecommons.org/licenses/by-nc-sa/4.0/ */
/******************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/*
La directive #define est utilisée pour définir des valeurs pour
des constantes qui serviront à déclarer des tableaux de taille fixe.
*/
#define m 5
#define n 3

/* Macro de calcul du maximum entre i et j */
int max(int i,int j){
    if (i<j)
        return j;
    else
        return i;
}


int recolte(int champ[m][n], int i, int j){

    /* Code question 2 */
}


void deplacements(int fleurs[m][n], int i, int j){

    /*Code question 6 */
}


int recolte_iterative(int champ[m][n], int i, int j,int fleurs[m][n]){

    /*Code question 5 */
}




int main(){
     int champ[m][n],fleurs[m][n];
     int i,j;

    /* Exemple du champ de fleurs : le nombre de fleurs par case est un entier
    aléatoire entre 0 et 10. On utilise la fonction int rand() de stdlib. Le générateur
    de nombre pseudo-aléatoires est tout d'abord initialisé.*/
     srand(time(NULL));
     for (i=0;i<m;i++) for(j=0;j<n;j++)
          champ[i][j] = rand() % 11;

     printf("\n Nombre de fleurs maximum cueillies : %d\n",recolte_iterative(champ,3,3,fleurs));

     return 0;
}

