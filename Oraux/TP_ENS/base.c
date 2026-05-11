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



int main(int argc, char** argv) {

  /* allocation d'un tableau de max_n entier pour stocker u_n */
  Un = malloc(max_n * sizeof(int));
  if (!Un) { printf("Erreur :  allocation Un\n"); exit(1); }

  return 0;
}
