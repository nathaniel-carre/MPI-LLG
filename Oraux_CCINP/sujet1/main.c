#include<stdbool.h>
#include<stdint.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

/* 
La directive #define est hors programme. Nous l'utilisons ici pour 
définir des valeurs pour des variables qui serviront à déclarer des tableaux de taile variable 
La taille est fixée à la compilation
*/
#define N 37  // taille maximale d'une chaine de caractere dans le programme
              // cela limite le nombre de couple de parentheses a 18

// structure pour la definition d'une liste chainee
struct liste_t {
    char chaine[N];
    struct liste_t * suivant;
}; 


// Fonction pour calculer le nombre de catalan n
// ENTREE : n - le nombre pour lequel on veut calculer
// SORTIE : le nombre de catalan cherche
uint64_t catalan(int n) {
    return n;
}

// Fonction verifier que la chaine passee en parametre contient un mot bien parenthese
// ENTREE : mot - le mot a tester
// SORTIE : true si le mot est bien parenthese, false sinon
bool verification(char * mot) {

    return false;
}

// Fonction pour afficher les chaines de parentheses bien formees 
// La valeur N est fixée et est valable pour tout n <= 18
// Vous pouvez utiliser char chaine[101] si vous préférez
// ENTREE : s - la chaine de caracteres courante
// ENTREE : o - le nombre de parentheses ouvertes
// ENTREE : f - le nombre de parentheses fermees
// ENTREE : n - le nombre de couples de parentheses
void dyck(char s[N], int o, int f, int n) {
}

// point d'entree du programme
int main() {

    printf("--- Premiers nombres de Catalan:\n");
    for(int i = 0; i< 30; ++i)
        printf("C(%d)=%lu\n", i, catalan(i));
    
    printf("Quelques verifications de mots : \n");
    printf("'((()())())' est bien parenthesee : %d\n", verification("((()())())"));
    printf("')(()())()(' est bien parenthesee : %d\n", verification(")(()())()("));
    printf("'((())())))' est bien parenthesee : %d\n", verification("((())())))"));


    uint64_t  nbmots = 0;
    char chaine[N] = ""; 
    int  n = 5;

    printf("--- Affichage des parentheses bien formées à %d couples \n", n);
    dyck(chaine, 0, 0, n);
    printf("Nombre de mots :%lu\n", nbmots);

    nbmots = 0;
    strcpy(chaine,""); 
    n = 11;
    printf("--- Affichage des parentheses bien formées à %d couples \n", n);
    dyck(chaine, 0, 0, n);
    printf("Nombre de mots :%lu\n", nbmots);

    return 0;
}