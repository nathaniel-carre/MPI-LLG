
/********************************************************************/
/* Concours Centrale-Supélec                                        */
/* Sujet 0 - MPI                                                    */
/* https://www.concours-centrale-supelec.fr                         */
/* Proposition de corrigé par N. Carré (MPI LLG)                    */
/* CC BY-NC-SA 3.0                                                  */
/********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

struct node_s {
  char* key;
  int val;
  struct node_s* left;
  struct node_s* right;
};
typedef struct node_s node;

int lexcmp(char* s1, char* s2) {
    int i = 0;
    while (s1[i] != '\0' && s2[i] != '\0'){
        // Tant qu'on n'est pas arrivé à la fin de l'un des deux mots, on compare les
        // lettres une à une.
        if (s1[i] < s2[i]){
            return -1;
        } else if (s1[i] > s2[i]) {
            return 1;
        }
        i++;
    }
    // On sort de la boucle si l'un des deux mots est préfixe de l'autre. On compare
    // alors les tailles pour conclure.
    if (s1[i] != '\0'){
        return 1;
    } else if (s2[i] != '\0') {
        return -1;
    } else {        
        return 0;
    }
}

void print_bst_aux(node* bst, int p) {
  if (bst == NULL) {
    return;
  }
  print_bst_aux(bst->right, p + 1);
  for (int i = 0; i < p; i = i + 1) {
    printf("\t");
  }
  printf("%s : %d\n", bst->key, bst->val);
  print_bst_aux(bst->left, p + 1);
}

void print_bst(node* bst) {
  print_bst_aux(bst, 0);
}

node* delete_min(node* bst, node** min) {
  assert(bst != NULL);
  if (bst->left == NULL) {
    *min = bst;
    return bst->right;
  } else {
    bst->left = delete_min(bst->left, min);
    return bst;
  }
}

node* delete(char* key, node* bst) {
  if (bst == NULL) {
    return NULL;
  } else {
    int c = lexcmp(key, bst->key);
    if (c == 0) {
      node* new;
      if (bst->left == NULL) {
        new = bst->right;
      } else if (bst->right == NULL) {
        new = bst->left;
      } else {
        node* right = delete_min(bst->right, &new);
        new->right = right;
        new->left = bst->left;
      }
      free(bst->key);
      free(bst);
      bst = new;
    } else if (c < 0) {
      bst->left = delete(key, bst->left);
    } else {
      bst->right = delete(key, bst->right);
    }
    return bst;
  }
}

int get(char* key, node* bst){
    // Renvoie le nombre d'occurrences dans un arbre binaire de recherche d'une 
    // clé donnée. Renvoie 0 si la clé n'apparait pas dans le dictionnaire.
    if (bst != NULL){
        int c = lexcmp(key, bst->key);
        if (c == 0){
            return bst->val;
        } else if (c < 0) {
            return get(key, bst->left);
        }
        return get(key, bst->right);
    }
    return 0;
}

node* insert(char* key, int val, node* bst){
    // Rajoute un nombre d'occurrences d'une clé dans un dictionnaire. Crée le nœud
    // correspondant s'il n'existe pas déjà. Renvoie l'arbre résultant.
    if (bst == NULL){
        bst = malloc(sizeof(node));
        int len = strlen(key);
        bst->key = malloc((len + 1) * sizeof(char));
        strcpy(bst->key, key);
        bst->val = val;
        bst->left = NULL;
        bst->right = NULL;
    } else {
        int c = lexcmp(key, bst->key);
        if (c == 0){
            bst->val += val;
        } else if (c < 0) {
            bst->left = insert(key, val, bst->left);
        } else {
            bst->right = insert(key, val, bst->right);
        }
    }
    return bst;
}

char* create_string(char* key){
    // Crée sur le tas une nouvelle chaîne égale à la clé donnée en argument.
    int len = strlen(key);
    char* new = malloc(len * sizeof(char));
    strcpy(new, key);
    return new;
}

void bst_destroy(node* bst){
    // Libère la mémoire utilisée par un dictionnaire.
    if (bst != NULL){
        bst_destroy(bst->left);
        bst_destroy(bst->right);
        free(bst->key);
        free(bst);
    }
}

node* build_bst(char* pathname){
    // Construit un arbre binaire de recherche de lexique à partir d'un fichier
    // texte dont le chemin est donné en argument.
    FILE* file = fopen(pathname, "r");
    node* bst = NULL;
    char line[101];
    int success = fscanf(file, "%s", line);
    while (success != EOF){
        // Tant qu'on n'est pas arrivé à la fin du fichier, on lit chaque ligne
        // et on insère le mot dans l'arbre.
        bst = insert(line, 1, bst);
        success = fscanf(file, "%s", line);     
    }    
    fclose(file);
    return bst;
}

void bst_to_lex(node* bst, FILE* file){
    // Écrit le parcours en profondeur infixe d'un arbre binaire de recherche
    // dans un fichier ouvert en mode écriture.
    if (bst != NULL){
        bst_to_lex(bst->left, file);
        fprintf(file, "%s %d\n", bst->key, bst->val);
        bst_to_lex(bst->right, file);
    }
}

node* text_to_lex(char* input, char* output){
    // Prend en argument des noms de fichiers d'entrée et de sortie et construit
    // le lexique associé à un fichier texte. La fonction renvoie l'arbre construit.
    node* bst = build_bst(input);
    FILE* file = fopen(output, "w");
    bst_to_lex(bst, file);
    fclose(file);
    return bst;
}

int size(node* bst){
    // Calcule la taille d'un arbre binaire.
    if (bst == NULL){
        return 0;
    }
    return 1 + size(bst->left) + size(bst->right);
}

void most_frequent(node* bst, node** best){
    // Stocke le pointeur du nœud le plus fréquent de bst dans le pointeur pointé par
    // best, si ce nœud apparaît plus de fois que best.
    if (bst != NULL){
        if (*best == NULL || bst->val > (*best)->val){
            *best = bst;
        }
        most_frequent(bst->left, best);
        most_frequent(bst->right, best);
    }
}

void number_exact_occurrences(node* bst, int occ, int* nb_words){
    // Compte le nombre de mots qui ont exactement occ occurrences et l'ajoute
    // à la valeur pointée par nb_words.
    if (bst != NULL){
        if (bst->val == occ){
            (*nb_words)++;
        }
        number_exact_occurrences(bst->left, occ, nb_words);
        number_exact_occurrences(bst->right, occ, nb_words);
    }
}

int height(node* bst){
    // Calcule la hauteur d'un arbre binaire.
    if (bst == NULL){
        return -1;
    }
    int hleft = height(bst->left);
    int hright = height(bst->right);
    return 1 + ((hleft > hright)?hleft:hright);
}

node* build_bst_big(char* pathname){
    // Construit un arbre binaire de recherche de bigrammes à partir d'un fichier
    // texte dont le chemin est donné en argument. Un bigramme sera simplement
    // une chaîne de caractères constituée des deux mots séparés par une espace.
    FILE* file = fopen(pathname, "r");
    node* bst = NULL;
    char line[101];
    char big[202];
    int success = fscanf(file, "%s", line);
    while (success != EOF){
        strcpy(big, line);
        int len = strlen(line);
        success = fscanf(file, "%s", line);
        big[len] = ' ';
        strcpy(&big[len + 1], line);
        bst = insert(big, 1, bst);
    }
    fclose(file);
    return bst;
}

void text_to_big(char* input, char* output){
    // Prend en argument des noms de fichiers d'entrée et de sortie et construit
    // le lexique de bigrammes associé à un fichier texte.
    node* bst = build_bst_big(input);
    FILE* file = fopen(output, "w");
    bst_to_lex(bst, file);
    fclose(file);
    bst_destroy(bst);
}

void tests_Q6(void){
    node* bst = NULL;    
    char foo[4] = "foo",
         bar[4] = "bar",
         baz[4] = "baz",
         qux[4] = "qux",
         quux[5] = "quux";
    printf("Recherche dans un arbre vide : %d\n", get(foo, bst));
    bst = insert(foo, 3, bst);
    bst = insert(bar, 4, bst);
    bst = insert(baz, 5, bst);
    bst = insert(qux, 1, bst);
    printf("Recherche dans un arbre ne contenant pas le nœud : %d\n", get(quux, bst));
    printf("Recherche dans un arbre contenant le nœud : %d\n", get(qux, bst));
    bst = insert(qux, 2, bst);
    printf("Insertion dans un arbre contenant le nœud : %d\n", get(qux, bst));
    print_bst(bst);
    bst_destroy(bst);
}

void tests_Q8(char* input, char* output){
    printf("Texte %s :\n", input);
    node* bst = text_to_lex(input, output);
    printf("\t - taille : %d\n", size(bst));
    node* best = NULL;
    most_frequent(bst, &best);
    printf("\t - mot le plus fréquent : %s\n", best->key);
    printf("\t - nombre d'occurrences de ce mot : %d\n", best->val);
    int nb_words = 0;
    number_exact_occurrences(bst, 10, &nb_words);
    printf("\t - nombre de mots apparaissant 10 fois : %d\n", nb_words);
    printf("\t - hauteur : %d\n", height(bst));
    bst_destroy(bst);
}

int main(void) {
    build_bst("les_trois_mousquetaires.txt");
    //tests_Q6();

    //tests_Q8("les_trois_mousquetaires.txt", "les_trois_mousquetaires.lex");
    //tests_Q8("la_comedie_humaine.txt", "la_comedie_humaine.lex");

    //text_to_big("les_trois_mousquetaires.txt", "les_trois_mousquetaires.big"); 
       
    //ext_to_big("la_comedie_humaine.txt", "la_comedie_humaine.big");
    return EXIT_SUCCESS;
}