
/********************************************************************/
/* Concours Centrale-Supélec                                        */
/* Sujet 0 - MPI                                                    */
/* https://www.concours-centrale-supelec.fr                         */
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
  // À implémenter
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

int main(void) {
  return EXIT_SUCCESS;
}
