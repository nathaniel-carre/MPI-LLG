#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

struct Tas {
    int capa;
    int size;
    int* data;
};

typedef struct Tas tas;

void swap(int* tab, int i, int j){
    // Fonction utilitaire qui inverse deux éléments dans un tableau.
    int tmp = tab[i];
    tab[i] = tab[j];
    tab[j] = tmp;
}

void inserer(tas* t, int x){
    // On met l'élément inséré en dernière position, puis on l'échange
    // avec son parent tant qu'il est plus petit. On pense à incrémenter
    // la taille.
    assert (t->size < t->capa);
    int i = t->size;
    t->data[i] = x;
    while (i > 0 && t->data[i] < t->data[(i - 1) / 2]){
        swap(t->data, i, (i - 1) / 2);
        i = (i - 1) / 2;
    }
    t->size = t->size + 1;
}

int enfant_min(tas t, int i){
    // Fonction utilitaire qui renvoie l'indice de l'enfant minimal d'un nœud à
    // l'indice i, et l'indice i lui-même si cet enfant n'existe pas.
    int g = 2 * i + 1;
    int d = g + 1;
    if (i == t.size - 1) return i;
    if (i == t.size - 2 || t.data[g] < t.data[d]) return g;
    return d;
}

int extraire_min(tas* t){
    // On met le dernier élément à la racine, puis on l'échange avec son enfant
    // min tant qu'il lui est plus grand. Quand cet enfant n'existe plus, le test
    // échoue nécessairement (à cause de l'inégalité stricte).
    assert (t->size > 0);
    t->size = t->size - 1;
    int min = t->data[0];
    t->data[0] = t->data[t->size];
    int i = 0;
    int em = enfant_min(*t, i);
    while (t->data[i] > t->data[em]){
        swap(t->data, i, em);
        i = em;
        em = enfant_min(*t, i);
    }
    return min;
}

int main(void){
    int tab[8] = {2, 5, 6, 8, 1, 18, 2, 4};
    int data[8] = {0};
    tas t = {.capa = 8, .size = 0, .data = data};
    for (int i=0; i<8; i++) inserer(&t, tab[i]);
    for (int i=0; i<8; i++) printf("%d, ", extraire_min(&t));
    printf("\n");
    return EXIT_SUCCESS;
}