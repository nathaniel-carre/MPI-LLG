#include <math.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>

#include "dicts.h"

const double phi = 1.61803398875;

void list_free(list* lst){
    // Libère l'espace mémoire occupé par une liste de doublons.
    if (lst != NULL){
        list_free(lst->next);
        free(lst);
    }
}

list* constr(int k, int v, list* lst){
    // Crée une nouvelle liste de doublons étant donnée une tête et une queue.
    list* ret = malloc(sizeof(list));
    ret->key = k;
    ret->val = v;
    ret->next = lst;
    return ret;
}

void dict_free(dict D){
    // Libère l'espace mémoire occupé par un dictionnaire.
    for (int i=0; i<D.capacity; i++){
        list_free(D.data[i]);
    }
    free(D.data);
    
}

dict create(void){
    // Crée un dictionnaire vide.
    list** data = malloc(sizeof(*data));
    data[0] = NULL;
    dict D = {.size = 0, .capacity = 1, .data = data};
    return D;
}

int size(dict D){
    // Renvoie le nombre d'éléments compris dans un dictionnaire.
    return D.size;
}

int hash(dict D, int k){
    // Renvoie le haché d'un entier.
    return (int)floor(D.capacity * (k % 1000) * phi) % D.capacity;
}

bool member_list(list* lst, int k){
    // Teste l'appartenance d'un entier à une liste.
    list* tmp = lst;
    while (tmp != NULL){
        if (tmp->key == k) return true;
        tmp = tmp->next;
    }
    return false;
}

bool member(dict D, int k){
    // Teste l'appartenance d'une clé à un dictionnaire.
    return member_list(D.data[hash(D, k)], k);
}

int get(dict D, int k){
    // Renvoie la valeur associée à une clé dans un dictionnaire.
    assert(member(D, k));
    list* tmp = D.data[hash(D, k)];
    while (tmp->key != k){
        tmp = tmp->next;
    }
    return tmp->val;
}

void resize(dict* D, int capa){
    // Redimensionne la table de hachage d'un dictionnaire.
    if (capa >= D->size){
        list** data = malloc(capa * sizeof(*data));
        for (int i=0; i<capa; i++){
            data[i] = NULL;
        }
        list** old_data = D->data;
        int old_capa = D->capacity;
        D->data = data;
        D->capacity = capa;
        D->size = 0;
        for (int i=0; i<old_capa; i++){
            list* tmp = old_data[i];
            while (tmp != NULL){
                add(D, tmp->key, tmp->val);
                tmp = tmp->next;
            }
            list_free(old_data[i]);
        }
        free(old_data);
    }
}

void add(dict* D, int k, int v){
    // Ajoute une association (clé, valeur) à un dictionnaire.
    if (!member(*D, k)){
        D->data[hash(*D, k)] = constr(k, v, D->data[hash(*D, k)]);
        D->size = D->size + 1;
        if (D->capacity < D->size){
            resize(D, D->capacity * 2);
        }
    }
}

void del(dict* D, int k){
    // Supprime une association d'un dictionnaire.
    int h = hash(*D, k);
    list* tmp = D->data[h];
    if (tmp->key == k){
        D->data[h] = tmp->next;
        D->size = D->size - 1;
        free(tmp);
    } else {
        while (tmp->next != NULL && tmp->next->key != k){
            tmp = tmp->next;
        }
        if (tmp->next != NULL){
            list* tmp2 = tmp->next;
            tmp->next = tmp2->next;
            D->size = D->size - 1;
            free(tmp2);
        }
    }
    if (D->capacity > 4 * D->size){
        resize(D, 1 + D->capacity / 2);
    }
}