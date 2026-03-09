#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <stdbool.h>
#include <stdlib.h>

#include "utils.h"

#define SEUIL_TRI 16
#define NB_FILS 4

int N = 1000000;
int cpt = 0;

void* incrementer1(void* arg){
    // À modifier
    return NULL;
}

pthread_mutex_t verrou;

void* incrementer2(void* arg){
    // À modifier
    return NULL;
}

mutex m = {.attente[0] = false, .attente[1] = false, .tour = 0};

void* incrementer3(void* arg){
    // À modifier
    return NULL;
}

void* tri_insertion(void* arg){
    // À modifier
    return NULL;
}

int partition(tranche tr){
    // À modifier
    // Écrire l'invariant de boucle AVANT d'écrire le corps de la boucle
    return 0; // À modifier
}

void* tri_rapide1(void* arg){
    // À modifier
    return NULL;
}

void traiter_tranche(file* f, tranche tr){
    // À modifier
}

void* traitement(void* arg){
    // À modifier
    return NULL;
}

void* tri_rapide2(void* arg){
    // À modifier
    return NULL;
}

int main(void){
    srand(time(0)); // Pour avoir des tirages aléatoires distincts à chaque exécution
    // À modifier
    return 0;
}