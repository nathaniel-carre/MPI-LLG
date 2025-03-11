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
    for (int k=1;k<=N; k++){
        cpt++;
    }
    return NULL;
}

pthread_mutex_t verrou;

void* incrementer2(void* arg){
    for (int k=1;k<=N; k++){
        pthread_mutex_lock(&verrou);
        cpt++;
        pthread_mutex_unlock(&verrou);
    }
    return NULL;
}

mutex m = {.attente[0] = false, .attente[1] = false, .tour = 0};

void* incrementer3(void* arg){
    int i = *(int*) arg;
    for (int k=1;k<=N; k++){
        verrouiller(&m, i);
        cpt++;
        deverrouiller(&m, i);
    }
    return NULL;
}

void* tri_insertion(void* arg){
    tranche tr = *(tranche*) arg;
    for (int i=1; i<tr.taille; i++){
        int j = i;
        while (j > 0 && tr.debut[j-1] > tr.debut[j]){
            swap(tr.debut, j, j-1);
            j--;
        }
    }
    return NULL;
}

int partition(tranche tr){
    int p = tr.debut[0];
    int i = 0;
    for (int j=1; j<tr.taille; j++){
        if (tr.debut[j] <= p){
            i++;
            swap(tr.debut, i, j);
        }
    }
    swap(tr.debut, 0, i);
    return i;
}

void* tri_rapide1(void* arg){
    tranche tr = *(tranche*) arg;
    if (tr.taille > SEUIL_TRI){
        int i = partition(tr);
        tranche tr0 = {tr.debut, i};
        tranche tr1 = {&tr.debut[i+1], tr.taille - i - 1};
        tri_rapide1(&tr0);
        tri_rapide1(&tr1);
    } else {
        tri_insertion(&tr);
    }
    return NULL;
}

void traiter_tranche(file* f, tranche tr){
    if (tr.taille > SEUIL_TRI){
        int i = partition(tr);
        tranche tr0 = {tr.debut, i};
        tranche tr1 = {&tr.debut[i+1], tr.taille - i - 1};                
        f->taille_max++;
        enfiler(f, tr0);
        traiter_tranche(f, tr1);
    } else {
        tri_insertion(&tr);
        f->taille_max--;
    }
}

void* traitement(void* arg){
    file* f = (file*) arg;
    while (f->taille_max > 0){
        tranche tr;
        if (defiler(f, &tr)){
            traiter_tranche(f, tr);
        }
    }
    return NULL;
}

void* tri_rapide2(void* arg){
    tranche tr = *(tranche*) arg;    
    file* f = creer_file();
    enfiler(f, tr);
    f->taille_max++;
    pthread_t fils[NB_FILS];
    for (int i=0; i<NB_FILS; i++){
        pthread_create(&fils[i], NULL, traitement, f);
    }
    for (int i=0; i<NB_FILS; i++){
        pthread_join(fils[i], NULL);
    }
    liberer_file(f);
    return NULL;
}

int main(void){
    srand(time(0)); // Pour avoir des tirages aléatoires distincts à chaque exécution
    /* 
    // Questions 2, 3
    pthread_t f0, f1;
    pthread_create(&f0, NULL, incrementer1, NULL);
    pthread_create(&f1, NULL, incrementer1, NULL);
    pthread_join(f0, NULL);
    pthread_join(f1, NULL);
    printf("cpt = %d\n", cpt);

    // Question 5
    // On pense à initialiser et détruire le mutex
    pthread_mutex_init(&verrou, NULL);
    pthread_create(&f0, NULL, incrementer2, NULL);
    pthread_create(&f1, NULL, incrementer2, NULL);
    pthread_join(f0, NULL);
    pthread_join(f1, NULL);
    printf("cpt = %d\n", cpt);
    pthread_mutex_destroy(&verrou);

    // Question 9
    // On crée des variables pour les numéros de fils
    int n0 = 0, n1 = 1;
    pthread_create(&f0, NULL, incrementer3, &n0);
    pthread_create(&f1, NULL, incrementer3, &n1);
    pthread_join(f0, NULL);
    pthread_join(f1, NULL);
    printf("cpt = %d\n", cpt);

    //Question 17
    int n = 1000;
    int* tab = tab_alea(n);
    tranche tr = {tab, n};
    double t1 = chronometre(tri_insertion, &tr);
    free(tab);
    tab = tab_alea(n);
    tr.debut = tab;
    double t2 = chronometre(tri_rapide1, &tr);
    free(tab);
    n = 10000;
    tab = tab_alea(n);
    tr.debut = tab;
    tr.taille = n;
    double t3 = chronometre(tri_insertion, &tr);
    free(tab);
    tab = tab_alea(n);
    tr.debut = tab;
    double t4 = chronometre(tri_rapide1, &tr);
    free(tab);
    printf("Tri insertion, 1000 : %lf\n", t1);
    printf("Tri rapide, 1000 : %lf\n", t2);
    printf("Tri insertion, 10000 : %lf\n", t3);
    printf("Tri rapide, 10000 : %lf\n", t4);
    */
    // Question 25
    int n = 1000000;
    int* tab = tab_alea(n);
    tranche tr = {tab, n};
    double t1 = chronometre(tri_rapide1, &tr);
    free(tab);
    tab = tab_alea(n);
    tr.debut = tab;
    double t2 = chronometre(tri_rapide2, &tr);
    free(tab);
    printf("Tri rapide 1 : %lf\n", t1);
    printf("Tri rapide 2 : %lf\n", t2);   
    return EXIT_SUCCESS;
}