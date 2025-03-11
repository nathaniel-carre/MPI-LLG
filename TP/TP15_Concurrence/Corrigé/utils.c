#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <stdbool.h>

#include "utils.h"

void afficher_tab(int* tab, int n){
    // Affiche les 20 premiers éléments d'un tableau d'entiers
    if (n > 20) {n = 20;}
    printf("[");
    for (int i=0; i<n-1; i++){
        printf("%d, ", tab[i]);
    }
    if (n>0){
        printf("%d", tab[n-1]);
    }
    printf("]\n");
}

int* tab_alea(int n){
    // Renvoie un tableau d'entiers aléatoires entre 0 et 99999
    int* tab = malloc(n * sizeof(int));
    for (int i=0; i<n; i++){
        tab[i] = rand() % 100000;
    }
    return tab;
}

void swap(int* tab, int i, int j){
    // Échange les valeurs de deux éléments d'un tableau
    int tmp = tab[i];
    tab[i] = tab[j];
    tab[j] = tmp;
}

double chronometre(void* f(void*), void* arg){
    // Renvoie le temps d'exécution en secondes pour calculer f(arg)
    struct timeval start, stop;
    gettimeofday(&start, NULL);
    f(arg); 
    gettimeofday(&stop, NULL);
    return (stop.tv_sec - start.tv_sec) + 1e-6 * (stop.tv_usec - start.tv_usec);
}

void verrouiller(mutex* m, int i){
    m->attente[i] = true;
    m->tour = 1 - i;
    while (m->attente[1 - i] && m->tour != i){}
}

void deverrouiller(mutex* m, int i){
    m->attente[i] = false;
}

file* creer_file(void){
    file* f = malloc(sizeof(file));
    f->premier = NULL;
    f->dernier = NULL;
    f->taille_max = 0;
    pthread_mutex_init(&(f->verrou), NULL);
    return f;
}

void liberer_file(file* f){
    if (f != NULL){
        maillon* m = f->premier;
        while (m != NULL){
            maillon* tmp = m;
            m = m->suivant;
            free(tmp);
        }
        pthread_mutex_destroy(&(f->verrou));
        free(f);
    }
}

void enfiler(file* f, tranche tr){    
    maillon* m = malloc(sizeof(maillon));
    m->tr = tr;
    m->suivant = NULL;
    pthread_mutex_lock(&(f->verrou));
    if (f->premier == NULL){
        f->premier = m;
    } else {
        f->dernier->suivant = m;
    }
    f->dernier = m;
    pthread_mutex_unlock(&(f->verrou));
}

bool defiler(file* f, tranche* tr){
    pthread_mutex_lock(&(f->verrou));
    if (f->premier == NULL){
        pthread_mutex_unlock(&(f->verrou));
        return false;
    }
    maillon* m = f->premier;
    *tr = m->tr;
    if (f->premier == f->dernier){
        f->premier = NULL;
        f->dernier = NULL;
    } else {
        f->premier = m->suivant;
    }
    pthread_mutex_unlock(&(f->verrou));
    free(m);
    return true;    
}