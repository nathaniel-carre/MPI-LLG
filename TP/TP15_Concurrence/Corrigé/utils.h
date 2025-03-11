#ifndef UTILS_H
#define UTILS_H

#include <pthread.h>
#include <stdatomic.h>

struct Mutex {
    atomic_bool attente[2];
    atomic_int tour;
};

typedef struct Mutex mutex;

struct Tranche {
    int* debut;
    int taille;
};

typedef struct Tranche tranche;

struct Maillon {
    tranche tr;
    struct Maillon* suivant;
};

typedef struct Maillon maillon;

struct File {
    maillon* premier;
    maillon* dernier;
    atomic_int taille_max;
    pthread_mutex_t verrou;
};

typedef struct File file;

void afficher_tab(int* tab, int n);

int* tab_alea(int n);

void swap(int* tab, int i, int j);

double chronometre(void* f(void*), void* arg);

void verrouiller(mutex* m, int i);

void deverrouiller(mutex* m, int i);

file* creer_file(void);

void liberer_file(file* f);

void enfiler(file* f, tranche tr);

bool defiler(file* f, tranche* tr);

#endif
