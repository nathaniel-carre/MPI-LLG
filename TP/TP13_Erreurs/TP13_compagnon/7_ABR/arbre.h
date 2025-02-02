#include "base.h"

//Structure utilisée pour les arbres binaires.
//Un arbre binaire est un pointeur vers un noeud.
//Un noeud stocke une donnée, un pointeur vers son fils gauche et un vers son fils droit.
struct noeud { 
  donnee val; 
  struct noeud *fg;
  struct noeud *fd;
};

typedef struct noeud noeud;
typedef noeud* arbre;

arbre cons_vide();

arbre cons(donnee v, arbre gauche, arbre droit);

int est_vide(arbre a);

donnee valeur(arbre a);

arbre gauche(arbre a);

arbre droit(arbre a);

void libere(arbre a);

void infixe(arbre a);

void insere(arbre* pa, donnee val);

