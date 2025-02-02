#include<stdlib.h>
#include<stdio.h>
#include<string.h>
#include "base.h"

int compare(donnee a1, donnee a2){
  return a1.prix - a2.prix;
}

void affiche_donnee(donnee a){
  printf("%s (%d euros)  ",a.nom, a.prix);
}
