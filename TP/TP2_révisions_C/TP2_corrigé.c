#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>
#include <assert.h>

int pgcd(int a, int b){
    // On utilise la formule de l'algorithme d'Euclide.
    // Inutile de vérifier que a > b : le premier appel récursif
    // inverse les valeurs le cas échéant.
    if (b == 0){
        return a;
    } else {
        return pgcd(b, a % b);
    }
}

float somme(int n){
    // On fait attention aux types, notamment pour faire
    // la division (sinon on obtient un entier).
    float S = 0;
    for (int k=1; k<=n; k++){
        S += 1.0 / (k * k);
    }
    return S;
}

int seuil(int n){
    // On divise par 2 tant qu'on peut.
    int k = 0;
    while (n>1){
        n /= 2;
        k += 1;
    }
    return k;
}

int maximum(int tab[], int len){
    // On vérifie que le tableau n'est pas vide, puis on
    // le parcourt en gardant en mémoire la valeur maximale,
    // initialisée avec le premier élément du tableau.
    assert (len > 0);
    int maxi = tab[0];
    for (int i=1; i<len; i++){
        if (tab[i] > maxi){
            maxi = tab[i];
        }
    }
    return maxi;
}

int indice_maximum(int tab[], int len){
    // Même idée qu'avant, sauf qu'on initialise l'indice.
    assert (len > 0);
    int imax = 0;
    for (int i=1; i<len; i++){
        if (tab[i] > tab[imax]){
            imax = i;
        }
    }
    return imax;
}

void afficher_tableau(int tab[], int len){
    // On commence par ouvrir le crochet et affichier les
    // n-1 premiers éléments (pour éviter d'avoir une virgule
    // en trop). Ensuite, on distingue selon que le tableau
    // est vide ou non.
    printf("[");
    for (int i=0; i<len-1; i++){
        printf("%d, ", tab[i]);
    }
    if (len > 0){
        printf("%d]\n", tab[len - 1]);
    } else {
        printf("]\n");
    }    
}

int* tableau_alea(int len, int m){
    // On alloue la mémoire nécessaire, puis on modifie
    // chaque case du tableau. On calcule module m en
    // comptant sur le fait que ça soit à peu près
    // uniforme si INT_MAX est très grand.
    int* tab = (int*)malloc(len * sizeof(int));
    for (int i=0; i<len; i++){
        tab[i] = rand() % m;
    }
    return tab;
}

bool premier(int n){
    // Un simple test où on vérifie chaque entier de 2
    // à racine de n. On pense à traiter à part les cas
    // de 0 et 1.
    if (n < 2){
        return false;
    }
    int d = 2;
    while (d * d <= n){
        if (n%d == 0){
            return false;
        }
        d += 1;
    }
    return true;
}

int* crible(int n, int* len){
    // On commence par créer le crible qui contient des booléens ;
    // une fois terminé, les indices des true (sauf 0 et 1)
    // correspondront aux nombres premiers.
    bool* grille = (bool*)malloc((n + 1) * sizeof(bool));
    *len = 0;
    for (int i=0; i<=n; i++){
        grille[i] = true;
    }
    for (int i=2; i<=n; i++){
        // Chaque fois qu'on trouve un nouveau nombre premier, on
        // augmente la valeur de len de 1, et on met à false tous
        // ses multiples.
        if (grille[i]){
            *len += 1;
            for (int j=i*i; j<=n; j+=i){
                grille[j] = false;
            }
        }
    }
    // Ensuite on crée un nouveau tableau qui va contenir les nombres
    // premiers jusqu'à n (il y en a len).
    int* premiers = (int*)malloc(*len * sizeof(int));    
    int k = 0;
    for (int i=2; i<=n; i++){
        if (grille[i]){
            premiers[k] = i;
            k += 1;
        }
    }
    free(grille);
    return premiers;
}

void tri_selection(int tab[], int len){
    // Pour chaque indice i sauf le dernier, on cherche la position de la
    // valeur minimale parmi les indices suivants. Une fois trouvée, on
    // inverse la valeur avec i.
    int imin;
    for (int i=0; i<len-1; i++){
        imin = i;
        for (int j=i+1; j<len; j++){
            if (tab[j] < tab[imin]){
                imin = j;
            }
        }
        int x = tab[imin];
        tab[imin] = tab[i];
        tab[i] = x;
    }
}

bool appartient(int x, int tab[], int len){
    // On se contente de parcourir chaque case du tableau jusqu'à trouver x.
    for (int i=0; i<len; i++){
        if (tab[i] == x){
            return true;
        }
    }
    return false;
}

bool dichotomie(int x, int tab[], int len){
    // On utilise deux indices g et d délimitant la portion du tableau dans
    // laquelle s'effectue la recherche de x.

    int g = 0, d = len;
    while (d > g + 1){
        // On utilise l'invariant de boucle suivant :
        // x ∈ tab si et seulement s'il existe un indice i ∈ [g, d-1] tel
        // que tab[i] = x
        int m = (g + d) / 2;
        if (tab[m] > x){
            d = m;
        }
        else{
            g = m;
        }
    }
    return (tab[g] == x);
}

struct Liste {
    int data;
    struct Liste* suivant;
};

typedef struct Liste liste;

liste* creer_liste(int x){
    // On fait l'allocation et la modification des pointeurs.
    liste* lst = (liste*)malloc(sizeof(liste));
    lst->data = x;
    lst->suivant = NULL;
    return lst;
}

liste* convertir_tab(int tab[], int len){
    // On part de la fin du tableau : pour chaque élément, on crée
    // un maillon qu'on fait pointer vers la liste obtenue jusqu'ici.
    liste* lst = NULL;
    for (int i=len-1; i>=0; i--){
        liste* lsti = creer_liste(tab[i]);
        lsti->suivant = lst;
        lst = lsti;
    }
    return lst;
}

void afficher_liste(liste* lst){
    // Même idée qu'avec les tableaux, sauf qu'on doit faire attention
    // à s'arrêter au bon moment (pour avoir un bon affichage).
    printf("[");
    if (lst == NULL){
        printf("]");
    } else {
        while (lst->suivant != NULL){
            printf("%d, ", lst->data);
            lst = lst->suivant;
        }
        printf("%d]\n", lst->data);
    }    
}

void liberer_liste(liste* lst){
    // On commence par vérifier que la liste n'est pas le pointeur nul.
    // Si c'est le cas, on commence par libérer la queue de la liste avant 
    // de libérer la tête.
    if (lst != NULL){        
        liberer_liste(lst->suivant);
        free(lst);
    }
}

struct File{
    liste* premier;
    liste* dernier;
};

typedef struct File file;

// Pas de difficulté particulière pour les trois fonctions suivantes.

file* creer_vide(void){
    file* f = (file*)malloc(sizeof(file));
    f->premier = NULL;
    f->dernier = NULL;
    return f;
}

bool est_vide(file* f){
    return f->premier == NULL;
}

void liberer_file(file* f){
    liberer_liste(f->premier);
    free(f);
}

void enfiler(file* f, int x){
    // Il faut bien penser à traiter à part le cas où la file est vide
    // au moment où on fait l'enfilage.
    liste* lst = creer_liste(x);
    if (est_vide(f)){
        f->premier = lst;
        f->dernier = lst;
    } else {
        f->dernier->suivant = lst;
        f->dernier = lst;
    } 
}

int defiler(file* f){
    // Ici, il faut penser à nommer le maillon de tête avant de faire les
    // modifications de pointeurs, pour pouvoir le libérer avant de 
    // renvoyer la valeur gardée en mémoire. Il faut également penser à
    // traiter à part le cas où l'élément défilé était le dernier.
    assert(!est_vide(f));
    int x = f->premier->data;
    liste* lst = f->premier;
    f->premier = f->premier->suivant;
    if (f->premier == NULL){
        f->dernier = NULL;
    }
    free(lst);
    return x;
}

int inversions_naif(int tab[], int n){
    // Une simple double boucle en parcourant toutes les paires d'indices.
    int nb_inv = 0;
    for (int i=0; i<n-1; i++){
        for (int j=i+1; j<n; j++){
            if (tab[i] > tab[j]){
                nb_inv += 1;
            }
        }
    }
    return nb_inv;
}

/* On s'inspire du tri fusion pour faire une fonction qui trie une partie du
   tableau entre deux indices g et d en plus de renvoyer le nombre d'inversions. 
   L'idée est la suivante :
      - si le tableau est de taille 0 ou 1, il n'y a rien à trier et le nombre 
        d'inversions est 0
      - sinon, on considère le milieu m de [g, d], on fait un appel récursif sur
        chaque moitié. Le nombre d'inversions entre g et d sera le nombre 
        d'inversions entre g et m + le nombre d'inversions entre m et d + le nombre
        d'inversions entre un élément de la première moitié et un de la deuxième
        moitié.
   On calcule cette dernière valeur pendant la fusion des deux moitiés : chaque fois
   qu'on ajoute un élément de la première moitié, il ne forme aucune inversion (car il
   est plus petit que tous les éléments qui suivent) ; chaque fois qu'on ajoute un
   élément de la deuxième moitié, il forme autant d'inversions que le nombre d'éléments
   restants dans la première moitié.
*/

int fusion(int tab[], int g, int m, int d){
    // On utilise un tableau tampon pour faire la fusion, on le recopie à la fin de
    // l'algorithme. On applique ce qui est décrit plus haut.
    int i = g, j = m;
    int nb_inv = 0;
    int* tmp = (int*)malloc((d - g)*sizeof(int));
    for (int k=0; k<d-g; k++){
        if (j == d || (i < m && tab[i] <= tab[j])){
            tmp[k] = tab[i];
            i += 1;
        } else {
            tmp[k] = tab[j];
            j += 1;
            nb_inv += m - i;
        }
    }
    // Une fois le tampon calculé, on le recopie avant de le libérer.
    for (int k=0; k<d-g; k++){
        tab[g+k] = tmp[k];
    }
    free(tmp);
    return nb_inv;
}

int inversions_DPR_aux(int tab[], int g, int d){
    // On écrit cette fonction auxiliaire qui a des arguments différents pour faciliter
    // les appels récursifs.
    if (d > g + 1){
        int m = (g + d) / 2;
        int ng = inversions_DPR_aux(tab, g, m);
        int nd = inversions_DPR_aux(tab, m, d);
        int ngd = fusion(tab, g, m, d);
        return ng + nd + ngd;
    } else {
        return 0;
    }
}

int inversions_DPR(int tab[], int n){
    return inversions_DPR_aux(tab, 0, n);
}

struct Tab_dyna {
    int taille;
    int capacite;
    int* data;
};

typedef struct Tab_dyna tab_dyna;

// Pas de difficulté particulière pour les 4 fonctions suivantes.

tab_dyna* creer_tab_dyna(void){
    tab_dyna* td = (tab_dyna*)malloc(sizeof(tab_dyna));
    int* tab = (int*)malloc(sizeof(int));
    td->taille = 0;
    td->capacite = 1;
    td->data = tab;
    return td;
}

void liberer_tab_dyna(tab_dyna* td){
    free(td->data);
    free(td);
}

int evaluer(tab_dyna* td, int i){
    assert (i < td->taille);
    return td->data[i];
}

int modifier(tab_dyna* td, int i, int x){
    assert(i < td->taille);
    td->data[i] = x;
}

void redimensionner(tab_dyna* td, int capa){
    // On pense bien à libérer l'ancien tableau et à modifier les pointeurs
    // et la nouvelle capacité.
    assert(capa >= td->taille);
    int* tab = (int*)malloc(capa*sizeof(int));
    for (int i=0; i<td->taille; i++){
        tab[i] = evaluer(td, i);
    }
    free(td->data);
    td->data = tab;
    td->capacite = capa;
}

void ajouter(tab_dyna* td, int x){
    // On suit ce qui est indiqué.
    if (td->taille == td->capacite){
        redimensionner(td, td->capacite * 2);
    }
    td->taille = td->taille + 1;
    modifier(td, td->taille - 1, x);
}

int retirer(tab_dyna* td){
    assert(td->taille > 0);
    int x = evaluer(td, td->taille - 1);
    td->taille = td->taille - 1;
    // Pour éviter des redimensionnements successifs en ajoutant et retirant des
    // éléments, il est plus judicieux de redimensionner quand le tableau est vide
    // aux trois quarts.
    if (td->taille * 4 < td->capacite){
        redimensionner(td, td->capacite / 2);
    }
    return x;
}

int main(void){
    //Exercice 1
    printf("le pgcd de 48 et 36 est : %d\n", pgcd(48, 36));
    printf("la somme jusqu'a n = 13 est : %f\n", somme(13));
    printf("le seuil pour n = 73 est %d\n", seuil(73));

    //Exercice 2
    int t1[10] = {8, 23, 3, 0, 23, 18, 37, 52, 1, 16};
    printf("le maximum de t1 est %d\n", maximum(t1, 10));
    printf("l'indice du maximum de t1 est %d\n", indice_maximum(t1, 10));
    printf("le tableau t1 est ");
    afficher_tableau(t1, 10);
    srand(time(0));
    int* t2 = tableau_alea(20, 30);
    printf("un tableau aleatoire t2 de taille 20 : ");
    afficher_tableau(t2, 20);

    //Exercice 3
    printf("la primalite de 37 et de 99 est %s et %s\n", 
            premier(37)?"true":"false", premier(99)?"true":"false");
    int len_prem = 0;
    int* prem = crible(100, &len_prem);
    printf("les %d nombres premiers de 2 a 100 sont :\n", len_prem);
    afficher_tableau(prem, len_prem);    
    free(prem);

    //Exercice 4
    tri_selection(t2, 20);
    printf("le tableau t2 trie : ");
    afficher_tableau(t2, 20);
    printf("le test d'appartenance de 15 a t2 avec les deux fonctions est %s et %s\n", 
            appartient(15, t2, 20)?"true":"false", dichotomie(15, t2, 20)?"true":"false");
    free(t2);

    //Exercice 5
    int* t3 = tableau_alea(20, 30);
    liste* lst = convertir_tab(t3, 20);
    free(t3);
    printf("la liste obtenue apres conversion d'un tableau aleatoire est :\n");
    afficher_liste(lst);
    liberer_liste(lst);
    file* f = creer_vide();
    enfiler(f, 1); enfiler(f, 2); enfiler(f, 3);
    printf("on defile %d en premier\n", defiler(f));
    printf("on defile %d en deuxieme\n", defiler(f));
    enfiler(f, 4); enfiler(f, 5);
    printf("on defile %d en troisieme\n", defiler(f));
    printf("l'etat de la file est : ");
    afficher_liste(f->premier);
    liberer_file(f);

    //Exercice 6
    int* t4 = tableau_alea(20, 30);
    printf("le nombre d'inversions de t4 est %d (algo naif)\n", inversions_naif(t4, 20));
    printf("le nombre d'inversions de t4 est %d (algo DPR)\n", inversions_DPR(t4, 20));
    free(t4);

    //Exercice 7
    int* t5 = tableau_alea(20, 30);
    tab_dyna* td = creer_tab_dyna();
    for (int i=0; i<20; i++){ajouter(td, t5[i]);}
    free(t5);
    printf("l'etat du tableau dynamique apres tous les ajouts est : ");
    afficher_tableau(td->data, td->taille);
    for (int i=0; i<10; i++){printf("on retire l'element %d du tableau\n", retirer(td));}
    printf("l'etat du tableau dynamique apres les retraits est : ");
    afficher_tableau(td->data, td->taille);
    liberer_tab_dyna(td);
    return EXIT_SUCCESS;
}