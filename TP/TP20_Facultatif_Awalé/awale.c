#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

int** Dw7;
int** Dw24;
int nDw7, nDw24;

int** creer_configurations(int* nDw, const char* nom_fichier){
    FILE* fichier = fopen(nom_fichier, "r");
    fscanf(fichier, "%d", nDw);
    int** Dw = malloc(*nDw * sizeof(*Dw));
    for (int i=0; i<*nDw; i++){
        Dw[i] = malloc(8 * sizeof(int));
        for (int j=0; j<8; j++){
            fscanf(fichier, "%d", &Dw[i][j]);
        }
    }
    fclose(fichier);
    return Dw;
}

void detruire_configurations(int** Dw, int nDw){
    for (int i=0; i<nDw; i++){
        free(Dw[i]);
    }
    free(Dw);
}

void afficher_tab(int* tab){
    printf("{%d", tab[0]);
    for (int i=1; i<8; i++){
        printf(", %d", tab[i]);
    }
    printf("}\n");
}

int* copie(int* tab){
    // Copie le tableau donné en argument.
    int* nouv_tab = malloc(8 * sizeof(int));
    for (int i=0;i<8;i++){
        nouv_tab[i] = tab[i];
    }
    return nouv_tab;
}

int semer(int* tab, int i){
    // Applique l'opération de semailles des graines de la case i.
    // Renvoie l'indice du dernier puits qui reçoit une graine.
    int x = tab[i], j = i;
    assert (x != 0);
    tab[i] = 0;
    while (x != 0){
        j = (j + 1) % 8;
        if (j == i){ j = (j + 1) % 8; }
        x -= 1;
        tab[j] += 1;        
    }
    return j;
}

int recolter(int* tab, int j){
    // Applique l'opération de récoltes depuis la case j.
    // Renvoie le nombre de graines récoltées.
    int x = 0;
    while (j > 3 && (tab[j] == 2 || tab[j] == 3)){
        x += tab[j];
        tab[j] = 0;
        j -= 1;
    }
    return x;
}

bool famine(int* tab, int joueur){
    // Teste s'il y a une situation de famine pour l'adversaire.
    int x = 0;
    for (int j=joueur * 4;j<(joueur + 1) * 4;j++){
        x += tab[j];
    }
    return (x == 0);
}

bool jouable(int* tab, int i){
    // Teste si un puit est jouable pour le joueur courant :
    // il doit y avoir des graines et cela ne doit pas mettre
    // l'adversaire en situation de famine.
    if (tab[i] == 0){ return false; }
    int* t = copie(tab);
    int j = semer(t, i);
    recolter(t, j);
    bool b = !famine(t, 1);
    free(t);
    return b;
}

bool bloque(int* tab){
    // Teste si le joueur courant n'a aucun coup jouable.
    bool b = true;
    int i = 0;
    while (i < 4 && b){
        b = !jouable(tab, i);
        i++;
    }
    return b;
}

int premier_bloque(int ni){
    // Calcule le premier numéro de configuration > ni
    // telle que le joueur courant est bloqué.
    ni++;
    while (famine(Dw7[ni], 0) || !bloque(Dw7[ni])){
        ni++;
    }
    return ni;
}

void q5(int u0){
    int ni = 0;
    for (int i=0;i<3;i++){
        ni = premier_bloque(ni);
        printf("n%d = %d\n", i, ni);
    }
}

int nb_puits_jouables(int* tab){
    // Détermine le nombre de puits jouables pour le 
    // joueur courant.
    int x = 0;
    for (int i=0;i<4;i++){
        if (jouable(tab, i)) { x++; }
    }
    return x;
}

int nb_max_graines(int* tab){
    // Calcule le nombre maximal de graines que peut remporter
    // le joueur courant en un coup.
    int maxi = 0;
    for (int i=0;i<4;i++){
        if (jouable(tab, i)){
            int* t = copie(tab);
            int j = semer(t, i);
            int x = recolter(t, j);
            if (x > maxi) { maxi = x; }
            free(t);
        }
    }
    return maxi;
}

int premier_k_jouables(int k, int ni){
    // Calcule le premier numéro de configuration > ni
    // telle que le joueur courant a exactement k puits
    // jouables.
    ni++;
    while (nb_puits_jouables(Dw24[ni]) != k) { ni++; }
    return ni;
}

void q6(int u0){
    for (int k=0;k<5;k++){
        int ni = premier_k_jouables(k, 0);
        int maxi = nb_max_graines(Dw24[ni]);
        printf("n%d = %d, r%d = %d\n", k, ni, k, maxi);
    }
}

void q7(int u0){
    int ni = 0;
    for (int i=0;i<3;i++){
        ni = premier_k_jouables(0, ni);
        printf("n%d = %d\n", i, ni);
    }
}

int main(void){
    Dw7 = creer_configurations(&nDw7, "configurations_7.txt");
    Dw24 = creer_configurations(&nDw24, "configurations_24.txt");
    // Les deux lignes suivantes peuvent être effacées après avoir comparé
    // avec les résultats de la question 4.
    //for (int i=1; i<4; i++) afficher_tab(Dw7[i]);
    //for (int i=4; i<7; i++) afficher_tab(Dw24[i]);

    q5(42);
    q6(42);
    q7(42);

    detruire_configurations(Dw7, nDw7);
    detruire_configurations(Dw24, nDw24);
}