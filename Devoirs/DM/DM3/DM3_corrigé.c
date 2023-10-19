#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

struct Graphe{
    int n;
    int** adj;
};

typedef struct Graphe graphe;

// On se contente de trouver la sentinelle -1 et de diviser par deux l'indice
// où elle se trouve.
int degre(graphe G, int s){
    int i = 0;
    // On suppose que le graphe est correctement construit, c'est-à-dire
    // que le tableau G.adj[s][i] contient bien la sentinelle -1.
    while (G.adj[s][i] != -1) i++;
    return i / 2;
}

struct Arete{
    int s;
    int t;
    int p;  
};

typedef struct Arete arete;

void ajouter_arete(graphe G, arete a){
    int degs = degre(G, a.s);
    int degt = degre(G, a.t);
    if (degs == 10 || degt == 10) return;
    // On vérifie que s et t ne sont pas déjà voisins.
    for (int i=0; i<degs; i++){
        if (G.adj[a.s][2 * i] == a.t) return;
    }
    // On modifie alors les tableaux d'adjacence de s et t.
    G.adj[a.s][2 * degs] = a.t;
    G.adj[a.s][2 * degs + 1] = a.p;
    G.adj[a.s][2 * degs + 2] = -1;
    G.adj[a.t][2 * degt] = a.s;
    G.adj[a.t][2 * degt + 1] = a.p;
    G.adj[a.t][2 * degt + 2] = -1;
}

// On ouvre le fichier, on lit la valeur de n, puis on initialise
// le graphe. Ensuite, on lit chaque ligne (tant qu'il y en a) et
// on rajoute l'arête correspondante.
graphe construire_graphe(const char* nom_fichier){
    FILE* canal = fopen(nom_fichier, "r");
    graphe G;
    int success = fscanf(canal, "%d", &(G.n));
    G.adj = malloc(G.n * sizeof(int*));
    for (int s=0; s<G.n; s++){
        G.adj[s] = malloc(21 * sizeof(int));
        G.adj[s][0] = -1;
    }
    while (success != EOF){
        int s, t, p;
        success = fscanf(canal, "%d %d %d", &s, &t, &p);
        if (success == 3){
            arete a = {.s = s, .t = t, .p = p};
            ajouter_arete(G, a);
        }
    }
    fclose(canal);
    return G;
}

// On crée une fonction pour libérer la mémoire occupée par le
// tableau des adjacences d'un graphe (pas besoin de libérer le
// graphe lui-même : il n'a pas d'allocation mémoire).
void liberer_graphe(graphe G){
    for (int s=0; s<G.n; s++){
        free(G.adj[s]);
    }
    free(G.adj);
}

// On somme les poids des arêtes telles que s < t.
int poids_graphe(graphe G){
    int poids = 0;
    for (int s=0; s<G.n; s++){
        for (int i=0; G.adj[s][2 * i] != -1; i++){
            if (s < G.adj[s][2 * i]) poids += G.adj[s][2 * i + 1];
        }
    }
    return poids;
}

// On commence par écrire une fonction de parcours en profondeur récursive,
// de manière similaire à celle écrite en cours.
void DFS(graphe H, int* m, int* cc, int s){
    if (cc[s] != -1) return;
    cc[s] = *m;
    for (int i=0; H.adj[s][2 * i] != -1; i++){
        DFS(H, m, cc, H.adj[s][2 * i]);
    }
}

// Dès lors, on lance un DFS depuis chaque composante connexe, c'est-à-dire
// chaque sommet qui n'a pas encore un numéro de composante.
int* composantes(graphe H, int* m){
    *m = 0;
    int* cc = malloc(H.n * sizeof(int));
    for (int s=0; s<H.n; s++) cc[s] = -1;
    for (int s=0; s<H.n; s++){
        if (cc[s] == -1){
            DFS(H, m, cc, s);
            (*m)++;
        }
    }
    return cc;
}

// Fonction utilitaire qui compare deux arêtes. Renvoie 1 si a > b, -1 si b > a
// et 0 si a = b.
int compare(arete a, arete b){
    if (a.p > b.p) return 1;
    if (a.p < b.p) return -1;
    if (a.s > b.s) return 1;
    if (a.s < b.s) return -1;
    if (a.t > b.t) return 1;
    if (a.t < b.t) return -1;
    return 0;
}

// On parcourt l'ensemble des arêtes de G, et pour chaque arête qui relie deux
// composantes de H, on vérifie pour chacune des deux composantes si elle est
// inférieure à l'arête déjà choisie.
arete* aretes_sures(graphe G, graphe H, int* cc, int m){
    arete* sures = malloc(m * sizeof(arete));
    // On initialise le premier sommet de chaque arete à -1, ce qui correspond
    // au cas où aucune arête n'a encore été choisie.
    for (int i=0; i<m; i++) sures[i].s = -1;
    for (int s=0; s<G.n; s++){
        for (int i=0; G.adj[s][2 * i] != -1; i++){
            int t = G.adj[s][2 * i];
            if (cc[s] == cc[t]) continue;
            arete a = {.s = s, .t = t, .p = G.adj[s][2 * i + 1]};
            if (sures[cc[s]].s == -1 || compare(a, sures[cc[s]]) == -1){
                sures[cc[s]] = a;
            }
            if (sures[cc[t]].s == -1 || compare(a, sures[cc[t]]) == -1){
                sures[cc[t]] = a;
            }
        }
    }
    return sures;
}

// On remarque qu'il existe des arêtes sûres si et seulement si H n'est
// pas connexe. On applique alors l'algorithme tel qu'il est décrit en
// utilisant les fonctions déjà implémentées. On pense bien à libérer
// la mémoire au fur et à mesure.
graphe boruvka(graphe G){
    graphe H;
    H.n = G.n;
    H.adj = malloc(H.n * sizeof(int*));
    for (int s=0; s<H.n; s++){
        H.adj[s] = malloc(21 * sizeof(int));
        H.adj[s][0] = -1;
    }
    int m = H.n;
    while (m != 1){
        int* cc = composantes(H, &m);
        if (m == 1){
            free(cc);
            break;
        }
        arete* sures = aretes_sures(G, H, cc, m);
        for (int i=0; i<m; i++) ajouter_arete(H, sures[i]);
        free(cc);
        free(sures);
    }
    return H;
}

int main(void){
    graphe g_10_ad = construire_graphe("g_10_ad.txt");
    graphe g_10_sd = construire_graphe("g_10_sd.txt");
    graphe g_1000_ad = construire_graphe("g_1000_ad.txt");
    graphe g_1000_sd = construire_graphe("g_1000_sd.txt");
    graphe g_50000_ad = construire_graphe("g_50000_ad.txt");
    graphe g_50000_sd = construire_graphe("g_50000_sd.txt");

    printf("Le degré du sommet 4 dans g_10_ad est %d\n", degre(g_10_ad, 4));

    printf("Le poids du graphe g_10_ad est %d\n", poids_graphe(g_10_ad));
    printf("Le poids du graphe g_10_sd est %d\n", poids_graphe(g_10_sd));
    printf("Le poids du graphe g_1000_ad est %d\n", poids_graphe(g_1000_ad));
    printf("Le poids du graphe g_1000_sd est %d\n", poids_graphe(g_1000_sd));
    // Remarque : il y a un dépassement d'entier pour le poids de g_50000_sd.

    graphe acm_10_ad = boruvka(g_10_ad);
    graphe acm_10_sd = boruvka(g_10_sd);
    graphe acm_1000_ad = boruvka(g_1000_ad);
    graphe acm_1000_sd = boruvka(g_1000_sd);
    graphe acm_50000_ad = boruvka(g_50000_ad);
    graphe acm_50000_sd = boruvka(g_50000_sd);

    printf("Le poids de l'arbre couvrant minimal de g_10_ad est %d\n", poids_graphe(acm_10_ad));
    printf("Le poids de l'arbre couvrant minimal de g_10_sd est %d\n", poids_graphe(acm_10_sd));
    printf("Le poids de l'arbre couvrant minimal de g_1000_ad est %d\n", poids_graphe(acm_1000_ad));
    printf("Le poids de l'arbre couvrant minimal de g_1000_sd est %d\n", poids_graphe(acm_1000_sd));
    printf("Le poids de l'arbre couvrant minimal de g_50000_ad est %d\n", poids_graphe(acm_50000_ad));
    printf("Le poids de l'arbre couvrant minimal de g_50000_sd est %d\n", poids_graphe(acm_50000_sd));

    liberer_graphe(g_10_ad);
    liberer_graphe(g_10_sd);
    liberer_graphe(g_1000_ad);
    liberer_graphe(g_1000_sd);
    liberer_graphe(g_50000_ad);
    liberer_graphe(g_50000_sd);

    liberer_graphe(acm_10_ad);
    liberer_graphe(acm_10_sd);
    liberer_graphe(acm_1000_ad);
    liberer_graphe(acm_1000_sd);
    liberer_graphe(acm_50000_ad);
    liberer_graphe(acm_50000_sd);
    return 0;
}