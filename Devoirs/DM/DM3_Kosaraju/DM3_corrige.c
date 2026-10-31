#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

struct Graphe {
    int n;
    int* deg;
    int** adj;
};

typedef struct Graphe graphe;

// Question 2
graphe lire_graphe(const char* nom_fichier){
    // On ouvre le fichier et on commence par lire le nombre de sommets.
    // Après avoir initialisé les tableaux, on lit les entiers un par un pour
    // chaque ligne. Le premier entier nous donne le degré (qui permet 
    // d'initialiser le tableau d'adjacence), et les suivants donnent les
    // voisins. Inutile d'essayer de lire les espaces ou les sauts de ligne.
    FILE* canal = fopen(nom_fichier, "r");
    graphe G;
    fscanf(canal, "%d", &(G.n));
    G.deg = malloc(G.n * sizeof(int));
    G.adj = malloc(G.n * sizeof(int*));
    for (int s=0; s<G.n; s++){
        fscanf(canal, "%d", &G.deg[s]);
        G.adj[s] = malloc(G.deg[s] * sizeof(int));
        for (int i=0; i<G.deg[s]; i++){
            fscanf(canal, "%d", &G.adj[s][i]);
        }
    }
    fclose(canal);
    return G;
}

// Question 3
void affiche_tableau(int* tab, int n){
    // On affiche tous les éléments séparés par des virgules. On traite
    // le dernier à part, pour envisager le cas du tableau vide.
    printf("{");
    for (int i=0; i<n-1; i++)
        printf("%d, ", tab[i]);
    if (n>0) printf("%d}\n", tab[n-1]);
}

void affiche_graphe(graphe G){
    // Fonction non demandée pour afficher les listes d'adjacence d'un graphe.
    for (int s=0; s<G.n; s++){
        printf("sommet %d : ", s);
        for (int i=0; i<G.deg[s];i++){
            printf("%d, ", G.adj[s][i]);
        }
        printf("\n");
    }
}

//Question 4
void liberer_graphe(graphe G){
    // On ne doit libérer que des tableaux, pas le graphe G lui-même.
    for (int s=0; s<G.n; s++) free(G.adj[s]);
    free(G.adj);
    free(G.deg);
}

//Question 5
void dfs(graphe G, int s, bool* vus){
    // On écrit une fonction auxiliaire qui fait le parcours depuis un
    // sommet, en prenant en argument le tableau de booléens des sommets vus.
    if (!vus[s]){
        vus[s] = true;
        for (int i=0; i<G.deg[s]; i++){
            dfs(G, G.adj[s][i], vus);
        }
    }
}

int nb_cc(graphe G){
    // On parcourt chaque sommet, et dès qu'on tombe sur un sommet non vu, on
    // lance un parcours depuis ce sommet et on incrémente le nombre de composantes.
    bool* vus = malloc(G.n * sizeof(bool));
    for (int s=0; s<G.n; s++) vus[s] = false;
    int nb = 0;
    for (int s=0; s<G.n; s++){
        if (!vus[s]){
            dfs(G, s, vus);
            nb++;
        }
    }
    free(vus);
    return nb;
}

// Question 6
int nb_aretes(graphe G){
    // Compte le nombre d'arêtes du graphe. On compte la somme des degrés
    // et on divise par 2. On fait l'hypothèse qu'il n'y a pas de débordement.
    // Comme on a accès au degré en temps constant, la complexité est O(|S|).
    int nb = 0;
    for (int s=0; s<G.n; s++){
        nb += G.deg[s];
    }
    return nb / 2;
}

bool acyclique(graphe G){
    // On utilise le fait que G est acyclique si et seulement si |A| = |S| - m,
    // où m est le nombre de composantes connexes. Pour avoir la complexité
    // attendue, on commence par compter le nombre d'arêtes, et on ne compte le
    // nombre de composantes que si |A| < |S|. Ainsi, la complexité du parcours
    // est garantie d'être en O(|S|).
    int nba = nb_aretes(G);
    if (nba < G.n){
        return nba == G.n - nb_cc(G);
    } else {
        return false;
    }
}

//Question 8
void dfs_post(graphe G, int s, int* post, int* k){
    // Fonction auxiliaire qui lance un parcours depuis un sommet,
    // en ayant en argument le tableau des numéros postfixes et un
    // pointeur vers le numéro courant. On utilise le tableau post
    // lui-même pour détecter les sommets non vus, en y mettant un
    // -1 si le sommet est vu, mais que ses voisins n'ont pas tous
    // été parcourus.
    if (post[s] == -2){
        (*k)++;
        post[s] = -1;
        for (int i=0; i<G.deg[s]; i++){
            dfs_post(G, G.adj[s][i], post, k);
        }
        (*k)++;
        post[s] = *k;
    }
}

int* num_post(graphe G){
    // On crée le tableau avec des -2, et on lance un parcours depuis
    // chaque sommet.
    int* post = malloc(G.n * sizeof(int));
    for (int s=0; s<G.n; s++) post[s] = -2;
    int k = 0;
    for (int s=0; s<G.n; s++){
        dfs_post(G, s, post, &k);
    }
    return post;
}

// Question 9
bool dag(graphe G){
    // On utilise le fait qu'un graphe est un DAG s'il n'existe pas d'arête
    // (s, t) telle que post[s] < post[t]. On parcourt toutes les arêtes et
    // on fait cette vérification. On fait attention à libérer le tableau
    // post dans le cas où on renvoie faux prématurément.
    int* post = num_post(G);
    for (int s=0; s<G.n; s++){
        for (int i=0; i<G.deg[s]; i++){
            int t = G.adj[s][i];
            if (post[s] < post[t]) {
                free(post);
                return false;
            }
        }
    }
    free(post);
    return true;
}

// Question 11
int* inverse_post(int* post, int n){
    // Comme le graphe n'est pas donné en argument, on ne peut pas juste
    // refaire un parcours postfixe en gardant en mémoire l'ordre. L'idée
    // est simplement de créer un tableau ordre de taille 2n, tel que
    // ordre[i - 1] est le sommet ayant le numéro postfixe i. Notons que certaines
    // cases du tableau ne sont pas remplies (car les numéros sont entre 1 et 2n).
    // Ensuite, il suffit de parcourir ce tableau ordre et de noter les valeurs
    // des cases remplies, dans l'ordre. On pense à libérer le tableau ordre.
    int* ordre = malloc(2 * n * sizeof(int));
    int* parcours = malloc(n * sizeof(int));
    for (int i=0; i<2*n; i++) ordre[i] = -1;
    for (int j=0; j<n; j++){
        parcours[j] = -1;
        ordre[post[j]-1] = j;
    }
    int j = n - 1;
    for (int i=0; i<2*n; i++){
        if (ordre[i] != -1){
            parcours[j] = ordre[i];
            j--;
        }
    }
    free(ordre);
    return parcours;
}

// Question 12
graphe transpose(graphe G){
    // Après avoir initialisé le graphe, on commence par parcourir les arêtes
    // une première fois pour calculer les degrés dans le graphe transposé.
    // Ensuite, on utilise un tableau d'indices pour garder en mémoire les
    // indices courants dans chaque tableau d'adjacence, et on refait un parcours
    // des arêtes pour créer les arêtes transposées.
    graphe GT;
    GT.n = G.n;
    GT.deg = malloc(G.n * sizeof(int));
    GT.adj = malloc(G.n * sizeof(int*));
    int* indices = malloc(G.n * sizeof(int));
    for (int s=0; s<G.n; s++){
        indices[s] = 0;
        GT.deg[s] = 0;
    }
    for (int s=0; s<G.n; s++){
        for (int i=0; i<G.deg[s]; i++){
            GT.deg[G.adj[s][i]]++;
        }
    }
    for (int s=0; s<G.n; s++){
        GT.adj[s] = malloc(GT.deg[s] * sizeof(int));
    }
    for (int s=0; s<G.n; s++){
        for (int i=0; i<G.deg[s]; i++){
            int t = G.adj[s][i];
            GT.adj[t][indices[t]] = s;
            indices[t]++;
        }
    }
    free(indices);
    return GT;
}

// Question 13
void dfs_kosaraju(graphe G, int s, int* cfc, int num){
    // Fonction auxiliaire qui fait un parcours depuis un sommet s
    // en ayant en argument le tableau des numéros de CFC et le numéro courant.
    if (cfc[s] == -1){
        cfc[s] = num;
        for (int i=0; i<G.deg[s]; i++){
            dfs_kosaraju(G, G.adj[s][i], cfc, num);
        }
    }
}

int* kosaraju(graphe G){
    // On applique l'algorithme du cours : on calcule le graphe transposé,
    // on calcule un parcours postfixe inverse sur ce graphe, puis on lance
    // un parcours dans G depuis chaque sommet non vu de ce parcours postfixe
    // inverse.
    graphe GT = transpose(G);
    int* cfc = malloc(G.n * sizeof(int));
    for (int s=0; s<G.n; s++) cfc[s] = -1;
    int* post = num_post(GT);
    int* parcours = inverse_post(post, G.n);
    int num = 0;
    for (int i=0; i<G.n; i++){
        int s = parcours[i];
        if (cfc[s] == -1){
            dfs_kosaraju(G, s, cfc, num);
            num++;
        }
    }
    free(post);
    free(parcours);
    liberer_graphe(GT);
    return cfc;
}

// Question 14
int nb_cfc(graphe G){
    // Il suffit de déterminer le plus grand numéro de CFC dans G, et
    // d'ajouter 1.
    int* cfc = kosaraju(G);
    int maxi = 0;
    for (int i=0; i<G.n; i++){
        if (cfc[i] > maxi) maxi = cfc[i];
    }
    free(cfc);
    return maxi + 1;
}

int main(void){
    // On lit tous les fichiers. La case d'indice 0 n'est pas remplie
    // pour rester cohérent avec la numérotation.
    graphe nonorientes[9];
    graphe orientes[9];
    char nom_no[] = "nonoriente0";
    char nom_o[] = "oriente0";
    for (int i=1; i<9; i++){
        nom_no[10] = i + '0'; // Conversion de l'entier i en le caractère idoine.
        nom_o[7] = i + '0';
        nonorientes[i] = lire_graphe(nom_no);
        orientes[i] = lire_graphe(nom_o);
    }

    // Question 7
    for (int i=1; i<9; i++){
        printf("Le graphe non orienté %d est-il acyclique ? %d\n", i, acyclique(nonorientes[i]));
    }

    // Question 10
    for (int i=1; i<9; i++){
        printf("Le graphe orienté %d est-il acyclique ? %d\n", i, dag(orientes[i]));
    }

    // Question 15
    for (int i=1; i<9; i++){
        printf("Le nombre de CFC dans le graphe orienté %d est %d.\n", i, nb_cfc(orientes[i]));
    }

    for (int i=1; i<9; i++){
        liberer_graphe(nonorientes[i]);
        liberer_graphe(orientes[i]);
    }
    return EXIT_SUCCESS;
}