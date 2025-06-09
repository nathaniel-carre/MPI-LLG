#include "lecture.h"
#include "liste.h"

const double DEGREE_VERS_RADIAN = M_PI / 180 ;
const double DIAMETRE_TERRE_EN_KM = 12742 ;

double ll_distance(geopoint p1, geopoint p2) {
    double x1 = p1.longitude * DEGREE_VERS_RADIAN, x2 = p2.longitude * DEGREE_VERS_RADIAN;
    double y1 = p1.latitude * DEGREE_VERS_RADIAN, y2 = p2.latitude * DEGREE_VERS_RADIAN;
    double v = sin((y1 - y2) / 2) * sin((y1 - y2) / 2);
    v += cos(y1) * cos(y2) * sin((x1 - x2) / 2) * sin((x1 - x2) / 2);
    v = DIAMETRE_TERRE_EN_KM * asin(sqrt(v));
    return v;
}

void longueur_moyenne(void){
    double lm_hav = 0.0;
    double lm_rou = 0.0;
    for (int id_route=0; id_route<nb_routes; id_route++){
        georoute r = routes[id_route];
        lm_rou += r.distance;
        lm_hav += ll_distance(positions[r.depart], positions[r.arrivee]) * 1000;
    }
    printf("Longueur moyenne haversine : %lf\n", lm_hav / nb_routes);
    printf("Longueur moyenne route : %lf\n", lm_rou / nb_routes);
}

void longueur_max(void){
    double lm_hav = 0.0;
    double lm_rou = 0.0;
    for (int id_route=0; id_route<nb_routes; id_route++){
        georoute r = routes[id_route];
        if (r.distance > lm_rou){ lm_rou = r.distance; }
        double hav = ll_distance(positions[r.depart], positions[r.arrivee]) * 1000;
        if (hav > lm_hav){ lm_hav = hav; }
    }
    printf("Longueur max haversine : %lf\n", lm_hav);
    printf("Longueur max route : %lf\n", lm_rou);
}

liste** creer_graphe(void){
    liste** G = malloc(nb_points * sizeof(*G));
    for (int id_point=0; id_point<nb_points; id_point++){
        G[id_point] = NULL;
    }
    for (int id_route=0; id_route<nb_routes; id_route++){
        georoute r = routes[id_route];
        G[r.depart] = ajoute(G[r.depart], r.arrivee, r.distance);
        G[r.arrivee] = ajoute(G[r.arrivee], r.depart, r.distance);
    }
    return G;
}

void liberer_graphe(liste** G, int taille){
    for (int i=0; i<taille; i++){
        liberer_liste(G[i]);
    }
    free(G);
}

void sauvegarde_graphe(liste** G, int taille){
    FILE * fp = fopen("graphe.txt", "w");

    fprintf(fp, "%d", taille);
    for (int i=0; i<taille; i++){
        liste* lst = G[i];
        fprintf(fp, "\n%d", taille_liste(lst));
        while (lst != NULL){
            fprintf(fp, " %d %lf", lst->voisin, lst->distance);
            lst = lst->suivant;
        }
    }
    fclose(fp);
}

int main(void){
    lit_positions();
    lit_routes();
    longueur_moyenne();
    longueur_max();
    //liste** G = creer_graphe();
    //sauvegarde_graphe(G, nb_points);
    nettoie();
    //liberer_graphe(G, nb_points);
    return 0;
}
