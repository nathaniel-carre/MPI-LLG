#include "lecture.h"

geopoint* positions;
georoute* routes;
int nb_points;
int nb_routes;

void lit_routes(void) {
    FILE * fp = fopen("routes.txt", "r"); // ouvre le fichier routes.txt en lecture
  
    fscanf(fp,"%d",&nb_routes); // lit la première ligne
    // qui donne le nombre de lignes dans le fichier

    routes = malloc(nb_routes* sizeof(*routes));
  
    for(int id_route=0; id_route<nb_routes; id_route++) {  // itère pour chaque ligne
        georoute* r = &routes[id_route];
        fscanf(fp,"%d %d %lf  %[^\n]\n",&r->depart,&r->arrivee,&r->distance,r->nomRoute);
        // lit une ligne et stocke le résultat dans les variables passées
        // en paramètre. On sait donc qu'il y a un segment de route de
        // depart à fin de longueur distance et la route s'appelle nomRoute
    }
    fclose(fp); // ferme le fichier routes.txt
}

void lit_positions(void) {
    // on rappelle que fscanf(fp,"%lf %lf\n",longitude,latitude);
    // lit une paire de coordonnées sur une ligne 
    FILE* fp = fopen("positions.txt", "r");
    fscanf(fp, "%d", &nb_points);

    positions = malloc(nb_points * sizeof(*positions));

    for (int id_point=0; id_point<nb_points; id_point++){
        double longi, lati;
        fscanf(fp, "%lf %lf\n", &longi, &lati);
        positions[id_point].latitude = lati;
        positions[id_point].longitude = longi;
    }
    fclose(fp);
}

void nettoie(void) {
    // cette fonction est appelée une fois de la fonction main et doit
    // libérer toute la mémoire allouée
    free(positions);
    free(routes);
}
