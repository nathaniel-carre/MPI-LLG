#ifndef LECTURE_H
#define LECTURE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

struct Geopoint {
    double latitude, longitude ;
};
typedef struct Geopoint geopoint;

struct Georoute {
    int depart, arrivee;
    double distance;
    char nomRoute[270];
};
typedef struct Georoute georoute;

extern geopoint* positions;
extern georoute* routes;
extern int nb_points;
extern int nb_routes;

void lit_routes(void) ;
void lit_positions(void) ;
void nettoie(void);

#endif
