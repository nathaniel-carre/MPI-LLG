#include <stdlib.h>
#include <stdio.h>

//Définition de listes
struct maillon {
    int valeur;
    struct maillon* suivant;
};

typedef struct maillon maillon;
typedef maillon* liste;


int est_vide(liste l)
{
    return (l == NULL);
}

liste cons(int tete, liste queue)
{
    liste nouvelle_liste = (liste)malloc(sizeof(maillon));
    nouvelle_liste->valeur = tete;
    nouvelle_liste->suivant = queue;
    return nouvelle_liste;
}

liste queue(liste l)
{
    return l->suivant;
}

int tete(liste l)
{
    return l->valeur;
}

// Cette fonction est correcte
void affiche_liste(liste l)
{
    if (est_vide(l))
    {
        printf("\n");
    }
    else
    {
        if (est_vide(queue(l))) //Petite coquetterie pour éviter que le dernier élément de la liste ne soit suivi par une virgule.
        {
            printf("%d \n", tete(l));
        }
        else
        {
            printf("%d,", tete(l));
            affiche_liste(queue(l));
        }
    }
}

void libere_liste(liste l)
{
    if (!est_vide(l))
    {
        libere_liste(queue(l));
        free(l);
    }
}

liste concatenation(liste l1, liste l2)
{
    if (est_vide(l1))
    {
        return l2; 
    }
    else
    {
        return cons(tete(l1),concatenation(queue(l1),l2));
    }
}


int main()
 {
    //Construction d'une liste test contenant les entiers 9,8,7,6,5,4,3,2,1,0
    liste test = NULL;
    for (int i = 0; i < 10; i++)
    {
        test = cons(i,test);
    }

    //Affichage de test.
    printf("La liste test est : "); affiche_liste(test);


    //Construction et affichage de la concaténation de test et test.
    liste c = concatenation(test,test);
    printf("Concaténation de test et test : "); affiche_liste(c);

    libere_liste(test);
    libere_liste(c);
}
