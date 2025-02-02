//Structure de données que l'arbre stockera.
//Dans notre cas, une donnée représente un article, caractérisé par son nom et son prix.
//Il suffira de remplacer base.{h,c} pour changer l'utilisation de l'arbre sans changer ses primitives.
typedef struct donnee{
 char* nom; 
 int prix; 
} donnee;

//Renvoie un entier strictement négatif si a1 < a2, 0 si a1 = a2 et un entier strictement positif si a1 > a2.
//Dans le cadre du TP, a1 < a2 ssi le prix de a1 est strictement inférieur à celui de a2.
int compare(donnee a1, donnee a2);

//Affiche la donnée en entrée.
//Dans le cadre du TP, on affiche le nom de l'article puis son prix entre parenthèses.
void affiche_donnee(donnee a);
