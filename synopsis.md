# Rubik's cube 

## gerer le cube

### composition 

8 coins
6 centres
12 arêtes

ce sont les 26 cubes unitaires (on néglige le centre)


### représentation de la position

le coin en haut à gauche est la position 0,0 (initialement le blanc-bleu-rouge)
ensuite on représente les autres relativement à ce coin, en x, y, z


### mouvements

R R' L L' U U' D D' F F' B B' (on pourra rajouter middle, et deux rangées en même temps)

exemple avec R : on fait "tourner" les cubes d'abscisse x dans le sens horaire


## fenetre

### entrees

lettres correspondant aux mouvements, minuscule pour sens classique, majuscule pour sens anti-classique

commande / boutons pour finir le rubik's cube en entier

- algo aidé (on donne une information précisant l'algo qu'on veut exécuter, et il s'éxecute)

### etapes

réaliser une étape de résolution (OLL / PLL) 


# 0.0 : essaies intuitifs pour le cube de taille n 

# 1.0 : on se concentre sur le cas du rubik's cube 3*3, on pourra aussi faire 2x2 en ne considérant que les aretes

# 2.0 : on se concentre sur le cas du 3*3 pour avoir un cube central, centre du repere des coordonnées
