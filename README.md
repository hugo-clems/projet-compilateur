# Projet : Compilateur de C vers Bytecode Java - L3 Info - 2016/2017
Par Hugo CLEMENT

## Informations
### Collaboration
Travail effectué en collaboration avec Pauline Fuzier (entraide et réflexions).

### Comment tester ?
**Dans le terminal :**

	make
	ocaml tests.ml

## Carnet de bord
### Exercice 1
Après plusieurs tests ayant échoués, malgré que mon code me semblais bon, j'ai du redéfinir dans `typing.ml` des fonctions présentes dans `lang.ml`.
Je ne trouve cependant pas pourquoi...

Le project compile.
Les tests unitaires ont réussis.

Le warning est du à l'absence du cas `CallE` dans la fonction principale.

### Exercice 2
Même problème, j'ai du redéfinir une fonction de `lang.ml` dans `typing.ml`.

Le project compile.
Les tests unitaires ont réussis.

Plus aucun Warning, en effet tous les cas de la fonction principale sont maintenant couverts.

### Exercice 3
RAS

Le project compile.
Les tests unitaires ont réussis.