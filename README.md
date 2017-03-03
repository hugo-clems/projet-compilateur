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

#### Tests
Le project compile. Le warning est du à l'absence du cas *CallE* dans la fonction principale.
Les tests unitaires ont tous réussis.