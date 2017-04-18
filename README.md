# Projet : Compilateur de C vers Bytecode Java - L3 Info - 2016/2017
Par Hugo CLEMENT

## Informations
### Collaboration
Travail effectué en collaboration avec Pauline Fuzier (entraide et réflexions).

### Comment tester ?
#### Les tests unitaires
Les tests unitaires sont là enfin de vérifier l'intégritée des fonctions au cours du développement.

En effet, elle ne suffisent pas à prouver que le code est bon, mais permettre de vérifier que les fonctions on toujours le comportement attendu en cas de modification du code.

	make
	ocaml testsUnitairesP1.ml
	ocaml testsUnitairesP2.ml

#### Avec jasmin et java

	make
	cd "Tests/"
	../comp even.c Even.j
	java -jar jasmin.jar Even.j
	javac Wrapper.java
	java Wrapper 3

## Carnet de bord
### Partie I
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

### Exercice 4
Le project compile.
Les tests unitaires ont réussis.

Ajout d'exception pour rendre la compilation de la partie I plus lisible (enlève l'affichage des exceptions du à l'absence de certains cas *comme demandé dans la consigne* dans la fonction principale).

### Exercice 5
#### Génération des expressions de test
Protocol présent dans `testsExpr.ml` pour passer d'une expression de type `BinOp(0, binop, e1, e2)` à `["sipush 3"; "sipush 4"; "iadd"]`.

Résultats obtenue :

	[["sipush 3"; "sipush 4"; "iadd"];
	["iload 0"; "sipush 4"; "isub"];
	["sipush 3"; "iload 1"; "imul"];
	["sipush 10"; "sipush 2"; "idiv"];
	["sipush 10"; "sipush 2"; "irem"]]
	
#### Test avec jasmin
Enfin de tester, j'ai copié à la main dans Even.j les résultats précédents.

Exemple :

	sipush 3
	sipush 4
	iadd

Et ensuite je lance les commandes suivantes

	java -jar jasmin.jar Even.j
	javac Wrapper.java
	java Wrapper 0

Et j'obtients le résultat de mes opérations :

	java Wrapper 0		result: 7			(* 3+4  *)
	java Wrapper 5		result: 1			(* 5-4  *)
	java Wrapper 4		result: 12			(* 3*4  *)
	java Wrapper 0		result: 5			(* 10/2 *)
	java Wrapper 0		result: 0			(* 10%2  *)

---

### Partie 2
### Exercice 6, 7 & 8
RAS

Le project compile. Les tests unitaires ont réussis.

### Exercice 9
TODO, voir brouillon