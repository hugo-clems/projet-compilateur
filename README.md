# Projet : Compilateur de C vers Bytecode Java - L3 Info - 2017 - 🐫
Par Hugo CLEMENT


## Informations
### Makefile
J'ai modifié le Makefile afin de pouvoir lancer les tests plus simplement (voir *Comment tester ?*).


### Comment tester ?
#### Les tests unitaires
Les tests unitaires sont là enfin de vérifier l'intégritée des fonctions au cours du développement.

En effet, elle ne suffisent pas à prouver que le code est bon, mais permettre de vérifier que les fonctions on toujours le comportement attendu en cas de modification du code.

	make
	make tests       (* Pour tester tout le projet *)
	make testsP1     (* Pour tester la partie 1 *)
	make testsP2     (* Pour tester la partie 2 *)

#### Avec jasmin et java

	make
	cd "Tests/"
	../comp even.c Even.j
	java -jar jasmin.jar Even.j
	javac Wrapper.java
	java Wrapper 3




## Carnet de bord
### Anciens problèmes
**Problème présent à la partie I :** "Après plusieurs tests ayant échoués, malgré que mon code me semblais bon, j'ai du redéfinir dans `typing.ml` des fonctions présentes dans `lang.ml`.
Je ne trouve cependant pas pourquoi..."

Maintenant ce problème n'est plus (`#load "lang.cmo"`).


---


### Partie I
### Exercice 1
Le project compile.
Les tests unitaires ont réussis.

Le warning est du à l'absence du cas `CallE` dans la fonction principale.

### Exercice 2
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

* Lancement du test avec `make testsP1`
	
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

### Exercice 9 & 10
#### Fonctionnement d'un IfThenElse :

	On insère le code du IF
	On insère un 0
	If Beq lFalse (* <=> Si le code du if renvoie faux, Goto lFalse *)
		On insère le code du THEN
		Goto lFin
	lFalse:
		On insère le code du ELSE
	lFin:
		...

#### Fonctionnement d'un Appel de fonction :

	On insère tous les paramètres
	Invoke la fonction
	Return

#### Fonctionnement d'un While :

	lWhile:
		On insère la condition de bouclage du While
		On insère un 0
		If Beq lFalse
			On insère le code du corps du While
			Goto lWhile
	lFalse:
		Nop (* While fini, on continue *)

Le project compile. Les tests unitaires ont réussis.

### Exercice 11, 12, 13 & 14

RAS

Le project compile. Les tests unitaires ont réussis.
