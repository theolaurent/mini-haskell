# Compilation

## Dépendance

mini-haskell nécessite Ocaml (>= 4.02).
Il dépend d' `ocamlgraph`.

## Compilation

La commande `make` compile mini-haskell.

# Choix

## Ast minimal

- 5 constructions : constantes, variables, lambda-abstraction, liaisons let, applications.
- Les primitives du langages (if, case, ...) sont abstraites par des constantes (constructeur CPrim)

## Report d'erreurs multiples

- Le compilateur essaie (dans la mesure du raisonnable) de reporter le plus d'erreurs possibles lors de chaque exécution.
- Nous tentons d'éviter les faux-négatifs : chaque erreur est est reporté est dans la mesure du possible indépendante des autres. Par contre, le compilateur n'est pas garanti reporter toutes les erreurs en une seule passe.
- Lors de l'analyse lexicale les caractères inconnus ne produisent pas de lexème, les chaines mal formées produisent un lexèmes STR, ...
- L'analyse syntaxique produit pour chaque non-terminal une instance de type option, celles-ci sont traités de façon monadique ce qui permet de continuer partiellement l'analyse.
- Un foncteur est instancié pour chaque phase d'erreur ; le système de report d'erreur est impératif (file).

## Typage

- Nous tentons d'implanter un système F 'complet' pour le typage (pouvant nécéssiter des annotations explicites de type dans les cas de polymorphisme de second ordre) à la MLF.
- Nous nous basons sur l'implantation décrite dans la thèse de Didier Le Botlan
- Pour l'instant nous n'avons pas implanté l'analyse lexicale et syntaxique des annotations de types (l'interêt de ce typage est donc encore très limité). Nous prevoyons d'utiliser une syntaxe proche de celle d'Haskell.

# Problèmes rencontrés

- Un conflit shift/reduce au niveau des listes séparées par des point-vigules a été résolu en utilisant un non-terminal paramétrique