# Compilateur-Mips

Christ MBOMBO MOKONDA


A partir des documents qui m'ont été donné , j'ai décidé d'ajouter des changements légers sur le parseur et de plus me concentrer 
sur la compilation .
La sous ensemble que j'ai réussi à compiler est l'assignation ( Variable = Valeur)
J'ai gardé les types lors de l'assignation , comme avec le c ( dont le langage
crée ici s'inspire largement , car c'est le langage que je maîtrise le mieux ) , et par
soucis de simplicité .


Etape 1 :Modification dans le parser (afin de créer "ma" version de python) :

- modification du if :  remplacement du then par col ( pour une syntaxe plus proche du "vrai Python")
                        ajout du elif 

- ajout de la boucle while (avec une structure associé pour la syntaxe, et l'ajout d'un 
  token while dans le lexer)
- ajout d'un return (ajouté en token aussi dans mon lexer)



Etape 2 : Au niveau de l'AST  
- Retrait des structures concernant les closûres , le test if , les blocks
  Seuls structure présente : celle permettant l'assignation ( Let , Var et Const)

Etape 3 : niveau de l'analyse sémantique 
- Élimination des vérification concernant les appels de fonctions ,  les definition 
  de fonction , le if et les blocs 
  On vérifie seulement le sens des structures ayant un lien avec l'assignation

Etapes 4: Definition des instructions mips sur un fichier à part 

Etape 5 : Match des structures de mon AST (Let Var Const ) avec le code assembleur correspondant  


Etape 6 : Compilation : 

- racket liec.rkt test.liec >test.s
- spim -f test.s
