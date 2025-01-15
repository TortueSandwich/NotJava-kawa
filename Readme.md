build and execute :
```sh
dune build && dune exec ./kawai.exe -- <args>
```
or
```sh
dune build && ./kawai.exe <args>
```

run using :
```sh
./kawai.exe <file1> [--help | --show-source | -s]
```

exemples d'utilisation :
```sh
./kawai.exe ./tests/var.kwa
```
```sh
dune build && ./kawai.exe -s
```


**Auteurs:**
- Arthur SAILLANT (LDD3IM)
- Rayan LALAOUI (LDD3IM)

[lien du repo github](https://github.com/TortueSandwich/NotJava-kawa)

## Sommaire

- [Rapport de Projet](#rapport-de-projet)
  - [Sommaire](#sommaire)
  - [Répartition du Travail](#répartition-du-travail)
  - [Difficultés](#difficultés)
  - [Tests](#tests)
  - [Fonctionnalités](#fonctionnalités)
  - [Fonctionnalités supplémentaires](#fonctionnalités-supplémentaires)
  - [Proposition d'amélioration](#proposition-damélioration)
  - [Commentaires personnels](#commentaires-personnels)
    - [Arthur SAILLANT](#arthur-saillant)
    - [Rayan LALAOUI](#rayan-lalaoui)

## Rapport de Projet

### Répartition du Travail<a name="répartition-du-travail"></a>

Nous avons abordé le projet de manière incrémentale, en établissant un ordre de mise en œuvre des fonctionnalités basé sur leur complexité et la simplicité des tests associés. Après avoir mis en place un framework permettant de choisir et de gérer les tests de manière efficace, notre objectif initial était d'afficher les tokens (en couleur !) afin de faciliter le débogage. Une fois cette étape terminée, nous avons poursuivi avec la phase de parsing, puis nous avons implémenté l'interpréteur capable de traiter tous les tests fournis dans le cadre du projet. Ce n'est qu'ensuite que nous avons développé le typechecker.

Au début, la répartition des tâches s'est faite de manière assez fluide : Arthur s'est principalement chargé de l'affichage des tokens, du parsing et de l'interpréteur, tandis que Rayan a pris en charge l'implémentation du typechecker, assurant ainsi la vérification des types pour tous les tests de base.

Ensuite, les fonctionnalités supplémentaires ont été implémentées individuellement, chaque membre prenant en charge l'ajout de fonctionnalités spécifiques de son côté. Cela a impliqué des modifications dans plusieurs fichiers du projet, ce qui a occasionné quelques difficultés lors des merges Git. Bien que ces conflits aient parfois causé des confusions, ils ont été résolus grâce au sang-froid de Rayan, qui a su gérer la situation avec calme et efficacité.

---

### Difficultés<a name="difficultés"></a>

Nous avons rencontré plusieurs difficultés au cours du projet, notamment en ce qui concerne l'utilisation de Git et la gestion des conflits. Mais, la difficulté majeure a été le parsing, surtout en ce qui concerne les fonctionnalités supplémentaires, qui introduisaient des aspects de syntaxe assez complexes.

Pour rendre notre langage compatible avec une analyse LR(1), nous avons dû modifier certaines parties de la syntaxe "habituelle" (inspirée de Java). Par exemple, pour le typecast, nous avons choisi une syntaxe inspirée de Rust, passant de la forme `(type) expr` à `expr as type`. Cette modification a permis une meilleure intégration avec le parseur, mais a ajouté de la complexité à la syntaxe.

Un autre défi est survenu avec la gestion des types génériques. Nous avons décidé d'introduire le mot-clé `generic` pour que Menhir puisse choisir plus facilement la règle appropriée, ce qui rend la syntaxe malheureusement verbeuse.

Enfin, l'ajout de messages d'erreur détaillés a constitué un casse-tête. Menhir ne dispose pas d'une documentation très complète, ce qui a rendu difficile la compréhension de son fonctionnement interne. Cela a rendu l'implémentation des messages d'erreur plus complexe que prévu.

---

### Tests<a name="tests"></a>

Nous avons ajouté un test pour chaque fonctionnalité implémentée, y compris des tests conçus pour échouer. Ils ont permis de vérifier le bon fonctionnement de la syntaxe et du type checker en simulant des erreurs intentionnelles. Ces tests ont validé la capacité du compilateur à réagir correctement aux erreurs et de s'assurer que les messages d'erreur étaient appropriés et clairs.

---

### Fonctionnalités<a name="fonctionnalités"></a>

- [x] Les fonctionnalités de base
- [x] Champs immuables
- [x] Visibilités
- [x] Déclarations en série
- [x] Déclaration avec valeur initiale
- [ ] Champs statiques
- [x] Test de type (`instance of`)
- [x] Transtypage (cast)
- [x] Super
- [x] Tableaux
- [x] Égalité structurelle (`===`)
- [ ] Classes et méthodes abstraites
- [ ] Surcharge statique

- [ ] Déclarations simplifiées (enlever var, method)
- [x] "Missing semicolon"
- [x] "Did you mean '...'?"
- [x] Le processus ne peut pas aboutir en raison d'un problème technique
- [x] Syntaxe abstraite typée (`annot` et `expr_`)

---

### Fonctionnalités supplémentaires<a name="fonctionnalités-supplémentaires"></a>

- déclaration et initialisation de variable en tant qu'instruction
- Portée & shadowing
- Classes génériques
- Interfaces (avec ou sans implémentation par défaut)
- des messages d'erreurs plus explicite ([voir l'handleling des erreurs](kawai.ml#L170))

---

### Proposition d'amélioration<a name="proposition-damélioration"></a>

- Implémenter les references et l'ownership
- Permettre d'avoir des constantes génériques au sein d'une classe
- "Sous-typage" des interfaces (passer une classe implémentant une interface en paramètre)

---

### Détails d'implémentation

Nous avons implémenté la gestion des portées et du *shadowing* à l'aide d'une structure de données personnalisée appelée `stack_env` ([source](./stack_env.ml)). Il s'agit d'une pile de tables de hachage, mais avec un "fond" commun (le dernier élément) partagé entre chaque instance de `stack_env`. Ce fond représente la portée globale du programme, c'est-à-dire les données accessibles à l'échelle mondiale. Chaque élément de la pile représente ensuite une portée imbriquée.

Pour résoudre une requête, il suffit de parcourir la pile de haut en bas, en cherchant la première occurrence de la donnée dans chaque table de hachage. À la fin d'une portée, le premier élément est retiré de la pile, permettant ainsi de revenir à l'environnement de portée précédent.

Le fond doit être commun à chaque instance de `stack_env` afin que les données globales restent accessibles, même dans des contextes totalement différents du programme principal, comme au sein des classes et méthodes.

![illustration stack env](./assets/stackenv.png)

---


### Commentaires personnels<a name="commentaires-personnels"></a>

#### Arthur SAILLANT

#### Rayan LALAOUI



