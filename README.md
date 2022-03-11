# Une plateforme pour l'authentification simple des utilisateurs suivi du chargement et du préprocessing des données et se terminant par une selection d'un ensemble d'algorithmes d'apprentissage

Sur cette plateforme on a utilisé une technique particulière pour mettre en place un système d'authentification gratuit sur ShinyApps.
Les données des utilisateurs sont sauvegardees dans une base de données sqlite dont la visulation de son contenu est disponible grace à l'utilisation de DB Browser.



## Comment cela fonctionne
-  ** À propos de l'uthentification

- Création d'une base de données SQLite pour stocker des informations. La table 'users' contient le nom d'utilisateur et les mots de passe hachés pour tous les utilisateurs enregistrés.
- L'utilisateur bascule entre deux fonctions renderUI() en fonction de son statut de connexion.
- Si un compte n'existe pas, l'utilisateur peut en créer un. Le nom d'utilisateur est stocké en texte clair avec le mot de passe haché en utilisant la fonction sha256() de la bibliothèque openssl.
- Si un compte existe, l'utilisateur peut se connecter avec le nom d'utilisateur et le mot de passe correspondants. L'état de connexion de l'application change et l'interface utilisateur change.
- Après s'être connecté, l'utilisateur peut se déconnecter en cliquant sur le lien d'action en bas à gauche sur la page menuLeft.

-  ** Une fois connecté

- L'utilisateur à la possibilité de charger un jeu de données qu'il pourra néttoyer avec un ensemble de fonctionnalités qui lui accéssible:

- ########### Pour les valeurs manquantes il pourrait choisir différentes méthodes d'inputation disponibles (KNN, moyenne, médian, mode). I peut aussi supprimer toutes les observations présentant des valeurs manquantes.
- ###########  Il peut gérer les variables par exemple en écartant les variables non pertinantes dans l'étude, en identifiant des variables ayant une forte influence,...
- Création d'une combinaison de variables 
- Paramettrer le mode d'apprentissage sur les différents modèles


