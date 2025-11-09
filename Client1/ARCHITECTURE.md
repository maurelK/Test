# 🏗️ Architecture du Client R-Type

## 📁 Structure des Fichiers

### **Gestion des Fenêtres**
- `WindowManager.hpp/cpp` - Gestion centralisée des fenêtres SFML
  - Création/fermeture de fenêtres
  - Gestion des événements
  - Toutes les fonctions < 20 lignes ✅

### **Rendu Graphique**
- `GameRenderer.hpp/cpp` - Gestion du rendu visuel
  - Background (grille + étoiles + particules)
  - Power-ups
  - Séparation claire de la logique de rendu

### **Menu**
- `Menu.hpp/cpp` - Menu principal avec navigation clavier
  - Navigation: Flèches ↑↓
  - Validation: Entrée/Espace
  - Compatible souris

### **Composants Visuels**
- `Background.hpp/cpp` - Étoiles, particules, grille animée
- `Logo.hpp/cpp` - Logo animé du jeu

### **Jeu**
- `TestGame.hpp/cpp` - Test du background + power-ups
- `game/PowerUp.hpp/cpp` - Bonus collectables (Vie, Score, Arme)
- `game/Player.hpp/cpp` - Vaisseau du joueur (à intégrer)
- `game/Enemy.hpp/cpp` - Ennemis (à intégrer)
- `game/Bullet.hpp/cpp` - Projectiles (à intégrer)
- `game/Game.hpp/cpp` - Jeu complet (à intégrer)

### **Client Principal**
- `GameClient.hpp/cpp` - Orchestrateur principal
  - Lance le menu
  - Lance le jeu selon le choix
  - Gère les inputs clavier

## 🎮 Flux du Programme

```
main.cpp
   ↓
GameClient::run()
   ↓
runMenu() ← Menu avec navigation clavier (↑↓ + Entrée)
   ↓
Si "NOUVELLE PARTIE":
   ↓
TestGame::run() ← Test Background + PowerUps
   ↓
[Prochaine étape: Intégrer Player + déplacements]
```

## 🔧 Principes de Code

✅ **Aucune fonction > 20 lignes**
✅ **Séparation des responsabilités**
✅ **Code propre et lisible**
✅ **Architecture modulaire**

## 📝 Prochaines Étapes

1. ✅ Background + Power-ups (FAIT)
2. 🔜 Intégrer Player avec inputs clavier
3. 🔜 Ajouter les ennemis
4. 🔜 Système de collision
5. 🔜 Jeu complet

## 🎯 Inputs Clavier

### Menu:
- **↑ / ↓** : Naviguer
- **Entrée / Espace** : Valider
- **Échap** : Quitter

### Jeu (à venir):
- **Flèches** : Déplacer le vaisseau
- **Espace** : Tirer
- **Échap** : Retour au menu
