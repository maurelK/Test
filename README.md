# 🎮 R-TYPE - EPITECH Project

<div align="center">

![C++](https://img.shields.io/badge/C++-00599C?style=for-the-badge&logo=c%2B%2B&logoColor=white)
![SFML](https://img.shields.io/badge/SFML-8CC445?style=for-the-badge&logo=sfml&logoColor=white)
![Network](https://img.shields.io/badge/Network-UDP%2FTCP-blue?style=for-the-badge)
![ECS](https://img.shields.io/badge/Architecture-ECS-orange?style=for-the-badge)

**Recréation du jeu culte R-Type en C++ avec architecture réseau multijoueur**

[Installation](#-installation) • [Gameplay](#-gameplay) • [Architecture](#️-architecture) • [Documentation](docs/
)

</div>

---

## 📖 À propos du projet

R-Type est un projet EPITECH visant à recréer le célèbre shoot'em up des années 80 en utilisant une architecture moderne basée sur le pattern **Entity Component System (ECS)**. Le projet met l'accent sur le développement d'un moteur de jeu réutilisable et d'un système réseau robuste permettant le jeu en multijoueur.

### ✨ Caractéristiques principales

- 🎯 **Gameplay fidèle** au R-Type original
- 🌐 **Multijoueur en réseau** avec synchronisation UDP/TCP
- 🏗️ **Architecture ECS** modulaire et extensible
- 🎨 **Graphismes 2D** avec SFML
- 🎮 **Mode Solo et Multijoueur**
- 🔧 **Moteur de jeu réutilisable**
- 📦 **Système de composants flexible**

---

## 🎮 Lobby & Modes de jeu

<div align="center">
<img src="docs/images/lobby.png" alt="R-Type Lobby" width="700"/>
</div>

Le jeu propose une interface de lobby permettant de :
- **Créer ou rejoindre des parties multijoueur**
- **Configurer les paramètres de jeu**
- **Voir les joueurs connectés**
- **Choisir entre mode solo et multijoueur**

### Architecture en 3 parties

Le projet est divisé en trois composants principaux :

1. **Client** - Interface graphique et gestion des inputs
2. **Serveur** - Logique de jeu et synchronisation réseau
3. **Engine** - Moteur ECS réutilisable

---

## 👥 Équipe de développement

### Client Team
- [@Pavel](https://github.com/pavel) - Développement client
- [@benaya](https://github.com/kokou) - Interface utilisateur
- [@Benaya](https://github.com/benaya) - Graphismes et animations
- [@Pavel](https://github.com/nenonene) - Système de rendu

### Server Team
- [@Alberic](https://github.com/alberic) - Architecture réseau
- [@Axel](https://github.com/axel) - Logique serveur(udp et tcp)

### Engine Team
- [@Maurel](https://github.com/maurel) - Moteur ECS

---

## 🏗️ Architecture du Client

<div align="center">
<img src="docs/images/client_diagram.png" alt="Diagramme Architecture Client" width="800"/>
</div>

Le client est structuré autour de plusieurs composants clés :

### Composants principaux

- **Game Manager** - Gestion de l'état du jeu
- **Network Manager** - Communication avec le serveur
- **Render System** - Affichage graphique avec SFML
- **Input System** - Gestion des contrôles joueur
- **Audio System** - Effets sonores et musique
- **UI Manager** - Interface utilisateur et menus

### Communication Client-Serveur

```
Client                          Serveur
  │                               │
  ├──── Connexion (TCP) ────────>│
  │<──── Confirmation ────────────┤
  │                               │
  ├──── Actions (UDP) ──────────>│
  │<──── État du jeu (UDP) ───────┤
  │                               │
  ├──── Heartbeat ──────────────>│
  │<──── Sync ────────────────────┤
```

---

## 🎮 Modes de jeu

### Mode Solo
- Affrontez des vagues d'ennemis progressivement plus difficiles
- Collectez des power-ups pour améliorer votre vaisseau
- Battez les boss de fin de niveau
- Système de scoring et high scores

### Mode Multijoueur
- Jusqu'à 2 joueurs simultanés
- Coopération pour vaincre les ennemis
- Synchronisation en temps réel
- Chat intégré
- Système de lobby

---

## 🚀 Installation

### Prérequis

```bash
# Compilateur C++20
g++ --version  # ou clang++

# CMake
cmake --version  # >= 3.20

# SFML
sudo apt-get install libsfml-dev  # Ubuntu/Debian
brew install sfml                  # macOS
```

### Compilation

```bash
# Cloner le repository
git clone https://github.com/EpitechPGE3-2025/G-CPP-500-COT-5-1-rtype-23
cd rtype

# Créer le dossier de build
mkdir build && cd build

# Compiler le projet
cmake -S . -B build
cmake --build build
 ``

### Lancement

```bash
# Démarrer le serveur
./r-type_server

# Démarrer le client (dans un autre terminal)
./r-type_client
```

---

## 🎮 Contrôles

| Action | Touche |
|--------|--------|
| Déplacement | ⬆️ ⬇️ ⬅️ ➡️ (Flèches) |
| Tir | Espace |
| Tir chargé | Maintenir Espace |
| Pause | Échap |
| Menu | M |

---

## 📸 Gameplay

<div align="center">
<img src="docs/images/gameplay.gif" alt="R-Type Gameplay" width="700"/>
</div>

---

## 🏗️ Architecture technique

### Entity Component System (ECS)

Le moteur utilise une architecture ECS pure :

```
Entity (ID unique)
    │
    ├── Component: Position {x, y}
    ├── Component: Velocity {vx, vy}
    ├── Component: Sprite {texture, rect}
    ├── Component: Collider {width, height}
    └── Component: Health {current, max}

Systems:
    ├── MovementSystem
    ├── RenderSystem
    ├── CollisionSystem
    ├── NetworkSystem
    └── AISystem
```

### Systèmes implémentés

- **Movement System** - Gestion des déplacements
- **Render System** - Affichage des entités
- **Collision System** - Détection et résolution des collisions
- **Network System** - Synchronisation réseau
- **AI System** - Intelligence artificielle des ennemis
- **Weapon System** - Gestion des armes et projectiles
- **Particle System** - Effets visuels
- **Audio System** - Sons et musique

---

## 🌐 Protocole réseau

### Communication

- **TCP** pour la connexion initiale et les messages critiques
- **UDP** pour les mises à jour de position en temps réel
- **Compression** des paquets pour optimiser la bande passante
- **Interpolation** pour un gameplay fluide
- **Prédiction côté client** pour réduire la latence

### Format des paquets

```cpp
struct Packet {
    uint32_t type;        // Type de paquet
    uint32_t timestamp;   // Horodatage
    uint32_t entityId;    // ID de l'entité
    uint8_t data[];       // Données variables
};
```

---

## 📚 Structure du projet

```
rtype/
├── client/              # Code du client
│   ├── src/
│   ├── include/
│   └── assets/
├── server/              # Code du serveur
│   ├── src/
│   └── include/
├── engine/              # Moteur ECS
│   ├── src/
│   └── include/
├── common/              # Code partagé
│   ├── protocol/
│   └── utils/
├── docs/                # Documentation
│   ├── images/
│   ├── GUIDE_RAPIDE.md
│   └── IMAGES_SETUP.md
├── tests/               # Tests unitaires
└── CMakeLists.txt
```

---

## 🎯 Fonctionnalités

### ✅ Implémenté

- [x] Moteur ECS complet
- [x] Système de rendu SFML
- [x] Gestion des inputs
- [x] Système de collision
- [x] Réseau UDP/TCP
- [x] Mode solo
- [x] Mode multijoueur
- [x] Lobby système
- [x] Ennemis et IA
- [x] Power-ups
- [x] Système de scoring

### 🚧 En développement

- [ ] Boss de fin de niveau
- [ ] Plus de types d'ennemis
- [ ] Système de sauvegarde
- [ ] Replays
- [ ] Classement en ligne

---

## 🤝 Contribution

Ce projet est un projet académique EPITECH. Les contributions externes ne sont pas acceptées pendant la période de développement.

---

## 📄 Licence

Ce projet est réalisé dans le cadre du cursus EPITECH et est soumis aux règles de l'école.

---

## 📞 Contact

Pour toute question concernant le projet :
- 📧 Email : [alberic.abotchi@epitech.eu](mailto:votre-email@epitech.eu)
- 📧 Email : [axel.ogouchi@epitech.eu](mailto:votre-email@epitech.eu)
- 📧 Email : [pavel.kokou@epitech.eu](mailto:votre-email@epitech.eu)
- 📧 Email : [maurel.kouassi@epitech.eu](mailto:votre-email@epitech.eu)
- 🌐 EPITECH : [www.epitech.eu](https://www.epitech.eu)

---

## 📚 Documentation supplémentaire

 [Guide Rapide](docs/Diagramme.pdf) - Instructions détaillées

[Guide rapide LOgique du jeu serveur ](docs/new_doc_rt.pdf) - Instructions détaillées sur server
[Guide rapide LOgique ecs ](docs/ecs_rtype_doc.pdf) - Instructions détaillées sur serveur
-
---

### ✨ Caractéristiques principales 'TRACK 2 GAME TRACK...'
[🎮 Guide Visuel Interactif - Track 2 Features](https://htmlpreview.github.io/?https://github.com/EpitechPGE3-2025/G-CPP-500-COT-5-1-rtype-23/blob/main/docs/second_part.html) - Présentation interactive des fonctionnalités


<div align="center">

**Fait avec ❤️ par l'équipe R-Type EPITECH**

⭐ N'oubliez pas de star le projet si vous l'aimez ce n est qu un prototype!

</div>
