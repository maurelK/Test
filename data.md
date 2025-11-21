# ACTION-REACTION
## Projet d'Automatisation de Services

**Équipe :**
- [Nom Prénom] - Tech Lead / Backend
- [Nom Prénom] - Frontend Web & Mobile
- [Nom Prénom] - DevOps / Base de données
- [Nom Prénom] - Backend / Intégrations APIs

**Module :** Application Development
**Date :** [Date de défense]
**Version :** 1.0 - Planification initiale


1. Introduction
   1.1 Contexte du projet
   1.2 Objectifs
   1.3 Périmètre fonctionnel

2. Analyse du besoin
   2.1 Étude du cahier des charges
   2.2 Contraintes techniques
   2.3 Contraintes du groupe

3. Analyse technologique
   3.1 État de l'art
   3.2 Comparaison des solutions
   3.3 Choix technologiques finaux
   3.4 Justifications

4. Architecture système
   4.1 Vue d'ensemble
   4.2 Architecture détaillée
   4.3 Diagrammes UML
   4.4 Flux de données

5. Base de données
   5.1 Modèle conceptuel (MCD)
   5.2 Modèle logique (MLD)
   5.3 Schéma relationnel
   5.4 Dictionnaire des données

6. Sécurité
   6.1 Authentification
   6.2 Autorisation
   6.3 Protection des données
   6.4 Gestion des secrets

7. Gestion de projet
   7.1 Organisation de l'équipe
   7.2 Méthodologie
   7.3 Outils de collaboration
   7.4 Planning détaillé
   7.5 Gestion des risques

8. Services et fonctionnalités
   8.1 Services prévus (NBS)
   8.2 Actions prévues (NBA)
   8.3 REActions prévues (NBR)
   8.4 Calcul de conformité

9. Déploiement
   9.1 Architecture Docker
   9.2 Docker Compose
   9.3 Configuration

10. Annexes
    10.1 Glossaire
    10.2 Références
    10.3 Diagrammes complémentaires


# 1. Introduction

## 1.1 Contexte du projet

Le projet ACTION-REACTION s'inscrit dans le cadre du module "Application 
Development" d'Epitech. Il vise à créer une plateforme d'automatisation 
similaire à IFTTT ou Zapier, permettant aux utilisateurs de connecter 
différents services web entre eux via un système d'Actions et de REActions.

### Inspiration
- **IFTTT** (If This Then That) : Pionnier de l'automatisation
- **Zapier** : Solution professionnelle d'automatisation
- **n8n** : Alternative open-source

### Vision du projet
Créer une plateforme permettant à tout utilisateur de :
- Connecter ses comptes de services externes (Gmail, Google Drive, etc.)
- Définir des automatisations personnalisées
- Surveiller et gérer ses workflows automatisés

## 1.2 Objectifs

### Objectifs pédagogiques
- Découvrir et maîtriser un écosystème technologique complet
- Intégrer des bibliothèques et APIs tierces
- Gérer un projet d'envergure en équipe
- Respecter une architecture client-serveur stricte

### Objectifs techniques
- Application server exposant une API REST
- Client web responsive
- Client mobile Android
- Déploiement via Docker Compose
- Support de minimum (1 + X) services
- Support de minimum 3*X actions/reactions

### Objectifs fonctionnels
- Gestion utilisateur complète
- Authentification OAuth2
- Système de hooks automatique
- Interface intuitive

## 1.3 Périmètre fonctionnel

### Inclus dans le MVP (2ème défense)
✅ User management (inscription, connexion)
✅ Authentification username/password
✅ OAuth2 avec au moins 1 provider (Google)
✅ 2-3 services fonctionnels
✅ 6-9 actions/reactions au total
✅ Création d'AREA basique
✅ Système de hooks fonctionnel
✅ API REST documentée
✅ Clients web et mobile de base

### Prévu pour la version finale (Défense finale)
✅ OAuth2 avec multiples providers
✅ 4-6 services
✅ 12-18 actions/reactions
✅ Interface utilisateur polie
✅ Dashboard complet
✅ Administration
✅ Documentation complète
✅ Déploiement Docker optimisé

### Hors périmètre
❌ Notifications push mobiles
❌ Workflows conditionnels complexes (if/else)
❌ Analytics avancées
❌ Marketplace de workflows


# 2. Analyse du besoin

## 2.1 Étude du cahier des charges

### Contraintes architecturales strictes

Le sujet impose une séparation client-serveur **stricte** :

> "No business process will be performed on the web & mobile client side, 
> which only serves as a user interface and redirects requests from/to 
> the application server."

**Implications concrètes :**

| Composant | Responsabilités | Interdit |
|-----------|----------------|----------|
| **Clients** (Flutter) | - Affichage UI<br>- Capture inputs<br>- Appels API REST | - Validation métier<br>- Calculs<br>- Appels APIs externes<br>- Logique de hooks |
| **Server** (NestJS) | - TOUTE logique métier<br>- Validation<br>- Orchestration<br>- Hooks | - Rendu UI<br>- Gestion navigation |
| **Database** (Supabase) | - Stockage données<br>- Persistance | - Logique métier |

### Architecture imposée
```
┌─────────────────────────────────────────┐
│  CLIENT WEB (port 8081)                  │
│  - Interface utilisateur                 │
│  - Appels API uniquement                 │
└────────────┬────────────────────────────┘
             │
             │ HTTP REST
             │
┌────────────▼────────────────────────────┐
│  CLIENT MOBILE                           │
│  - Interface utilisateur                 │
│  - Configuration serveur                 │
│  - Appels API uniquement                 │
└────────────┬────────────────────────────┘
             │
             │ HTTP REST
             │
┌────────────▼────────────────────────────┐
│  APPLICATION SERVER (port 8080)          │
│  ═══════════════════════════════════     │
│  - API REST complète                     │
│  - Endpoint /about.json                  │
│  - TOUTE la logique métier               │
│  - Gestion OAuth2                        │
│  - Exécution des hooks                   │
└────────────┬────────────────────────────┘
             │
             │ SQL
             │
┌────────────▼────────────────────────────┐
│  DATABASE (Supabase PostgreSQL)          │
│  - Stockage données                      │
│  - OAuth providers config                │
└─────────────────────────────────────────┘
```

### Contraintes Docker

Le projet DOIT être déployable via `docker-compose` avec :

**Services obligatoires :**
```yaml
services:
  server:           # Port 8080
  client_web:       # Port 8081
  client_mobile:    # Build APK
```

**Endpoints obligatoires :**
- `http://localhost:8080/about.json` → Info services
- `http://localhost:8081/client.apk` → APK mobile

## 2.2 Contraintes techniques

### Calcul des services requis

Notre groupe : **X = 4 personnes**

**Formules :**
- NBS (Nombre de services) >= 1 + X = **5 services minimum**
- NBA (Nombre d'Actions) + NBR (Nombre de REActions) >= 3 * X = **12 minimum**

**Notre objectif :**

| Phase | Services | Actions | REActions | Total A+R |
|-------|----------|---------|-----------|-----------|
| **MVP** | 3 | 5 | 4 | 9 |
| **Final** | 6 | 9 | 9 | 18 |

✅ MVP dépasse le minimum (3*4 = 12 requis, on fait 9... à ajuster!)
✅ Final dépasse largement les requis

**⚠️ CORRECTION DU TABLEAU MVP:**

| Phase | Services | Actions | REActions | Total A+R |
|-------|----------|---------|-----------|-----------|
| **MVP** | 4 | 7 | 6 | 13 |
| **Final** | 6 | 10 | 10 | 20 |

### Contraintes OAuth2

Le projet requiert :
- Authentification username/password ET OAuth2
- Support de multiples providers (Google, Facebook, GitHub, etc.)
- Gestion des tokens (access + refresh)
- Sécurisation des secrets (variables d'environnement)

### Contraintes de performance

Les **hooks** doivent :
- Vérifier régulièrement les conditions (polling)
- Être performants (milliers d'utilisateurs potentiels)
- Éviter les appels API inutiles
- Gérer les rate limits des services externes

## 2.3 Contraintes du groupe

### Composition de l'équipe

| Membre | Rôle principal | Compétences | Technologies |
|--------|---------------|-------------|--------------|
| [Nom 1] | Tech Lead / Backend | Node.js, Architecture | NestJS, PostgreSQL |
| [Nom 2] | Frontend Lead | Flutter, UI/UX | Flutter, Material Design |
| [Nom 3] | Backend / APIs | REST, OAuth2 | NestJS, Passport.js |
| [Nom 4] | DevOps / Database | Docker, SQL | Docker, Supabase |

### Disponibilités

- **Temps disponible :** 6 semaines (jusqu'à défense finale)
- **Heures / semaine :** ~15h par personne
- **Total :** ~360 heures équipe

### Répartition des tâches

**Sprint 1-2 (MVP):**
- [Nom 1] : Architecture NestJS + API REST
- [Nom 2] : Clients Flutter (web + mobile)
- [Nom 3] : OAuth2 + Intégration services
- [Nom 4] : Database + Docker setup

**Sprint 3-4 (Final):**
- Tous : Ajout de services
- Tous : Tests et optimisation
- Tous : Documentation



# 3. Analyse technologique

## 3.1 État de l'art

### Backend Frameworks

#### Option 1 : Java + Spring Boot
**Description :** Framework enterprise-grade, très utilisé en entreprise

**Avantages :**
✅ Robustesse éprouvée
✅ Écosystème mature (Maven, JUnit)
✅ Performance
✅ Typage fort
✅ Support OAuth2 excellent (Spring Security)

**Inconvénients :**
❌ Verbose (beaucoup de boilerplate)
❌ Courbe d'apprentissage raide
❌ Setup initial lourd
❌ Moins moderne que Node.js

**Note finale : 6/10**

---

#### Option 2 : C# + ASP.NET Core
**Description :** Framework Microsoft moderne et performant

**Avantages :**
✅ Performance excellente
✅ Typage fort avec C#
✅ Excellent tooling (Visual Studio)
✅ Support natif OAuth2
✅ Compatible Windows Mobile

**Inconvénients :**
❌ Moins de bibliothèques pour APIs externes
❌ Écosystème moins riche que Node.js/Java
❌ Moins populaire dans startups
❌ Expertise équipe limitée

**Note finale : 7/10**

---

#### Option 3 : Node.js + NestJS ⭐
**Description :** Framework Node.js moderne inspiré d'Angular

**Avantages :**
✅ Architecture modulaire PARFAITE pour le projet
✅ TypeScript = type safety + DX excellente
✅ Décorateurs élégants (@Get, @Post, @UseGuards)
✅ Écosystème NPM gigantesque (toutes APIs disponibles)
✅ Gestion asynchrone NATIVE (parfait pour hooks)
✅ Passport.js = OAuth2 simplifié
✅ Performance I/O excellente
✅ JSON native = REST API naturel
✅ Documentation excellente
✅ Expertise équipe déjà présente

**Inconvénients :**
❌ Typage moins strict que Java/C# (mais TypeScript compense)
❌ Runtime errors possibles

**Note finale : 9.5/10** ⭐

**Exemple de code :**
```typescript
// Structure élégante et modulaire
@Controller('services')
export class ServicesController {
  @Get()
  async getAllServices() {
    return this.servicesService.findAll();
  }
  
  @Post()
  @UseGuards(JwtAuthGuard)
  async createService(@Body() dto: CreateServiceDto) {
    return this.servicesService.create(dto);
  }
}
```

**Décision : NestJS** ✅

---

### Frontend Frameworks

#### Contrainte : Web ET Mobile

Notre projet nécessite :
- Un client web (port 8081)
- Un client mobile (Android)

**Options :**

1. **Développement séparé**
   - React pour web
   - Android natif (Java/Kotlin) pour mobile
   - ❌ Double travail, double maintenance

2. **Framework hybride**
   - Flutter / React Native
   - ✅ Un seul codebase

---

#### Option 1 : React + React Native
**Description :** JavaScript pour web et mobile

**Avantages :**
✅ Même langage (JS/TS)
✅ Réutilisation de code possible
✅ Communauté énorme
✅ Écosystème riche

**Inconvénients :**
❌ Pas vraiment un seul codebase (React ≠ React Native)
❌ UI différente web vs mobile
❌ Performance mobile moyenne
❌ Setup complexe

**Note finale : 7/10**

---

#### Option 2 : Flutter ⭐
**Description :** Framework Google multiplateforme (Dart)

**Avantages :**
✅ **UN SEUL CODEBASE** pour web + mobile + desktop
✅ Performance NATIVE mobile
✅ Hot reload = développement ultra-rapide
✅ UI consistante toutes plateformes
✅ Material Design intégré
✅ Widgets riches et personnalisables
✅ Dart = langage moderne et propre
✅ Compilation AOT (Ahead Of Time) = rapide
✅ Moins de bugs plateforme-spécifiques
✅ Documentation Google excellente

**Inconvénients :**
❌ Dart = nouveau langage à apprendre
❌ Flutter Web encore jeune (mais suffisant ici)
❌ Taille bundle web importante
❌ SEO limité (mais pas important pour notre app)

**Note finale : 9/10** ⭐

**Exemple de code :**
```dart
// Même code pour web ET mobile !
class LoginScreen extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: Center(
        child: ElevatedButton(
          onPressed: () => AuthService().signInWithGoogle(),
          child: Text('Login with Google'),
        ),
      ),
    );
  }
}
```

**Décision : Flutter** ✅

---

### Base de données

#### Option 1 : PostgreSQL auto-hébergé
**Avantages :**
✅ Gratuit
✅ Contrôle total
✅ Performance

**Inconvénients :**
❌ Setup complexe
❌ Pas d'OAuth intégré
❌ Pas de UI admin

**Note : 6/10**

---

#### Option 2 : Supabase ⭐
**Description :** Backend-as-a-Service basé sur PostgreSQL

**Avantages :**
✅ PostgreSQL puissant
✅ OAuth providers préconfigurés (Google, Facebook, GitHub...)
✅ UI admin incluse
✅ Real-time capabilities (utile pour hooks !)
✅ Row Level Security
✅ API REST auto-générée (mais on ne l'utilisera pas)
✅ Free tier généreux
✅ TypeScript SDK
✅ Migrations faciles

**Inconvénients :**
❌ Dépendance à un service tiers
❌ Peut encourager mauvaises pratiques (accès direct)

**Note : 8.5/10** ⭐

**Comment l'utiliser CORRECTEMENT :**
```typescript
// ✅ Dans NestJS UNIQUEMENT
import { createClient } from '@supabase/supabase-js'

const supabase = createClient(
  process.env.SUPABASE_URL,
  process.env.SUPABASE_SERVICE_KEY // Service role, pas anon
)

// Clients Flutter N'ACCÈDENT JAMAIS directement à Supabase
```

**Décision : Supabase** ✅

---

## 3.2 Tableau comparatif récapitulatif

| Critère | Java/Spring | C#/.NET | Node.js/NestJS |
|---------|-------------|---------|----------------|
| **Architecture modulaire** | 7/10 | 8/10 | **10/10** |
| **Support OAuth2** | 9/10 | 9/10 | **9/10** |
| **Écosystème APIs** | 8/10 | 6/10 | **10/10** |
| **Async/Performance** | 8/10 | 9/10 | **10/10** |
| **Expertise équipe** | 6/10 | 5/10 | **9/10** |
| **Courbe apprentissage** | 5/10 | 6/10 | **9/10** |
| **Documentation** | 8/10 | 8/10 | **9/10** |
| **Adapté au projet** | 7/10 | 7/10 | **10/10** |
| **TOTAL** | **58/80** | **58/80** | **76/80** |

---

| Critère | React + RN | Flutter |
|---------|------------|---------|
| **Codebase unifié** | 6/10 | **10/10** |
| **Performance mobile** | 7/10 | **10/10** |
| **Hot reload** | 8/10 | **10/10** |
| **UI consistency** | 6/10 | **10/10** |
| **Courbe apprentissage** | 8/10 | **7/10** |
| **Écosystème** | 10/10 | **8/10** |
| **Adapté au projet** | 7/10 | **10/10** |
| **TOTAL** | **52/70** | **65/70** |

---

## 3.3 Stack technologique finale

### Vue d'ensemble
```
┌─────────────────────────────────────────┐
│         CLIENTS (Flutter/Dart)           │
│  ────────────────────────────────────   │
│  • Flutter Web (Material Design)         │
│  • Flutter Mobile (Android)              │
│  • Packages : http, flutter_secure_storage│
└────────────┬────────────────────────────┘
             │ HTTP REST / JSON
             │
┌────────────▼────────────────────────────┐
│    APPLICATION SERVER (NestJS/TS)        │
│  ────────────────────────────────────   │
│  • Framework : NestJS 10.x               │
│  • Language : TypeScript 5.x             │
│  • Auth : Passport.js + JWT              │
│  • ORM : Prisma / TypeORM                │
│  • Validation : class-validator          │
│  • APIs : axios, @nestjs/axios           │
│  • Scheduler : @nestjs/schedule (hooks)  │
└────────────┬────────────────────────────┘
             │ Prisma / pg
             │
┌────────────▼────────────────────────────┐
│      DATABASE (Supabase/PostgreSQL)      │
│  ────────────────────────────────────   │
│  • PostgreSQL 15                         │
│  • Supabase Auth (OAuth providers)       │
│  • Supabase Studio (UI admin)            │
└─────────────────────────────────────────┘
```

### Détail des technologies

#### Backend Stack
```json
{
  "framework": "NestJS 10.3.0",
  "runtime": "Node.js 20.x LTS",
  "language": "TypeScript 5.3",
  "packageManager": "npm",
  
  "dependencies": {
    "core": [
      "@nestjs/core",
      "@nestjs/common",
      "@nestjs/platform-express"
    ],
    "auth": [
      "@nestjs/passport",
      "@nestjs/jwt",
      "passport-google-oauth20",
      "passport-facebook",
      "bcrypt"
    ],
    "database": [
      "@nestjs/typeorm",
      "typeorm",
      "pg",
      "@supabase/supabase-js"
    ],
    "validation": [
      "class-validator",
      "class-transformer"
    ],
    "apis": [
      "@nestjs/axios",
      "axios"
    ],
    "scheduling": [
      "@nestjs/schedule"
    ],
    "config": [
      "@nestjs/config",
      "dotenv"
    ]
  },
  
  "devDependencies": [
    "@nestjs/testing",
    "jest",
    "@types/node",
    "ts-node",
    "eslint",
    "prettier"
  ]
}
```

#### Frontend Stack
```yaml
framework: Flutter 3.16.0
language: Dart 3.2
platforms:
  - web
  - android

dependencies:
  # HTTP client
  - http: ^1.1.0
  
  # State management
  - provider: ^6.1.0
  # ou riverpod: ^2.4.0
  
  # Storage
  - flutter_secure_storage: ^9.0.0
  - shared_preferences: ^2.2.0
  
  # OAuth / Auth
  - flutter_web_auth: ^0.5.0
  
  # UI
  - flutter_svg: ^2.0.0
  - google_fonts: ^6.1.0
  
  # Navigation
  - go_router: ^13.0.0
  
  # Forms
  - flutter_form_builder: ^9.1.0

dev_dependencies:
  - flutter_test
  - flutter_lints: ^3.0.0
```

#### DevOps Stack
```yaml
containerization:
  - Docker 24.x
  - Docker Compose 2.x

services:
  server:
    image: node:20-alpine
    build: ./server
    ports:
      - "8080:8080"
    environment:
      - DATABASE_URL
      - JWT_SECRET
      - GOOGLE_CLIENT_ID
      - GOOGLE_CLIENT_SECRET
      
  client_web:
    image: nginx:alpine
    build: ./client
    ports:
      - "8081:80"
    depends_on:
      - server
      - client_mobile
    volumes:
      - mobile_apk:/usr/share/nginx/html/downloads
      
  client_mobile:
    image: cirrusci/flutter:3.16.0
    build: ./client
    volumes:
      - mobile_apk:/app/build/app/outputs/flutter-apk

volumes:
  mobile_apk:
```

## 3.4 Justifications détaillées

### Pourquoi NestJS ?

**1. Architecture modulaire native**

Le projet nécessite une organisation claire :
- Module `auth` (user management + OAuth)
- Module `services` (Google, Facebook, etc.)
- Module `actions` (composants Action)
- Module `reactions` (composants REAction)
- Module `areas` (gestion AREA)
- Module `hooks` (système de polling)

NestJS offre cette structure nativement :
```typescript
src/
├── auth/
│   ├── auth.module.ts
│   ├── auth.controller.ts
│   ├── auth.service.ts
│   ├── strategies/
│   │   ├── jwt.strategy.ts
│   │   ├── google.strategy.ts
│   │   └── facebook.strategy.ts
│   └── guards/
│       └── jwt-auth.guard.ts
├── services/
│   ├── services.module.ts
│   ├── gmail/
│   ├── google-drive/
│   └── facebook/
├── actions/
│   ├── actions.module.ts
│   └── actions.service.ts
├── reactions/
│   ├── reactions.module.ts
│   └── reactions.service.ts
├── areas/
│   ├── areas.module.ts
│   ├── areas.controller.ts
│   └── areas.service.ts
└── hooks/
    ├── hooks.module.ts
    └── hooks.service.ts
```

**2. Dependency Injection native**

Facilite les tests et la maintenabilité :
```typescript
@Injectable()
export class AreasService {
  constructor(
    private readonly actionsService: ActionsService,
    private readonly reactionsService: ReactionsService,
    private readonly hooksService: HooksService,
  ) {}
  
  async createArea(dto: CreateAreaDto) {
    // Services injectés automatiquement
    const action = await this.actionsService.findOne(dto.actionId);
    const reaction = await this.reactionsService.findOne(dto.reactionId);
    // ...
  }
}
```

**3. Décorateurs élégants**

Code lisible et maintenable :
```typescript
@Controller('areas')
@UseGuards(JwtAuthGuard) // Protection globale
export class AreasController {
  
  @Get()
  async findAll(@User() user: UserEntity) {
    return this.areasService.findAllByUser(user.id);
  }
  
  @Post()
  @UsePipes(new ValidationPipe())
  async create(
    @Body() dto: CreateAreaDto,
    @User() user: UserEntity
  ) {
    return this.areasService.create(dto, user);
  }
}
```

**4. Gestion asynchrone native**

Parfait pour les hooks qui doivent interroger des APIs régulièrement :
```typescript
@Injectable()
export class HooksService {
  @Cron('*/5 * * * *') // Toutes les 5 minutes
  async checkAllHooks() {
    const areas = await this.areasService.findAllEnabled();
    
    // Traitement asynchrone en parallèle
    await Promise.all(
      areas.map(area => this.checkArea(area))
    );
  }
  
  private async checkArea(area: Area) {
    const actionTriggered = await this.checkAction(area.action);
    if (actionTriggered) {
      await this.executeReaction(area.reaction);
    }
  }
}
```

**5. Écosystème NPM**

Toutes les APIs externes ont des SDK npm :
- `googleapis` pour Google (Gmail, Drive, Calendar)
- `@microsoft/microsoft-graph-client` pour Microsoft
- `fb` pour Facebook
- `twitter-api-v2` pour X/Twitter
- etc.

### Pourquoi Flutter ?

**1. Productivité maximale**

Un seul code pour web + mobile = **gain de temps de 50%+**
```dart
// Ce widget fonctionne sur WEB et MOBILE sans changement !
class ServiceCard extends StatelessWidget {
  final Service service;
  
  @override
  Widget build(BuildContext context) {
    return Card(
      child: ListTile(
        leading: Image.network(service.iconUrl),
        title: Text(service.name),
        subtitle: Text(service.description),
        trailing: Switch(
          value: service.isConnected,
          onChanged: (value) => _toggleService(value),
        ),
      ),
    );
  }
}
```

**2. Performance native mobile**

Flutter compile en code machine natif (pas de bridge JavaScript comme React Native).

Résultat : **60 FPS constant**, animations fluides.

**3. Hot Reload = développement rapide**

Changement de code visible en **< 1 seconde** sans perdre l'état de l'app.

**4. UI cohérente**

Material Design intégré, widgets identiques sur toutes plateformes.

### Pourquoi Supabase ?

**1. PostgreSQL robuste**

Base relationnelle éprouvée, parfaite pour notre modèle de données.

**2. OAuth préconfigué**

Supabase Auth supporte nativement :
- Google
- Facebook
- GitHub
- Microsoft
- Discord
- Etc.

Configuration simple :
```typescript
// Dans NestJS
const { data } = await supabase.auth.signInWithOAuth({
  provider: 'google',
  options: {
    scopes: 'email profile',
    redirectTo: 'http://localhost:8080/auth/callback'
  }
})
```

**3. Real-time subscriptions**

Utile si on veut notifier les clients en temps réel quand un hook s'exécute :
```typescript
// Écouter les changements dans la table 'areas'
supabase
  .channel('areas')
  .on('postgres_changes', { 
    event: 'UPDATE', 
    schema: 'public', 
    table: 'areas' 
  }, payload => {
    console.log('Area updated:', payload)
  })
  .subscribe()
```

**4. UI Admin gratuite**

Supabase Studio permet de visualiser et modifier la DB sans code.

**5. Migrations versionnées**
```sql
-- migrations/001_initial_schema.sql
CREATE TABLE users (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  email VARCHAR(255) UNIQUE NOT NULL,
  created_at TIMESTAMP DEFAULT NOW()
);
```

---

## 3.5 Alternatives rejetées et pourquoi

### Backend : Pourquoi pas Express.js ?

Express.js est plus léger mais :
❌ Pas d'architecture par défaut → structure manuelle
❌ Pas de decorators → code verbose
❌ Pas de DI native → tests plus difficiles
❌ Moins adapté aux gros projets

### Frontend : Pourquoi pas React Native ?

React Native serait bien mais :
❌ Pas vraiment un seul codebase (React vs RN)
❌ Performances mobiles inférieures à Flutter
❌ UI différente web vs mobile
❌ Bridging JS → plus de bugs potentiels

### Database : Pourquoi pas MongoDB ?

MongoDB (NoSQL) serait inadapté car :
❌ Relations complexes (users ↔ services ↔ areas)
❌ Pas de foreign keys natives
❌ Transactions moins robustes
❌ Pas d'OAuth intégré




# 4. Architecture système

## 4.1 Vue d'ensemble

### Architecture globale en couches
```
┌───────────────────────────────────────────────────────────┐
│                    PRESENTATION LAYER                      │
│  ════════════════════════════════════════════════════     │
│                                                            │
│  ┌──────────────────────┐    ┌─────────────────────────┐ │
│  │   Flutter Web        │    │   Flutter Mobile        │ │
│  │   (port 8081)        │    │   (Android APK)         │ │
│  │                      │    │                         │ │
│  │  • UI Components     │    │  • UI Components        │ │
│  │  • Forms             │    │  • Forms                │ │
│  │  • Navigation        │    │  • Server config        │ │
│  │  • API calls         │    │  • API calls            │ │
│  └──────────┬───────────┘    └──────────┬──────────────┘ │
└─────────────┼──────────────────────────┼─────────────────┘
              │                          │
              │      HTTP REST / JSON    │
              │                          │
┌─────────────▼──────────────────────────▼─────────────────┐
│                   APPLICATION LAYER                       │
│  ════════════════════════════════════════════════════    │
│                                                           │
│              NestJS Application Server                    │
│                    (port 8080)                            │
│                                                           │
│  ┌─────────────────────────────────────────────────────┐ │
│  │  API REST Layer (@Controller)                       │ │
│  │  • /auth/*           • /areas/*                     │ │
│  │  • /services/*       • /about.json                  │ │
│  │  • /actions/*        • /users/*                     │ │
│  │  • /reactions/*                                     │ │
│  └─────────────────────────────────────────────────────┘ │
│                         │                                 │
│  ┌─────────────────────▼───────────────────────────────┐ │
│  │  Business Logic Layer (@Service)                    │ │
│  │                                                     │ │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────┐ │ │
│  │  │ AuthService  │  │AreasService  │  │HooksServ.│ │ │
│  │  └──────────────┘  └──────────────┘  └──────────┘ │ │
│  │                                                     │ │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────┐ │ │
│  │  │ServicesServ. │  │ActionsServ.  │  │Reactions │ │ │
│  │  └──────────────┘  └──────────────┘  └──────────┘ │ │
│  └─────────────────────────────────────────────────────┘ │
│                         │                                 │
│  ┌─────────────────────▼───────────────────────────────┐ │
│  │  Data Access Layer (Repository Pattern)             │ │
│  │  • TypeORM / Prisma                                 │ │
│  │  • Supabase Client                                  │ │
│  └─────────────────────────────────────────────────────┘ │
└─────────────────────────┬───────────────────────────────┘
                          │ SQL / Supabase Client
                          │
┌─────────────────────────▼───────────────────────────────┐
│                    DATA LAYER                            │
│  ════════════════════════════════════════════════════   │
│                                                          │
│              Supabase (PostgreSQL 15)                    │
│                                                          │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌───────────┐ │
│  │  users   │ │ services │ │ actions  │ │ reactions │ │
│  └──────────┘ └──────────┘ └──────────┘ └───────────┘ │
│                                                          │
│  ┌──────────┐ ┌──────────┐ ┌──────────────────────────┐│
│  │  areas   │ │user_serv.│ │ oauth_tokens            ││
│  └──────────┘ └──────────┘ └──────────────────────────┘│
└──────────────────────────────────────────────────────────┘
```

### Services externes intégrés
```
┌─────────────────────────────────────────────────────────┐
│              EXTERNAL SERVICES                          │
│  ════════════════════════════════════════════════════  │
│                                                         │
│  ┌────────────┐  ┌────────────┐  ┌────────────┐       │
│  │  Google    │  │ Facebook   │  │  GitHub    │       │
│  │  • Gmail   │  │ • Graph API│  │  • Repos   │       │
│  │  • Drive   │  │ • Pages    │  │  • Issues  │       │
│  │  • Calendar│  │            │  │  • Stars   │       │
│  └──────┬─────┘  └──────┬─────┘  └──────┬─────┘       │
└─────────┼────────────────┼────────────────┼─────────────┘
          │                │                │
          │   OAuth 2.0    │                │
          └────────────────┴────────────────┘
                           │
                           ▼
          ┌────────────────────────────────┐
          │  NestJS (Backend uniquement)   │
          │  • Gère OAuth flow             │
          │  • Stocke access tokens        │
          │  • Fait les appels API         │
          │  • Gère rate limiting          │
          └────────────────────────────────┘
```

**Clients Flutter N'APPELLENT JAMAIS directement les services externes !**

---

## 4.2 Architecture détaillée du Backend (NestJS)

### Structure modulaire
```
server/
├── src/
│   ├── main.ts                    # Entry point
│   ├── app.module.ts              # Root module
│   │
│   ├── auth/                      # Module d'authentification
│   │   ├── auth.module.ts
│   │   ├── auth.controller.ts     # Endpoints /auth/*
│   │   ├── auth.service.ts        # Logique auth
│   │   ├── dto/
│   │   │   ├── register.dto.ts
│   │   │   ├── login.dto.ts
│   │   │   └── oauth-callback.dto.ts
│   │   ├── strategies/
│   │   │   ├── jwt.strategy.ts
│   │   │   ├── google.strategy.ts
│   │   │   ├── facebook.strategy.ts
│   │   │   └── local.strategy.ts
│   │   ├── guards/
│   │   │   ├── jwt-auth.guard.ts
│   │   │   └── roles.guard.ts
│   │   └── decorators/
│   │       └── user.decorator.ts
│   │
│   ├── users/                     # Module utilisateurs
│   │   ├── users.module.ts
│   │   ├── users.controller.ts
│   │   ├── users.service.ts
│   │   ├── entities/
│   │   │   └── user.entity.ts
│   │   └── dto/
│   │       ├── create-user.dto.ts
│   │       └── update-user.dto.ts
│   │
│   ├── services/                  # Module des services externes
│   │   ├── services.module.ts
│   │   ├── services.controller.ts # /services/*
│   │   ├── services.service.ts    # Liste des services
│   │   ├── entities/
│   │   │   ├── service.entity.ts
│   │   │   └── user-service.entity.ts
│   │   └── integrations/          # Intégrations spécifiques
│   │       ├── gmail/
│   │       │   ├── gmail.service.ts
│   │       │   └── gmail.dto.ts
│   │       ├── google-drive/
│   │       │   ├── drive.service.ts
│   │       │   └── drive.dto.ts
│   │       ├── facebook/
│   │       │   ├── facebook.service.ts
│   │       │   └── facebook.dto.ts
│   │       └── github/
│   │           ├── github.service.ts
│   │           └── github.dto.ts
│   │
│   ├── actions/                   # Module Actions
│   │   ├── actions.module.ts
│   │   ├── actions.controller.ts
│   │   ├── actions.service.ts
│   │   ├── entities/
│   │   │   └── action.entity.ts
│   │   └── implementations/       # Actions spécifiques
│   │       ├── gmail-new-email.action.ts
│   │       ├── drive-new-file.action.ts
│   │       └── github-new-issue.action.ts
│   │
│   ├── reactions/                 # Module REActions
│   │   ├── reactions.module.ts
│   │   ├── reactions.controller.ts
│   │   ├── reactions.service.ts
│   │   ├── entities/
│   │   │   └── reaction.entity.ts
│   │   └── implementations/       # REActions spécifiques
│   │       ├── send-email.reaction.ts
│   │       ├── create-drive-file.reaction.ts
│   │       └── create-github-issue.reaction.ts
│   │
│   ├── areas/                     # Module AREA
│   │   ├── areas.module.ts
│   │   ├── areas.controller.ts    # /areas/*
│   │   ├── areas.service.ts
│   │   ├── entities/
│   │   │   └── area.entity.ts
│   │   └── dto/
│   │       ├── create-area.dto.ts
│   │       └── update-area.dto.ts
│   │
│   ├── hooks/                     # Module Hooks (système de polling)
│   │   ├── hooks.module.ts
│   │   ├── hooks.service.ts       # Logique de vérification
│   │   └── schedulers/
│   │       └── hooks.scheduler.ts # Cron jobs
│   │
│   ├── about/                     # Module /about.json
│   │   ├── about.module.ts
│   │   ├── about.controller.ts
│   │   └── about.service.ts
│   │
│   ├── database/                  # Configuration DB
│   │   ├── database.module.ts
│   │   └── supabase.service.ts
│   │
│   └── common/                    # Utilitaires partagés
│       ├── decorators/
│       ├── filters/
│       ├── interceptors/
│       ├── pipes/
│       └── utils/
│
├── prisma/                        # ORM Prisma (ou TypeORM)
│   ├── schema.prisma
│   └── migrations/
│
├── test/
│   ├── unit/
│   └── e2e/
│
├── .env.example
├── .eslintrc.js
├── .prettierrc
├── nest-cli.json
├── package.json
├── tsconfig.json
└── Dockerfile
```

### Exemple de module complet : AuthModule
```typescript
// auth/auth.module.ts
import { Module } from '@nestjs/common';
import { JwtModule } from '@nestjs/jwt';
import { PassportModule } from '@nestjs/passport';
import { AuthController } from './auth.controller';
import { AuthService } from './auth.service';
import { JwtStrategy } from './strategies/jwt.strategy';
import { GoogleStrategy } from './strategies/google.strategy';
import { FacebookStrategy } from './strategies/facebook.strategy';
import { LocalStrategy } from './strategies/local.strategy';
import { UsersModule } from '../users/users.module';

@Module({
  imports: [
    UsersModule,
    PassportModule,
    JwtModule.register({
      secret: process.env.JWT_SECRET,
      signOptions: { expiresIn: '7d' },
    }),
  ],
  controllers: [AuthController],
  providers: [
    AuthService,
    LocalStrategy,
    JwtStrategy,
    GoogleStrategy,
    FacebookStrategy,
  ],
  exports: [AuthService],
})
export class AuthModule {}
```
```typescript
// auth/auth.controller.ts
import { Controller, Post, Get, Body, UseGuards, Req, Res } from '@nestjs/common';
import { AuthGuard } from '@nestjs/passport';
import { AuthService } from './auth.service';
import { RegisterDto } from './dto/register.dto';
import { LoginDto } from './dto/login.dto';

@Controller('auth')
export class AuthController {
  constructor(private readonly authService: AuthService) {}

  // ✅ Inscription username/password
  @Post('register')
  async register(@Body() registerDto: RegisterDto) {
    return this.authService.register(registerDto);
  }

  // ✅ Login username/password
  @Post('login')
  @UseGuards(AuthGuard('local'))
  async login(@Req() req) {
    return this.authService.login(req.user);
  }

  // ✅ Initier OAuth Google
  @Get('google')
  @UseGuards(AuthGuard('google'))
  async googleAuth() {
    // Passport redirige automatiquement vers Google
  }

  // ✅ Callback OAuth Google
  @Get('google/callback')
  @UseGuards(AuthGuard('google'))
  async googleAuthCallback(@Req() req, @Res() res) {
    const jwt = await this.authService.loginWithOAuth(req.user);
    // Redirige vers le client avec le token
    res.redirect(`http://localhost:8081/auth/success?token=${jwt.access_token}`);
  }

  // ✅ Même chose pour Facebook
  @Get('facebook')
  @UseGuards(AuthGuard('facebook'))
  async facebookAuth() {}

  @Get('facebook/callback')
  @UseGuards(AuthGuard('facebook'))
  async facebookAuthCallback(@Req() req, @Res() res) {
    const jwt = await this.authService.loginWithOAuth(req.user);
    res.redirect(`http://localhost:8081/auth/success?token=${jwt.access_token}`);
  }

  // ✅ Endpoint pour valider un JWT
  @Get('me')
  @UseGuards(AuthGuard('jwt'))
  async getProfile(@Req() req) {
    return req.user;
  }
}
```
```typescript
// auth/auth.service.ts
import { Injectable, UnauthorizedException } from '@nestjs/common';
import { JwtService } from '@nestjs/jwt';
import { UsersService } from '../users/users.service';
import * as bcrypt from 'bcrypt';
import { RegisterDto } from './dto/register.dto';

@Injectable()
export class AuthService {
  constructor(
    private usersService: UsersService,
    private jwtService: JwtService,
  ) {}

  // ✅ Inscription
  async register(registerDto: RegisterDto) {
    const hashedPassword = await bcrypt.hash(registerDto.password, 10);
    
    const user = await this.usersService.create({
      email: registerDto.email,
      password: hashedPassword,
      name: registerDto.name,
    });

    return this.login(user);
  }

  // ✅ Login username/password
  async validateUser(email: string, password: string) {
    const user = await this.usersService.findByEmail(email);
    
    if (user && await bcrypt.compare(password, user.password)) {
      const { password, ...result } = user;
      return result;
    }
    
    throw new UnauthorizedException('Invalid credentials');
  }

  // ✅ Générer JWT
  async login(user: any) {
    const payload = { email: user.email, sub: user.id };
    return {
      access_token: this.jwtService.sign(payload),
      user,
    };
  }

  // ✅ Login via OAuth (Google, Facebook, etc.)
  async loginWithOAuth(oauthUser: any) {
    // Trouve ou crée l'utilisateur
    let user = await this.usersService.findByEmail(oauthUser.email);
    
    if (!user) {
      user = await this.usersService.create({
        email: oauthUser.email,
        name: oauthUser.name,
        oauthProvider: oauthUser.provider,
        oauthId: oauthUser.id,
      });
    }

    // Stocke les tokens OAuth pour appels API futurs
    await this.usersService.updateOAuthTokens(user.id, {
      accessToken: oauthUser.accessToken,
      refreshToken: oauthUser.refreshToken,
    });

    return this.login(user);
  }
}
```

### Flux d'authentification OAuth2
```
┌─────────────┐
│   Flutter   │ User clicks "Login with Google"
└──────┬──────┘
       │
       │ 1. GET http://localhost:8080/auth/google
       │
       ▼
┌──────────────────────────────────────────────┐
│  NestJS: GoogleStrategy intercepte            │
│  Redirige vers Google OAuth consent screen   │
└──────┬───────────────────────────────────────┘
       │
       │ 2. Browser opens Google login
       │
       ▼
┌──────────────────────────────────────────────┐
│  User logs in on Google                       │
│  User accepts permissions                     │
└──────┬───────────────────────────────────────┘
       │
       │ 3. Google redirects to callback
       │    with authorization code
       │
       ▼
┌──────────────────────────────────────────────┐
│  GET /auth/google/callback?code=ABC123        │
│                                               │
│  NestJS GoogleStrategy:                       │
│  • Exchange code for access_token             │
│  • Get user profile from Google               │
│  • Call authService.loginWithOAuth()          │
│    ├── Find or create user in DB              │
│    ├── Store OAuth tokens                     │
│    └── Generate JWT                           │
│                                               │
│  • Redirect to client with JWT                │
└──────┬───────────────────────────────────────┘
       │
       │ 4. Redirect http://localhost:8081/auth/success?token=JWT_TOKEN
       │
       ▼
┌──────────────────────────────────────────────┐
│  Flutter extracts JWT from URL                │
│  Stores JWT in secure storage                 │
│  Navigates to dashboard                       │
└───────────────────────────────────────────────┘
```

---

## 4.3 Architecture détaillée du Frontend (Flutter)

### Structure du projet Flutter
```
client/
├── lib/
│   ├── main.dart                      # Entry point
│   │
│   ├── core/                          # Core functionality
│   │   ├── constants/
│   │   │   ├── api_constants.dart     # API URLs
│   │   │   └── app_constants.dart
│   │   ├── theme/
│   │   │   ├── app_theme.dart
│   │   │   └── colors.dart
│   │   ├── utils/
│   │   │   ├── validators.dart
│   │   │   └── formatters.dart
│   │   └── router/
│   │       └── app_router.dart        # Navigation routes
│   │
│   ├── data/                          # Data layer
│   │   ├── models/
│   │   │   ├── user.dart
│   │   │   ├── service.dart
│   │   │   ├── action.dart
│   │   │   ├── reaction.dart
│   │   │   └── area.dart
│   │   ├── repositories/
│   │   │   ├── auth_repository.dart
│   │   │   ├── services_repository.dart
│   │   │   ├── actions_repository.dart
│   │   │   ├── reactions_repository.dart
│   │   │   └── areas_repository.dart
│   │   └── providers/
│   │       ├── api_client.dart         # HTTP client wrapper
│   │       └── storage_provider.dart   # Secure storage
│   │
│   ├── features/                      # Features (par écran)
│   │   ├── auth/
│   │   │   ├── screens/
│   │   │   │   ├── login_screen.dart
│   │   │   │   ├── register_screen.dart
│   │   │   │   └── oauth_callback_screen.dart
│   │   │   ├── widgets/
│   │   │   │   ├── login_form.dart
│   │   │   │   └── oauth_buttons.dart
│   │   │   └── providers/
│   │   │       └── auth_provider.dart
│   │   │
│   │   ├── dashboard/
│   │   │   ├── screens/
│   │   │   │   └── dashboard_screen.dart
│   │   │   └── widgets/
│   │   │       ├── areas_list.dart
│   │   │       └── stats_card.dart
│   │   │
│   │   ├── services/
│   │   │   ├── screens/
│   │   │   │   ├── services_list_screen.dart
│   │   │   │   └── service_detail_screen.dart
│   │   │   └── widgets/
│   │   │       └── service_card.dart
│   │   │
│   │   ├── areas/
│   │   │   ├── screens/
│   │   │   │   ├── areas_list_screen.dart
│   │   │   │   ├── create_area_screen.dart
│   │   │   │   └── area_detail_screen.dart
│   │   │   └── widgets/
│   │   │       ├── area_card.dart
│   │   │       ├── action_selector.dart
│   │   │       └── reaction_selector.dart
│   │   │
│   │   └── profile/
│   │       ├── screens/
│   │       │   └── profile_screen.dart
│   │       └── widgets/
│   │           └── settings_list.dart
│   │
│   └── shared/                        # Shared widgets
│       ├── widgets/
│       │   ├── custom_button.dart
│       │   ├── custom_text_field.dart
│       │   ├── loading_indicator.dart
│       │   └── error_message.dart
│       └── layouts/
│           └── responsive_layout.dart
│
├── assets/
│   ├── images/
│   ├── icons/
│   └── fonts/
│
├── android/                           # Android specific
│   ├── app/
│   │   ├── build.gradle
│   │   └── src/
│   └── gradle.properties
│
├── web/                               # Web specific
│   ├── index.html
│   └── manifest.json
│
├── test/
│   ├── unit/
│   └── widget/
│
├── pubspec.yaml
└── Dockerfile
```

### Exemple d'écran : Login
```dart
// features/auth/screens/login_screen.dart
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import '../providers/auth_provider.dart';
import '../widgets/oauth_buttons.dart';

class LoginScreen extends StatefulWidget {
  @override
  _LoginScreenState createState() => _LoginScreenState();
}

class _LoginScreenState extends State<LoginScreen> {
  final _formKey = GlobalKey<FormState>();
  final _emailController = TextEditingController();
  final _passwordController = TextEditingController();

  @override
  Widget build(BuildContext context) {
    final authProvider = Provider.of<AuthProvider>(context);

    return Scaffold(
      body: Center(
        child: SingleChildScrollView(
          padding: EdgeInsets.all(24),
          child: Card(
            child: Padding(
              padding: EdgeInsets.all(32),
              child: Form(
                key: _formKey,
                child: Column(
                  mainAxisSize: MainAxisSize.min,
                  children: [
                    // Logo
                    FlutterLogo(size: 80),
                    SizedBox(height: 32),
                    
                    // Title
                    Text(
                      'ACTION-REACTION',
                      style: Theme.of(context).textTheme.headlineMedium,
                    ),
                    SizedBox(height: 32),
                    
                    // Email field
                    TextFormField(
                      controller: _emailController,
                      decoration: InputDecoration(
                        labelText: 'Email',
                        prefixIcon: Icon(Icons.email),
                      ),
                      keyboardType: TextInputType.emailAddress,
                      validator: (value) {
                        if (value == null || value.isEmpty) {
                          return 'Please enter your email';
                        }
                        return null;
                      },
                    ),
                    SizedBox(height: 16),
                    
                    // Password field
                    TextFormField(
                      controller: _passwordController,
                      decoration: InputDecoration(
                        labelText: 'Password',
                        prefixIcon: Icon(Icons.lock),
                      ),
                      obscureText: true,
                      validator: (value) {
                        if (value == null || value.isEmpty) {
                          return 'Please enter your password';
                        }
                        return null;
                      },
                    ),
                    SizedBox(height: 24),
                    
                    // Login button
                    SizedBox(
                      width: double.infinity,
                      child: ElevatedButton(
                        onPressed: authProvider.isLoading
                            ? null
                            : () => _handleLogin(authProvider),
                        child: authProvider.isLoading
                            ? CircularProgressIndicator()
                            : Text('Login'),
                      ),
                    ),
                    
                    SizedBox(height: 16),
                    
                    // Divider
                    Row(
                      children: [
                        Expanded(child: Divider()),
                        Padding(
                          padding: EdgeInsets.symmetric(horizontal: 16),
                          child: Text('OR'),
                        ),
                        Expanded(child: Divider()),
                      ],
                    ),
                    
                    SizedBox(height: 16),
                    
                    // OAuth buttons
                    OAuthButtons(),
                    
                    SizedBox(height: 16),
                    
                    // Register link
                    TextButton(
                      onPressed: () => Navigator.pushNamed(context, '/register'),
                      child: Text('Don\'t have an account? Register'),
                    ),
                    
                    // Error message
                    if (authProvider.error != null) ...[
                      SizedBox(height: 16),
                      Text(
                        authProvider.error!,
                        style: TextStyle(color: Colors.red),
                      ),
                    ],
                  ],
                ),
              ),
            ),
          ),
        ),
      ),
    );
  }

  void _handleLogin(AuthProvider authProvider) async {
    if (_formKey.currentState!.validate()) {
      final success = await authProvider.login(
        _emailController.text,
        _passwordController.text,
      );
      
      if (success) {
        Navigator.pushReplacementNamed(context, '/dashboard');
      }
    }
  }

  @override
  void dispose() {
    _emailController.dispose();
    _passwordController.dispose();
    super.dispose();
  }
}
```
```dart
// features/auth/widgets/oauth_buttons.dart
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import '../providers/auth_provider.dart';

class OAuthButtons extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    final authProvider = Provider.of<AuthProvider>(context);

    return Column(
      children: [
        // Google
        SizedBox(
          width: double.infinity,
          child: OutlinedButton.icon(
            icon: Image.asset('assets/icons/google.png', height: 24),
            label: Text('Continue with Google'),
            onPressed: () => authProvider.signInWithGoogle(),
          ),
        ),
        
        SizedBox(height: 12),
        
        // Facebook
        SizedBox(
          width: double.infinity,
          child: OutlinedButton.icon(
            icon: Image.asset('assets/icons/facebook.png', height: 24),
            label: Text('Continue with Facebook'),
            onPressed: () => authProvider.signInWithFacebook(),
          ),
        ),
        
        SizedBox(height: 12),
        
        // GitHub
        SizedBox(
          width: double.infinity,
          child: OutlinedButton.icon(
            icon: Image.asset('assets/icons/github.png', height: 24),
            label: Text('Continue with GitHub'),
            onPressed: () => authProvider.signInWithGitHub(),
          ),
        ),
      ],
    );
  }
}
```
```dart
// features/auth/providers/auth_provider.dart
import 'package:flutter/foundation.dart';
import 'package:flutter_web_auth/flutter_web_auth.dart';
import '../../../data/repositories/auth_repository.dart';
import '../../../data/providers/storage_provider.dart';

class AuthProvider with ChangeNotifier {
  final AuthRepository _authRepository;
  final StorageProvider _storageProvider;
  
  bool _isLoading = false;
  String? _error;
  
  bool get isLoading => _isLoading;
  String? get error => _error;
  
  AuthProvider(this._authRepository, this._storageProvider);
  
  // ✅ Login username/password
  Future<bool> login(String email, String password) async {
    _isLoading = true;
    _error = null;
    notifyListeners();
    
    try {
      final response = await _authRepository.login(email, password);
      await _storageProvider.saveToken(response['access_token']);
      _isLoading = false;
      notifyListeners();
      return true;
    } catch (e) {
      _error = e.toString();
      _isLoading = false;
      notifyListeners();
      return false;
    }
  }
  
  // ✅ OAuth Google
  Future<bool> signInWithGoogle() async {
    _isLoading = true;
    _error = null;
    notifyListeners();
    
    try {
      // 1. Ouvre le flow OAuth via browser
      final result = await FlutterWebAuth.authenticate(
        url: 'http://localhost:8080/auth/google',
        callbackUrlScheme: 'actionreaction',
      );
      
      // 2. Extrait le token de l'URL de callback
      final token = Uri.parse(result).queryParameters['token'];
      
      if (token != null) {
        await _storageProvider.saveToken(token);
        _isLoading = false;
        notifyListeners();
        return true;
      }
      
      throw Exception('No token received');
    } catch (e) {
      _error = 'Google login failed: ${e.toString()}';
      _isLoading = false;
      notifyListeners();
      return false;
    }
  }
  
  // ✅ OAuth Facebook (similaire)
  Future<bool> signInWithFacebook() async {
    // Même logique que Google
    // ...
  }
  
  // ✅ OAuth GitHub (similaire)
  Future<bool> signInWithGitHub() async {
    // Même logique que Google
    // ...
  }
  
  // ✅ Logout
  Future<void> logout() async {
    await _storageProvider.deleteToken();
    notifyListeners();
  }
}
```
```dart
// data/repositories/auth_repository.dart
import '../providers/api_client.dart';

class AuthRepository {
  final ApiClient _apiClient;
  
  AuthRepository(this._apiClient);
  
  // ✅ Login
  Future<Map<String, dynamic>> login(String email, String password) async {
    final response = await _apiClient.post('/auth/login', {
      'email': email,
      'password': password,
    });
    
    return response.data;
  }
  
  // ✅ Register
  Future<Map<String, dynamic>> register(String email, String password, String name) async {
    final response = await _apiClient.post('/auth/register', {
      'email': email,
      'password': password,
      'name': name,
    });
    
    return response.data;
  }
  
  // ✅ Get current user
  Future<Map<String, dynamic>> getCurrentUser(String token) async {
    final response = await _apiClient.get(
      '/auth/me',
      headers: {'Authorization': 'Bearer $token'},
    );
    
    return response.data;
  }
}
```
```dart
// data/providers/api_client.dart
import 'package:http/http.dart' as http;
import 'dart:convert';

class ApiClient {
  final String baseUrl;
  
  ApiClient({required this.baseUrl});
  
  Future<dynamic> get(String path, {Map<String, String>? headers}) async {
    final response = await http.get(
      Uri.parse('$baseUrl$path'),
      headers: headers ?? {'Content-Type': 'application/json'},
    );
    
    if (response.statusCode >= 200 && response.statusCode < 300) {
      return json.decode(response.body);
    } else {
      throw Exception('API Error: ${response.statusCode}');
    }
  }
  
  Future<dynamic> post(String path, Map<String, dynamic> body, {Map<String, String>? headers}) async {
    final response = await http.post(
      Uri.parse('$baseUrl$path'),
      headers: headers ?? {'Content-Type': 'application/json'},
      body: json.encode(body),
    );
    
    if (response.statusCode >= 200 && response.statusCode < 300) {
      return json.decode(response.body);
    } else {
      throw Exception('API Error: ${response.statusCode}');
    }
  }
  
  // put, delete, etc...
}
```

---

## 4.4 Diagrammes UML

### Diagramme de classes principal
```
┌─────────────────────────────────────────────────────────────┐
│                          User                               │
├─────────────────────────────────────────────────────────────┤
│ - id: UUID                                                   │
│ - email: string                                              │
│ - password: string (hashed)                                  │
│ - name: string                                               │
│ - createdAt: DateTime                                        │
│ - updatedAt: DateTime                                        │
├─────────────────────────────────────────────────────────────┤
│ + register()                                                 │
│ + login()                                                    │
│ + updateProfile()                                            │
└────────────┬────────────────────────────────────────────────┘
             │ 1
             │
             │ *
┌────────────▼────────────────────────────────────────────────┐
│                      UserService                            │
├─────────────────────────────────────────────────────────────┤
│ - id: UUID                                                   │
│ - userId: UUID (FK)                                          │
│ - serviceId: UUID (FK)                                       │
│ - accessToken: string (encrypted)                            │
│ - refreshToken: string (encrypted)                           │
│ - expiresAt: DateTime                                        │
│ - isActive: boolean                                          │
├─────────────────────────────────────────────────────────────┤
│ + connect()                                                  │
│ + disconnect()                                               │
│ + refreshToken()                                             │
└────────────┬───────────────┬────────────────────────────────┘
             │ *             │ 1
             │               │
┌────────────▼──────┐  ┌────▼────────────────────────────────┐
│      Service      │  │            Area                      │
├───────────────────┤  ├─────────────────────────────────────┤
│ - id: UUID        │  │ - id: UUID                           │
│ - name: string    │  │ - userId: UUID (FK)                  │
│ - iconUrl: string │  │ - actionId: UUID (FK)                │
│ - description     │  │ - reactionId: UUID (FK)              │
│ - oauthProvider   │  │ - isEnabled: boolean                 │
│                   │  │ - lastTriggered: DateTime            │
│                   │  │ - createdAt: DateTime                │
│                   │  ├─────────────────────────────────────┤
│                   │  │ + create()                           │
│                   │  │ + update()                           │
│                   │  │ + delete()                           │
│                   │  │ + toggle()                           │
└───────┬───────────┘  └────┬───────────────┬────────────────┘
        │ 1                 │ *             │ *
        │                   │               │
        │ *                 │               │
┌───────▼───────────┐ ┌─────▼────────┐ ┌──▼─────────────────┐
│      Action       │ │   Reaction   │ │       Hook         │
├───────────────────┤ ├──────────────┤ ├────────────────────┤
│ - id: UUID        │ │ - id: UUID   │ │ - areaId: UUID (FK)│
│ - serviceId: UUID │ │ - serviceId  │ │ - lastChecked      │
│ - name: string    │ │ - name       │ │ - nextCheck        │
│ - description     │ │ - description│ │ - interval         │
│ - parameters: JSON│ │ - parameters │ │ - state: JSON      │
├───────────────────┤ ├──────────────┤ ├────────────────────┤
│ + check()         │ │ + execute()  │ │ + schedule()       │
│ + validate()      │ │ + validate() │ │ + check()          │
└───────────────────┘ └──────────────┘ │ + trigger()        │
                                        └────────────────────┘
```

### Diagramme de séquence : Création d'une AREA
```
User          Flutter         NestJS          ActionsServ    ReactionsServ    AreasServ      Database
 │               │               │                  │              │              │              │
 │ Click "Create AREA"           │                  │              │              │              │
 ├──────────────>│               │                  │              │              │              │
 │               │               │                  │              │              │              │
 │               │ Select Action │                  │              │              │              │
 │               ├──────────────>│                  │              │              │              │
 │               │               │ GET /actions     │              │              │              │
 │               │               ├─────────────────>│              │              │              │
 │               │               │                  │ Query actions│              │              │
 │               │               │                  ├─────────────────────────────>│              │
 │               │               │                  │<─────────────────────────────┤              │
 │               │<──────────────┤<─────────────────┤              │              │              │
 │               │ [List actions]│                  │              │              │              │
 │               │               │                  │              │              │              │
 │               │ Select REAction                  │              │              │              │
 │               ├──────────────>│                  │              │              │              │
 │               │               │ GET /reactions   │              │              │              │
 │               │               ├───────────────────────────────>│              │              │
 │               │               │                  │              │ Query reactions             │
 │               │               │                  │              ├─────────────────────────────>│
 │               │               │                  │              │<─────────────────────────────┤
 │               │<──────────────┤<──────────────────────────────┤              │              │
 │               │ [List reactions]                 │              │              │              │
 │               │               │                  │              │              │              │
 │               │ Submit AREA   │                  │              │              │              │
 │               ├──────────────>│ POST /areas      │              │              │              │
 │               │               ├──────────────────────────────────────────────>│              │
 │               │               │                  │              │              │ Validate     │
 │               │               │                  │<──────────────────────────┤              │
 │               │               │                  │ Check action │              │              │
 │               │               │                  ├──────>       │              │              │
 │               │               │                  │<──────       │              │              │
 │               │               │                  │              │<──────────────────────────┤
 │               │               │                  │              │ Check reaction            │
 │               │               │                  │              ├──────>                    │
 │               │               │                  │              │<──────                    │
 │               │               │                  │              │              │ Create AREA │
 │               │               │                  │              │              ├────────────>│
 │               │               │                  │              │              │<────────────┤
 │               │               │                  │              │              │ Create Hook │
 │               │               │                  │              │              ├────────────>│
 │               │               │                  │              │              │<────────────┤
 │               │<─────────────────────────────────────────────────────────────┤              │
 │<──────────────┤ [AREA created]│                  │              │              │              │
 │               │               │                  │              │              │              │
```

### Diagramme de séquence : Exécution d'un Hook
```
HookScheduler    HooksService    AreasService    ActionImpl    ReactionImpl    ExternalAPI    Database
      │               │               │               │               │               │           │
      │ Cron trigger  │               │               │               │               │           │
      │ (every 5 min) │               │               │               │               │           │
      ├──────────────>│               │               │               │               │           │
      │               │ Get enabled AREAs             │               │               │           │
      │               ├──────────────>│               │               │               │           │
      │               │               │ Query enabled │               │               │           │
      │               │               ├───────────────────────────────────────────────────────────>│
      │               │               │<───────────────────────────────────────────────────────────┤
      │               │<──────────────┤ [List of AREAs]               │               │           │
      │               │               │               │               │               │           │
      │               │ For each AREA │               │               │               │           │
      │               ├───────────────────────────────>│               │               │           │
      │               │               │ Check action  │               │               │           │
      │               │               │               ├──────────────────────────────>│           │
      │               │               │               │ API call (check Gmail, etc.)  │           │
      │               │               │               │<──────────────────────────────┤           │
      │               │               │<──────────────┤ [Triggered: true]             │           │
      │               │               │               │               │               │           │
      │               │               │ IF triggered  │               │               │           │
      │               │               ├───────────────────────────────>│               │           │
      │               │               │               │ Execute reaction              │           │
      │               │               │               │               ├──────────────>│           │
      │               │               │               │               │ API call (send email, etc)│
      │               │               │               │               │<──────────────┤           │
      │               │               │               │<──────────────┤               │           │
      │               │               │ Update lastTriggered                          │           │
      │               │               ├───────────────────────────────────────────────────────────>│
      │               │               │<───────────────────────────────────────────────────────────┤
      │               │<──────────────┤               │               │               │           │
      │<──────────────┤               │               │               │               │           │
      │               │               │               │               │               │           │
```

---

## 4.5 Diagramme d'architecture Docker

┌─────────────────────────────────────────────────────────────────────────┐ │ Docker Compose │ │ ═════════════════════════════════════════════════════════════════════ │ │ │ │ ┌────────────────────────────────────────────────────────────────┐ │ │ │ SERVICE: server │ │ │ │ ════════════════ │ │ │ │ Image: node:20-alpine │ │ │ │ Build: ./server │ │ │ │ Port: 8080:8080 │ │ │ │ Environment: │ │ │ │ - DATABASE_URL │ │ │ │ - JWT_SECRET │ │ │ │ - GOOGLE_CLIENT_ID │ │ │ │ - GOOGLE_CLIENT_SECRET │ │ │ │ Command: npm run start:prod │ │ │ └────────────────────────────────────────────────────────────────┘ │ │ │ │ ┌────────────────────────────────────────────────────────────────┐ │ │ │ SERVICE: client_mobile │ │ │ │ ════════════════════════ │ │ │ │ Image: cirrusci/flutter:3.16.0 │ │ │ │ Build: ./client │ │ │ │ Command: flutter build apk --release │ │ │ │ Volumes: │ │ │ │ - mobile_apk:/app/build/app/outputs/flutter-apk │ │ │ └────────────────────────────────────────────────────────────────┘ │ │ │ │ ┌────────────────────────────────────────────────────────────────┐ │ │ │ SERVICE: client_web │ │ │ │ ════════════════════ │ │ │ │ Image: nginx:alpine │ │ │ │ Build: ./client │ │ │ │ Port: 8081:80 │ │ │ │ Depends_on: │ │ │ │ - server │ │ │ │ - client_mobile │ │ │ │ Volumes: │ │ │ │ - mobile_apk:/usr/share/nginx/html/downloads │ │ │ │ Command: nginx -g 'daemon off;' │ │ │ └────────────────────────────────────────────────────────────────┘ │ │ │ │ ┌────────────────────────────────────────────────────────────────┐ │ │ │ VOLUME: mobile_apk │ │ │ │ Partagé entre client_mobile et client_web │ │ │ └────────────────────────────────────────────────────────────────┘ │ │ │ └─────────────────────────────────────────────────────────────────────────┘






