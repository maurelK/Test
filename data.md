<div align="center">

#  A-REA - Planning Complet du Projet

</div>
<div align="center">

> **Plateforme d'Automatisation de Services** | EPITECH G-DEV-500  
</div>


<div align="center">

![Status](https://img.shields.io/badge/Status-En%20Cours-yellow) 
![Sprint](https://img.shields.io/badge/Sprint-2%2F4-blue) 
![Progression](https://img.shields.io/badge/Progression-45%25-orange) 
![Coverage](https://img.shields.io/badge/Coverage-82%25-brightgreen)

**Stack Technique:** NestJS · Flutter (Web + Mobile) · PostgreSQL · Docker
<div align="center">

![NestJS](https://img.shields.io/badge/NestJS-E0234E?logo=nestjs&logoColor=white)
![Flutter](https://img.shields.io/badge/Flutter-02569B?logo=flutter&logoColor=white)
![PostgreSQL](https://img.shields.io/badge/PostgreSQL-316192?logo=postgresql&logoColor=white)
![Docker](https://img.shields.io/badge/Docker-2496ED?logo=docker&logoColor=white)
![Prisma](https://img.shields.io/badge/Prisma-5.0-blue?logo=prisma&logoColor=white)
![Supabase](https://img.shields.io/badge/Supabase-3ECF8E?logo=supabase&logoColor=black)
</div>


</div>

---

## 📋 Navigation Rapide

- [1. Vue d'Ensemble](#1-vue-densemble)
- [2. Stack Technologique & Étude Comparative](#2-stack-technologique--étude-comparative)
- [3. Architecture Système](#3-architecture-système)
- [4. Base de Données](#4-base-de-données)
- [5. Sécurité](#5-sécurité)
- [6. Système de Hooks](#6-système-de-hooks)
- [7. Gestion de Projet](#7-gestion-de-projet)
- [8. WBS & Gantt](#8-wbs--gantt)
- [9. Services & Fonctionnalités](#9-services--fonctionnalités)
- [10. Déploiement](#10-déploiement)
- [11. Tests](#11-tests)
- [12. Documentation](#12-documentation)
- [13. Annexes](#13-annexes)

---

# 1. Vue d'Ensemble

## Contexte
Plateforme d'automatisation de services (type IFTTT/Zapier) développée dans le cadre du module Application Development d'Epitech.

## Objectifs
-  API REST (NestJS + PostgreSQL)
-  Client Web (Flutter Web)
-  Client Mobile (Flutter - APK Android)
-  Authentification OAuth2 + JWT
-  Intégrations de services (5+ services)
-  Système de hooks automatisé

## Exigences Projet (EPITECH T-WEB-600)

| Exigence | Formule | Cible | Actuel |
|----------|---------|-------|--------|
| Services (NBS) | ≥ 1 + X | 5 | 3 (60%) |
| Actions (NBA) | ≥ 3 × X | 13 | 9 (69%) |
| REActions (NBR) | ≥ 3 × X | 9 | 6 (67%) |

**Conformité MVP:** 🟡 65% (Cible Défense 2: 70%)

---

# 2. Stack Technologique & Étude Comparative

## 2.1 Stack Sélectionnée

### Backend
```yaml
Runtime: Node.js 20 LTS
Framework: NestJS 10
Langage: TypeScript 5
Base de données: PostgreSQL 15 (Supabase)
ORM: Prisma 5
Auth: JWT + Passport OAuth2
```

### Frontend
```yaml
Web: Flutter 3.16 (compilation Web)
Mobile: Flutter 3.16 (APK Android)
UI: Material Design / Cupertino
State Management: Provider / Riverpod
HTTP Client: dio
Storage: shared_preferences / flutter_secure_storage
```

**Avantages Flutter Web + Mobile:**
-  **Code partagé**: 90%+ du code entre web et mobile
-  **Maintenance simplifiée**: Un seul codebase
-  **Cohérence UI/UX**: Design identique toutes plateformes
-  **Vélocité développement**: Pas besoin d'apprendre React en plus
-  **Hot Reload**: Développement rapide web et mobile
-  **Performance**: Compilation native (Web Assembly + AOT)

### DevOps
```yaml
Conteneurisation: Docker 24
Orchestration: Docker Compose 2
CI/CD: GitHub Actions
Testing: Jest + Supertest (Backend) / Flutter Test (Frontend)
```

## 2.2 Étude Comparative Détaillée

### Frameworks Backend

| Critère | NestJS | Express.js | Fastify | Vainqueur |
|---------|--------|-----------|---------|-----------|
| **Support TypeScript** | ✅ Natif | 🟡 Via @types | ✅ Natif | NestJS |
| **Architecture** | ✅ Modulaire (DI) | ❌ Libre | 🟡 Plugins | **NestJS** ✅ |
| **Écosystème** | ✅ Riche | ✅ Immense | 🟡 Moyen | NestJS |
| **Performance** | 🟡 Bon | 🟡 Bon | ✅ Excellent | Fastify |
| **Courbe apprentissage** | 🟡 Moyenne | ✅ Faible | 🟡 Moyenne | Express |
| **Documentation** | ✅ Excellente | ✅ Bonne | 🟡 Moyenne | NestJS |
| **Enterprise Ready** | ✅ Oui | ❌ Non | 🟡 Partiel | **NestJS** ✅ |
| **Adoption équipe** | ✅ Adapté | 🟡 Trop libre | 🟡 Peu connu | **NestJS** ✅ |

**Score Final:** NestJS 8/10 | Express 6/10 | Fastify 7/10

**Verdict: NestJS** 
- Architecture modulaire parfaite pour service-based system
- TypeScript first-class citizen
- Patterns clairs (controllers, services, modules)
- Équipe peut collaborer efficacement avec structure définie

### Frameworks Frontend

| Critère | Flutter | React Native | Native Android | Vainqueur |
|---------|---------|--------------|----------------|-----------|
| **Cross-platform** | ✅ Web+iOS+Android | 🟡 iOS+Android | ❌ Android only | **Flutter** ✅ |
| **Performance** | ✅ Native (Dart AOT) | 🟡 Bridge JS | ✅ Native | Flutter |
| **Partage de code** | ✅ 95%+ | 🟡 70-80% | ❌ 0% | **Flutter** ✅ |
| **Qualité UI** | ✅ Pixel-perfect | 🟡 Widgets platform | ✅ Native | Flutter |
| **Courbe apprentissage** | 🟡 Dart (nouveau) | ✅ JavaScript | 🟡 Kotlin | React Native |
| **Hot Reload** | ✅ Instantané | ✅ Bon | 🟡 Lent | **Flutter** ✅ |
| **Écosystème** | ✅ Riche (pub.dev) | ✅ Riche (npm) | ✅ Riche | Égalité |
| **Taille app** | 🟡 15-20 MB | 🟡 20-30 MB | ✅ 5-10 MB | Native |
| **Web Support** | ✅ Production ready | ❌ Experimental | ❌ N/A | **Flutter** ✅ |

**Score Final:** Flutter 9/10 | React Native 7/10 | Native 6/10

**Verdict: Flutter** 
- **Avantage majeur**: Un seul codebase pour Web + Mobile
- Performance native excellente
- UI moderne out-of-the-box
- Équipe confortable avec Dart (similaire TypeScript)
- Web compilation stable (Flutter 3.16+)

### Bases de Données

| Critère | PostgreSQL | MongoDB | MySQL | Vainqueur |
|---------|-----------|---------|-------|-----------|
| **Relations** | ✅ Excellent | ❌ Pas de JOIN | ✅ Bon | **PostgreSQL** ✅ |
| **Support JSON** | ✅ JSONB natif | ✅ Natif | 🟡 JSON basique | PostgreSQL |
| **Performance** | ✅ Excellent | ✅ Excellent | ✅ Excellent | Égalité |
| **Transactions** | ✅ ACID complet | 🟡 Limité | ✅ ACID | PostgreSQL |
| **Scaling** | ✅ Vert+Horiz | ✅ Horizontal | ✅ Vertical | MongoDB |
| **Maturité** | ✅ 30+ ans | 🟡 Récent | ✅ 25+ ans | PostgreSQL |
| **Solution hébergée** | ✅ Supabase | ✅ MongoDB Atlas | 🟡 Options limitées | **Supabase** ✅ |
| **Auth intégrée** | ✅ Supabase | ❌ Non | ❌ Non | **Supabase** ✅ |

**Score Final:** PostgreSQL 9/10 | MongoDB 7/10 | MySQL 6/10

**Verdict: PostgreSQL (Supabase)** 
- Relations users ↔ areas ↔ services essentielles pour notre modèle
- JSONB parfait pour stocker action/reaction configs dynamiques
- Supabase = PostgreSQL + Auth OAuth2 + Storage gratuit
- Hosted solution évite setup serveur

### Bibliothèques OAuth

| Critère | Passport.js | Auth0 | NextAuth.js | Vainqueur |
|---------|------------|-------|-------------|-----------|
| **Intégration NestJS** | ✅ Native | 🟡 SDK | ❌ Next.js only | **Passport** ✅ |
| **Providers disponibles** | ✅ 500+ strategies | ✅ Tous | 🟡 Principaux | Passport |
| **Flexibilité** | ✅ Contrôle total | 🟡 Managed | 🟡 Limité | **Passport** ✅ |
| **Complexité setup** | 🟡 Moyenne | ✅ Simple | ✅ Simple | Auth0 |
| **Coût** | ✅ Gratuit | 🟡 Payant (>10k users) | ✅ Gratuit | **Passport** ✅ |
| **Documentation** | ✅ Excellente | ✅ Excellente | ✅ Bonne | Égalité |

**Score Final:** Passport.js 8/10 | Auth0 7/10 | NextAuth 5/10

**Verdict: Passport.js** 
- Gratuit, flexible, intégration NestJS parfaite
- Contrôle total sur le flow OAuth
- Stratégies pour tous nos providers (Google, Facebook, GitHub)

### State Management (Flutter)

| Critère | Provider | Riverpod | Bloc | GetX | Vainqueur |
|---------|----------|----------|------|------|-----------|
| **Simplicité** | ✅ Simple | 🟡 Moyen | 🟡 Complexe | ✅ Simple | Provider |
| **Boilerplate** | ✅ Minimal | ✅ Minimal | 🟡 Important | ✅ Minimal | **Provider** ✅ |
| **Type Safety** | 🟡 Basique | ✅ Excellent | ✅ Bon | 🟡 Basique | Riverpod |
| **Testing** | ✅ Facile | ✅ Facile | ✅ Facile | 🟡 Moyen | Égalité |
| **Communauté** | ✅ Officiel Flutter | ✅ Populaire | ✅ Populaire | ✅ Populaire | **Provider** ✅ |
| **Courbe apprentissage** | ✅ Faible | 🟡 Moyen | 🟡 Élevé | ✅ Faible | Provider |

**Score Final:** Provider 8/10 | Riverpod 9/10 | Bloc 7/10 | GetX 7/10

**Verdict: Provider avec migration Riverpod possible** 
- Recommandé officiellement par Flutter team
- Simple pour démarrer rapidement
- Suffisant pour scope projet
- Migration vers Riverpod facile si besoin (syntaxe compatible)

## 2.3 Résumé Décisions Techniques

| Technologie | Choix | Raison Principale |
|-------------|-------|-------------------|
| **Backend Framework** | **NestJS** | Modularity + TypeScript + Enterprise patterns |
| **Frontend** | **Flutter (Web+Mobile)** | 95% code sharing + Performance + Cohérence UI |
| **Base de données** | **PostgreSQL (Supabase)** | Relations + JSONB + Hosted Auth gratuit |
| **OAuth** | **Passport.js** | Gratuit + Flexible + Intégration NestJS native |
| **Déploiement** | **Docker** | Exigence sujet + Portabilité + Dev=Prod |
| **State Management** | **Provider** | Officiel + Simple + Suffisant scope projet |

---

# 3. Architecture Système

## Vue d'ensemble

```
┌─────────────── CLIENTS ───────────────┐
│ Flutter Web    │  Flutter Mobile      │
│  Port: 8081    │  Android APK         │
│  (Material/Web)│  (Material/Cupertino)│
└────────┬───────┴──────────┬───────────┘
         │ HTTPS REST API   │
┌────────▼──────────────────▼───────────┐
│      NestJS API (Port 8080)           │
│  ┌────────────────────────────────┐   │
│  │ Auth │ Areas │ Services │ About│   │
│  └────────────────────────────────┘   │
│  ┌────────────────────────────────┐   │
│  │    Service Integrations        │   │
│  │ Timer│Gmail│Spotify│GitHub│... │   │
│  │    (Tous implémentent IService)│   │
│  └────────────────────────────────┘   │
│  ┌────────────────────────────────┐   │
│  │      Hooks System (Cron)       │   │
│  │ Check Areas → Execute Reactions│   │
│  │    (Chaque minute)             │   │
│  └────────────────────────────────┘   │
└────────────┬──────────────────────────┘
             │
┌────────────▼──────────────────────────┐
│   PostgreSQL (Supabase)               │
│ users│services│actions│reactions│areas│
│ user_services│area_state│audit_logs   │
└───────────────────────────────────────┘
```
### Services externes intégrés
```
┌───────────────────────────────────────────────────────┐
│              EXTERNAL SERVICES                        │
│  ════════════════════════════════════════════════════ │
│                                                       │
│  ┌────────────┐  ┌────────────┐  ┌────────────┐       │
│  │  Google    │  │ Facebook   │  │  GitHub    │       │
│  │  • Gmail   │  │ • Graph API│  │  • Repos   │       │
│  │  • Drive   │  │ • Pages    │  │  • Issues  │       │
│  │  • Calendar│  │            │  │  • Stars   │       │
│  └──────┬─────┘  └───────┬────┘  └────────┬───┘       │
└─────────┼────────────────┼────────────────┼───────────┘
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
│  NestJS: GoogleStrategy intercepte           │
│  Redirige vers Google OAuth consent screen   │
└──────┬───────────────────────────────────────┘
       │
       │ 2. Browser opens Google login
       │
       ▼
┌──────────────────────────────────────────────┐
│  User logs in on Google                      │
│  User accepts permissions                    │
└──────┬───────────────────────────────────────┘
       │
       │ 3. Google redirects to callback
       │    with authorization code
       │
       ▼
┌──────────────────────────────────────────────┐
│  GET /auth/google/callback?code=ABC123       │
│                                              │
│  NestJS GoogleStrategy:                      │
│  • Exchange code for access_token            │
│  • Get user profile from Google              │
│  • Call authService.loginWithOAuth()         │
│    ├── Find or create user in DB             │
│    ├── Store OAuth tokens                    │
│    └── Generate JWT                          │
│                                              │
│  • Redirect to client with JWT               │
└──────┬───────────────────────────────────────┘
       │
       │ 4. Redirect http://localhost:8081/auth/success?token=JWT_TOKEN
       │
       ▼
┌──────────────────────────────────────────────┐
│  Flutter extracts JWT from URL               │
│  Stores JWT in secure storage                │
│  Navigates to dashboard                      │
└──────────────────────────────────────────────┘
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
## Composants Clés

**ServiceRegistry:**
- Route vers services implémentant interface `IService`
- Pattern Strategy pour polymorphisme services

**HooksScheduler:**
- Cron (1 min) vérifie toutes actions
- Execute reactions si triggered
- Retry logic + Circuit breaker

**OAuth2 Flow:**
- Passport.js strategies
- Token encryption AES-256 at rest
- Auto-refresh avant expiration

---

# 4. Base de Données

# 📊 Architecture Base de Données - ACTION-REACTION

> Modèle Logique Universel (MLU) et Description Complète

---

## 🗺️ Diagramme MLU (Modèle Logique Universel)

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                            ARCHITECTURE BDD ACTION-REACTION                      │
└─────────────────────────────────────────────────────────────────────────────────┘

                                    ┌──────────────────┐
                                    │     users        │
                                    ├──────────────────┤
                                    │ 🔑 id            │ uuid PK
                                    │    email         │ varchar UNIQUE
                                    │    supabase_id   │ uuid UNIQUE
                                    │    full_name     │ varchar
                                    │    avatar_url    │ text
                                    │    provider      │ varchar (google, github, etc.)
                                    │    created_at    │ timestamp
                                    │    updated_at    │ timestamp
                                    └────────┬─────────┘
                                             │
                      ┌──────────────────────┼──────────────────────┐
                      │                      │                      │
                      │                      │                      │
         ┌────────────▼────────┐  ┌─────────▼──────────┐  ┌───────▼──────────────┐
         │  service_connections│  │      areas         │  │    activities        │
         ├─────────────────────┤  ├────────────────────┤  ├──────────────────────┤
         │ 🔑 id               │  │ 🔑 id              │  │ 🔑 id                │ uuid PK
         │ 🔗 user_id          │──│ 🔗 user_id         │──│ 🔗 user_id           │ uuid FK → users
         │    service_id       │  │    name            │  │ 🔗 area_id           │ uuid FK → areas
         │    access_token     │  │    description     │  │    area_name         │ varchar
         │    refresh_token    │  │    trigger_service │  │    action            │ text
         │    expires_at       │  │    trigger_event   │  │    success           │ bool
         │    connected_at     │  │    trigger_config  │  │    error_message     │ text
         └─────────────────────┘  │    action_service  │  │    timestamp         │ timestamp
                                  │    action_name     │  └──────────────────────┘
                                  │    action_config   │
                                  │    is_active       │  Logs d'exécution
                                  │    last_executed_at│  des AREAs
                                  │    last_triggered_ │
                                  │      _params       │
                                  │    created_at      │
                                  │    updated_at      │
                                  └────────────────────┘
                                  
                                  Table centrale AREA
                                  (Action + REaction)


┌─────────────────────────────────────────────────────────────────────────────────┐
│                              RELATIONS & CARDINALITÉS                            │
└─────────────────────────────────────────────────────────────────────────────────┘

users (1) ──────< (N) service_connections
    │
    └──────< (N) areas
               │
               └──────< (N) activities


┌─────────────────────────────────────────────────────────────────────────────────┐
│                          CONTRAINTES & INDEX                                     │
└─────────────────────────────────────────────────────────────────────────────────┘

CONTRAINTES:
• users.email         → UNIQUE, NOT NULL
• users.supabase_id   → UNIQUE, NOT NULL
• areas.user_id       → FK CASCADE DELETE
• activities.user_id  → FK CASCADE DELETE
• activities.area_id  → FK CASCADE DELETE
• service_connections → UNIQUE(user_id, service_id)

INDEX CRITIQUES:
 idx_areas_user_active     → (user_id, is_active)    [Hooks cron]
 idx_areas_last_executed   → (last_executed_at)      [Scheduler]
 idx_activities_user_time  → (user_id, timestamp)    [Dashboard]
 idx_service_conn_user     → (user_id, service_id)   [OAuth lookup]
```

---

##  Description Détaillée de l'Architecture

### 1. **Table `users` - Utilisateurs**

**Rôle:** Gestion des comptes utilisateurs avec authentification Supabase Auth

**Structure:**
```sql
CREATE TABLE users (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  email varchar(255) UNIQUE NOT NULL,
  supabase_id uuid UNIQUE NOT NULL,  -- Lien avec Supabase Auth
  full_name varchar(255),
  avatar_url text,
  provider varchar(50),               -- 'google', 'github', 'facebook'
  created_at timestamp DEFAULT now(),
  updated_at timestamp DEFAULT now()
);
```

**Champs clés:**
- `id`: Identifiant interne de l'application
- `supabase_id`: Identifiant Supabase Auth (auth.users.id)
- `email`: Email unique de l'utilisateur
- `provider`: Fournisseur OAuth utilisé pour l'inscription

**Relations:**
- 1 user → N service_connections
- 1 user → N areas
- 1 user → N activities

**Index:**
```sql
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_users_supabase_id ON users(supabase_id);
```

---

### 2. **Table `service_connections` - Connexions Services OAuth**

**Rôle:** Stocker les tokens OAuth pour les services externes (Gmail, Spotify, GitHub, etc.)

**Structure:**
```sql
CREATE TABLE service_connections (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  service_id varchar(50) NOT NULL,    -- 'gmail', 'spotify', 'github', etc.
  access_token text NOT NULL,          -- Chiffré AES-256 en production
  refresh_token text,                  -- Chiffré AES-256
  expires_at timestamp,
  connected_at timestamp DEFAULT now(),
  UNIQUE(user_id, service_id)
);
```

**Champs clés:**
- `service_id`: Identifiant du service ('gmail', 'spotify', 'github', 'weather')
- `access_token`: Token d'accès OAuth (chiffré)
- `refresh_token`: Token de rafraîchissement (chiffré)
- `expires_at`: Date d'expiration du token

**Sécurité:**
- Tokens chiffrés avec AES-256-GCM
- Contrainte UNIQUE empêche doublons (user, service)
- CASCADE DELETE si user supprimé

**Index:**
```sql
CREATE INDEX idx_service_conn_user_service 
  ON service_connections(user_id, service_id);
```

**Exemple de données:**
```json
{
  "user_id": "550e8400-e29b-41d4-a716-446655440000",
  "service_id": "gmail",
  "access_token": "encrypted_token_xyz...",
  "refresh_token": "encrypted_refresh_abc...",
  "expires_at": "2025-11-24T14:30:00Z"
}
```

---

### 3. **Table `areas` - AREAs (Actions + REactions)**

**Rôle:** Table centrale stockant les automatisations créées par les utilisateurs

**Structure:**
```sql
CREATE TABLE areas (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  
  -- Métadonnées AREA
  name varchar(255) NOT NULL,
  description text,
  
  -- Configuration TRIGGER (Action)
  trigger_service varchar(50) NOT NULL,   -- 'timer', 'gmail', 'spotify', etc.
  trigger_event varchar(100) NOT NULL,    -- 'time_match', 'new_email', etc.
  trigger_config jsonb DEFAULT '{}',      -- Config spécifique trigger
  
  -- Configuration ACTION (REaction)
  action_service varchar(50) NOT NULL,    -- 'discord', 'gmail', 'spotify', etc.
  action_name varchar(100) NOT NULL,      -- 'send_message', 'send_email', etc.
  action_config jsonb DEFAULT '{}',       -- Config spécifique action
  
  -- État & Exécution
  is_active boolean DEFAULT true,
  last_executed_at timestamp,
  last_triggered_params jsonb,            -- Derniers params qui ont déclenché
  
  -- Audit
  created_at timestamp DEFAULT now(),
  updated_at timestamp DEFAULT now()
);
```

**Champs clés:**

**Trigger (Action qui déclenche):**
- `trigger_service`: Service surveillé (ex: 'gmail', 'timer')
- `trigger_event`: Événement surveillé (ex: 'new_email', 'time_match')
- `trigger_config`: Configuration JSON du trigger
  ```json
  {
    "time": "09:00",
    "timezone": "Europe/Paris"
  }
  ```

**Action (REaction exécutée):**
- `action_service`: Service qui exécute (ex: 'discord', 'spotify')
- `action_name`: Action à exécuter (ex: 'send_message', 'play_track')
- `action_config`: Configuration JSON de l'action
  ```json
  {
    "webhook_url": "https://discord.com/api/webhooks/...",
    "message": "Nouvel email reçu de {{sender}}"
  }
  ```

**État:**
- `is_active`: AREA activée ou non (toggle on/off)
- `last_executed_at`: Timestamp dernière exécution
- `last_triggered_params`: Contexte du dernier trigger (pour debugging)

**Index:**
```sql
-- Index critique pour le système de hooks
CREATE INDEX idx_areas_active_last_executed 
  ON areas(is_active, last_executed_at) 
  WHERE is_active = true;

-- Index pour requêtes utilisateur
CREATE INDEX idx_areas_user_id ON areas(user_id);
```

**Exemple de données:**
```json
{
  "id": "a1b2c3d4-...",
  "user_id": "550e8400-...",
  "name": "Email du matin → Discord",
  "description": "Notifier Discord quand j'ai un email entre 9h-10h",
  
  "trigger_service": "gmail",
  "trigger_event": "new_email",
  "trigger_config": {
    "from": "",
    "subject_contains": "",
    "time_window": "09:00-10:00"
  },
  
  "action_service": "discord",
  "action_name": "send_message",
  "action_config": {
    "webhook_url": "https://discord.com/api/webhooks/...",
    "message": "📧 Nouvel email de {{sender}}: {{subject}}"
  },
  
  "is_active": true,
  "last_executed_at": "2025-11-24T09:15:23Z",
  "last_triggered_params": {
    "sender": "boss@company.com",
    "subject": "Urgent: Meeting today"
  }
}
```

---

### 4. **Table `activities` - Logs d'Activité**

**Rôle:** Journal d'exécution de chaque AREA (succès, échecs, erreurs)

**Structure:**
```sql
CREATE TABLE activities (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  area_id uuid REFERENCES areas(id) ON DELETE CASCADE,
  
  area_name varchar(255),              -- Dénormalisé pour historique
  action text NOT NULL,                -- Description de l'action
  success boolean DEFAULT false,       -- Succès ou échec
  error_message text,                  -- Message d'erreur si échec
  timestamp timestamp DEFAULT now()
);
```

**Champs clés:**
- `area_name`: Nom de l'AREA (dénormalisé car AREA peut être supprimée)
- `action`: Description textuelle de ce qui s'est passé
- `success`: true = succès, false = erreur
- `error_message`: Détails de l'erreur si échec

**Index:**
```sql
-- Dashboard utilisateur (activités récentes)
CREATE INDEX idx_activities_user_time 
  ON activities(user_id, timestamp DESC);

-- Debugging d'une AREA spécifique
CREATE INDEX idx_activities_area 
  ON activities(area_id, timestamp DESC);
```

**Exemple de données:**
```json
[
  {
    "id": "log-001",
    "user_id": "550e8400-...",
    "area_id": "a1b2c3d4-...",
    "area_name": "Email du matin → Discord",
    "action": "Email trigger matched: 'boss@company.com' → Discord webhook sent",
    "success": true,
    "error_message": null,
    "timestamp": "2025-11-24T09:15:23Z"
  },
  {
    "id": "log-002",
    "user_id": "550e8400-...",
    "area_id": "a1b2c3d4-...",
    "area_name": "Email du matin → Discord",
    "action": "Failed to send Discord webhook",
    "success": false,
    "error_message": "Discord API returned 429 (Rate Limited)",
    "timestamp": "2025-11-24T09:16:10Z"
  }
]
```

**Utilisation:**
- **Dashboard utilisateur**: Afficher les 10 dernières activités
- **Debugging**: Voir pourquoi une AREA a échoué
- **Analytics**: Statistiques d'exécution (taux de succès, etc.)

---

## 🔗 Relations & Cardinalités

### Hiérarchie des Relations

```
users (1)
  ├─< service_connections (N)  [1 user peut connecter plusieurs services]
  │    └─ Exemple: user_001 connecte Gmail, Spotify, GitHub
  │
  ├─< areas (N)                 [1 user peut créer plusieurs AREAs]
  │    ├─ Exemple: user_001 crée 5 AREAs différentes
  │    └─< activities (N)       [1 AREA génère plusieurs logs]
  │         └─ Exemple: area_001 a 50 logs d'exécution
  │
  └─< activities (N)            [1 user peut avoir des logs orphelins]
       └─ Cas: AREA supprimée mais logs conservés
```

### Contraintes d'Intégrité Référentielle

**CASCADE DELETE:**
```sql
-- Si user supprimé → tout est supprimé
service_connections → ON DELETE CASCADE
areas               → ON DELETE CASCADE
activities          → ON DELETE CASCADE

-- Si area supprimée → logs conservés (area_id devient NULL)
activities.area_id  → ON DELETE SET NULL (optionnel)
```

**UNIQUE Constraints:**
```sql
-- 1 user ne peut connecter un service qu'une fois
UNIQUE(user_id, service_id) ON service_connections

-- Email unique par utilisateur
UNIQUE(email) ON users
```

---

##  Index Optimisés pour Performance

### Index Critiques

**1. Système de Hooks (Requête chaque minute):**
```sql
CREATE INDEX idx_areas_hooks 
  ON areas(is_active, last_executed_at) 
  WHERE is_active = true;

-- Requête optimisée:
SELECT * FROM areas 
WHERE is_active = true 
  AND (last_executed_at IS NULL 
       OR last_executed_at < NOW() - INTERVAL '1 minute')
ORDER BY last_executed_at ASC NULLS FIRST;
```

**2. Lookup OAuth Tokens:**
```sql
CREATE INDEX idx_service_conn_lookup 
  ON service_connections(user_id, service_id);

-- Requête optimisée:
SELECT access_token, refresh_token, expires_at 
FROM service_connections 
WHERE user_id = $1 AND service_id = 'gmail';
```

**3. Dashboard Utilisateur:**
```sql
CREATE INDEX idx_activities_user_dashboard 
  ON activities(user_id, timestamp DESC);

-- Requête optimisée:
SELECT * FROM activities 
WHERE user_id = $1 
ORDER BY timestamp DESC 
LIMIT 20;
```

**4. Recherche AREAs Utilisateur:**
```sql
CREATE INDEX idx_areas_user_search 
  ON areas(user_id, name);

-- Requête optimisée:
SELECT * FROM areas 
WHERE user_id = $1 
  AND name ILIKE '%gmail%'
ORDER BY created_at DESC;
```

### Performance Estimée

| Requête | Sans Index | Avec Index | Gain |
|---------|-----------|-----------|------|
| Hooks (1000 AREAs) | ~50ms | ~2ms | **25x** |
| OAuth Lookup | ~20ms | ~1ms | **20x** |
| Dashboard (1000 logs) | ~100ms | ~5ms | **20x** |

---

## 🛡️ Sécurité & Chiffrement

### Données Sensibles Chiffrées

**1. Tokens OAuth (`service_connections`):**
```typescript
// Chiffrement AES-256-GCM avant stockage
const encryptToken = (token: string): string => {
  const cipher = crypto.createCipheriv('aes-256-gcm', key, iv);
  return cipher.update(token, 'utf8', 'hex') + cipher.final('hex');
};

// Déchiffrement à la lecture
const decryptToken = (encrypted: string): string => {
  const decipher = crypto.createDecipheriv('aes-256-gcm', key, iv);
  return decipher.update(encrypted, 'hex', 'utf8') + decipher.final('utf8');
};
```

**2. Row Level Security (RLS) Supabase:**
```sql
-- Users ne peuvent voir que leurs propres données
ALTER TABLE areas ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Users can view own areas"
  ON areas FOR SELECT
  USING (auth.uid() = user_id);

CREATE POLICY "Users can insert own areas"
  ON areas FOR INSERT
  WITH CHECK (auth.uid() = user_id);

CREATE POLICY "Users can update own areas"
  ON areas FOR UPDATE
  USING (auth.uid() = user_id);

CREATE POLICY "Users can delete own areas"
  ON areas FOR DELETE
  USING (auth.uid() = user_id);
```

### Audit Trail

**Triggers PostgreSQL pour audit:**
```sql
CREATE OR REPLACE FUNCTION audit_area_changes()
RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP = 'UPDATE' THEN
    INSERT INTO audit_logs(user_id, action, table_name, record_id, old_data, new_data)
    VALUES (NEW.user_id, 'UPDATE', 'areas', NEW.id, row_to_json(OLD), row_to_json(NEW));
  ELSIF TG_OP = 'DELETE' THEN
    INSERT INTO audit_logs(user_id, action, table_name, record_id, old_data)
    VALUES (OLD.user_id, 'DELETE', 'areas', OLD.id, row_to_json(OLD));
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER area_audit_trigger
  AFTER UPDATE OR DELETE ON areas
  FOR EACH ROW EXECUTE FUNCTION audit_area_changes();
```

---

## 📊 Exemples de Requêtes Fréquentes

### 1. **Récupérer toutes les AREAs actives d'un utilisateur**
```sql
SELECT 
  a.id,
  a.name,
  a.description,
  a.trigger_service,
  a.trigger_event,
  a.action_service,
  a.action_name,
  a.is_active,
  a.last_executed_at,
  COUNT(act.id) as execution_count
FROM areas a
LEFT JOIN activities act ON act.area_id = a.id
WHERE a.user_id = $1
  AND a.is_active = true
GROUP BY a.id
ORDER BY a.created_at DESC;
```

### 2. **Vérifier si un service est connecté**
```sql
SELECT 
  EXISTS(
    SELECT 1 FROM service_connections
    WHERE user_id = $1 
      AND service_id = $2
      AND expires_at > NOW()
  ) as is_connected;
```

### 3. **Récupérer les tokens OAuth d'un service**
```sql
SELECT 
  access_token,
  refresh_token,
  expires_at
FROM service_connections
WHERE user_id = $1 
  AND service_id = $2;
```

### 4. **Dashboard: Statistiques utilisateur**
```sql
SELECT 
  (SELECT COUNT(*) FROM areas WHERE user_id = $1) as total_areas,
  (SELECT COUNT(*) FROM areas WHERE user_id = $1 AND is_active = true) as active_areas,
  (SELECT COUNT(*) FROM service_connections WHERE user_id = $1) as connected_services,
  (SELECT COUNT(*) FROM activities WHERE user_id = $1 AND success = true) as successful_executions,
  (SELECT COUNT(*) FROM activities WHERE user_id = $1 AND success = false) as failed_executions;
```

### 5. **Logs récents avec détails AREA**
```sql
SELECT 
  act.id,
  act.area_name,
  act.action,
  act.success,
  act.error_message,
  act.timestamp,
  a.name as current_area_name,
  a.is_active as area_is_active
FROM activities act
LEFT JOIN areas a ON act.area_id = a.id
WHERE act.user_id = $1
ORDER BY act.timestamp DESC
LIMIT 50;
```

---

## 🎯 Bonnes Pratiques Appliquées

### 1. **Normalisation**
✅ Pas de duplication de données (sauf dénormalisation intentionnelle dans `activities`)  
✅ Relations claires avec foreign keys  
✅ Contraintes d'unicité appropriées  

### 2. **Performance**
✅ Index sur toutes les colonnes de jointure  
✅ Index composites pour requêtes fréquentes  
✅ Partial indexes (WHERE is_active = true)  

### 3. **Sécurité**
✅ Row Level Security (RLS) activée  
✅ Tokens OAuth chiffrés  
✅ CASCADE DELETE pour éviter orphelins  
✅ Audit trail automatique  

### 4. **Scalabilité**
✅ JSONB pour configs flexibles (évite migrations)  
✅ UUID pour IDs (distribué, pas de collision)  
✅ Timestamps pour partitioning futur  

### 5. **Observabilité**
✅ Table `activities` pour monitoring  
✅ `last_executed_at` pour debugging  
✅ `error_message` pour diagnostics  

---

## 🚀 Évolutions Futures

### Phase 1 (Actuelle)
- ✅ 4 tables essentielles
- ✅ Relations de base
- ✅ Index critiques

### Phase 2 (Court terme)
- 🔄 Table `services` (catalogue de services disponibles)
- 🔄 Table `service_actions` (catalogue d'actions par service)
- 🔄 Table `service_reactions` (catalogue de reactions par service)

### Phase 3 (Moyen terme)
- ⏳ Table `area_execution_history` (logs détaillés des exécutions)
- ⏳ Table `user_preferences` (préférences utilisateur)
- ⏳ Table `notifications` (notifications système)

### Phase 4 (Long terme)
- ⏳ Partitioning de `activities` par date (>1M rows)
- ⏳ Read replicas pour analytics
- ⏳ Cache Redis pour tokens OAuth

---

## 📏 Métriques & Monitoring

### Tailles Estimées (10,000 utilisateurs)

| Table | Rows | Size | Growth |
|-------|------|------|--------|
| users | 10,000 | ~2 MB | Lent |
| service_connections | 30,000 | ~5 MB | Moyen |
| areas | 50,000 | ~25 MB | Moyen |
| activities | 500,000 | ~150 MB | **Rapide** |

### Requêtes à Surveiller

```sql
-- Requêtes lentes (>100ms)
SELECT query, calls, total_time, mean_time
FROM pg_stat_statements
WHERE mean_time > 100
ORDER BY mean_time DESC
LIMIT 10;

-- Tables avec le plus de scans séquentiels (manque d'index)
SELECT schemaname, tablename, seq_scan, seq_tup_read
FROM pg_stat_user_tables
WHERE seq_scan > 1000
ORDER BY seq_scan DESC;

-- Index jamais utilisés (à supprimer)
SELECT schemaname, tablename, indexname, idx_scan
FROM pg_stat_user_indexes
WHERE idx_scan = 0 
  AND indexrelname NOT LIKE 'pg_toast%';
```

---

<div align="center">

**📊 Diagramme:** MLU (Modèle Logique Universel)  

</div>

## Migrations
<div align="center">
         
![Prisma](https://img.shields.io/badge/Prisma-ORM-success?color=3ECF8E&logo=prisma&logoColor=white)


</div>

**Outil:** Prisma Migrate  
**Stratégie:** Zero-downtime (add column → deploy → remove old)  
**Versionning:** Git (`prisma/migrations/`)

---

# 5. Sécurité

## Authentification
- **JWT:** HS256, expiration 7 jours, secret 32+ caractères
- **OAuth2:** Google, Facebook, GitHub (PKCE + state CSRF)
- **Passwords:** bcrypt (cost=12)

## Protection Données
- **Tokens:** Chiffrement AES-256 at rest
- **Transport:** HTTPS only (TLS 1.3)
- **Mobile:** flutter_secure_storage (Keychain/EncryptedSharedPreferences)

## Rate Limiting

| Endpoint | Limite | Fenêtre | Protection |
|----------|--------|---------|------------|
| `/auth/*` | 5 req | 15 min | Brute force |
| `/areas/*` | 100 req | 1 min | Spam |
| Hooks (internes) | 1 req | 1 min/AREA | Surcharge |

## Audit Trail

```sql
audit_logs (user_id, action_type, resource_type, 
            ip_address, user_agent, created_at)
```

**Rétention:** 90 jours (hot), 1 an (warm), puis suppression (RGPD)

---

# 6. Système de Hooks

## Flow d'Exécution

```
Cron (1 min) → Récupérer AREAs actives → Pour chaque AREA:
  → Vérifier action (timeout 10s)
  → Si triggered → Exécuter reaction (timeout 30s)
  → Mettre à jour last_triggered_at
  → Logger résultat
```

## Gestion Erreurs

**Stratégie Retry:** 3 tentatives (immédiat, 30s, 2min) avec exponential backoff

**Auto-disable:** 10 échecs consécutifs → AREA désactivée + notification utilisateur

**Circuit Breaker:** 5 échecs service → pause 10 min (évite spam si API down)

## Persistance État

```sql
area_state (area_id, state_key, state_value)
-- Exemple: ('area-123', 'last_email_id', 'msg_abc456')
```

Permet actions comme "new email" de tracker dernier email vu.

---

# 7. Gestion de Projet

## Équipe (5 membres)

| Membre | Rôle | Focus Actuel |
|--------|------|--------------|
| **Maurel** | Tech Lead/Backend | Architecture, GitHub service |
| **Chrisnaud** | Frontend/Mobile | Flutter (Web + Mobile) |
| **James** | DevOps/Full Stack | CI/CD, Spotify service |
| **Germain** | Backend/APIs | OAuth2, Gmail service |
| **Isaac** | Backend/System | Hooks, Weather service |

## Méthodologie: Agile Scrum

- **Sprints:** 2 semaines × 4 sprints
- **Daily Standup:** 9:00 (15 min)
- **Sprint Review:** Vendredi 14:00
- **Retrospective:** Vendredi 16:00

## Timeline (6 semaines)

```
Semaine 1-2 (Sprint 1): Setup + Auth + Timer
Semaine 3-4 (Sprint 2): Gmail + Spotify + Hooks → Défense 2 (70%)
Semaine 5 (Sprint 3): GitHub + Weather + Mobile polish
Semaine 6 (Sprint 4): Tests + Documentation → Défense 3 (100%)
```

## KPIs

| Métrique | Cible | Actuel | Status |
|----------|-------|--------|--------|
| Sprint Velocity | 40-45 SP | 42 SP | ✅ |
| Backend Coverage | >80% | 82% | ✅ |
| Frontend Coverage | >60% | 45% | 🟡 |
| API Response (P95) | <200ms | 180ms | ✅ |
| Hook Execution | <5s | 3.2s | ✅ |

## Risques Identifiés

| Risque | Probabilité | Impact | Mitigation |
|--------|-------------|--------|------------|
| Complexité OAuth | 🔴 Haute | Haute | POC early, Passport.js |
| Rate limits API | 🟡 Moyenne | Haute | Caching + backoff |
| Contraintes temps | 🟡 Moyenne | Critique | Priorisation MoSCoW |
| Flutter Web bugs | 🟡 Moyenne | Moyenne | Version stable 3.16+ |

---

# 8. WBS & Gantt

## Work Breakdown Structure

```
1. BACKEND (45%)
   1.1 Infrastructure (5%): NestJS, Supabase, Docker
   1.2 Authentication (10%): JWT, OAuth2 (3 providers)
   1.3 Services (20%): 6 services × (actions + reactions)
   1.4 AREA System (10%): CRUD, Hooks, State, Retry

2. FRONTEND (40% - Flutter Web + Mobile)
   2.1 Setup (5%): Flutter projet, config Web + Mobile
   2.2 Auth (6%): Login, Register, OAuth callback
   2.3 Marketplace (8%): Liste services, connexion
   2.4 AREA Builder (12%): Sélection action/reaction, config
   2.5 Dashboard (5%): Statistiques, gestion
   2.6 Responsive (4%): Adaptation Web, optimisation mobile

3. DEVOPS (5%): Docker, CI/CD, Monitoring

4. TESTS (5%): Unit, Integration, E2E

5. DOCUMENTATION (5%): API Ref, User Guide, README
```

## Gantt Condensé

| Phase | S1 | S2 | S3 | S4 | S5 | S6 |
|-------|----|----|----|----|----|----|
| Backend Core | ██ | ██ | ░░ | ░░ | ░░ | ░░ |
| Services | ░░ | ██ | ██ | ██ | ██ | ░░ |
| Flutter (Web+Mobile) | ██ | ░░ | ██ | ██ | ██ | ░░ |
| Tests/Docs | ░░ | ░░ | ░░ | ░░ | ██ | ██ |

**Jalons:** Auth (S1) → Timer (S2) → Défense 2 (S4) → Défense 3 (S6)

---

# 9. Services & Fonctionnalités

## Services Prévus (NBS = 5)

| Service | Type | OAuth | Status | Responsable |
|---------|------|-------|--------|-------------|
| **Timer** | Action only | ❌ | ✅ 100% | Isaac |
| **Discord** | REaction only | ❌ | ✅ 100% | Maurel |
| **Gmail** | Both | ✅ | 🔄 80% | Germain |
| **Spotify** | Both | ✅ | 🔄 60% | James |
| **GitHub** | Both | ✅ | ⏳ 0% | Maurel |
| **Weather** | Action only | ❌ (API Key) | ⏳ 0% | Isaac |

## Actions (NBA = 13)

**Timer (3):** date_match, time_match, relative_date  
**Gmail (3):** new_email, email_from, email_subject  
**Spotify (2):** new_saved_track, playlist_updated  
**GitHub (3):** new_issue, new_pr, issue_closed  
**Weather (2):** temperature_above, rain_forecast

## REActions (NBR = 9)

**Discord (1):** webhook_message  
**Gmail (2):** send_email, create_draft  
**Spotify (2):** play_track, add_to_playlist  
**GitHub (2):** create_issue, comment_issue  
**Drive (2 - bonus):** create_file, upload_file

## Matrice Compatibilité

| Action | REActions Compatibles |
|--------|----------------------|
| Timer (any) | Toutes REActions |
| Gmail new_email | Discord, Drive, GitHub |
| Spotify new_track | Discord, Drive |
| GitHub new_issue | Gmail, Discord |
| Weather rain | Gmail, Discord |

**Restriction:** Pas de boucles (Gmail → Gmail interdit)

---

# 10. Déploiement

## Docker Compose

```yaml
services:
  server:
    build: ./server
    ports: ["8080:8080"]
    environment:
      - DATABASE_URL
      - JWT_SECRET
      - GOOGLE_CLIENT_ID
    
  client:
    build: ./client
    ports: ["8081:80"]
    volumes:
      - ./client/build/web:/usr/share/nginx/html/web
      - ./client/build/apk:/usr/share/nginx/html/downloads
    depends_on:
      - server
```

**Commande:** `docker-compose up`  
**Résultat:** Backend (8080) + Flutter Web (8081) + APK téléchargeable

## CI/CD (GitHub Actions)

```
git push → Lint → Tests (Jest + Flutter Test) 
         → Coverage Check (>80%) → Build Docker
         → Deploy Staging → [Manuel] Deploy Production
```

**Durée:** 8-12 minutes

## Environments

| Env | URL | Database | Purpose |
|-----|-----|----------|---------|
| **Local** | localhost:8080 | Supabase local | Dev |
| **Staging** | staging.area.com | Supabase staging | Tests |
| **Production** | area.com | Supabase prod | Live |

## Monitoring

**Health Checks:**
- `GET /health` → Server status
- `GET /health/db` → Database status
- `GET /health/hooks` → Dernière exécution hook

**Métriques (Prometheus):**
- Temps réponse API (P95)
- Temps exécution hooks
- Taux d'erreur (%)
- Connexions database

**Alerting:** Email si downtime >5 min

---

# 11. Tests

## Pyramide de Tests

```
      E2E (10%)
     /         \
  Integration (30%)
 /                  \
Unit Tests (60%)
```

## Stratégie

**Backend (Jest):**
- Unit tests: Services, controllers, utilities
- Integration tests: API endpoints, ServiceRegistry
- E2E tests: Auth flow, Create AREA flow, Hook trigger

**Frontend (Flutter Test):**
- Widget tests: Components UI
- Integration tests: Flows complets
- Golden tests: Snapshots visuels (optionnel)

## Cibles Coverage

| Composant | Cible | Actuel | Status |
|-----------|-------|--------|--------|
| Backend | 80% | 82% | ✅ |
| Frontend | 60% | 45% | 🟡 En cours |
| E2E | 90% flows critiques | 60% | 🟡 En cours |

---

# 12. Documentation

## API Reference (Swagger)

**URL:** `http://localhost:8080/api/docs`

**Contient:**
- Tous endpoints documentés
- Exemples request/response
- Authentification requise
- Codes d'erreur

## README.md

```markdown
# ACTION-REACTION

## Quick Start
docker-compose up

## Installation
## Configuration (.env)
## API Documentation
## Testing
## Deployment
## Contributing
```

## User Guide

**Sections:**
1. Créer compte
2. Connecter service (OAuth)
3. Créer AREA
4. Gérer AREAs
5. FAQ

## Screenshots

**Requis:**
- Login screen (web + mobile)
- Service marketplace
- AREA creation flow (4-5 écrans)
- Dashboard

**Video Demo:** 2-3 min sur YouTube (unlisted)

---

# 13. Annexes

## Glossaire

| Terme | Définition |
|-------|------------|
| **AREA** | Action-REAction automation |
| **Action** | Déclencheur (ex: "nouvel email") |
| **REAction** | Réponse (ex: "envoyer message Discord") |
| **Hook** | Système vérifiant si Action déclenchée |
| **Service** | Plateforme externe (Gmail, Discord, etc.) |
| **OAuth2** | Protocole d'autorisation |

## Références

**Documentation:**
- NestJS: https://docs.nestjs.com
- Flutter: https://flutter.dev/docs
- Supabase: https://supabase.com/docs

**APIs:**
- Gmail: https://developers.google.com/gmail/api
- Discord Webhooks: https://discord.com/developers/docs
- Spotify: https://developer.spotify.com
- GitHub: https://docs.github.com/en/rest

**Inspiration:**
- IFTTT: https://ifttt.com
- Zapier: https://zapier.com
- n8n: https://n8n.io

---

##  Défenses

### Défense 2 (Semaine 4 - MVP 70%)

- [ ] Système auth (JWT + OAuth Google)
- [ ] 3 services (Timer, Discord, Gmail)
- [ ] 9 actions + 6 reactions
- [ ] Système hooks opérationnel
- [ ] Flutter Web: marketplace + création AREA
- [ ] Flutter Mobile: auth + UI basique
- [ ] Docker Compose fonctionnel
- [ ] Endpoint about.json
- [ ] Tests >80% coverage
- [ ] Documentation basique

### Défense 3 (Semaine 6 - Final 100%)

- [ ] 5+ services opérationnels
- [ ] 13+ actions, 9+ reactions
- [ ] Toutes features polies
- [ ] Flutter Mobile app complète
- [ ] Tests E2E passing
- [ ] Documentation complète
- [ ] API Reference (Swagger)
- [ ] User guide + screenshots
- [ ] Video demo
- [ ] Slides défense

---

<div align="center">

** Dernière mise à jour:** 24 Novembre 2025  
** Version:** 2.1 - Flutter Web + Étude Comparative  

[![GitHub](https://img.shields.io/badge/GitHub-Repository-black?logo=github)](https://github.com/team/area)
[![Supabase](https://img.shields.io/badge/Supabase-Database-3ECF8E?logo=supabase)](https://app.supabase.com)
[![Docs](https://img.shields.io/badge/API-Documentation-blue?logo=swagger)](http://localhost:8080/api/docs)
> **Équipe:** [Maurel KOUASSI](maurel.kouassi@epitech.eu), [Chrisnaud AGOSSOU](chrisnaud.agossou@epitech.eu), [James GBETCHEDJI](james.gbetchedji), [Germain DANDJI](germain.dandji@epitech.eu), [Isaac TOFFA](isaac.tofa@epitech.eu)
> 
**EPITECH**

</div>











