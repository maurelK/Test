# 🚀 ACTION-REACTION - Planning Complet du Projet

> **Plateforme d'Automatisation de Services** | EPITECH T-WEB-600  
> **Équipe:** [Maurel KOUASSI](maurel.kouassi@epitech.eu), [Chrisnaud AGOSSOU](chrisnaud.agossou@epitech.eu), [James GBETCHEDJI](james.gbetchedji), [Germain DANDJI](germain.dandji@epitech.eu), [Isaac TOFFA](isaac.tofa@epitech.eu)

<div align="center">

![Status](https://img.shields.io/badge/Status-En%20Cours-yellow) 
![Sprint](https://img.shields.io/badge/Sprint-2%2F4-blue) 
![Progression](https://img.shields.io/badge/Progression-45%25-orange) 
![Coverage](https://img.shields.io/badge/Coverage-82%25-brightgreen)

**Stack Technique:** NestJS · Flutter (Web + Mobile) · PostgreSQL · Docker

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
- ✅ API REST (NestJS + PostgreSQL)
- ✅ Client Web (Flutter Web)
- ✅ Client Mobile (Flutter - APK Android)
- ✅ Authentification OAuth2 + JWT
- ✅ Intégrations de services (5+ services)
- ✅ Système de hooks automatisé

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
- ✅ **Code partagé**: 90%+ du code entre web et mobile
- ✅ **Maintenance simplifiée**: Un seul codebase
- ✅ **Cohérence UI/UX**: Design identique toutes plateformes
- ✅ **Vélocité développement**: Pas besoin d'apprendre React en plus
- ✅ **Hot Reload**: Développement rapide web et mobile
- ✅ **Performance**: Compilation native (Web Assembly + AOT)

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

**Verdict: NestJS** ✅
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

**Verdict: Flutter** ✅
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

**Verdict: PostgreSQL (Supabase)** ✅
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

**Verdict: Passport.js** ✅
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

**Verdict: Provider avec migration Riverpod possible** ✅
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

## Schéma Relationnel Simplifié

```sql
users (id, email, password_hash, created_at)
  ↓ 1:N
areas (id, user_id, action_id, reaction_id, 
       action_config, reaction_config, is_enabled)
  ↓ N:1        ↓ N:1
actions       reactions
  ↓ N:1         ↓ N:1
services (id, name, requires_oauth)
  ↓ N:M (via user_services)
users (connexion services OAuth)
```

## Index Critiques

```sql
-- Hooks (requête chaque minute)
CREATE INDEX idx_areas_hooks 
  ON areas (is_enabled, last_triggered_at) 
  WHERE is_enabled = true;

-- Connexions services
CREATE INDEX idx_userservices 
  ON user_services (user_id, service_id);

-- État des AREAs
CREATE INDEX idx_areastate 
  ON area_state (area_id, state_key);
```

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

## ✅ Checklist Défenses

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

**📄 Dernière mise à jour:** 24 Novembre 2025  
**📝 Version:** 2.1 - Flutter Web + Étude Comparative  
**👥 Équipe:** Maurel · Chrisnaud · James · Germain · Isaac

[![GitHub](https://img.shields.io/badge/GitHub-Repository-black?logo=github)](https://github.com/team/area)
[![Supabase](https://img.shields.io/badge/Supabase-Database-3ECF8E?logo=supabase)](https://app.supabase.com)
[![Docs](https://img.shields.io/badge/API-Documentation-blue?logo=swagger)](http://localhost:8080/api/docs)

**EPITECH**

</div>


