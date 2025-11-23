# 🚀 ACTION-REACTION - Comprehensive Project Planning

> **Service Automation Platform** | EPITECH T-WEB-600  
> Team: Maurel, Chrisnaud, James, Germain, Isaac

<div align="center">

![Status](https://img.shields.io/badge/Status-In%20Progress-yellow) 
![Sprint](https://img.shields.io/badge/Sprint-2%2F4-blue) 
![Progress](https://img.shields.io/badge/Progress-45%25-orange) 
![Coverage](https://img.shields.io/badge/Coverage-82%25-brightgreen)

**Stack:** NestJS · Flutter · React · PostgreSQL · Docker

[Introduction](#introduction) · [Architecture](#architecture) · [Planning](#planning) · [Team](#team)

</div>

---

Ce document est volontairement **condensé et professionnel** pour GitHub, avec toutes les sections essentielles sans code excessif.

**📦 Structure:**
- ✅ Sections 1-8: Analyse, Architecture, Gestion de projet
- ✅ Sections 9-13: Services, Déploiement, Tests, Documentation

---

# 1. Project Overview

## Context
Platform d'automatisation de services (type IFTTT/Zapier) développée dans le cadre du module Application Development d'Epitech.

## Objectives
- ✅ REST API (NestJS + PostgreSQL)
- ✅ Web Client (React)
- ✅ Mobile Client (Flutter - Android APK)
- ✅ OAuth2 + JWT Authentication
- ✅ Service integrations (5+ services)
- ✅ Automated hooks system

## Requirements (EPITECH T-WEB-600)
| Requirement | Formula | Target | Current |
|-------------|---------|--------|---------|
| Services (NBS) | ≥ 1 + X | 5 | 3 (60%) |
| Actions (NBA) | ≥ 3 × X | 13 | 9 (69%) |
| REActions (NBR) | ≥ 3 × X | 9 | 6 (67%) |

**MVP Compliance:** 🟡 65% (Target Defense 2: 70%)

---

# 2. Technology Stack

### Backend
```yaml
Runtime: Node.js 20 LTS
Framework: NestJS 10
Language: TypeScript 5
Database: PostgreSQL 15 (Supabase)
ORM: Prisma 5
Auth: JWT + Passport OAuth2
```

### Frontend
```yaml
Web: React 18 + Vite 5
Mobile: Flutter 3.16
UI: Tailwind CSS + Shadcn
State: Zustand + TanStack Query
```

### DevOps
```yaml
Container: Docker 24
Orchestration: Docker Compose 2
CI/CD: GitHub Actions
Testing: Jest + Supertest
```

**Justification des choix:**
- NestJS: Architecture modulaire + TypeScript natif
- Flutter: Cross-platform + performance native
- Supabase: PostgreSQL hébergé + Auth OAuth2 intégré
- Docker: Portabilité + conformité sujet

---

# 3. System Architecture

```
┌─────────────── CLIENTS ───────────────┐
│  React Web     │     Flutter Mobile   │
│  Port: 8081    │     APK Download     │
└────────┬───────┴──────────┬───────────┘
         │ HTTPS            │
┌────────▼──────────────────▼───────────┐
│         NestJS API (Port 8080)        │
│  ┌────────────────────────────────┐   │
│  │ Auth │ Areas │ Services │ About│   │
│  └────────────────────────────────┘   │
│  ┌────────────────────────────────┐   │
│  │    Service Integrations        │   │
│  │ Timer│Gmail│Spotify│GitHub│... │   │
│  └────────────────────────────────┘   │
│  ┌────────────────────────────────┐   │
│  │      Hooks System (Cron)       │   │
│  │ Check Areas → Execute Reactions│   │
│  └────────────────────────────────┘   │
└────────────┬──────────────────────────┘
             │
┌────────────▼──────────────────────────┐
│   PostgreSQL (Supabase)               │
│ users│services│actions│reactions│areas│
└───────────────────────────────────────┘
```

### Key Components
- **ServiceRegistry:** Route vers services implémentant `IService`
- **HooksScheduler:** Cron (1 min) vérifie actions → execute reactions
- **OAuth2 Flow:** Passport.js + token encryption (AES-256)

---

# 4. Database Schema

```sql
users (id, email, password_hash, created_at)
  ↓ 1:N
areas (id, user_id, action_id, reaction_id, action_config, reaction_config, is_enabled)
  ↓ N:1
actions (id, service_id, action_id, name, config_schema)
reactions (id, service_id, reaction_id, name, config_schema)
  ↓ N:1
services (id, name, requires_oauth)
  ↓ N:1
user_services (id, user_id, service_id, access_token_encrypted, expires_at)
```

**Indexes critiques:**
```sql
CREATE INDEX idx_areas_hooks ON areas (is_enabled, last_triggered_at) WHERE is_enabled = true;
CREATE INDEX idx_userservices_user_service ON user_services (user_id, service_id);
```

**Migrations:** Prisma Migrate avec stratégie zero-downtime

---

# 5. Security

### Authentication
- **JWT:** HS256, 7 days expiry, secret 32+ chars
- **OAuth2:** Google, Facebook, GitHub (PKCE + state)
- **Passwords:** bcrypt (cost=12)

### Data Protection
- **Tokens:** AES-256 encryption at rest
- **Transport:** HTTPS only (TLS 1.3)
- **Mobile:** flutter_secure_storage (Keychain/Android EncryptedSharedPreferences)

### Rate Limiting
| Endpoint | Limit | Window |
|----------|-------|--------|
| `/auth/*` | 5 req | 15 min |
| `/areas/*` | 100 req | 1 min |
| Hooks | 1 req | 1 min/AREA |

### Audit Trail
```sql
audit_logs (user_id, action_type, resource_type, ip_address, created_at)
```
Rétention: 90 jours (hot), 1 an (warm)

---

# 6. Hooks System

### Execution Flow
```
Cron (1 min) → Get enabled AREAs → For each:
  → Check action (timeout 10s)
  → If triggered → Execute reaction (timeout 30s)
  → Update last_triggered_at
  → Log result
```

### Error Handling
**Retry Strategy:** 3 attempts (immediate, 30s, 2min) avec exponential backoff

**Auto-disable:** 10 échecs consécutifs → AREA disabled + notification user

**Circuit Breaker:** 5 échecs service → pause 10 min (évite spam API down)

### State Persistence
```sql
area_state (area_id, state_key, state_value)
-- Exemple: ('area-123', 'last_email_id', 'msg_abc456')
```
Permet actions comme "new email" de tracker dernier email vu.

---

# 7. Project Management

## Team (5 members)

| Member | Role | Focus |
|--------|------|-------|
| **Maurel** | Tech Lead/Backend | Architecture, GitHub service |
| **Chrisnaud** | Frontend/Mobile | React + Flutter |
| **James** | DevOps/Full Stack | CI/CD, Spotify service |
| **Germain** | Backend/APIs | OAuth2, Gmail service |
| **Isaac** | Backend/System | Hooks, Weather service |

## Methodology: Agile Scrum
- **Sprints:** 2 weeks × 4 sprints
- **Daily Standup:** 9:00 AM (15 min)
- **Sprint Review:** Vendredi 14:00
- **Sprint Retrospective:** Vendredi 16:00

## Timeline (6 weeks)

```
Week 1-2 (Sprint 1): Setup + Auth + Timer
Week 3-4 (Sprint 2): Gmail + Spotify + Hooks → Defense 2 (70%)
Week 5 (Sprint 3): GitHub + Weather + Mobile
Week 6 (Sprint 4): Tests + Docs → Defense 3 (100%)
```

## KPIs

| Metric | Target | Current |
|--------|--------|---------|
| Sprint Velocity | 40-45 SP | 42 SP ✅ |
| Backend Coverage | >80% | 82% ✅ |
| Frontend Coverage | >60% | 65% ✅ |
| API Response (P95) | <200ms | 180ms ✅ |
| Hook Execution | <5s | 3.2s ✅ |

## Risk Management

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|------------|
| OAuth Complexity | 🔴 High | High | POC early, Passport.js |
| API Rate Limits | 🟡 Medium | High | Caching + backoff |
| Time Constraints | 🟡 Medium | Critical | MoSCoW prioritization |

---

# 8. Work Breakdown Structure

```
1. BACKEND (45%)
   1.1 Infrastructure (5%): NestJS, Supabase, Docker
   1.2 Authentication (10%): JWT, OAuth2 (Google, Facebook, GitHub)
   1.3 Services (20%): Timer, Discord, Gmail, Spotify, GitHub, Weather
   1.4 AREA System (10%): CRUD, Hooks, State, Retry

2. FRONTEND WEB (20%)
   2.1 Setup (3%): React, Routing, API client
   2.2 Auth (4%): Login, Register, OAuth
   2.3 Marketplace (5%): Service list, connection
   2.4 AREA Builder (6%): Action/REaction selection, config
   2.5 Dashboard (2%): Stats, management

3. MOBILE (20%)
   3.1 Setup Flutter (3%)
   3.2 Auth (4%)
   3.3 Marketplace (5%)
   3.4 AREA Management (6%)
   3.5 Notifications (2%)

4. DEVOPS (5%): Docker, CI/CD, Monitoring

5. TESTS (5%): Unit, Integration, E2E

6. DOCUMENTATION (5%): API Ref, User Guide, README
```

## Gantt (Condensé)

| Phase | W1 | W2 | W3 | W4 | W5 | W6 |
|-------|----|----|----|----|----|----|
| Backend Core | ██ | ██ | ░░ | ░░ | ░░ | ░░ |
| Services | ░░ | ██ | ██ | ██ | ██ | ░░ |
| Frontend | ██ | ░░ | ██ | ██ | ██ | ░░ |
| Mobile | ░░ | ░░ | ██ | ██ | ██ | ░░ |
| Tests/Docs | ░░ | ░░ | ░░ | ░░ | ██ | ██ |

**Milestones:** Auth (W1) → Timer (W2) → Defense 2 (W4) → Defense 3 (W6)

---

# 9. Services & Features

## Planned Services (NBS = 5)

| Service | Type | OAuth | Status |
|---------|------|-------|--------|
| **Timer** | Action only | ❌ | ✅ 100% |
| **Discord** | REaction only | ❌ | ✅ 100% |
| **Gmail** | Both | ✅ | 🔄 80% |
| **Spotify** | Both | ✅ | 🔄 60% |
| **GitHub** | Both | ✅ | ⏳ 0% |
| **Weather** | Action only | ❌ | ⏳ 0% |

## Actions (NBA = 13)

**Timer (3):**
- `timer_date_match`: Trigger à DD/MM
- `timer_time_match`: Trigger à HH:MM
- `timer_relative_date`: In X days = [day of week]

**Gmail (3):**
- `gmail_new_email`: Nouvel email reçu
- `gmail_email_from`: Email de [sender]
- `gmail_email_subject`: Subject contient [text]

**Spotify (2):**
- `spotify_new_saved_track`: Track ajoutée
- `spotify_playlist_updated`: Playlist modifiée

**GitHub (3):**
- `github_new_issue`: Nouvelle issue
- `github_new_pr`: Nouvelle PR
- `github_issue_closed`: Issue fermée

**Weather (2):**
- `weather_temperature_above`: Température > X°C
- `weather_rain_forecast`: Pluie prévue

## REActions (NBR = 9)

**Discord (1):**
- `discord_webhook_message`: Envoyer message

**Gmail (2):**
- `gmail_send_email`: Envoyer email
- `gmail_create_draft`: Créer brouillon

**Spotify (2):**
- `spotify_play_track`: Jouer track
- `spotify_add_to_playlist`: Ajouter à playlist

**GitHub (2):**
- `github_create_issue`: Créer issue
- `github_comment_issue`: Commenter issue

**Drive (2 - bonus):**
- `drive_create_file`: Créer fichier
- `drive_upload_file`: Upload fichier

## Compatibility Matrix

| Action | Compatible REActions |
|--------|---------------------|
| Timer (any) | All REActions |
| Gmail new_email | Discord, Drive, GitHub |
| Spotify new_track | Discord, Drive |
| GitHub new_issue | Gmail, Discord |
| Weather rain | Gmail, Discord |

**Restriction:** Pas de boucles (Gmail → Gmail interdit)

---

# 10. Deployment

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
    
  client_mobile:
    build: ./client
    volumes:
      - mobile_apk:/app/build
    
  client_web:
    image: nginx:alpine
    ports: ["8081:80"]
    volumes:
      - mobile_apk:/usr/share/nginx/html/downloads
    depends_on:
      - server
      - client_mobile
```

**Commande:** `docker-compose up` → Backend (8080) + Web (8081) + APK téléchargeable

## CI/CD Pipeline (GitHub Actions)

```
git push → Lint → Tests (Jest) → Build Docker
         → Coverage Check (>80%) → Deploy Staging
         → [Manual] Deploy Production
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
- `GET /health/hooks` → Last hook execution

**Metrics (Prometheus):**
- API response time (P95)
- Hook execution time
- Error rate (%)
- Database connections

**Alerting:** Email si downtime >5 min, Slack si erreur critique

---

# 11. Testing Strategy

## Test Pyramid

```
      E2E (10%)
     /         \
  Integration (30%)
 /                  \
Unit Tests (60%)
```

### Backend Tests (Jest)

**Unit Tests (Coverage >80%):**
- Services: `TimerService.checkTimeMatch()`, `GmailService.fetchEmails()`
- Controllers: Isolation avec mocks
- Utilities: Validation, encryption

**Integration Tests:**
- API endpoints avec DB mockée (Supertest)
- ServiceRegistry + Services
- Hooks + AreasService

**E2E Tests:**
- Auth flow complet
- Create AREA flow
- Hook trigger flow

### Frontend Tests

**React (Jest + Testing Library):**
- Component tests
- Integration tests

**Flutter:**
- Widget tests
- Golden tests (snapshots UI)

### Coverage Targets

| Component | Target | Current |
|-----------|--------|---------|
| Backend | 80% | 82% ✅ |
| Frontend | 60% | 65% ✅ |
| Mobile | 60% | 40% 🟡 |

**Command:** `npm test -- --coverage`

---

# 12. Documentation

## API Reference (Swagger)

**URL:** `http://localhost:8080/api/docs`

**Includes:**
- All endpoints documented
- Request/response examples
- Authentication requirements
- Error codes

## README.md Structure

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
1. Create account
2. Connect service (OAuth)
3. Create AREA
4. Manage AREAs
5. FAQ

## Screenshots

**Required:**
- Login screen (web + mobile)
- Service marketplace
- AREA creation flow (4-5 screens)
- Dashboard

**Video Demo:** 2-3 min sur YouTube (unlisted)

---

# 13. Appendices

## Glossary

| Term | Definition |
|------|------------|
| **AREA** | Action-REAction automation |
| **Action** | Trigger (ex: "new email") |
| **REAction** | Response (ex: "send Discord message") |
| **Hook** | System checking if Action triggered |
| **Service** | External platform (Gmail, Discord, etc.) |
| **OAuth2** | Authorization protocol |

## References

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

## ✅ Defense Checklist

### Defense 2 (Week 4 - MVP 70%)

- [ ] Auth system (JWT + OAuth Google)
- [ ] 3 services (Timer, Discord, Gmail)
- [ ] 9 actions + 6 reactions
- [ ] Hooks system operational
- [ ] Frontend marketplace + AREA creation
- [ ] Mobile auth + basic UI
- [ ] Docker Compose working
- [ ] about.json endpoint
- [ ] Tests >80% coverage
- [ ] Basic documentation

### Defense 3 (Week 6 - Final 100%)

- [ ] 5+ services
- [ ] 13+ actions, 9+ reactions
- [ ] All features polished
- [ ] Mobile app complete
- [ ] E2E tests passing
- [ ] Complete documentation
- [ ] API Reference (Swagger)
- [ ] User guide + screenshots
- [ ] Video demo
- [ ] Defense slides

---

<div align="center">

**📄 Last Updated:** November 24, 2025  
**📝 Version:** 2.0  
**👥 Team:** Maurel · Chrisnaud · James · Germain · Isaac

[![GitHub](https://img.shields.io/badge/GitHub-Repository-black?logo=github)](https://github.com/team/area)
[![Supabase](https://img.shields.io/badge/Supabase-Database-3ECF8E?logo=supabase)](https://app.supabase.com)
[![Docs](https://img.shields.io/badge/API-Documentation-blue?logo=swagger)](http://localhost:8080/api/docs)

**Built with ❤️ by EPITECH students**

</div>
