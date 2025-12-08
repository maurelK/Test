# Module AREAS - Documentation

## Vue d'ensemble

Le module AREAS permet de créer, gérer et exécuter des automatisations (AREAs) qui connectent des triggers (déclencheurs) à des actions.

## Architecture

```
src/areas/
├── dto/
│   ├── create-area.dto.ts    # DTO pour créer une AREA
│   ├── update-area.dto.ts    # DTO pour mettre à jour une AREA
│   └── index.ts              # Exports
├── areas.controller.ts       # Contrôleur REST
├── areas.service.ts          # Logique métier
└── areas.module.ts           # Module NestJS
```

## Modèle de Données

### Interface Area

```typescript
interface Area {
  id: string;                    // Identifiant unique
  user_id: string;               // ID de l'utilisateur propriétaire
  name: string;                  // Nom de l'AREA
  trigger: {
    service: string;             // Service déclencheur (gmail, github, etc.)
    event: string;               // Événement (new_email, push, etc.)
    config: Record<string, any>; // Configuration spécifique
  };
  action: {
    service: string;             // Service d'action (discord, slack, etc.)
    type: string;                // Type d'action (send_message, etc.)
    config: Record<string, any>; // Configuration spécifique
  };
  is_active: boolean;            // AREA active ou non
  created_at: string;            // Date de création (ISO 8601)
  updated_at: string;            // Date de dernière modification
  last_triggered: string | null; // Date de dernière exécution
  execution_count: number;       // Nombre d'exécutions
}
```

## Endpoints API

### 1. Créer une AREA

**POST** `/api/areas`

**Request Body:**
```json
{
  "name": "Gmail to Discord",
  "trigger_service": "gmail",
  "trigger_event": "new_email",
  "trigger_config": {
    "from": "boss@company.com",
    "subject_contains": "urgent"
  },
  "action_service": "discord",
  "action_type": "send_message",
  "action_config": {
    "webhook_url": "https://discord.com/api/webhooks/...",
    "message": "New urgent email from boss!"
  },
  "is_active": true
}
```

**Response:** `201 Created`
```json
{
  "id": "area-1234567890-abc123",
  "user_id": "mock-user-id",
  "name": "Gmail to Discord",
  "trigger": {
    "service": "gmail",
    "event": "new_email",
    "config": {
      "from": "boss@company.com",
      "subject_contains": "urgent"
    }
  },
  "action": {
    "service": "discord",
    "type": "send_message",
    "config": {
      "webhook_url": "https://discord.com/api/webhooks/...",
      "message": "New urgent email from boss!"
    }
  },
  "is_active": true,
  "created_at": "2024-01-15T10:30:00.000Z",
  "updated_at": "2024-01-15T10:30:00.000Z",
  "last_triggered": null,
  "execution_count": 0
}
```

### 2. Lister toutes les AREAs

**GET** `/api/areas`

**Response:** `200 OK`
```json
{
  "areas": [
    {
      "id": "area-1234567890-abc123",
      "user_id": "mock-user-id",
      "name": "Gmail to Discord",
      "trigger": { ... },
      "action": { ... },
      "is_active": true,
      "created_at": "2024-01-15T10:30:00.000Z",
      "updated_at": "2024-01-15T10:30:00.000Z",
      "last_triggered": "2024-01-15T11:00:00.000Z",
      "execution_count": 5
    }
  ],
  "total": 1
}
```

### 3. Récupérer une AREA spécifique

**GET** `/api/areas/:id`

**Response:** `200 OK`
```json
{
  "id": "area-1234567890-abc123",
  "user_id": "mock-user-id",
  "name": "Gmail to Discord",
  ...
}
```

**Erreurs:**
- `404 Not Found` - AREA non trouvée

### 4. Mettre à jour une AREA

**PATCH** `/api/areas/:id`

**Request Body:**
```json
{
  "name": "Gmail to Discord (Updated)",
  "is_active": false,
  "trigger_config": {
    "from": "boss@company.com",
    "subject_contains": "very urgent"
  }
}
```

**Response:** `200 OK`
```json
{
  "id": "area-1234567890-abc123",
  "name": "Gmail to Discord (Updated)",
  "is_active": false,
  ...
}
```

### 5. Supprimer une AREA

**DELETE** `/api/areas/:id`

**Response:** `200 OK`
```json
{
  "success": true,
  "message": "AREA deleted successfully"
}
```

### 6. Activer/Désactiver une AREA

**POST** `/api/areas/:id/toggle`

**Response:** `200 OK`
```json
{
  "id": "area-1234567890-abc123",
  "is_active": false,
  ...
}
```

### 7. Exécuter manuellement une AREA

**POST** `/api/areas/:id/execute`

**Response:** `200 OK`
```json
{
  "success": true,
  "message": "AREA executed successfully",
  "execution_time": "2024-01-15T12:00:00.000Z"
}
```

**Erreurs:**
- `400 Bad Request` - AREA non active
```json
{
  "success": false,
  "message": "AREA is not active"
}
```

## Exemples d'utilisation

### PowerShell

#### Créer une AREA
```powershell
$body = @{
    name = "GitHub to Slack"
    trigger_service = "github"
    trigger_event = "push"
    trigger_config = @{
        repository = "my-repo"
        branch = "main"
    }
    action_service = "slack"
    action_type = "send_message"
    action_config = @{
        channel = "#dev"
        message = "New push on main!"
    }
    is_active = $true
} | ConvertTo-Json

Invoke-WebRequest -Uri http://localhost:8080/api/areas `
    -Method POST `
    -Body $body `
    -ContentType 'application/json'
```

#### Lister les AREAs
```powershell
curl http://localhost:8080/api/areas
```

#### Mettre à jour une AREA
```powershell
$body = @{
    name = "GitHub to Slack (Updated)"
    is_active = $false
} | ConvertTo-Json

Invoke-WebRequest -Uri http://localhost:8080/api/areas/area-123 `
    -Method PATCH `
    -Body $body `
    -ContentType 'application/json'
```

#### Supprimer une AREA
```powershell
Invoke-WebRequest -Uri http://localhost:8080/api/areas/area-123 `
    -Method DELETE
```

#### Toggle une AREA
```powershell
Invoke-WebRequest -Uri http://localhost:8080/api/areas/area-123/toggle `
    -Method POST
```

#### Exécuter une AREA
```powershell
Invoke-WebRequest -Uri http://localhost:8080/api/areas/area-123/execute `
    -Method POST
```

## Validation des Données

### CreateAreaDto

- `name` (string, required) - Nom de l'AREA
- `trigger_service` (string, required) - Service déclencheur
- `trigger_event` (string, required) - Événement déclencheur
- `trigger_config` (object, optional) - Configuration du trigger
- `action_service` (string, required) - Service d'action
- `action_type` (string, required) - Type d'action
- `action_config` (object, optional) - Configuration de l'action
- `is_active` (boolean, optional, default: true) - État actif

### UpdateAreaDto

- `name` (string, optional) - Nouveau nom
- `is_active` (boolean, optional) - Nouvel état
- `trigger_config` (object, optional) - Nouvelle config trigger
- `action_config` (object, optional) - Nouvelle config action

## Stockage Actuel

**⚠️ Important:** Actuellement, les AREAs sont stockées en mémoire (Map). Elles seront perdues au redémarrage du serveur.

**TODO:** Implémenter la persistance avec Supabase/PostgreSQL.

## Prochaines Étapes

### 1. Persistance en Base de Données

Créer la table `areas` dans Supabase :

```sql
CREATE TABLE areas (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id UUID REFERENCES users(id) ON DELETE CASCADE,
  name VARCHAR(255) NOT NULL,
  trigger_service VARCHAR(50) NOT NULL,
  trigger_event VARCHAR(50) NOT NULL,
  trigger_config JSONB DEFAULT '{}',
  action_service VARCHAR(50) NOT NULL,
  action_type VARCHAR(50) NOT NULL,
  action_config JSONB DEFAULT '{}',
  is_active BOOLEAN DEFAULT true,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW(),
  last_triggered TIMESTAMP,
  execution_count INTEGER DEFAULT 0
);

CREATE INDEX idx_areas_user_id ON areas(user_id);
CREATE INDEX idx_areas_is_active ON areas(is_active);
```

### 2. Authentification JWT

Remplacer `mock-user-id` par le vrai user ID du token JWT :

```typescript
@UseGuards(JwtAuthGuard)
@Controller('areas')
export class AreasController {
  @Post()
  create(@CurrentUser() user: User, @Body() dto: CreateAreaDto) {
    return this.areasService.create(user.id, dto);
  }
}
```

### 3. Exécution Automatique

Implémenter un système de polling ou webhooks pour déclencher automatiquement les AREAs :

```typescript
@Cron('*/5 * * * *') // Toutes les 5 minutes
async checkTriggers() {
  const activeAreas = await this.areasService.findAllActive();
  for (const area of activeAreas) {
    await this.triggerService.check(area);
  }
}
```

### 4. Intégrations Réelles

Connecter les services réels (Gmail, GitHub, Discord, etc.) via leurs APIs.

### 5. Logs et Historique

Créer une table `area_executions` pour tracer l'historique :

```sql
CREATE TABLE area_executions (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  area_id UUID REFERENCES areas(id) ON DELETE CASCADE,
  executed_at TIMESTAMP DEFAULT NOW(),
  success BOOLEAN NOT NULL,
  error_message TEXT,
  trigger_data JSONB,
  action_result JSONB
);
```

## Tests

### Test Complet du Flow

```powershell
# 1. Créer une AREA
$createBody = @{
    name = "Test AREA"
    trigger_service = "test"
    trigger_event = "test_event"
    action_service = "test"
    action_type = "test_action"
} | ConvertTo-Json

$response = Invoke-WebRequest -Uri http://localhost:8080/api/areas `
    -Method POST -Body $createBody -ContentType 'application/json'

$area = ($response.Content | ConvertFrom-Json)
$areaId = $area.id

# 2. Lister les AREAs
curl http://localhost:8080/api/areas

# 3. Récupérer l'AREA
curl "http://localhost:8080/api/areas/$areaId"

# 4. Mettre à jour
$updateBody = @{ name = "Test AREA Updated" } | ConvertTo-Json
Invoke-WebRequest -Uri "http://localhost:8080/api/areas/$areaId" `
    -Method PATCH -Body $updateBody -ContentType 'application/json'

# 5. Toggle
Invoke-WebRequest -Uri "http://localhost:8080/api/areas/$areaId/toggle" `
    -Method POST

# 6. Exécuter
Invoke-WebRequest -Uri "http://localhost:8080/api/areas/$areaId/execute" `
    -Method POST

# 7. Supprimer
Invoke-WebRequest -Uri "http://localhost:8080/api/areas/$areaId" `
    -Method DELETE
```

## Statut

✅ **Implémenté:**
- CRUD complet (Create, Read, Update, Delete)
- Toggle actif/inactif
- Exécution manuelle
- Validation des DTOs
- Gestion des erreurs

⏳ **À faire:**
- Persistance en base de données
- Authentification JWT
- Exécution automatique
- Intégrations réelles
- Logs et historique

---

**Dernière mise à jour:** 2024-01-15
