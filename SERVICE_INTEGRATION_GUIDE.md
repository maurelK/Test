# Service Integration Guide - AREA Backend

## 🎯 Overview

This guide explains how to add a new service integration (Google, GitHub, Spotify, Weather, etc.) to the AREA backend.

All services must implement the `IService` interface to ensure consistency and allow the system to work generically.

## 📋 Service Interface

Every service must implement this interface:

```typescript
export interface IService {
  name: string; // Unique service identifier (e.g., 'discord', 'spotify')
  
  getDefinitions(): ServiceActionDefinition[]; // List of actions/reactions
  
  checkAction?(actionId: string, params: any, userToken?: string): Promise<boolean>;
  
  executeReaction?(reactionId: string, params: any, userToken?: string): Promise<void>;
}
```

## 🏗️ Step-by-Step: Adding a New Service

### Step 1: Create Service Directory

```bash
mkdir -p src/integrations/your-service
```

### Step 2: Create Service File

**File**: `src/integrations/your-service/your-service.service.ts`

```typescript
import { Injectable } from '@nestjs/common';
import { IService, ServiceActionDefinition } from '../../common/interfaces/service.interface';

@Injectable()
export class YourService implements IService {
  name = 'your-service'; // Must be unique

  getDefinitions(): ServiceActionDefinition[] {
    return [
      {
        id: 'your_service_action_id',
        name: 'Action Name',
        description: 'What this action does',
        type: 'ACTION', // or 'REACTION'
        parameters: [
          {
            name: 'parameterName',
            type: 'string', // 'string' | 'number' | 'boolean'
            required: true,
            description: 'Parameter description',
          },
        ],
      },
    ];
  }

  // For ACTIONS (Triggers)
  async checkAction(actionId: string, params: any, userToken?: string): Promise<boolean> {
    if (actionId === 'your_service_action_id') {
      // Check if the condition is met
      // Return true to trigger the AREA
      return false;
    }
    return false;
  }

  // For REACTIONS
  async executeReaction(reactionId: string, params: any, userToken?: string): Promise<void> {
    if (reactionId === 'your_service_reaction_id') {
      // Execute the reaction
      console.log('Executing reaction with params:', params);
    }
  }
}
```

### Step 3: Create Module File

**File**: `src/integrations/your-service/your-service.module.ts`

```typescript
import { Module } from '@nestjs/common';
import { YourService } from './your-service.service';

@Module({
  providers: [YourService],
  exports: [YourService],
})
export class YourServiceModule {}
```

### Step 4: Register in IntegrationsModule

**File**: `src/integrations/integrations.module.ts`

```typescript
import { Module } from '@nestjs/common';
import { YourServiceModule } from './your-service/your-service.module';
import { ServiceRegistry } from './service-registry.service';

@Module({
  imports: [
    YourServiceModule, // Add your module here
    // ... other modules
  ],
  providers: [ServiceRegistry],
  exports: [ServiceRegistry],
})
export class IntegrationsModule {}
```

### Step 5: Register in ServiceRegistry

**File**: `src/integrations/service-registry.service.ts`

```typescript
constructor(
  private yourService: YourService, // Inject your service
  // ... other services
) {}

onModuleInit() {
  this.registerService(this.yourService); // Register it
  // ... other registrations
}
```

## 📚 Examples

### Example 1: Discord (REACTION - Webhook)

```typescript
@Injectable()
export class DiscordService implements IService {
  name = 'discord';

getDefinitions(): ServiceActionDefinition[] {
    return [
      {
        id: 'discord_webhook_message',
        name: 'Send message via Webhook',
        description: 'Sends a text message to a Discord channel',
        type: 'REACTION',
        parameters: [
          { name: 'webhookUrl', type: 'string', required: true },
          { name: 'messageContent', type: 'string', required: true },
        ],
      },
    ];
  }

  async executeReaction(reactionId: string, params: any): Promise<void> {
    if (reactionId === 'discord_webhook_message') {
      const { webhookUrl, messageContent } = params;
      await axios.post(webhookUrl, {
        content: messageContent,
        username: 'AREA Bot',
      });
    }
  }
}
```

### Example 2: Timer (ACTION - Trigger)

```typescript
@Injectable()
export class TimerService implements IService {
  name = 'timer';

  getDefinitions(): ServiceActionDefinition[] {
    return [
      {
        id: 'timer_interval',
        name: 'Timer Interval',
        description: 'Triggers at specified intervals',
        type: 'ACTION',
        parameters: [
          { name: 'intervalMinutes', type: 'number', required: true },
        ],
      },
    ];
  }

  async checkAction(actionId: string, params: any): Promise<boolean> {
    if (actionId === 'timer_interval') {
      const { intervalMinutes, lastTriggered } = params;
      const now = new Date();
      const lastTime = new Date(lastTriggered);
      const diffMinutes = (now.getTime() - lastTime.getTime()) / (1000 * 60);
      return diffMinutes >= intervalMinutes;
    }
    return false;
  }
}
```

### Example 3: Spotify (REACTION - OAuth Required)

```typescript
@Injectable()
export class SpotifyService implements IService {
  name = 'spotify';

  getDefinitions(): ServiceActionDefinition[] {
    return [
      {
        id: 'spotify_play_track',
        name: 'Play Track',
        description: 'Plays a specific track on Spotify',
        type: 'REACTION',
        parameters: [
          { name: 'trackUri', type: 'string', required: true },
        ],
      },
    ];
  }

  async executeReaction(reactionId: string, params: any, userToken?: string): Promise<void> {
    if (reactionId === 'spotify_play_track') {
      const { trackUri } = params;
      
      // Use userToken to call Spotify API
      await axios.put(
        'https://api.spotify.com/v1/me/player/play',
        { uris: [trackUri] },
        { headers: { Authorization: `Bearer ${userToken}` } }
      );
    }
  }
}
```

## 🔑 OAuth Services

For services requiring OAuth (Google, GitHub, Spotify, etc.):

1. **Store tokens in database** when user connects the service
2. **Retrieve token** from database using `userToken` parameter
3. **Refresh token** if expired
4. **Use token** to call the service API

Example token retrieval:

```typescript
async executeReaction(reactionId: string, params: any, userToken?: string): Promise<void> {
  // Get user's service token from database
  const serviceConnection = await this.supabase
    .from('service_connections')
    .select('access_token, refresh_token')
    .eq('user_id', userToken)
    .eq('service_id', this.name)
    .single();

  const accessToken = serviceConnection.data.access_token;
  
  // Use token to call API
  await axios.get('https://api.service.com/endpoint', {
    headers: { Authorization: `Bearer ${accessToken}` }
  });
}
```

## 🧪 Testing Your Service

### Test the about.json endpoint

```bash
curl http://localhost:8080/about.json
```

Should show your service in the list with its actions/reactions.

### Test executeReaction

```typescript
// In a controller or test
const result = await this.serviceRegistry.executeReaction(
  'your-service',
  'your_action_id',
  { param1: 'value1' },
  'user-token'
);
```

## 📝 Service Assignment

| Developer | Service | Type | Complexity |
|-----------|---------|------|------------|
| You | Discord | REACTION | Easy (Webhook) |
| Germain | Google | BOTH | Hard (OAuth) |
| James | Spotify | BOTH | Medium (OAuth) |
| Maurel | GitHub | BOTH | Medium (OAuth) |
| Isaac | Weather | ACTION | Easy (API Key) |

## ✅ Checklist

Before submitting your service:

- [ ] Implements `IService` interface
- [ ] Has `getDefinitions()` method
- [ ] Has `checkAction()` or `executeReaction()` (or both)
- [ ] Registered in `IntegrationsModule`
- [ ] Registered in `ServiceRegistry`
- [ ] Tested with `about.json` endpoint
- [ ] Error handling implemented
- [ ] Documented parameters

## 🚀 Next Steps

1. Create your service following this guide
2. Test it locally
3. Create a PR with your service
4. Team reviews and merges

## 📞 Questions?

Ask the team lead (you!) if you have questions about:
- OAuth implementation
- Token management
- Error handling
- Testing strategies

---

**Good luck! Let's build amazing integrations! 🎉**
