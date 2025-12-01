# Backend Implementation Plan - AREA

## 🎯 Overview

This document provides a clear implementation plan for the AREA backend using NestJS. Follow this step-by-step guide to implement all required endpoints.

## 📋 Prerequisites

Dependencies installed:
- `@nestjs/config` - Environment variables
- `@nestjs/jwt` - JWT authentication
- `@nestjs/passport` - Passport integration
- `passport-jwt` - JWT strategy
- `bcrypt` - Password hashing
- `class-validator` - DTO validation
- `class-transformer` - DTO transformation

## 🏗️ Project Structure

```
src/
├── auth/
│   ├── auth.controller.ts
│   ├── auth.service.ts
│   ├── auth.module.ts
│   ├── dto/
│   │   ├── validate-token.dto.ts
│   │   └── oauth-validate.dto.ts
│   ├── guards/
│   │   └── jwt-auth.guard.ts
│   └── strategies/
│       └── jwt.strategy.ts
├── services/
│   ├── services.controller.ts
│   ├── services.service.ts
│   ├── services.module.ts
│   └── dto/
│       └── service.dto.ts
├── dashboard/
│   ├── dashboard.controller.ts
│   ├── dashboard.service.ts
│   └── dashboard.module.ts
├── areas/
│   ├── areas.controller.ts
│   ├── areas.service.ts
│   ├── areas.module.ts
│   └── dto/
│       ├── create-area.dto.ts
│       └── update-area.dto.ts
├── common/
│   ├── decorators/
│   │   └── current-user.decorator.ts
│   └── interfaces/
│       └── user.interface.ts
└── main.ts
```

## 🚀 Implementation Steps

### Step 1: Configure Main Application

**File**: `src/main.ts`

```typescript
import { NestFactory } from '@nestjs/core';
import { ValidationPipe } from '@nestjs/common';
import { AppModule } from './app.module';

async function bootstrap() {
  const app = await NestFactory.create(AppModule);
  
  // Enable CORS for mobile app
  app.enableCors({
    origin: '*', // In production, specify mobile app origin
    credentials: true,
  });
  
  // Enable validation
  app.useGlobalPipes(new ValidationPipe({
    whitelist: true,
    transform: true,
  }));
  
  // Set global prefix
  app.setGlobalPrefix('api');
  
  await app.listen(8080);
  console.log('🚀 Server running on http://localhost:8080');
}
bootstrap();
```

### Step 2: Create Auth Module

**Priority**: HIGH - Required for all other endpoints

#### 2.1 Create DTOs

**File**: `src/auth/dto/validate-token.dto.ts`

```typescript
import { IsString, IsEmail } from 'class-validator';

export class ValidateTokenDto {
  @IsString()
  access_token: string;

  @IsEmail()
  email: string;
}
```

**File**: `src/auth/dto/oauth-validate.dto.ts`

```typescript
import { IsString, IsEmail, IsObject, IsOptional } from 'class-validator';

export class OAuthValidateDto {
  @IsString()
  access_token: string;

  @IsString()
  provider: string;

  @IsEmail()
  email: string;

  @IsObject()
  @IsOptional()
  user_metadata?: any;
}
```

#### 2.2 Create Auth Service

**File**: `src/auth/auth.service.ts`

```typescript
import { Injectable } from '@nestjs/common';
import { JwtService } from '@nestjs/jwt';
import { ValidateTokenDto, OAuthValidateDto } from './dto';

@Injectable()
export class AuthService {
  constructor(private jwtService: JwtService) {}

  async validateToken(dto: ValidateTokenDto) {
    // TODO: Validate with Supabase
    // For now, create a mock user
    const user = {
      id: 'user-uuid-' + Date.now(),
      email: dto.email,
      created_at: new Date().toISOString(),
    };

    const token = this.jwtService.sign({
      sub: user.id,
      email: user.email,
    });

    return {
      access_token: token,
      refresh_token: 'refresh-token-placeholder',
      user,
    };
  }

  async validateOAuth(dto: OAuthValidateDto) {
    // TODO: Validate with Supabase
    const user = {
      id: 'user-uuid-' + Date.now(),
      email: dto.email,
      full_name: dto.user_metadata?.full_name || '',
      avatar_url: dto.user_metadata?.avatar_url || '',
      provider: dto.provider,
    };

    const token = this.jwtService.sign({
      sub: user.id,
      email: user.email,
    });

    return {
      access_token: token,
      refresh_token: 'refresh-token-placeholder',
      user,
    };
  }
}
```

#### 2.3 Create Auth Controller

**File**: `src/auth/auth.controller.ts`

```typescript
import { Controller, Post, Body } from '@nestjs/common';
import { AuthService } from './auth.service';
import { ValidateTokenDto, OAuthValidateDto } from './dto';

@Controller('auth')
export class AuthController {
  constructor(private authService: AuthService) {}

  @Post('validate')
  async validate(@Body() dto: ValidateTokenDto) {
    return this.authService.validateToken(dto);
  }

  @Post('oauth/validate')
  async validateOAuth(@Body() dto: OAuthValidateDto) {
    return this.authService.validateOAuth(dto);
  }
}
```

#### 2.4 Create Auth Module

**File**: `src/auth/auth.module.ts`

```typescript
import { Module } from '@nestjs/common';
import { JwtModule } from '@nestjs/jwt';
import { AuthController } from './auth.controller';
import { AuthService } from './auth.service';

@Module({
  imports: [
    JwtModule.register({
      secret: 'your-secret-key', // TODO: Move to env
      signOptions: { expiresIn: '7d' },
    }),
  ],
  controllers: [AuthController],
  providers: [AuthService],
})
export class AuthModule {}
```

### Step 3: Create Services Module

**Priority**: HIGH - Required for service marketplace

#### 3.1 Create Service DTO

**File**: `src/services/dto/service.dto.ts`

```typescript
export class ServiceDto {
  id: string;
  name: string;
  description: string;
  icon_url: string;
  is_connected: boolean;
  connected_at?: string;
}
```

#### 3.2 Create Services Service

**File**: `src/services/services.service.ts`

```typescript
import { Injectable } from '@nestjs/common';

@Injectable()
export class ServicesService {
  // Mock data for now
  private mockServices = [
    {
      id: 'google',
      name: 'Google',
      description: 'Gmail, Calendar, Drive',
      icon_url: 'https://www.google.com/favicon.ico',
      is_connected: false,
    },
    {
      id: 'github',
      name: 'GitHub',
      description: 'Repositories, Issues, Pull Requests',
      icon_url: 'https://github.com/favicon.ico',
      is_connected: false,
    },
    {
      id: 'discord',
      name: 'Discord',
      description: 'Messages, Channels, Servers',
      icon_url: 'https://discord.com/assets/favicon.ico',
      is_connected: false,
    },
    {
      id: 'spotify',
      name: 'Spotify',
      description: 'Music, Playlists, Podcasts',
      icon_url: 'https://www.spotify.com/favicon.ico',
      is_connected: false,
    },
  ];

  async getServices(userId?: string) {
    // TODO: Check user's connected services from database
    return { services: this.mockServices };
  }

  async connectService(serviceId: string, userId: string) {
    // TODO: Generate real OAuth URL
    const authUrls = {
      google: 'https://accounts.google.com/o/oauth2/v2/auth?client_id=YOUR_CLIENT_ID&redirect_uri=YOUR_REDIRECT&scope=email%20profile',
      github: 'https://github.com/login/oauth/authorize?client_id=YOUR_CLIENT_ID&redirect_uri=YOUR_REDIRECT',
      discord: 'https://discord.com/api/oauth2/authorize?client_id=YOUR_CLIENT_ID&redirect_uri=YOUR_REDIRECT&scope=identify%20email',
      spotify: 'https://accounts.spotify.com/authorize?client_id=YOUR_CLIENT_ID&redirect_uri=YOUR_REDIRECT&scope=user-read-email',
    };

    return {
      auth_url: authUrls[serviceId] || 'https://example.com/oauth',
    };
  }

  async disconnectService(serviceId: string, userId: string) {
    // TODO: Remove service connection from database
    return {
      success: true,
      message: 'Service disconnected successfully',
    };
  }

  async getConnectedServices(userId: string) {
    // TODO: Get from database
    return { services: [] };
  }
}
```

#### 3.3 Create Services Controller

**File**: `src/services/services.controller.ts`

```typescript
import { Controller, Get, Post, Param, UseGuards } from '@nestjs/common';
import { ServicesService } from './services.service';
// import { JwtAuthGuard } from '../auth/guards/jwt-auth.guard';
// import { CurrentUser } from '../common/decorators/current-user.decorator';

@Controller('services')
// @UseGuards(JwtAuthGuard) // TODO: Enable after implementing JWT guard
export class ServicesController {
  constructor(private servicesService: ServicesService) {}

  @Get()
  async getServices(/* @CurrentUser() user */) {
    return this.servicesService.getServices();
  }

  @Post(':serviceId/connect')
  async connectService(
    @Param('serviceId') serviceId: string,
    /* @CurrentUser() user */
  ) {
    return this.servicesService.connectService(serviceId, 'mock-user-id');
  }

  @Post(':serviceId/disconnect')
  async disconnectService(
    @Param('serviceId') serviceId: string,
    /* @CurrentUser() user */
  ) {
    return this.servicesService.disconnectService(serviceId, 'mock-user-id');
  }

  @Get('connected')
  async getConnectedServices(/* @CurrentUser() user */) {
    return this.servicesService.getConnectedServices('mock-user-id');
  }
}
```

#### 3.4 Create Services Module

**File**: `src/services/services.module.ts`

```typescript
import { Module } from '@nestjs/common';
import { ServicesController } from './services.controller';
import { ServicesService } from './services.service';

@Module({
  controllers: [ServicesController],
  providers: [ServicesService],
})
export class ServicesModule {}
```

### Step 4: Create Dashboard Module

**Priority**: MEDIUM

#### 4.1 Create Dashboard Service

**File**: `src/dashboard/dashboard.service.ts`

```typescript
import { Injectable } from '@nestjs/common';

@Injectable()
export class DashboardService {
  async getStats(userId: string) {
    // TODO: Get real stats from database
    return {
      active_areas: 5,
      connected_services: 3,
      total_executions: 42,
      recent_activities: [
        {
          id: 'activity-1',
          area_name: 'Gmail to Discord',
          action: 'Message sent to Discord',
          timestamp: new Date().toISOString(),
          success: true,
        },
        {
          id: 'activity-2',
          area_name: 'GitHub to Slack',
          action: 'Notification sent',
          timestamp: new Date(Date.now() - 3600000).toISOString(),
          success: true,
        },
      ],
    };
  }
}
```

#### 4.2 Create Dashboard Controller

**File**: `src/dashboard/dashboard.controller.ts`

```typescript
import { Controller, Get } from '@nestjs/common';
import { DashboardService } from './dashboard.service';

@Controller('dashboard')
export class DashboardController {
  constructor(private dashboardService: DashboardService) {}

  @Get('stats')
  async getStats() {
    return this.dashboardService.getStats('mock-user-id');
  }
}
```

#### 4.3 Create Dashboard Module

**File**: `src/dashboard/dashboard.module.ts`

```typescript
import { Module } from '@nestjs/common';
import { DashboardController } from './dashboard.controller';
import { DashboardService } from './dashboard.service';

@Module({
  controllers: [DashboardController],
  providers: [DashboardService],
})
export class DashboardModule {}
```

### Step 5: Update App Module

**File**: `src/app.module.ts`

```typescript
import { Module } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { AuthModule } from './auth/auth.module';
import { ServicesModule } from './services/services.module';
import { DashboardModule } from './dashboard/dashboard.module';

@Module({
  imports: [
    ConfigModule.forRoot({
      isGlobal: true,
    }),
    AuthModule,
    ServicesModule,
    DashboardModule,
  ],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
```

## 🧪 Testing

### Start the server

```bash
npm run start:dev
```

### Test endpoints

```bash
# Test auth
curl -X POST http://localhost:8080/api/auth/validate \
  -H "Content-Type: application/json" \
  -d '{"access_token":"test","email":"test@example.com"}'

# Test services
curl http://localhost:8080/api/services

# Test dashboard
curl http://localhost:8080/api/dashboard/stats
```

## 📝 Next Steps

1. **Database Integration** - Add Prisma or TypeORM
2. **Real OAuth** - Implement actual OAuth flows
3. **JWT Guards** - Protect routes with authentication
4. **Areas Module** - Implement AREA CRUD
5. **Service Integrations** - Connect to real APIs (Google, GitHub, etc.)
6. **Error Handling** - Add proper error responses
7. **Validation** - Add more DTO validations
8. **Tests** - Write unit and e2e tests

## 🔗 Resources

- [NestJS Documentation](https://docs.nestjs.com/)
- [Mobile API Requirements](../area/BACKEND_API_REQUIREMENTS.md)
- [Passport JWT](https://docs.nestjs.com/security/authentication)

## 👥 Team

- **Backend**: Maurel & Isaac
- **Mobile**: [Your Name] (API specifications ready)

---

**Status**: 🟡 In Progress
**Last Updated**: [Current Date]
