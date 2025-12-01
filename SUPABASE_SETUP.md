# Supabase Setup Guide

## Overview

This backend uses Supabase for authentication and database. Follow this guide to set up Supabase integration.

## Prerequisites

- Supabase account (https://supabase.com)
- Supabase project created

## Configuration

### 1. Get Supabase Credentials

1. Go to your Supabase project dashboard
2. Navigate to **Settings** → **API**
3. Copy the following:
   - **Project URL** (e.g., `https://xxxxx.supabase.co`)
   - **anon/public key** (for mobile app)
   - **service_role key** (for backend - keep secret!)

### 2. Create .env File

Create a `.env` file in the `area-backend` directory:

```bash
cp .env.example .env
```

Update the values:

```env
# Server
PORT=8080

# Supabase
SUPABASE_URL=https://your-project.supabase.co
SUPABASE_ANON_KEY=your-anon-key-here
SUPABASE_SERVICE_ROLE_KEY=your-service-role-key-here

# JWT
JWT_SECRET=your-super-secret-jwt-key-change-this
```

⚠️ **Important**: Never commit `.env` to git! It's already in `.gitignore`.

### 3. Database Schema

Create the following tables in Supabase:

#### Users Table

```sql
CREATE TABLE users (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  email VARCHAR(255) UNIQUE NOT NULL,
  supabase_id UUID UNIQUE,
  full_name VARCHAR(255),
  avatar_url TEXT,
  provider VARCHAR(50),
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

-- Enable RLS (Row Level Security)
ALTER TABLE users ENABLE ROW LEVEL SECURITY;

-- Policy: Users can read their own data
CREATE POLICY "Users can read own data" ON users
  FOR SELECT USING (auth.uid() = supabase_id);
```

#### Service Connections Table

```sql
CREATE TABLE service_connections (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id UUID REFERENCES users(id) ON DELETE CASCADE,
  service_id VARCHAR(50) NOT NULL,
  access_token TEXT NOT NULL,
  refresh_token TEXT,
  expires_at TIMESTAMP,
  connected_at TIMESTAMP DEFAULT NOW(),
  UNIQUE(user_id, service_id)
);

-- Enable RLS
ALTER TABLE service_connections ENABLE ROW LEVEL SECURITY;

-- Policy: Users can manage their own connections
CREATE POLICY "Users can manage own connections" ON service_connections
  FOR ALL USING (user_id IN (
    SELECT id FROM users WHERE supabase_id = auth.uid()
  ));
```

#### Areas Table

```sql
CREATE TABLE areas (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id UUID REFERENCES users(id) ON DELETE CASCADE,
  name VARCHAR(255) NOT NULL,
  description TEXT,
  trigger_service VARCHAR(50) NOT NULL,
  trigger_event VARCHAR(100) NOT NULL,
  trigger_config JSONB,
  action_service VARCHAR(50) NOT NULL,
  action_name VARCHAR(100) NOT NULL,
  action_config JSONB,
  is_active BOOLEAN DEFAULT true,
  last_executed_at TIMESTAMP,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

-- Enable RLS
ALTER TABLE areas ENABLE ROW LEVEL SECURITY;

-- Policy: Users can manage their own areas
CREATE POLICY "Users can manage own areas" ON areas
  FOR ALL USING (user_id IN (
    SELECT id FROM users WHERE supabase_id = auth.uid()
  ));
```

#### Activities Table (for dashboard)

```sql
CREATE TABLE activities (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id UUID REFERENCES users(id) ON DELETE CASCADE,
  area_id UUID REFERENCES areas(id) ON DELETE CASCADE,
  area_name VARCHAR(255),
  action TEXT NOT NULL,
  success BOOLEAN DEFAULT true,
  error_message TEXT,
  timestamp TIMESTAMP DEFAULT NOW()
);

-- Enable RLS
ALTER TABLE activities ENABLE ROW LEVEL SECURITY;

-- Policy: Users can read their own activities
CREATE POLICY "Users can read own activities" ON activities
  FOR SELECT USING (user_id IN (
    SELECT id FROM users WHERE supabase_id = auth.uid()
  ));

-- Index for faster queries
CREATE INDEX idx_activities_user_timestamp ON activities(user_id, timestamp DESC);
```

### 4. Enable Auth Providers

In Supabase Dashboard:

1. Go to **Authentication** → **Providers**
2. Enable **Email** provider
3. Enable **Google** provider (optional):
   - Add Google Client ID and Secret
   - Set redirect URL: `https://your-project.supabase.co/auth/v1/callback`
4. Enable **GitHub** provider (optional):
   - Add GitHub Client ID and Secret
   - Set redirect URL: `https://your-project.supabase.co/auth/v1/callback`

### 5. Configure Mobile App

Update the mobile app's Supabase config (`area/lib/core/config/supabase_config.dart`):

```dart
class SupabaseConfig {
  static const String supabaseUrl = 'https://your-project.supabase.co';
  static const String supabaseAnonKey = 'your-anon-key-here';
}
```

## Usage in Backend

### Inject Supabase Client

```typescript
import { Inject, Injectable } from '@nestjs/common';
import { SupabaseClient } from '@supabase/supabase-js';
import { SUPABASE_CLIENT } from '../supabase/supabase.module';

@Injectable()
export class YourService {
  constructor(
    @Inject(SUPABASE_CLIENT) private supabase: SupabaseClient,
  ) {}

  async getUsers() {
    const { data, error } = await this.supabase
      .from('users')
      .select('*');
    
    if (error) throw error;
    return data;
  }
}
```

### Validate Supabase Token

```typescript
async validateSupabaseToken(token: string) {
  const { data, error } = await this.supabase.auth.getUser(token);
  
  if (error) {
    throw new UnauthorizedException('Invalid token');
  }
  
  return data.user;
}
```

## Testing

### Test Supabase Connection

```bash
# Start the server
npm run start:dev

# The server should start without errors
# Check logs for Supabase connection
```

### Test with Postman

```bash
# Get a token from mobile app or Supabase dashboard
# Then test endpoints with Authorization header:
Authorization: Bearer your-supabase-token
```

## Security Best Practices

1. **Never expose service_role key** - Only use in backend
2. **Use anon key in mobile** - It's safe for client-side
3. **Enable RLS** - Always enable Row Level Security on tables
4. **Validate tokens** - Always validate Supabase tokens in backend
5. **Use environment variables** - Never hardcode credentials

## Troubleshooting

### Error: "Supabase URL and Service Role Key must be provided"

- Check that `.env` file exists
- Verify `SUPABASE_URL` and `SUPABASE_SERVICE_ROLE_KEY` are set
- Restart the server after changing `.env`

### Error: "Invalid API key"

- Verify you're using the correct key (service_role for backend, anon for mobile)
- Check that the key hasn't been regenerated in Supabase dashboard

### Database queries fail

- Check that tables exist in Supabase
- Verify RLS policies are correctly configured
- Check that user has permission to access the data

## Resources

- [Supabase Documentation](https://supabase.com/docs)
- [Supabase JS Client](https://supabase.com/docs/reference/javascript/introduction)
- [Row Level Security](https://supabase.com/docs/guides/auth/row-level-security)

---

**Status**: 🟡 Configuration Required
**Last Updated**: [Current Date]
