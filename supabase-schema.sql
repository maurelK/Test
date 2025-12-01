-- AREA Backend Database Schema
-- Run this in Supabase SQL Editor

-- Enable UUID extension
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Users Table
CREATE TABLE IF NOT EXISTS users (
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

-- Service Connections Table
CREATE TABLE IF NOT EXISTS service_connections (
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

-- Areas Table
CREATE TABLE IF NOT EXISTS areas (
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
  last_triggered_params JSONB,
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

-- Activities Table (for dashboard)
CREATE TABLE IF NOT EXISTS activities (
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
CREATE INDEX IF NOT EXISTS idx_activities_user_timestamp ON activities(user_id, timestamp DESC);
CREATE INDEX IF NOT EXISTS idx_areas_user_active ON areas(user_id, is_active);
CREATE INDEX IF NOT EXISTS idx_service_connections_user ON service_connections(user_id);

-- Function to update updated_at timestamp
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Trigger for users table
CREATE TRIGGER update_users_updated_at BEFORE UPDATE ON users
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- Trigger for areas table
CREATE TRIGGER update_areas_updated_at BEFORE UPDATE ON areas
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- Grant permissions (adjust as needed)
GRANT ALL ON users TO authenticated;
GRANT ALL ON service_connections TO authenticated;
GRANT ALL ON areas TO authenticated;
GRANT ALL ON activities TO authenticated;

-- Success message
DO $$
BEGIN
  RAISE NOTICE 'Database schema created successfully!';
  RAISE NOTICE 'Tables created: users, service_connections, areas, activities';
  RAISE NOTICE 'RLS policies enabled for all tables';
END $$;
