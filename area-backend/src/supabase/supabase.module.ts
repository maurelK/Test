import { Module, Global } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { createClient, SupabaseClient } from '@supabase/supabase-js';

export const SUPABASE_CLIENT = 'SUPABASE_CLIENT';

@Global()
@Module({
  providers: [
    {
      provide: SUPABASE_CLIENT,
      useFactory: (configService: ConfigService): SupabaseClient => {
        const supabaseUrl = configService.get<string>('SUPABASE_URL');
        // Prefer service role key when available; fall back to anon key for local/dev usage
        const serviceRoleKey = configService.get<string>('SUPABASE_SERVICE_ROLE_KEY');
        const anonKey = configService.get<string>('SUPABASE_ANON_KEY');

        if (!supabaseUrl) {
          throw new Error('Supabase URL (SUPABASE_URL) must be provided');
        }

        const supabaseKey = serviceRoleKey ?? anonKey;
        if (!supabaseKey) {
          throw new Error(
            'Supabase key must be provided. Set SUPABASE_SERVICE_ROLE_KEY or SUPABASE_ANON_KEY in your environment',
          );
        }

        if (!serviceRoleKey && anonKey) {
          // eslint-disable-next-line no-console
          console.warn(
            '⚠️ Using SUPABASE_ANON_KEY (less privileged) because SUPABASE_SERVICE_ROLE_KEY is not set',
          );
        }

        return createClient(supabaseUrl, supabaseKey);
      },
      inject: [ConfigService],
    },
  ],
  exports: [SUPABASE_CLIENT],
})
export class SupabaseModule {}
