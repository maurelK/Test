import { Module } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { AuthModule } from './auth/auth.module';
import { ServicesModule } from './services/services.module';
import { DashboardModule } from './dashboard/dashboard.module';
import { SupabaseModule } from './supabase/supabase.module';
import { IntegrationsModule } from './integrations/integrations.module';
import { AboutModule } from './about/about.module';
import { AreasModule } from './areas/areas.module';

@Module({
  imports: [
    ConfigModule.forRoot({
      isGlobal: true,
    }),
    SupabaseModule,
    AuthModule,
    ServicesModule,
    DashboardModule,
    AreasModule,
    IntegrationsModule,
    AboutModule,
  ],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
