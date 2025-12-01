import { Module } from '@nestjs/common';
import { DiscordModule } from './discord/discord.module';
import { TimerModule } from './timer/timer.module';
import { ServiceRegistry } from './service-registry.service';

@Module({
  imports: [
    DiscordModule,
    TimerModule,
    // Add other service modules here as they are created
  ],
  providers: [ServiceRegistry],
  exports: [ServiceRegistry],
})
export class IntegrationsModule {}
