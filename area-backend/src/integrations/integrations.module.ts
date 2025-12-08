import { Module } from '@nestjs/common';
import { TimerModule } from './timer/timer.module';
import { ServiceRegistry } from './service-registry.service';

@Module({
  imports: [
    TimerModule,
  ],
  providers: [ServiceRegistry],
  exports: [ServiceRegistry],
})
export class IntegrationsModule {}
