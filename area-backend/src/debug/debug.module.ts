import { Module } from '@nestjs/common';
import { DebugController } from './debug.controller';
import { IntegrationsModule } from '../integrations/integrations.module';

@Module({
  imports: [IntegrationsModule],
  controllers: [DebugController],
})
export class DebugModule {}
