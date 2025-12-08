import { Module } from '@nestjs/common';
import { AboutController } from './about.controller';
import { IntegrationsModule } from '../integrations/integrations.module';

@Module({
  imports: [IntegrationsModule],
  controllers: [AboutController],
})
export class AboutModule {}
