import { Controller, Post, Body } from '@nestjs/common';
import { ServiceRegistry } from '../integrations/service-registry.service';

@Controller('debug')
export class DebugController {
  constructor(private readonly serviceRegistry: ServiceRegistry) {}

  @Post('timer-check')
  async timerCheck(@Body() body: any) {
    const { intervalMinutes, lastTriggered } = body || {};

    const params = { intervalMinutes, lastTriggered };

    const shouldTrigger = await this.serviceRegistry.executeAction(
      'timer',
      'timer_interval',
      params,
    );

    return { shouldTrigger };
  }
}
