import { Controller, Get, Req } from '@nestjs/common';
import { ServiceRegistry } from '../integrations/service-registry.service';
import type { Request } from 'express';

@Controller()
export class AboutController {
  constructor(private serviceRegistry: ServiceRegistry) {}

  @Get('about.json')
  getAbout(@Req() request: Request) {
    const host = request.get('host') || 'localhost:8080';
    const protocol = request.protocol;

    const services = this.serviceRegistry.getAllServices();
    const serviceDefinitions = this.serviceRegistry.getServiceDefinitions();

    return {
      client: {
        host: `${protocol}://${host}`,
      },
      server: {
        current_time: Math.floor(Date.now() / 1000),
        services: services.map((service) => ({
          name: service.name,
          actions: serviceDefinitions[service.name]
            .filter((def) => def.type === 'ACTION')
            .map((action) => ({
              name: action.id,
              description: action.description,
              parameters: action.parameters,
            })),
          reactions: serviceDefinitions[service.name]
            .filter((def) => def.type === 'REACTION')
            .map((reaction) => ({
              name: reaction.id,
              description: reaction.description,
              parameters: reaction.parameters,
            })),
        })),
      },
    };
  }
}
