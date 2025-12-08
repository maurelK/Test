import { Injectable, OnModuleInit } from '@nestjs/common';
import { IService } from '../common/interfaces/service.interface';
import { TimerService } from './timer/timer.service';

@Injectable()
export class ServiceRegistry implements OnModuleInit {
  private services: Map<string, IService> = new Map();

  constructor(
    private timerService: TimerService,
    // Add other services here as they are created by teammates
  ) {}

  onModuleInit() {
    // Register all services
    // Add more services here as teammates create them
    this.registerService(this.timerService);

    console.log(
      `✅ Registered ${this.services.size} services:`,
      Array.from(this.services.keys()),
    );
  }

  private registerService(service: IService) {
    this.services.set(service.name, service);
  }

  getService(name: string): IService | undefined {
    return this.services.get(name);
  }

  getAllServices(): IService[] {
    return Array.from(this.services.values());
  }

  getServiceDefinitions() {
    const definitions = {};
    this.services.forEach((service, name) => {
      definitions[name] = service.getDefinitions();
    });
    return definitions;
  }

  async executeAction(
    serviceName: string,
    actionId: string,
    params: any,
    userToken?: string,
  ): Promise<boolean> {
    const service = this.getService(serviceName);
    if (!service || !service.checkAction) {
      throw new Error(`Service ${serviceName} not found or doesn't support actions`);
    }
    return service.checkAction(actionId, params, userToken);
  }

  async executeReaction(
    serviceName: string,
    reactionId: string,
    params: any,
    userToken?: string,
  ): Promise<void> {
    const service = this.getService(serviceName);
    if (!service || !service.executeReaction) {
      throw new Error(
        `Service ${serviceName} not found or doesn't support reactions`,
      );
    }
    return service.executeReaction(reactionId, params, userToken);
  }
}
