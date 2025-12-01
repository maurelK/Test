import { Controller, Get, Post, Param } from '@nestjs/common';
import { ServicesService } from './services.service';

@Controller('services')
export class ServicesController {
  constructor(private servicesService: ServicesService) {}

  @Get()
  async getServices() {
    return this.servicesService.getServices();
  }

  @Post(':serviceId/connect')
  async connectService(@Param('serviceId') serviceId: string) {
    return this.servicesService.connectService(serviceId, 'mock-user-id');
  }

  @Post(':serviceId/disconnect')
  async disconnectService(@Param('serviceId') serviceId: string) {
    return this.servicesService.disconnectService(serviceId, 'mock-user-id');
  }

  @Get('connected')
  async getConnectedServices() {
    return this.servicesService.getConnectedServices('mock-user-id');
  }
}
