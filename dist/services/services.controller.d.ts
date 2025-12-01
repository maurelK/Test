import { ServicesService } from './services.service';
export declare class ServicesController {
    private servicesService;
    constructor(servicesService: ServicesService);
    getServices(): Promise<{
        services: {
            id: string;
            name: string;
            description: string;
            icon_url: string;
            is_connected: boolean;
        }[];
    }>;
    connectService(serviceId: string): Promise<{
        auth_url: any;
    }>;
    disconnectService(serviceId: string): Promise<{
        success: boolean;
        message: string;
    }>;
    getConnectedServices(): Promise<{
        services: never[];
    }>;
}
