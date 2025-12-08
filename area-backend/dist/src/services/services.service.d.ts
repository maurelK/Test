export declare class ServicesService {
    private mockServices;
    getServices(userId?: string): Promise<{
        services: {
            id: string;
            name: string;
            description: string;
            icon_url: string;
            is_connected: boolean;
        }[];
    }>;
    connectService(serviceId: string, userId: string): Promise<{
        auth_url: any;
    }>;
    disconnectService(serviceId: string, userId: string): Promise<{
        success: boolean;
        message: string;
    }>;
    getConnectedServices(userId: string): Promise<{
        services: never[];
    }>;
}
