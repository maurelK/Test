import { ServiceRegistry } from '../integrations/service-registry.service';
import type { Request } from 'express';
export declare class AboutController {
    private serviceRegistry;
    constructor(serviceRegistry: ServiceRegistry);
    getAbout(request: Request): {
        client: {
            host: string;
        };
        server: {
            current_time: number;
            services: {
                name: string;
                actions: any;
                reactions: any;
            }[];
        };
    };
}
