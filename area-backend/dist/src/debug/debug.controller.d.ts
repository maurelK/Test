import { ServiceRegistry } from '../integrations/service-registry.service';
export declare class DebugController {
    private readonly serviceRegistry;
    constructor(serviceRegistry: ServiceRegistry);
    timerCheck(body: any): Promise<{
        shouldTrigger: boolean;
    }>;
}
