import { OnModuleInit } from '@nestjs/common';
import { IService } from '../common/interfaces/service.interface';
import { TimerService } from './timer/timer.service';
export declare class ServiceRegistry implements OnModuleInit {
    private timerService;
    private services;
    constructor(timerService: TimerService);
    onModuleInit(): void;
    private registerService;
    getService(name: string): IService | undefined;
    getAllServices(): IService[];
    getServiceDefinitions(): {};
    executeAction(serviceName: string, actionId: string, params: any, userToken?: string): Promise<boolean>;
    executeReaction(serviceName: string, reactionId: string, params: any, userToken?: string): Promise<void>;
}
