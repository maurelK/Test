import { OnModuleInit } from '@nestjs/common';
import { IService } from '../common/interfaces/service.interface';
import { DiscordService } from './discord/discord.service';
import { TimerService } from './timer/timer.service';
export declare class ServiceRegistry implements OnModuleInit {
    private discordService;
    private timerService;
    private services;
    constructor(discordService: DiscordService, timerService: TimerService);
    onModuleInit(): void;
    private registerService;
    getService(name: string): IService | undefined;
    getAllServices(): IService[];
    getServiceDefinitions(): {};
    executeAction(serviceName: string, actionId: string, params: any, userToken?: string): Promise<boolean>;
    executeReaction(serviceName: string, reactionId: string, params: any, userToken?: string): Promise<void>;
}
