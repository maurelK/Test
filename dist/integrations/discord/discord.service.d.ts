import { IService, ServiceActionDefinition } from '../../common/interfaces/service.interface';
export declare class DiscordService implements IService {
    name: string;
    getDefinitions(): ServiceActionDefinition[];
    executeReaction(reactionId: string, params: any, userToken?: string): Promise<void>;
}
