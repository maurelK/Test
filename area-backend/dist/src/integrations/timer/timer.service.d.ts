import { IService, ServiceActionDefinition } from '../../common/interfaces/service.interface';
export declare class TimerService implements IService {
    name: string;
    getDefinitions(): ServiceActionDefinition[];
    checkAction(actionId: string, params: any, userToken?: string): Promise<boolean>;
}
