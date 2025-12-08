export interface ServiceActionDefinition {
    id: string;
    name: string;
    description: string;
    type: 'ACTION' | 'REACTION';
    parameters: {
        name: string;
        type: 'string' | 'number' | 'boolean';
        required: boolean;
        description?: string;
    }[];
}
export interface IService {
    name: string;
    getDefinitions(): ServiceActionDefinition[];
    checkAction?(actionId: string, params: any, userToken?: string): Promise<boolean>;
    executeReaction?(reactionId: string, params: any, userToken?: string): Promise<void>;
}
