export declare class CreateAreaDto {
    name: string;
    trigger_service: string;
    trigger_event: string;
    trigger_config?: Record<string, any>;
    action_service: string;
    action_type: string;
    action_config?: Record<string, any>;
    is_active?: boolean;
}
