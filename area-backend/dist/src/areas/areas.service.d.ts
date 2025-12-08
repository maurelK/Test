import { CreateAreaDto, UpdateAreaDto } from './dto';
export interface Area {
    id: string;
    user_id: string;
    name: string;
    trigger: {
        service: string;
        event: string;
        config: Record<string, any>;
    };
    action: {
        service: string;
        type: string;
        config: Record<string, any>;
    };
    is_active: boolean;
    created_at: string;
    updated_at: string;
    last_triggered: string | null;
    execution_count: number;
}
export declare class AreasService {
    private areas;
    create(userId: string, createAreaDto: CreateAreaDto): Area;
    findAll(userId: string): {
        areas: Area[];
        total: number;
    };
    findOne(userId: string, areaId: string): Area;
    update(userId: string, areaId: string, updateAreaDto: UpdateAreaDto): Area;
    remove(userId: string, areaId: string): {
        success: boolean;
        message: string;
    };
    toggleActive(userId: string, areaId: string): Area;
    execute(userId: string, areaId: string): {
        success: boolean;
        message: string;
        execution_time?: undefined;
    } | {
        success: boolean;
        message: string;
        execution_time: string;
    };
}
