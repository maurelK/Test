import { AreasService } from './areas.service';
import { CreateAreaDto, UpdateAreaDto } from './dto';
export declare class AreasController {
    private readonly areasService;
    constructor(areasService: AreasService);
    create(createAreaDto: CreateAreaDto): import("./areas.service").Area;
    findAll(): {
        areas: import("./areas.service").Area[];
        total: number;
    };
    findOne(id: string): import("./areas.service").Area;
    update(id: string, updateAreaDto: UpdateAreaDto): import("./areas.service").Area;
    remove(id: string): {
        success: boolean;
        message: string;
    };
    toggleActive(id: string): import("./areas.service").Area;
    execute(id: string): {
        success: boolean;
        message: string;
        execution_time?: undefined;
    } | {
        success: boolean;
        message: string;
        execution_time: string;
    };
}
