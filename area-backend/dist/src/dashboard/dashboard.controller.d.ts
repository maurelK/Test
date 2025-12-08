import { DashboardService } from './dashboard.service';
export declare class DashboardController {
    private dashboardService;
    constructor(dashboardService: DashboardService);
    getStats(): Promise<{
        active_areas: number;
        connected_services: number;
        total_executions: number;
        recent_activities: {
            id: string;
            area_name: string;
            action: string;
            timestamp: string;
            success: boolean;
        }[];
    }>;
}
