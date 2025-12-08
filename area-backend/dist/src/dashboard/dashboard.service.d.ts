export declare class DashboardService {
    getStats(userId: string): Promise<{
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
