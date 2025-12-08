import { Injectable } from '@nestjs/common';

@Injectable()
export class DashboardService {
  async getStats(userId: string) {
    // TODO: Get real stats from database
    return {
      active_areas: 5,
      connected_services: 3,
      total_executions: 42,
      recent_activities: [
        {
          id: 'activity-1',
          area_name: 'Gmail to Discord',
          action: 'Message sent to Discord',
          timestamp: new Date().toISOString(),
          success: true,
        },
        {
          id: 'activity-2',
          area_name: 'GitHub to Slack',
          action: 'Notification sent',
          timestamp: new Date(Date.now() - 3600000).toISOString(),
          success: true,
        },
        {
          id: 'activity-3',
          area_name: 'Spotify to Discord',
          action: 'Now playing updated',
          timestamp: new Date(Date.now() - 7200000).toISOString(),
          success: true,
        },
      ],
    };
  }
}
