import { Injectable } from '@nestjs/common';
import { IService, ServiceActionDefinition } from '../../common/interfaces/service.interface';

@Injectable()
export class TimerService implements IService {
  name = 'timer';

  getDefinitions(): ServiceActionDefinition[] {
    return [
      // Timer Interval: déclenche tous les X minutes (relative time)
      // Ex: dans 5 minutes, envoyer un message sur Discord
      {
        id: 'timer_interval',
        name: 'Timer Interval',
        description: 'Triggers at specified intervals (in minutes)',
        type: 'ACTION',
        parameters: [
          {
            name: 'intervalMinutes',
            type: 'number',
            required: true,
            description: 'Interval in minutes between triggers',
          },
          {
            name: 'lastTriggered',
            type: 'string',
            required: false,
            description: 'ISO timestamp of last execution',
          },
        ],
      },
      // Date Match: déclenche à une date spécifique (jour + mois)
      {
        id: 'timer_date_match',
        name: 'Date Match',
        description: 'Triggers on a specific date (e.g., birthday, deadline)',
        type: 'ACTION',
        parameters: [
          {
            name: 'day',
            type: 'number',
            required: true,
            description: 'Day of month (1-31)',
          },
          {
            name: 'month',
            type: 'number',
            required: true,
            description: 'Month (1-12)',
          },
        ],
      },
      // Time Match: déclenche à une heure spécifique (HH:mm)
      {
        id: 'timer_time_match',
        name: 'Time Match',
        description: 'Triggers at a specific time (e.g., 11:30)',
        type: 'ACTION',
        parameters: [
          {
            name: 'time',
            type: 'string',
            required: true,
            description: 'Time in HH:mm format (e.g., 11:30)',
          },
        ],
      },
    ];
  }


  async checkAction(actionId: string, params: any, userToken?: string): Promise<boolean> {
    switch (actionId) {
      case 'timer_interval':
        return this.checkIntervalMatch(params);
      case 'timer_date_match':
        return this.checkDateMatch(params);
      case 'timer_time_match':
        return this.checkTimeMatch(params);
      default:
        console.warn(`[Timer] Unknown action: ${actionId}`);
        return false;
    }
  }

  private async checkIntervalMatch(params: { intervalMinutes: number; lastTriggered?: string }): Promise<boolean> {
    if (typeof params.intervalMinutes !== 'number' || params.intervalMinutes <= 0) {
      return false;
    }

    if (!params.lastTriggered) {
      console.log('[Timer] Interval: first execution (no lastTriggered)');
      return true;
    }

    const now = new Date();
    const last = new Date(params.lastTriggered);
    if (isNaN(last.getTime())) {
      console.warn('[Timer] Interval: invalid lastTriggered, triggering');
      return true;
    }

    const diffMinutes = (now.getTime() - last.getTime()) / (1000 * 60);
    const shouldTrigger = diffMinutes >= params.intervalMinutes;

    if (shouldTrigger) {
      console.log(`[Timer] Interval match: ${diffMinutes.toFixed(2)}min >= ${params.intervalMinutes}min`);
    }
    return shouldTrigger;
  }

  private async checkDateMatch(params: { day: number; month: number }): Promise<boolean> {
    if (!params.day || !params.month) {
      return false;
    }

    const now = new Date();
    const actualDay = now.getDate();
    const actualMonth = now.getMonth() + 1;
    const isMatching = actualDay === params.day && actualMonth === params.month;

    if (isMatching) {
      console.log(`[Timer] Date match: ${params.day}/${params.month}`);
    }
    return isMatching;
  }

  private async checkTimeMatch(params: { time: string }): Promise<boolean> {
    if (!params.time || typeof params.time !== 'string') {
      return false;
    }

    const now = new Date();
    const hours = String(now.getHours()).padStart(2, '0');
    const minutes = String(now.getMinutes()).padStart(2, '0');
    const actualTime = `${hours}:${minutes}`;
    const isMatching = params.time === actualTime;

    if (isMatching) {
      console.log(`[Timer] Time match: ${params.time}`);
    }
    return isMatching;
  }
}
