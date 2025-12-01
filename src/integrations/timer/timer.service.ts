import { Injectable } from '@nestjs/common';
import { IService, ServiceActionDefinition } from '../../common/interfaces/service.interface';

@Injectable()
export class TimerService implements IService {
  name = 'timer';

  getDefinitions(): ServiceActionDefinition[] {
    return [
      {
        id: 'timer_interval',
        name: 'Timer Interval',
        description: 'Triggers at specified intervals',
        type: 'ACTION',
        parameters: [
          { name: 'intervalMinutes', type: 'number', required: true, description: 'Interval in minutes' },
        ],
      },
    ];
  }

  /**
   * checkAction: returns true when the interval has elapsed since lastTriggered
   * params expected shape: { intervalMinutes: number, lastTriggered?: string }
   */
  async checkAction(actionId: string, params: any, userToken?: string): Promise<boolean> {
    if (actionId !== 'timer_interval') return false;

    const { intervalMinutes, lastTriggered } = params || {};
    if (typeof intervalMinutes !== 'number' || Number.isNaN(intervalMinutes) || intervalMinutes <= 0) {
      // invalid params -> do not trigger
      return false;
    }

    const now = new Date();
    if (!lastTriggered) {
      // never triggered -> trigger immediately
      return true;
    }

    const last = new Date(lastTriggered);
    if (isNaN(last.getTime())) {
      // invalid lastTriggered -> trigger
      return true;
    }

    const diffMinutes = (now.getTime() - last.getTime()) / (1000 * 60);
    return diffMinutes >= intervalMinutes;
  }
}
