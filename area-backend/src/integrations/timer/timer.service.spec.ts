import { TimerService } from './timer.service';

describe('TimerService', () => {
  let service: TimerService;

  beforeEach(() => {
    service = new TimerService();
  });

  it('should trigger when lastTriggered is not provided', async () => {
    const result = await service.checkAction('timer_interval', { intervalMinutes: 10 });
    expect(result).toBe(true);
  });

  it('should not trigger if interval has not elapsed', async () => {
    const now = new Date();
    const fiveMinutesAgo = new Date(now.getTime() - 5 * 60 * 1000).toISOString();
    const result = await service.checkAction('timer_interval', { intervalMinutes: 10, lastTriggered: fiveMinutesAgo });
    expect(result).toBe(false);
  });

  it('should trigger if interval has elapsed', async () => {
    const now = new Date();
    const fifteenMinutesAgo = new Date(now.getTime() - 15 * 60 * 1000).toISOString();
    const result = await service.checkAction('timer_interval', { intervalMinutes: 10, lastTriggered: fifteenMinutesAgo });
    expect(result).toBe(true);
  });

  it('should return false for invalid intervalMinutes', async () => {
    const result = await service.checkAction('timer_interval', { intervalMinutes: -1 });
    expect(result).toBe(false);
  });
});
