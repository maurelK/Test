/**
 * Quick standalone script to validate TimerService logic without booting Nest.
 * Run with: npx ts-node test-scripts/check-timer-standalone.ts
 */
import { TimerService } from '../src/integrations/timer/timer.service';

async function run() {
  const svc = new TimerService();

  console.log('=== TimerService quick checks ===');

  console.log('- No lastTriggered (should trigger)');
  console.log(await svc.checkAction('timer_interval', { intervalMinutes: 5 }));

  const now = new Date();
  const twoMinutesAgo = new Date(now.getTime() - 2 * 60 * 1000).toISOString();
  console.log('- lastTriggered 2 minutes ago, interval 5 (should be false)');
  console.log(await svc.checkAction('timer_interval', { intervalMinutes: 5, lastTriggered: twoMinutesAgo }));

  const tenMinutesAgo = new Date(now.getTime() - 10 * 60 * 1000).toISOString();
  console.log('- lastTriggered 10 minutes ago, interval 5 (should be true)');
  console.log(await svc.checkAction('timer_interval', { intervalMinutes: 5, lastTriggered: tenMinutesAgo }));
}

run().catch((err) => {
  // eslint-disable-next-line no-console
  console.error('Script error:', err);
  process.exit(1);
});
