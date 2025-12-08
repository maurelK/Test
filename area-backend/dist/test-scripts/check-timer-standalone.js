"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const timer_service_1 = require("../src/integrations/timer/timer.service");
async function run() {
    const svc = new timer_service_1.TimerService();
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
    console.error('Script error:', err);
    process.exit(1);
});
//# sourceMappingURL=check-timer-standalone.js.map