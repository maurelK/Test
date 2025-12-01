"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.TimerService = void 0;
const common_1 = require("@nestjs/common");
let TimerService = class TimerService {
    name = 'timer';
    getDefinitions() {
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
    async checkAction(actionId, params, userToken) {
        if (actionId !== 'timer_interval')
            return false;
        const { intervalMinutes, lastTriggered } = params || {};
        if (typeof intervalMinutes !== 'number' || Number.isNaN(intervalMinutes) || intervalMinutes <= 0) {
            return false;
        }
        const now = new Date();
        if (!lastTriggered) {
            return true;
        }
        const last = new Date(lastTriggered);
        if (isNaN(last.getTime())) {
            return true;
        }
        const diffMinutes = (now.getTime() - last.getTime()) / (1000 * 60);
        return diffMinutes >= intervalMinutes;
    }
};
exports.TimerService = TimerService;
exports.TimerService = TimerService = __decorate([
    (0, common_1.Injectable)()
], TimerService);
//# sourceMappingURL=timer.service.js.map