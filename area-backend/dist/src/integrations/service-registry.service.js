"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.ServiceRegistry = void 0;
const common_1 = require("@nestjs/common");
const timer_service_1 = require("./timer/timer.service");
let ServiceRegistry = class ServiceRegistry {
    timerService;
    services = new Map();
    constructor(timerService) {
        this.timerService = timerService;
    }
    onModuleInit() {
        this.registerService(this.timerService);
        console.log(`✅ Registered ${this.services.size} services:`, Array.from(this.services.keys()));
    }
    registerService(service) {
        this.services.set(service.name, service);
    }
    getService(name) {
        return this.services.get(name);
    }
    getAllServices() {
        return Array.from(this.services.values());
    }
    getServiceDefinitions() {
        const definitions = {};
        this.services.forEach((service, name) => {
            definitions[name] = service.getDefinitions();
        });
        return definitions;
    }
    async executeAction(serviceName, actionId, params, userToken) {
        const service = this.getService(serviceName);
        if (!service || !service.checkAction) {
            throw new Error(`Service ${serviceName} not found or doesn't support actions`);
        }
        return service.checkAction(actionId, params, userToken);
    }
    async executeReaction(serviceName, reactionId, params, userToken) {
        const service = this.getService(serviceName);
        if (!service || !service.executeReaction) {
            throw new Error(`Service ${serviceName} not found or doesn't support reactions`);
        }
        return service.executeReaction(reactionId, params, userToken);
    }
};
exports.ServiceRegistry = ServiceRegistry;
exports.ServiceRegistry = ServiceRegistry = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [timer_service_1.TimerService])
], ServiceRegistry);
//# sourceMappingURL=service-registry.service.js.map