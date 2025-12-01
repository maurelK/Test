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
var __param = (this && this.__param) || function (paramIndex, decorator) {
    return function (target, key) { decorator(target, key, paramIndex); }
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.AboutController = void 0;
const common_1 = require("@nestjs/common");
const service_registry_service_1 = require("../integrations/service-registry.service");
let AboutController = class AboutController {
    serviceRegistry;
    constructor(serviceRegistry) {
        this.serviceRegistry = serviceRegistry;
    }
    getAbout(request) {
        const host = request.get('host') || 'localhost:8080';
        const protocol = request.protocol;
        const services = this.serviceRegistry.getAllServices();
        const serviceDefinitions = this.serviceRegistry.getServiceDefinitions();
        return {
            client: {
                host: `${protocol}://${host}`,
            },
            server: {
                current_time: Math.floor(Date.now() / 1000),
                services: services.map((service) => ({
                    name: service.name,
                    actions: serviceDefinitions[service.name]
                        .filter((def) => def.type === 'ACTION')
                        .map((action) => ({
                        name: action.id,
                        description: action.description,
                        parameters: action.parameters,
                    })),
                    reactions: serviceDefinitions[service.name]
                        .filter((def) => def.type === 'REACTION')
                        .map((reaction) => ({
                        name: reaction.id,
                        description: reaction.description,
                        parameters: reaction.parameters,
                    })),
                })),
            },
        };
    }
};
exports.AboutController = AboutController;
__decorate([
    (0, common_1.Get)('about.json'),
    __param(0, (0, common_1.Req)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", void 0)
], AboutController.prototype, "getAbout", null);
exports.AboutController = AboutController = __decorate([
    (0, common_1.Controller)(),
    __metadata("design:paramtypes", [service_registry_service_1.ServiceRegistry])
], AboutController);
//# sourceMappingURL=about.controller.js.map