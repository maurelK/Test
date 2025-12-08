"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.AreasService = void 0;
const common_1 = require("@nestjs/common");
let AreasService = class AreasService {
    areas = new Map();
    create(userId, createAreaDto) {
        const areaId = `area-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
        const area = {
            id: areaId,
            user_id: userId,
            name: createAreaDto.name,
            trigger: {
                service: createAreaDto.trigger_service,
                event: createAreaDto.trigger_event,
                config: createAreaDto.trigger_config || {},
            },
            action: {
                service: createAreaDto.action_service,
                type: createAreaDto.action_type,
                config: createAreaDto.action_config || {},
            },
            is_active: createAreaDto.is_active ?? true,
            created_at: new Date().toISOString(),
            updated_at: new Date().toISOString(),
            last_triggered: null,
            execution_count: 0,
        };
        this.areas.set(areaId, area);
        console.log(`✅ AREA created: ${area.name} (${areaId})`);
        return area;
    }
    findAll(userId) {
        const userAreas = Array.from(this.areas.values()).filter((area) => area.user_id === userId);
        return {
            areas: userAreas,
            total: userAreas.length,
        };
    }
    findOne(userId, areaId) {
        const area = this.areas.get(areaId);
        if (!area) {
            throw new common_1.NotFoundException(`AREA with ID ${areaId} not found`);
        }
        if (area.user_id !== userId) {
            throw new common_1.NotFoundException(`AREA with ID ${areaId} not found`);
        }
        return area;
    }
    update(userId, areaId, updateAreaDto) {
        const area = this.findOne(userId, areaId);
        const updatedArea = {
            ...area,
            name: updateAreaDto.name ?? area.name,
            is_active: updateAreaDto.is_active ?? area.is_active,
            trigger: {
                ...area.trigger,
                config: updateAreaDto.trigger_config ?? area.trigger.config,
            },
            action: {
                ...area.action,
                config: updateAreaDto.action_config ?? area.action.config,
            },
            updated_at: new Date().toISOString(),
        };
        this.areas.set(areaId, updatedArea);
        console.log(`✅ AREA updated: ${updatedArea.name} (${areaId})`);
        return updatedArea;
    }
    remove(userId, areaId) {
        const area = this.findOne(userId, areaId);
        this.areas.delete(areaId);
        console.log(`✅ AREA deleted: ${area.name} (${areaId})`);
        return {
            success: true,
            message: 'AREA deleted successfully',
        };
    }
    toggleActive(userId, areaId) {
        const area = this.findOne(userId, areaId);
        const updatedArea = {
            ...area,
            is_active: !area.is_active,
            updated_at: new Date().toISOString(),
        };
        this.areas.set(areaId, updatedArea);
        console.log(`✅ AREA ${updatedArea.is_active ? 'activated' : 'deactivated'}: ${updatedArea.name} (${areaId})`);
        return updatedArea;
    }
    execute(userId, areaId) {
        const area = this.findOne(userId, areaId);
        if (!area.is_active) {
            return {
                success: false,
                message: 'AREA is not active',
            };
        }
        const updatedArea = {
            ...area,
            last_triggered: new Date().toISOString(),
            execution_count: area.execution_count + 1,
        };
        this.areas.set(areaId, updatedArea);
        console.log(`✅ AREA executed: ${area.name} (${areaId})`);
        return {
            success: true,
            message: 'AREA executed successfully',
            execution_time: new Date().toISOString(),
        };
    }
};
exports.AreasService = AreasService;
exports.AreasService = AreasService = __decorate([
    (0, common_1.Injectable)()
], AreasService);
//# sourceMappingURL=areas.service.js.map