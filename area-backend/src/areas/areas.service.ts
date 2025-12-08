import { Injectable, NotFoundException } from '@nestjs/common';
import { CreateAreaDto, UpdateAreaDto } from './dto';

export interface Area {
  id: string;
  user_id: string;
  name: string;
  trigger: {
    service: string;
    event: string;
    config: Record<string, any>;
  };
  action: {
    service: string;
    type: string;
    config: Record<string, any>;
  };
  is_active: boolean;
  created_at: string;
  updated_at: string;
  last_triggered: string | null;
  execution_count: number;
}

@Injectable()
export class AreasService {
  // Mock database - will be replaced with real database
  private areas: Map<string, Area> = new Map();

  create(userId: string, createAreaDto: CreateAreaDto): Area {
    const areaId = `area-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

    const area: Area = {
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

  findAll(userId: string) {
    const userAreas = Array.from(this.areas.values()).filter(
      (area) => area.user_id === userId,
    );

    return {
      areas: userAreas,
      total: userAreas.length,
    };
  }

  findOne(userId: string, areaId: string): Area {
    const area = this.areas.get(areaId);

    if (!area) {
      throw new NotFoundException(`AREA with ID ${areaId} not found`);
    }

    if (area.user_id !== userId) {
      throw new NotFoundException(`AREA with ID ${areaId} not found`);
    }

    return area;
  }

  update(userId: string, areaId: string, updateAreaDto: UpdateAreaDto): Area {
    const area = this.findOne(userId, areaId);

    const updatedArea: Area = {
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

  remove(userId: string, areaId: string) {
    const area = this.findOne(userId, areaId);

    this.areas.delete(areaId);

    console.log(`✅ AREA deleted: ${area.name} (${areaId})`);

    return {
      success: true,
      message: 'AREA deleted successfully',
    };
  }

  toggleActive(userId: string, areaId: string): Area {
    const area = this.findOne(userId, areaId);

    const updatedArea: Area = {
      ...area,
      is_active: !area.is_active,
      updated_at: new Date().toISOString(),
    };

    this.areas.set(areaId, updatedArea);

    console.log(
      `✅ AREA ${updatedArea.is_active ? 'activated' : 'deactivated'}: ${updatedArea.name} (${areaId})`,
    );

    return updatedArea;
  }

  // Method to simulate AREA execution (for testing)
  execute(userId: string, areaId: string) {
    const area = this.findOne(userId, areaId);

    if (!area.is_active) {
      return {
        success: false,
        message: 'AREA is not active',
      };
    }

    // Simulate execution
    const updatedArea: Area = {
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
}
