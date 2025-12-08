import {
  Controller,
  Get,
  Post,
  Body,
  Patch,
  Param,
  Delete,
  HttpCode,
  HttpStatus,
} from '@nestjs/common';
import { AreasService } from './areas.service';
import { CreateAreaDto, UpdateAreaDto } from './dto';

@Controller('areas')
export class AreasController {
  constructor(private readonly areasService: AreasService) {}

  @Post()
  @HttpCode(HttpStatus.CREATED)
  create(@Body() createAreaDto: CreateAreaDto) {
    // TODO: Get user ID from JWT token
    const userId = 'mock-user-id';
    return this.areasService.create(userId, createAreaDto);
  }

  @Get()
  findAll() {
    // TODO: Get user ID from JWT token
    const userId = 'mock-user-id';
    return this.areasService.findAll(userId);
  }

  @Get(':id')
  findOne(@Param('id') id: string) {
    // TODO: Get user ID from JWT token
    const userId = 'mock-user-id';
    return this.areasService.findOne(userId, id);
  }

  @Patch(':id')
  update(@Param('id') id: string, @Body() updateAreaDto: UpdateAreaDto) {
    // TODO: Get user ID from JWT token
    const userId = 'mock-user-id';
    return this.areasService.update(userId, id, updateAreaDto);
  }

  @Delete(':id')
  @HttpCode(HttpStatus.OK)
  remove(@Param('id') id: string) {
    // TODO: Get user ID from JWT token
    const userId = 'mock-user-id';
    return this.areasService.remove(userId, id);
  }

  @Post(':id/toggle')
  toggleActive(@Param('id') id: string) {
    // TODO: Get user ID from JWT token
    const userId = 'mock-user-id';
    return this.areasService.toggleActive(userId, id);
  }

  @Post(':id/execute')
  execute(@Param('id') id: string) {
    // TODO: Get user ID from JWT token
    const userId = 'mock-user-id';
    return this.areasService.execute(userId, id);
  }
}
