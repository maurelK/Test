import { IsString, IsBoolean, IsObject, IsOptional } from 'class-validator';

export class CreateAreaDto {
  @IsString()
  name: string;

  @IsString()
  trigger_service: string;

  @IsString()
  trigger_event: string;

  @IsObject()
  @IsOptional()
  trigger_config?: Record<string, any>;

  @IsString()
  action_service: string;

  @IsString()
  action_type: string;

  @IsObject()
  @IsOptional()
  action_config?: Record<string, any>;

  @IsBoolean()
  @IsOptional()
  is_active?: boolean;
}
