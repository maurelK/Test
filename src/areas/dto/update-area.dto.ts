import { IsString, IsBoolean, IsObject, IsOptional } from 'class-validator';

export class UpdateAreaDto {
  @IsString()
  @IsOptional()
  name?: string;

  @IsBoolean()
  @IsOptional()
  is_active?: boolean;

  @IsObject()
  @IsOptional()
  trigger_config?: Record<string, any>;

  @IsObject()
  @IsOptional()
  action_config?: Record<string, any>;
}
