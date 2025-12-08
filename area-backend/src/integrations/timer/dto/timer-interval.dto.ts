import { IsNumber, IsOptional, Min, IsString } from 'class-validator';

export class TimerIntervalDto {
  @IsNumber()
  @Min(1)
  intervalMinutes: number;

  @IsOptional()
  @IsString()
  lastTriggered?: string;
}
