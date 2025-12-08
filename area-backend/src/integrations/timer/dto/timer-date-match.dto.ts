// dto/date-match.dto.ts
import { IsInt, Min, Max } from 'class-validator';

export class DateMatchDto {
  @IsInt()
  @Min(1)
  @Max(31)
  day: number;
  
  @IsInt()
  @Min(1) 
  @Max(12)
  month: number;
}
