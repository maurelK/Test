import { IsString, IsEmail } from 'class-validator';

export class ValidateTokenDto {
  @IsString()
  access_token: string;

  @IsEmail()
  email: string;
}
