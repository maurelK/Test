import { IsString, IsEmail, IsObject, IsOptional } from 'class-validator';

export class OAuthValidateDto {
  @IsString()
  access_token: string;

  @IsString()
  provider: string;

  @IsEmail()
  email: string;

  @IsObject()
  @IsOptional()
  user_metadata?: any;
}
