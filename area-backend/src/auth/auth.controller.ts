import { Controller, Post, Body } from '@nestjs/common';
import { AuthService } from './auth.service';
import { ValidateTokenDto, OAuthValidateDto } from './dto';

@Controller('auth')
export class AuthController {
  constructor(private authService: AuthService) {}

  @Post('validate')
  async validate(@Body() dto: ValidateTokenDto) {
    return this.authService.validateToken(dto);
  }

  @Post('oauth/validate')
  async validateOAuth(@Body() dto: OAuthValidateDto) {
    return this.authService.validateOAuth(dto);
  }
}
