import { AuthService } from './auth.service';
import { ValidateTokenDto, OAuthValidateDto } from './dto';
export declare class AuthController {
    private authService;
    constructor(authService: AuthService);
    validate(dto: ValidateTokenDto): Promise<{
        access_token: string;
        refresh_token: string;
        user: {
            id: any;
            email: any;
            created_at: any;
        };
    }>;
    validateOAuth(dto: OAuthValidateDto): Promise<{
        access_token: string;
        refresh_token: string;
        user: {
            id: any;
            email: any;
            full_name: any;
            avatar_url: any;
            provider: any;
        };
    }>;
}
