import { JwtService } from '@nestjs/jwt';
import { SupabaseClient } from '@supabase/supabase-js';
import { ValidateTokenDto, OAuthValidateDto } from './dto';
export declare class AuthService {
    private jwtService;
    private supabase;
    constructor(jwtService: JwtService, supabase: SupabaseClient);
    validateToken(dto: ValidateTokenDto): Promise<{
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
