"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
var __param = (this && this.__param) || function (paramIndex, decorator) {
    return function (target, key) { decorator(target, key, paramIndex); }
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.AuthService = void 0;
const common_1 = require("@nestjs/common");
const jwt_1 = require("@nestjs/jwt");
const supabase_js_1 = require("@supabase/supabase-js");
const supabase_module_1 = require("../supabase/supabase.module");
let AuthService = class AuthService {
    jwtService;
    supabase;
    constructor(jwtService, supabase) {
        this.jwtService = jwtService;
        this.supabase = supabase;
    }
    async validateToken(dto) {
        const { data: supabaseUser, error } = await this.supabase.auth.getUser(dto.access_token);
        if (error || !supabaseUser.user) {
            throw new common_1.UnauthorizedException('Invalid Supabase token');
        }
        let { data: user } = await this.supabase
            .from('users')
            .select('*')
            .eq('supabase_id', supabaseUser.user.id)
            .single();
        if (!user) {
            const { data: newUser, error: createError } = await this.supabase
                .from('users')
                .insert({
                supabase_id: supabaseUser.user.id,
                email: dto.email,
            })
                .select()
                .single();
            if (createError) {
                throw new Error('Failed to create user');
            }
            user = newUser;
        }
        const token = this.jwtService.sign({
            sub: user.id,
            email: user.email,
            supabase_id: supabaseUser.user.id,
        });
        return {
            access_token: token,
            refresh_token: 'refresh-token-placeholder',
            user: {
                id: user.id,
                email: user.email,
                created_at: user.created_at,
            },
        };
    }
    async validateOAuth(dto) {
        const { data: supabaseUser, error } = await this.supabase.auth.getUser(dto.access_token);
        if (error || !supabaseUser.user) {
            throw new common_1.UnauthorizedException('Invalid OAuth token');
        }
        let { data: user } = await this.supabase
            .from('users')
            .select('*')
            .eq('supabase_id', supabaseUser.user.id)
            .single();
        if (!user) {
            const { data: newUser, error: createError } = await this.supabase
                .from('users')
                .insert({
                supabase_id: supabaseUser.user.id,
                email: dto.email,
                full_name: dto.user_metadata?.full_name || '',
                avatar_url: dto.user_metadata?.avatar_url || '',
                provider: dto.provider,
            })
                .select()
                .single();
            if (createError) {
                throw new Error('Failed to create user');
            }
            user = newUser;
        }
        const token = this.jwtService.sign({
            sub: user.id,
            email: user.email,
            supabase_id: supabaseUser.user.id,
        });
        return {
            access_token: token,
            refresh_token: 'refresh-token-placeholder',
            user: {
                id: user.id,
                email: user.email,
                full_name: user.full_name,
                avatar_url: user.avatar_url,
                provider: user.provider,
            },
        };
    }
};
exports.AuthService = AuthService;
exports.AuthService = AuthService = __decorate([
    (0, common_1.Injectable)(),
    __param(1, (0, common_1.Inject)(supabase_module_1.SUPABASE_CLIENT)),
    __metadata("design:paramtypes", [jwt_1.JwtService,
        supabase_js_1.SupabaseClient])
], AuthService);
//# sourceMappingURL=auth.service.js.map