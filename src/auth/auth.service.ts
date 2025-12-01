import { Injectable, Inject, UnauthorizedException } from '@nestjs/common';
import { JwtService } from '@nestjs/jwt';
import { SupabaseClient } from '@supabase/supabase-js';
import { SUPABASE_CLIENT } from '../supabase/supabase.module';
import { ValidateTokenDto, OAuthValidateDto } from './dto';

@Injectable()
export class AuthService {
  constructor(
    private jwtService: JwtService,
    @Inject(SUPABASE_CLIENT) private supabase: SupabaseClient,
  ) {}

  async validateToken(dto: ValidateTokenDto) {
    // Validate token with Supabase
    const { data: supabaseUser, error } = await this.supabase.auth.getUser(
      dto.access_token,
    );

    if (error || !supabaseUser.user) {
      throw new UnauthorizedException('Invalid Supabase token');
    }

    // Check if user exists in our database, if not create it
    let { data: user } = await this.supabase
      .from('users')
      .select('*')
      .eq('supabase_id', supabaseUser.user.id)
      .single();

    if (!user) {
      // Create user in our database
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

    // Generate our own JWT token
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

  async validateOAuth(dto: OAuthValidateDto) {
    // Validate OAuth token with Supabase
    const { data: supabaseUser, error } = await this.supabase.auth.getUser(
      dto.access_token,
    );

    if (error || !supabaseUser.user) {
      throw new UnauthorizedException('Invalid OAuth token');
    }

    // Check if user exists in our database, if not create it
    let { data: user } = await this.supabase
      .from('users')
      .select('*')
      .eq('supabase_id', supabaseUser.user.id)
      .single();

    if (!user) {
      // Create user in our database
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

    // Generate our own JWT token
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
}
