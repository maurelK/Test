import { Injectable } from '@nestjs/common';

@Injectable()
export class ServicesService {
  // Mock data for now
  private mockServices = [
    {
      id: 'google',
      name: 'Google',
      description: 'Gmail, Calendar, Drive',
      icon_url: 'https://www.google.com/favicon.ico',
      is_connected: false,
    },
    {
      id: 'github',
      name: 'GitHub',
      description: 'Repositories, Issues, Pull Requests',
      icon_url: 'https://github.com/favicon.ico',
      is_connected: false,
    },
    {
      id: 'discord',
      name: 'Discord',
      description: 'Messages, Channels, Servers',
      icon_url: 'https://discord.com/assets/favicon.ico',
      is_connected: false,
    },
    {
      id: 'spotify',
      name: 'Spotify',
      description: 'Music, Playlists, Podcasts',
      icon_url: 'https://www.spotify.com/favicon.ico',
      is_connected: false,
    },
  ];

  async getServices(userId?: string) {
    // TODO: Check user's connected services from database
    return { services: this.mockServices };
  }

  async connectService(serviceId: string, userId: string) {
    // TODO: Generate real OAuth URL
    const authUrls = {
      google:
        'https://accounts.google.com/o/oauth2/v2/auth?client_id=YOUR_CLIENT_ID&redirect_uri=YOUR_REDIRECT&scope=email%20profile',
      github:
        'https://github.com/login/oauth/authorize?client_id=YOUR_CLIENT_ID&redirect_uri=YOUR_REDIRECT',
      discord:
        'https://discord.com/api/oauth2/authorize?client_id=YOUR_CLIENT_ID&redirect_uri=YOUR_REDIRECT&scope=identify%20email',
      spotify:
        'https://accounts.spotify.com/authorize?client_id=YOUR_CLIENT_ID&redirect_uri=YOUR_REDIRECT&scope=user-read-email',
    };

    return {
      auth_url: authUrls[serviceId] || 'https://example.com/oauth',
    };
  }

  async disconnectService(serviceId: string, userId: string) {
    // TODO: Remove service connection from database
    return {
      success: true,
      message: 'Service disconnected successfully',
    };
  }

  async getConnectedServices(userId: string) {
    // TODO: Get from database
    return { services: [] };
  }
}