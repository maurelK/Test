"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.ServicesService = void 0;
const common_1 = require("@nestjs/common");
let ServicesService = class ServicesService {
    mockServices = [
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
    async getServices(userId) {
        return { services: this.mockServices };
    }
    async connectService(serviceId, userId) {
        const authUrls = {
            google: 'https://accounts.google.com/o/oauth2/v2/auth?client_id=YOUR_CLIENT_ID&redirect_uri=YOUR_REDIRECT&scope=email%20profile',
            github: 'https://github.com/login/oauth/authorize?client_id=YOUR_CLIENT_ID&redirect_uri=YOUR_REDIRECT',
            discord: 'https://discord.com/api/oauth2/authorize?client_id=YOUR_CLIENT_ID&redirect_uri=YOUR_REDIRECT&scope=identify%20email',
            spotify: 'https://accounts.spotify.com/authorize?client_id=YOUR_CLIENT_ID&redirect_uri=YOUR_REDIRECT&scope=user-read-email',
        };
        return {
            auth_url: authUrls[serviceId] || 'https://example.com/oauth',
        };
    }
    async disconnectService(serviceId, userId) {
        return {
            success: true,
            message: 'Service disconnected successfully',
        };
    }
    async getConnectedServices(userId) {
        return { services: [] };
    }
};
exports.ServicesService = ServicesService;
exports.ServicesService = ServicesService = __decorate([
    (0, common_1.Injectable)()
], ServicesService);
//# sourceMappingURL=services.service.js.map