"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.DiscordService = void 0;
const common_1 = require("@nestjs/common");
const axios_1 = __importDefault(require("axios"));
let DiscordService = class DiscordService {
    name = 'discord';
    getDefinitions() {
        return [
            {
                id: 'discord_webhook_message',
                name: 'Send message via Webhook',
                description: 'Sends a text message to a Discord channel via Webhook URL',
                type: 'REACTION',
                parameters: [
                    {
                        name: 'webhookUrl',
                        type: 'string',
                        required: true,
                        description: 'Discord Webhook URL',
                    },
                    {
                        name: 'messageContent',
                        type: 'string',
                        required: true,
                        description: 'Message content to send',
                    },
                ],
            },
        ];
    }
    async executeReaction(reactionId, params, userToken) {
        if (reactionId === 'discord_webhook_message') {
            const { webhookUrl, messageContent } = params;
            if (!webhookUrl || !messageContent) {
                throw new common_1.HttpException('Missing required parameters: webhookUrl and messageContent', common_1.HttpStatus.BAD_REQUEST);
            }
            try {
                await axios_1.default.post(webhookUrl, {
                    content: messageContent,
                    username: 'AREA Bot',
                });
                console.log('✅ Discord message sent successfully!');
            }
            catch (error) {
                console.error('❌ Discord Webhook error:', error.message);
                throw new common_1.HttpException('Failed to send Discord message', common_1.HttpStatus.INTERNAL_SERVER_ERROR);
            }
        }
        else {
            throw new common_1.HttpException(`Unknown reaction ID: ${reactionId}`, common_1.HttpStatus.BAD_REQUEST);
        }
    }
};
exports.DiscordService = DiscordService;
exports.DiscordService = DiscordService = __decorate([
    (0, common_1.Injectable)()
], DiscordService);
//# sourceMappingURL=discord.service.js.map