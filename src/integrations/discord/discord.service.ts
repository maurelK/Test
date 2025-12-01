import { Injectable, HttpException, HttpStatus } from '@nestjs/common';
import {
  IService,
  ServiceActionDefinition,
} from '../../common/interfaces/service.interface';
import axios from 'axios';

@Injectable()
export class DiscordService implements IService {
  name = 'discord';

  getDefinitions(): ServiceActionDefinition[] {
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

  // Cette méthode sera appelée par le "Cerveau" (Hook Engine)
  async executeReaction(
    reactionId: string,
    params: any,
    userToken?: string,
  ): Promise<void> {
    if (reactionId === 'discord_webhook_message') {
      const { webhookUrl, messageContent } = params;

      if (!webhookUrl || !messageContent) {
        throw new HttpException(
          'Missing required parameters: webhookUrl and messageContent',
          HttpStatus.BAD_REQUEST,
        );
      }

      try {
        // Appel simple à l'API Webhook de Discord
        await axios.post(webhookUrl, {
          content: messageContent,
          username: 'AREA Bot', // Nom qui apparaîtra
        });

        console.log('✅ Discord message sent successfully!');
      } catch (error) {
        console.error('❌ Discord Webhook error:', error.message);
        throw new HttpException(
          'Failed to send Discord message',
          HttpStatus.INTERNAL_SERVER_ERROR,
        );
      }
    } else {
      throw new HttpException(
        `Unknown reaction ID: ${reactionId}`,
        HttpStatus.BAD_REQUEST,
      );
    }
  }
}
