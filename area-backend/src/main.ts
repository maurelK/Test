import { NestFactory } from '@nestjs/core';
import { ValidationPipe } from '@nestjs/common';
import { AppModule } from './app.module';

async function bootstrap() {
  const app = await NestFactory.create(AppModule);

  // Enable CORS for mobile app
  app.enableCors({
    origin: '*', // In production, specify mobile app origin
    credentials: true,
  });

  // Enable validation
  app.useGlobalPipes(
    new ValidationPipe({
      whitelist: true,
      transform: true,
    }),
  );

  // Set global prefix
  app.setGlobalPrefix('api');

  const port = process.env.PORT ?? 8080;
  const host = process.env.HOST ?? '0.0.0.0'; // Listen on all interfaces for physical devices
  
  await app.listen(port, host);
  console.log(`🚀 Server running on http://localhost:${port}`);
  console.log(`📱 Physical devices can connect via: http://[YOUR_LOCAL_IP]:${port}`);
}
bootstrap();
