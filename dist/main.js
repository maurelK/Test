"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const core_1 = require("@nestjs/core");
const common_1 = require("@nestjs/common");
const app_module_1 = require("./app.module");
async function bootstrap() {
    const app = await core_1.NestFactory.create(app_module_1.AppModule);
    app.enableCors({
        origin: '*',
        credentials: true,
    });
    app.useGlobalPipes(new common_1.ValidationPipe({
        whitelist: true,
        transform: true,
    }));
    app.setGlobalPrefix('api');
    const port = process.env.PORT ?? 8080;
    const host = process.env.HOST ?? '0.0.0.0';
    await app.listen(port, host);
    console.log(`🚀 Server running on http://localhost:${port}`);
    console.log(`📱 Physical devices can connect via: http://[YOUR_LOCAL_IP]:${port}`);
}
bootstrap();
//# sourceMappingURL=main.js.map