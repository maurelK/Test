// config/app.js - Configuration générale de l'application
require('dotenv').config();

// Configuration de l'application
const appConfig = {
    // Serveur
    port: parseInt(process.env.PORT) || 3000,
    host: process.env.HOST || 'localhost',
    env: process.env.NODE_ENV || 'development',
    
    // API
    apiVersion: process.env.API_VERSION || 'v1',
    apiPrefix: process.env.API_PREFIX || '/api',
    
    // Sécurité
    corsOrigins: process.env.ALLOWED_ORIGINS ? 
        process.env.ALLOWED_ORIGINS.split(',').map(origin => origin.trim()) : 
        ['http://localhost:3000', 'http://localhost:3001'],
    
    // Rate Limiting
    rateLimitWindow: parseInt(process.env.RATE_LIMIT_WINDOW) || 15 * 60 * 1000, // 15 minutes
    rateLimitMax: parseInt(process.env.RATE_LIMIT_MAX) || 100,
    rateLimitAuthMax: parseInt(process.env.RATE_LIMIT_AUTH_MAX) || 5,
    
    // Upload de fichiers
    uploadDir: process.env.UPLOAD_DIR || './uploads',
    maxFileSize: parseInt(process.env.MAX_FILE_SIZE) || 5 * 1024 * 1024, // 5MB
    allowedFileTypes: ['image/jpeg', 'image/png', 'image/jpg', 'application/pdf'],
    
    // Logging
    logLevel: process.env.LOG_LEVEL || 'info',
    logFile: process.env.LOG_FILE || './logs/app.log',
    logErrorFile: process.env.LOG_ERROR_FILE || './logs/error.log',
    
    // Pagination
    defaultPageSize: parseInt(process.env.DEFAULT_PAGE_SIZE) || 20,
    maxPageSize: parseInt(process.env.MAX_PAGE_SIZE) || 100,
    
    // Timeouts
    requestTimeout: parseInt(process.env.REQUEST_TIMEOUT) || 30000, // 30 secondes
    
    // Features flags
    features: {
        enableSwagger: process.env.ENABLE_SWAGGER !== 'false',
        enableMetrics: process.env.ENABLE_METRICS !== 'false',
        enableAuditLog: process.env.ENABLE_AUDIT_LOG !== 'false',
        enableFileUpload: process.env.ENABLE_FILE_UPLOAD !== 'false'
    }
};

/**
 * Valide la configuration de l'application
 */
const validateAppConfig = () => {
    // Validation du port
    if (isNaN(appConfig.port) || appConfig.port < 1 || appConfig.port > 65535) {
        throw new Error('PORT doit être un nombre entre 1 et 65535');
    }
    
    // Validation de l'environnement
    const validEnvs = ['development', 'test', 'staging', 'production'];
    if (!validEnvs.includes(appConfig.env)) {
        throw new Error(`NODE_ENV doit être l'un de: ${validEnvs.join(', ')}`);
    }
    
    // Validation des origines CORS
    appConfig.corsOrigins.forEach(origin => {
        try {
            new URL(origin);
        } catch (error) {
            throw new Error(`Origine CORS invalide: ${origin}`);
        }
    });
    
    // Validation des tailles de fichier
    if (appConfig.maxFileSize < 1024) { // Minimum 1KB
        throw new Error('MAX_FILE_SIZE doit être au moins 1024 bytes');
    }
    
    console.log('✅ Configuration de l\'application validée');
};

/**
 * Retourne la configuration en fonction de l'environnement
 */
const getConfig = () => {
    validateAppConfig();
    
    // Configuration spécifique par environnement
    const envSpecific = {
        development: {
            debug: true,
            verbose: true,
            corsOrigins: ['http://localhost:3000', 'http://localhost:3001', 'http://localhost:8080']
        },
        test: {
            debug: false,
            verbose: false,
            rateLimitMax: 1000, // Plus permissif pour les tests
            logLevel: 'error' // Moins de logs pendant les tests
        },
        production: {
            debug: false,
            verbose: false,
            secure: true,
            trustProxy: true
        }
    };
    
    return {
        ...appConfig,
        ...envSpecific[appConfig.env]
    };
};

/**
 * Retourne des informations sur l'application
 */
const getAppInfo = () => {
    return {
        name: 'AgriDistriConnect API',
        version: process.env.npm_package_version || '1.0.0',
        description: 'API pour la gestion de distribution d\'intrants agricoles',
        environment: appConfig.env,
        nodeVersion: process.version,
        startTime: new Date().toISOString(),
        features: appConfig.features
    };
};

/**
 * Vérifie si une fonctionnalité est activée
 */
const isFeatureEnabled = (featureName) => {
    return appConfig.features[featureName] === true;
};

/**
 * Configuration pour les en-têtes de sécurité
 */
const getSecurityHeaders = () => {
    return {
        'X-Content-Type-Options': 'nosniff',
        'X-Frame-Options': 'DENY',
        'X-XSS-Protection': '1; mode=block',
        'Strict-Transport-Security': appConfig.env === 'production' ? 
            'max-age=31536000; includeSubDomains' : undefined,
        'Content-Security-Policy': appConfig.env === 'production' ? 
            "default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline'" : undefined
    };
};

// Initialiser la configuration
const config = getConfig();

module.exports = {
    config,
    getConfig,
    getAppInfo,
    isFeatureEnabled,
    getSecurityHeaders,
    validateAppConfig
};