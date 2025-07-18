
// config/index.js - Point d'entrée centralisé pour toute la configuration
const databaseConfig = require('./database');
const jwtConfig = require('./jwt');
const appConfig = require('./app');

/**
 * Initialise toute la configuration de l'application
 */
const initializeConfig = async () => {
    console.log('🔧 Initialisation de la configuration...');
    
    try {
        // Valider la configuration de l'application
        appConfig.validateAppConfig();
        
        // Initialiser la base de données
        await databaseConfig.initialize();
        
        console.log('✅ Configuration initialisée avec succès');
        return true;
    } catch (error) {
        console.error('❌ Erreur lors de l\'initialisation de la configuration:', error.message);
        throw error;
    }
};

/**
 * Effectue un health check complet de tous les composants
 */
const fullHealthCheck = async () => {
    const checks = {
        app: {
            status: 'healthy',
            info: appConfig.getAppInfo()
        },
        database: await databaseConfig.healthCheck()
    };
    
    const overallStatus = Object.values(checks).every(check => check.status === 'healthy') ? 
        'healthy' : 'unhealthy';
    
    return {
        status: overallStatus,
        timestamp: new Date().toISOString(),
        checks
    };
};

/**
 * Ferme proprement toutes les connexions
 */
const shutdown = async () => {
    console.log('🛑 Arrêt de l\'application...');
    
    try {
        await databaseConfig.closeAll();
        console.log('✅ Arrêt propre terminé');
    } catch (error) {
        console.error('❌ Erreur lors de l\'arrêt:', error.message);
        throw error;
    }
};

module.exports = {
    // Configurations spécifiques
    database: databaseConfig,
    jwt: jwtConfig,
    app: appConfig,
    
    // Fonctions utilitaires
    initializeConfig,
    fullHealthCheck,
    shutdown,
    
    // Accès rapide aux configs
    config: appConfig.config
};