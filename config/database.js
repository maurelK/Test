// config/database.js - Configuration et gestion de la base de données
const { Pool } = require('pg');

// Configuration de la base de données avec validation
const dbConfig = {
    host: process.env.DB_HOST || 'localhost',
    port: parseInt(process.env.DB_PORT) || 5432,
    database: process.env.DB_NAME || 'agridistri',
    user: process.env.DB_USER || 'agridistri_user',
    password: process.env.DB_PASSWORD,
    
    // Configuration du pool de connexions
    max: parseInt(process.env.DB_POOL_MAX) || 20,
    min: parseInt(process.env.DB_POOL_MIN) || 2,
    idleTimeoutMillis: parseInt(process.env.DB_IDLE_TIMEOUT) || 30000,
    connectionTimeoutMillis: parseInt(process.env.DB_CONNECT_TIMEOUT) || 5000,
    
    // Configuration SSL pour la production
    ssl: process.env.NODE_ENV === 'production' ? {
        rejectUnauthorized: process.env.DB_SSL_REJECT_UNAUTHORIZED !== 'false'
    } : false,
    
    // Configuration des timeouts
    statement_timeout: parseInt(process.env.DB_STATEMENT_TIMEOUT) || 30000,
    query_timeout: parseInt(process.env.DB_QUERY_TIMEOUT) || 30000,
};

// Validation des paramètres critiques
const validateConfig = () => {
    const requiredEnvVars = ['DB_PASSWORD'];
    const missingVars = requiredEnvVars.filter(varName => !process.env[varName]);
    
    if (missingVars.length > 0) {
        throw new Error(`Variables d'environnement manquantes: ${missingVars.join(', ')}`);
    }
    
    if (dbConfig.max < dbConfig.min) {
        throw new Error('DB_POOL_MAX doit être supérieur à DB_POOL_MIN');
    }
};

// Valider la configuration au chargement du module
validateConfig();

// Création du pool de connexions
const pool = new Pool(dbConfig);

// Gestion des événements du pool
pool.on('connect', (client) => {
    console.log('✅ Nouvelle connexion à la base de données établie');
    // Configuration de session par défaut
    client.query('SET TIME ZONE UTC');
    client.query(`SET statement_timeout = ${dbConfig.statement_timeout}`);
});

pool.on('remove', () => {
    console.log('🔌 Connexion fermée du pool');
});

pool.on('error', (err) => {
    console.error('❌ Erreur sur le pool de connexions:', {
        message: err.message,
        code: err.code,
        severity: err.severity
    });
});

// Fonctions utilitaires
const testConnection = async () => {
    try {
        const client = await pool.connect();
        const result = await client.query('SELECT NOW() as current_time, version() as version');
        client.release();
        
        console.log('🔍 Test de connexion réussi');
        console.log(`📅 Heure serveur: ${result.rows[0].current_time}`);
        
        return { success: true, data: result.rows[0] };
    } catch (error) {
        console.error('❌ Échec du test de connexion:', error.message);
        return { success: false, error: error.message };
    }
};

const healthCheck = async () => {
    try {
        const client = await pool.connect();
        const startTime = Date.now();
        
        // Tests de santé multiples
        await client.query('SELECT 1');
        const extensions = await client.query(
            "SELECT extname, extversion FROM pg_extension WHERE extname IN ('uuid-ossp', 'postgis')"
        );
        const tableCount = await client.query(
            "SELECT COUNT(*) as count FROM information_schema.tables WHERE table_schema = 'public' AND table_type = 'BASE TABLE'"
        );
        
        const responseTime = Date.now() - startTime;
        client.release();
        
        return {
            status: 'healthy',
            responseTime: `${responseTime}ms`,
            extensions: extensions.rows,
            tablesCount: parseInt(tableCount.rows[0].count),
            poolStats: {
                total: pool.totalCount,
                idle: pool.idleCount,
                waiting: pool.waitingCount
            }
        };
    } catch (error) {
        return {
            status: 'unhealthy',
            error: error.message
        };
    }
};

const query = async (text, params = [], context = 'unknown') => {
    const startTime = Date.now();
    try {
        const result = await pool.query(text, params);
        const duration = Date.now() - startTime;
        
        // Log pour requêtes lentes
        if (duration > 100) {
            console.warn(`🐌 Requête lente (${duration}ms) dans ${context}`);
        }
        
        return result;
    } catch (error) {
        console.error(`❌ Erreur requête dans ${context}:`, error.message);
        throw error;
    }
};

const transaction = async (callback) => {
    const client = await pool.connect();
    try {
        await client.query('BEGIN');
        const result = await callback(client);
        await client.query('COMMIT');
        return result;
    } catch (error) {
        await client.query('ROLLBACK');
        throw error;
    } finally {
        client.release();
    }
};

const initialize = async () => {
    console.log('🚀 Initialisation de la base de données...');
    const testResult = await testConnection();
    
    if (!testResult.success) {
        throw new Error(`Impossible de se connecter à la base: ${testResult.error}`);
    }
    
    console.log('✅ Base de données initialisée avec succès');
    return true;
};

const closeAll = async () => {
    console.log('🔌 Fermeture des connexions...');
    await pool.end();
    console.log('✅ Toutes les connexions fermées');
};

module.exports = {
    pool,
    query,
    transaction,
    testConnection,
    healthCheck,
    initialize,
    closeAll,
    config: dbConfig
};
