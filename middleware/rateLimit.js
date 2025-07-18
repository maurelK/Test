// middleware/rateLimit.js - Rate limiting personnalisé
const rateLimit = require('express-rate-limit');
const config = require('../config');

/**
 * Rate limiter global pour toutes les routes API
 */
const globalLimiter = rateLimit({
    windowMs: config.config.rateLimitWindow, // Fenêtre de temps
    max: config.config.rateLimitMax, // Nombre maximum de requêtes
    message: {
        success: false,
        message: 'Trop de requêtes depuis cette IP, veuillez réessayer plus tard.',
        retryAfter: Math.ceil(config.config.rateLimitWindow / 1000 / 60) + ' minutes'
    },
    standardHeaders: true, // Inclure les headers de rate limit
    legacyHeaders: false, // Désactiver les anciens headers
    
    // Fonction pour identifier les clients (par défaut par IP)
    keyGenerator: (req) => {
        return req.ip;
    },
    
    // Handler personnalisé quand la limite est atteinte
    onLimitReached: (req, res, options) => {
        console.warn(`Rate limit atteint pour IP: ${req.ip}, URL: ${req.originalUrl}`);
    }
});

/**
 * Rate limiter strict pour les routes d'authentification
 */
const authLimiter = rateLimit({
    windowMs: 15 * 60 * 1000, // 15 minutes
    max: config.config.rateLimitAuthMax, // Très limité pour l'auth
    message: {
        success: false,
        message: 'Trop de tentatives de connexion depuis cette IP. Réessayez dans 15 minutes.',
        retryAfter: '15 minutes'
    },
    standardHeaders: true,
    legacyHeaders: false,
    
    // Identifier par IP ET user-agent pour plus de précision
    keyGenerator: (req) => {
        return `${req.ip}-${req.get('User-Agent')}`;
    },
    
    onLimitReached: (req, res, options) => {
        console.warn(`Auth rate limit atteint pour IP: ${req.ip}, employeeId: ${req.body?.employeeId || 'unknown'}`);
    }
});

/**
 * Rate limiter pour les uploads de fichiers
 */
const uploadLimiter = rateLimit({
    windowMs: 60 * 60 * 1000, // 1 heure
    max: 50, // 50 uploads par heure
    message: {
        success: false,
        message: 'Trop d\'uploads depuis cette IP. Réessayez dans 1 heure.',
        retryAfter: '1 hour'
    },
    standardHeaders: true,
    legacyHeaders: false
});

/**
 * Rate limiter flexible pour les recherches
 */
const searchLimiter = rateLimit({
    windowMs: 1 * 60 * 1000, // 1 minute
    max: 30, // 30 recherches par minute
    message: {
        success: false,
        message: 'Trop de recherches depuis cette IP. Réessayez dans 1 minute.',
        retryAfter: '1 minute'
    },
    standardHeaders: true,
    legacyHeaders: false
});

/**
 * Rate limiter pour les créations de ressources
 */
const createLimiter = rateLimit({
    windowMs: 10 * 60 * 1000, // 10 minutes
    max: 20, // 20 créations par 10 minutes
    message: {
        success: false,
        message: 'Trop de créations depuis cette IP. Réessayez dans 10 minutes.',
        retryAfter: '10 minutes'
    },
    standardHeaders: true,
    legacyHeaders: false
});

/**
 * Factory pour créer des limiters personnalisés
 */
const createCustomLimiter = (options) => {
    return rateLimit({
        windowMs: options.windowMs || 15 * 60 * 1000,
        max: options.max || 100,
        message: {
            success: false,
            message: options.message || 'Trop de requêtes.',
            retryAfter: options.retryAfter || 'quelques minutes'
        },
        standardHeaders: true,
        legacyHeaders: false,
        keyGenerator: options.keyGenerator,
        onLimitReached: options.onLimitReached
    });
};

module.exports = {
    globalLimiter,
    authLimiter,
    uploadLimiter,
    searchLimiter,
    createLimiter,
    createCustomLimiter
};
