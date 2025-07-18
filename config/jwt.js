
// =============================================================================

// config/jwt.js - Configuration JWT et tokens
const jwt = require('jsonwebtoken');

// Configuration JWT
const jwtConfig = {
    secret: process.env.JWT_SECRET,
    expiresIn: process.env.JWT_EXPIRES_IN || '24h',
    refreshExpiresIn: process.env.JWT_REFRESH_EXPIRES_IN || '7d',
    issuer: process.env.JWT_ISSUER || 'agridistri-api',
    audience: process.env.JWT_AUDIENCE || 'agridistri-client',
    algorithm: 'HS256'
};

// Validation de la configuration JWT
const validateJwtConfig = () => {
    if (!jwtConfig.secret) {
        throw new Error('JWT_SECRET est requis dans les variables d\'environnement');
    }
    
    if (jwtConfig.secret.length < 32) {
        throw new Error('JWT_SECRET doit contenir au moins 32 caractères');
    }
    
    // Vérifier que le secret n'est pas une valeur par défaut
    const defaultSecrets = [
        'your-secret-key',
        'change-me',
        'default-secret',
        'secret'
    ];
    
    if (defaultSecrets.includes(jwtConfig.secret)) {
        throw new Error('JWT_SECRET ne peut pas être une valeur par défaut');
    }
};

// Valider au chargement
validateJwtConfig();

/**
 * Génère un token JWT
 * @param {Object} payload - Données à inclure dans le token
 * @param {Object} options - Options supplémentaires
 * @returns {string} Token JWT
 */
const generateToken = (payload, options = {}) => {
    const tokenOptions = {
        expiresIn: options.expiresIn || jwtConfig.expiresIn,
        issuer: jwtConfig.issuer,
        audience: jwtConfig.audience,
        algorithm: jwtConfig.algorithm
    };
    
    // Ajouter des métadonnées
    const enhancedPayload = {
        ...payload,
        iat: Math.floor(Date.now() / 1000),
        jti: require('crypto').randomBytes(16).toString('hex') // JWT ID unique
    };
    
    return jwt.sign(enhancedPayload, jwtConfig.secret, tokenOptions);
};

/**
 * Génère un refresh token
 * @param {Object} payload - Données minimales
 * @returns {string} Refresh token
 */
const generateRefreshToken = (payload) => {
    return generateToken(
        { userId: payload.userId, type: 'refresh' },
        { expiresIn: jwtConfig.refreshExpiresIn }
    );
};

/**
 * Vérifie et décode un token JWT
 * @param {string} token - Token à vérifier
 * @returns {Object} Payload décodé
 */
const verifyToken = (token) => {
    try {
        return jwt.verify(token, jwtConfig.secret, {
            issuer: jwtConfig.issuer,
            audience: jwtConfig.audience,
            algorithms: [jwtConfig.algorithm]
        });
    } catch (error) {
        // Transformer les erreurs JWT en erreurs plus explicites
        if (error.name === 'TokenExpiredError') {
            throw new Error('Token expiré');
        } else if (error.name === 'JsonWebTokenError') {
            throw new Error('Token invalide');
        } else if (error.name === 'NotBeforeError') {
            throw new Error('Token pas encore valide');
        }
        throw error;
    }
};

/**
 * Décode un token sans vérification (pour debug)
 * @param {string} token - Token à décoder
 * @returns {Object} Payload décodé
 */
const decodeToken = (token) => {
    return jwt.decode(token, { complete: true });
};

/**
 * Extrait le token de l'header Authorization
 * @param {Object} req - Objet request Express
 * @returns {string|null} Token extrait ou null
 */
const extractTokenFromHeader = (req) => {
    const authHeader = req.headers.authorization;
    
    if (!authHeader) {
        return null;
    }
    
    const parts = authHeader.split(' ');
    
    if (parts.length !== 2 || parts[0] !== 'Bearer') {
        return null;
    }
    
    return parts[1];
};

/**
 * Vérifie si un token va bientôt expirer
 * @param {Object} payload - Payload décodé du token
 * @param {number} thresholdMinutes - Seuil en minutes (défaut: 30)
 * @returns {boolean} True si le token expire bientôt
 */
const isTokenExpiringSoon = (payload, thresholdMinutes = 30) => {
    if (!payload.exp) {
        return false;
    }
    
    const expirationTime = payload.exp * 1000; // Convertir en millisecondes
    const thresholdTime = Date.now() + (thresholdMinutes * 60 * 1000);
    
    return expirationTime < thresholdTime;
};

module.exports = {
    jwtConfig,
    generateToken,
    generateRefreshToken,
    verifyToken,
    decodeToken,
    extractTokenFromHeader,
    isTokenExpiringSoon
};

// =============================================================================
