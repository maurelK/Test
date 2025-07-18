
// middleware/errorHandler.js - Gestionnaire d'erreurs global
const { sendServerError, sendValidationError, sendAuthError } = require('../utils/response');

/**
 * Middleware de gestion d'erreurs global
 * Doit être le dernier middleware dans la chaîne
 */
const errorHandler = (err, req, res, next) => {
    console.error('Erreur non gérée:', {
        message: err.message,
        stack: err.stack,
        url: req.originalUrl,
        method: req.method,
        user: req.user?.id || 'anonymous',
        timestamp: new Date().toISOString()
    });
    
    // Erreurs JWT
    if (err.name === 'JsonWebTokenError') {
        return sendAuthError(res, 'Token invalide');
    }
    
    if (err.name === 'TokenExpiredError') {
        return sendAuthError(res, 'Token expiré, veuillez vous reconnecter');
    }
    
    if (err.name === 'NotBeforeError') {
        return sendAuthError(res, 'Token pas encore valide');
    }
    
    // Erreurs de validation
    if (err.name === 'ValidationError') {
        return sendValidationError(res, 'Erreur de validation', err.details || []);
    }
    
    // Erreurs de base de données PostgreSQL
    if (err.code) {
        switch (err.code) {
            case '23505': // Contrainte unique violée
                return sendValidationError(res, 'Ressource déjà existante', [
                    { field: 'unique', message: 'Cette valeur existe déjà' }
                ]);
            
            case '23503': // Contrainte de clé étrangère violée
                return sendValidationError(res, 'Référence invalide', [
                    { field: 'reference', message: 'Référence vers une ressource inexistante' }
                ]);
            
            case '23502': // Contrainte NOT NULL violée
                return sendValidationError(res, 'Champ requis manquant', [
                    { field: err.column, message: 'Ce champ est requis' }
                ]);
            
            case '23514': // Contrainte CHECK violée
                return sendValidationError(res, 'Valeur invalide', [
                    { field: 'check', message: 'Valeur ne respecte pas les contraintes' }
                ]);
            
            case '08006': // Connexion échouée
            case '08001': // Connexion rejetée
                console.error('Erreur de connexion base de données:', err);
                return sendServerError(res, 'Service temporairement indisponible');
            
            default:
                console.error('Erreur base de données non gérée:', err.code, err.message);
                break;
        }
    }
    
    // Erreurs de syntaxe JSON
    if (err instanceof SyntaxError && err.status === 400 && 'body' in err) {
        return sendValidationError(res, 'JSON invalide', [
            { field: 'body', message: 'Format JSON incorrect' }
        ]);
    }
    
    // Erreurs de timeout
    if (err.code === 'ECONNRESET' || err.code === 'ETIMEDOUT') {
        return sendServerError(res, 'Délai d\'attente dépassé');
    }
    
    // Erreur générique avec code de statut
    if (err.statusCode) {
        const message = err.message || 'Erreur serveur';
        return res.status(err.statusCode).json({
            success: false,
            message,
            timestamp: new Date().toISOString()
        });
    }
    
    // Erreur générique
    const message = process.env.NODE_ENV === 'production' 
        ? 'Erreur interne du serveur' 
        : err.message;
    
    return sendServerError(res, message);
};

/**
 * Middleware pour les routes non trouvées
 */
const notFoundHandler = (req, res) => {
    res.status(404).json({
        success: false,
        message: `Route ${req.method} ${req.originalUrl} non trouvée`,
        timestamp: new Date().toISOString()
    });
};

/**
 * Wrapper pour capturer les erreurs async
 */
const asyncErrorHandler = (fn) => (req, res, next) => {
    Promise.resolve(fn(req, res, next)).catch(next);
};

module.exports = {
    errorHandler,
    notFoundHandler,
    asyncErrorHandler
};
