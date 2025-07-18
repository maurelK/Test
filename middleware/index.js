
// middleware/index.js - Point d'entrée pour tous les middlewares
const auth = require('./auth');
const validation = require('./validation');
const upload = require('./upload');
const errorHandler = require('./errorHandler');
const rateLimit = require('./rateLimit');

module.exports = {
    // Authentification et autorisation
    ...auth,
    
    // Validation
    ...validation,
    
    // Upload de fichiers
    ...upload,
    
    // Gestion d'erreurs
    ...errorHandler,
    
    // Rate limiting
    ...rateLimit
};