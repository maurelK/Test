// middleware/auth.js - Middleware d'authentification et d'autorisation
const config = require('../config');
const { sendAuthError, sendForbiddenError } = require('../utils/response');

/**
 * Middleware d'authentification JWT
 * Vérifie le token et récupère les informations utilisateur
 */
const authenticateToken = async (req, res, next) => {
    try {
        // Extraire le token de l'header Authorization
        const token = config.jwt.extractTokenFromHeader(req);
        
        if (!token) {
            return sendAuthError(res, 'Token d\'accès requis');
        }
        
        // Vérifier et décoder le token
        const decoded = config.jwt.verifyToken(token);
        
        // Vérifier que ce n'est pas un refresh token
        if (decoded.type === 'refresh') {
            return sendAuthError(res, 'Token de type invalide');
        }
        
        // Récupérer les informations utilisateur complètes depuis la base
        const userQuery = `
            SELECT u.*, r.name as role_name, r.permissions, o.name as organization_name
            FROM users u
            LEFT JOIN roles r ON u.role_id = r.id
            LEFT JOIN organizations o ON u.organization_id = o.id
            WHERE u.id = $1 AND u.is_active = true
        `;
        
        const userResult = await config.database.query(userQuery, [decoded.userId], 'auth-middleware');
        
        if (userResult.rows.length === 0) {
            return sendAuthError(res, 'Utilisateur non trouvé ou inactif');
        }
        
        const user = userResult.rows[0];
        
        // Vérifier si le token va bientôt expirer (dans les 30 prochaines minutes)
        const isExpiringSoon = config.jwt.isTokenExpiringSoon(decoded, 30);
        
        // Ajouter les informations utilisateur à la requête
        req.user = user;
        req.token = {
            payload: decoded,
            raw: token,
            isExpiringSoon
        };
        
        // Logger l'accès authentifié
        console.log(`🔐 Accès authentifié: ${user.employee_id} (${user.role_name}) - ${req.method} ${req.path}`);
        
        next();
        
    } catch (error) {
        console.error('❌ Erreur d\'authentification:', error.message);
        
        // Gérer les différents types d'erreurs JWT
        if (error.message === 'Token expiré') {
            return sendAuthError(res, 'Token expiré, veuillez vous reconnecter');
        } else if (error.message === 'Token invalide') {
            return sendAuthError(res, 'Token invalide');
        }
        
        return sendAuthError(res, 'Erreur d\'authentification');
    }
};

/**
 * Middleware d'autorisation par rôles
 * @param {...string} allowedRoles - Rôles autorisés
 */
const authorizeRoles = (...allowedRoles) => {
    return (req, res, next) => {
        // Vérifier que l'utilisateur est authentifié
        if (!req.user) {
            return sendAuthError(res, 'Authentification requise');
        }
        
        // Vérifier le rôle
        if (!allowedRoles.includes(req.user.role_name)) {
            console.warn(`🚫 Accès refusé: ${req.user.employee_id} (${req.user.role_name}) tentative d'accès à ${req.path}`);
            return sendForbiddenError(res, `Accès réservé aux rôles: ${allowedRoles.join(', ')}`);
        }
        
        console.log(`✅ Autorisation accordée: ${req.user.employee_id} (${req.user.role_name})`);
        next();
    };
};

/**
 * Middleware d'autorisation par permissions spécifiques
 * @param {...string} requiredPermissions - Permissions requises
 */
const authorizePermissions = (...requiredPermissions) => {
    return (req, res, next) => {
        if (!req.user) {
            return sendAuthError(res, 'Authentification requise');
        }
        
        const userPermissions = req.user.permissions || {};
        
        // Vérifier chaque permission requise
        const hasAllPermissions = requiredPermissions.every(permission => {
            // Support de permissions hiérarchiques (ex: "users.create", "users.*", "*")
            if (userPermissions['*'] || userPermissions[permission]) {
                return true;
            }
            
            // Vérifier les permissions wildcard (ex: "users.*" pour "users.create")
            const permissionParts = permission.split('.');
            if (permissionParts.length > 1) {
                const wildcardPermission = permissionParts[0] + '.*';
                return userPermissions[wildcardPermission];
            }
            
            return false;
        });
        
        if (!hasAllPermissions) {
            console.warn(`🚫 Permissions insuffisantes: ${req.user.employee_id} - Requis: ${requiredPermissions.join(', ')}`);
            return sendForbiddenError(res, 'Permissions insuffisantes');
        }
        
        next();
    };
};

/**
 * Middleware d'autorisation pour sa propre organisation seulement
 */
const authorizeOwnOrganization = (req, res, next) => {
    if (!req.user) {
        return sendAuthError(res, 'Authentification requise');
    }
    
    // Ajouter l'ID de l'organisation de l'utilisateur pour filtrer les données
    req.userOrganizationId = req.user.organization_id;
    
    next();
};

/**
 * Middleware optionnel - authentifie si token présent mais n'échoue pas si absent
 */
const authenticateOptional = async (req, res, next) => {
    try {
        const token = config.jwt.extractTokenFromHeader(req);
        
        if (token) {
            // Si token présent, essayer de l'authentifier
            await authenticateToken(req, res, next);
        } else {
            // Si pas de token, continuer sans authentification
            next();
        }
    } catch (error) {
        // Si erreur d'authentification, continuer sans authentification
        console.warn('⚠️ Authentification optionnelle échouée:', error.message);
        next();
    }
};

/**
 * Middleware pour vérifier si l'utilisateur peut accéder à une ressource spécifique
 * @param {string} resourceType - Type de ressource (ex: 'beneficiary', 'distribution')
 * @param {string} resourceIdParam - Nom du paramètre contenant l'ID de la ressource
 */
const authorizeResourceAccess = (resourceType, resourceIdParam = 'id') => {
    return async (req, res, next) => {
        try {
            if (!req.user) {
                return sendAuthError(res, 'Authentification requise');
            }
            
            const resourceId = req.params[resourceIdParam];
            
            if (!resourceId) {
                return sendForbiddenError(res, 'ID de ressource requis');
            }
            
            // Les admins ont accès à tout
            if (req.user.role_name === 'admin') {
                return next();
            }
            
            // Vérifier l'accès selon le type de ressource
            let hasAccess = false;
            
            switch (resourceType) {
                case 'beneficiary':
                    // Vérifier que le bénéficiaire appartient à la même organisation
                    const beneficiaryResult = await config.database.query(
                        'SELECT organization_id FROM beneficiaries WHERE id = $1',
                        [resourceId],
                        'resource-access-check'
                    );
                    
                    hasAccess = beneficiaryResult.rows.length > 0 && 
                               beneficiaryResult.rows[0].organization_id === req.user.organization_id;
                    break;
                    
                case 'distribution':
                    // Vérifier via la jointure avec les bénéficiaires
                    const distributionResult = await config.database.query(
                        `SELECT b.organization_id 
                         FROM distributions d 
                         JOIN beneficiaries b ON d.beneficiary_id = b.id 
                         WHERE d.id = $1`,
                        [resourceId],
                        'resource-access-check'
                    );
                    
                    hasAccess = distributionResult.rows.length > 0 && 
                               distributionResult.rows[0].organization_id === req.user.organization_id;
                    break;
                    
                default:
                    console.warn(`⚠️ Type de ressource non géré: ${resourceType}`);
                    hasAccess = false;
            }
            
            if (!hasAccess) {
                console.warn(`🚫 Accès à la ressource refusé: ${req.user.employee_id} - ${resourceType}:${resourceId}`);
                return sendForbiddenError(res, 'Accès à cette ressource non autorisé');
            }
            
            next();
            
        } catch (error) {
            console.error('❌ Erreur lors de la vérification d\'accès à la ressource:', error);
            return sendForbiddenError(res, 'Erreur de vérification d\'accès');
        }
    };
};

module.exports = {
    authenticateToken,
    authorizeRoles,
    authorizePermissions,
    authorizeOwnOrganization,
    authenticateOptional,
    authorizeResourceAccess
};
