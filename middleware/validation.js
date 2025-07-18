// =============================================================================

// middleware/validation.js - Middleware de validation des données
const { body, param, query, validationResult } = require('express-validator');
const { sendValidationError } = require('../utils/response');
const { PAGINATION } = require('../utils/constants');

/**
 * Middleware pour gérer les erreurs de validation
 */
const handleValidationErrors = (req, res, next) => {
    const errors = validationResult(req);
    
    if (!errors.isEmpty()) {
        const formattedErrors = errors.array().map(error => ({
            field: error.param,
            message: error.msg,
            value: error.value,
            location: error.location
        }));
        
        console.warn(`⚠️ Erreurs de validation sur ${req.path}:`, formattedErrors);
        return sendValidationError(res, 'Erreurs de validation', formattedErrors);
    }
    
    next();
};

/**
 * Validations communes pour l'authentification
 */
const validateLogin = [
    body('employeeId')
        .notEmpty()
        .withMessage('ID employé requis')
        .trim()
        .escape()
        .isLength({ min: 2, max: 50 })
        .withMessage('ID employé doit contenir entre 2 et 50 caractères'),
    
    body('password')
        .isLength({ min: 6 })
        .withMessage('Mot de passe requis (minimum 6 caractères)')
        .not()
        .isEmpty()
        .withMessage('Mot de passe ne peut pas être vide'),
    
    handleValidationErrors
];

const validateRegister = [
    body('employeeId')
        .notEmpty()
        .withMessage('ID employé requis')
        .trim()
        .escape()
        .isLength({ min: 2, max: 50 })
        .withMessage('ID employé doit contenir entre 2 et 50 caractères')
        .matches(/^[a-zA-Z0-9_-]+$/)
        .withMessage('ID employé ne peut contenir que lettres, chiffres, tirets et underscores'),
    
    body('firstName')
        .notEmpty()
        .withMessage('Prénom requis')
        .trim()
        .escape()
        .isLength({ min: 2, max: 100 })
        .withMessage('Prénom doit contenir entre 2 et 100 caractères'),
    
    body('lastName')
        .notEmpty()
        .withMessage('Nom requis')
        .trim()
        .escape()
        .isLength({ min: 2, max: 100 })
        .withMessage('Nom doit contenir entre 2 et 100 caractères'),
    
    body('email')
        .isEmail()
        .withMessage('Email valide requis')
        .normalizeEmail()
        .isLength({ max: 255 })
        .withMessage('Email trop long (maximum 255 caractères)'),
    
    body('password')
        .isLength({ min: 8 })
        .withMessage('Mot de passe requis (minimum 8 caractères)')
        .matches(/^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]/)
        .withMessage('Mot de passe doit contenir au moins une majuscule, une minuscule, un chiffre et un caractère spécial'),
    
    body('roleId')
        .isUUID()
        .withMessage('ID de rôle valide requis'),
    
    body('phone')
        .optional()
        .trim()
        .matches(/^\+?[1-9]\d{1,14}$/)
        .withMessage('Numéro de téléphone invalide'),
    
    handleValidationErrors
];

/**
 * Validations pour les bénéficiaires
 */
const validateBeneficiary = [
    body('firstName')
        .notEmpty()
        .withMessage('Prénom requis')
        .trim()
        .escape()
        .isLength({ min: 2, max: 100 })
        .withMessage('Prénom doit contenir entre 2 et 100 caractères'),
    
    body('lastName')
        .notEmpty()
        .withMessage('Nom requis')
        .trim()
        .escape()
        .isLength({ min: 2, max: 100 })
        .withMessage('Nom doit contenir entre 2 et 100 caractères'),
    
    body('phone')
        .optional()
        .trim()
        .matches(/^\+?[1-9]\d{1,14}$/)
        .withMessage('Numéro de téléphone invalide'),
    
    body('email')
        .optional()
        .isEmail()
        .withMessage('Email invalide')
        .normalizeEmail(),
    
    body('gender')
        .optional()
        .isIn(['male', 'female', 'other'])
        .withMessage('Genre doit être: male, female, ou other'),
    
    body('age')
        .optional()
        .isInt({ min: 1, max: 120 })
        .withMessage('Âge doit être entre 1 et 120 ans'),
    
    body('locationId')
        .optional()
        .isUUID()
        .withMessage('ID de localisation invalide'),
    
    body('address')
        .optional()
        .trim()
        .isLength({ max: 500 })
        .withMessage('Adresse trop longue (maximum 500 caractères)'),
    
    body('latitude')
        .optional()
        .isFloat({ min: -90, max: 90 })
        .withMessage('Latitude doit être entre -90 et 90'),
    
    body('longitude')
        .optional()
        .isFloat({ min: -180, max: 180 })
        .withMessage('Longitude doit être entre -180 et 180'),
    
    handleValidationErrors
];

/**
 * Validations pour les distributions
 */
const validateDistribution = [
    body('beneficiaryId')
        .isUUID()
        .withMessage('ID bénéficiaire valide requis'),
    
    body('campaignId')
        .isUUID()
        .withMessage('ID campagne valide requis'),
    
    body('siteId')
        .isUUID()
        .withMessage('ID site valide requis'),
    
    body('items')
        .isArray({ min: 1 })
        .withMessage('Au moins un item requis'),
    
    body('items.*.inputId')
        .isUUID()
        .withMessage('ID intrant valide requis'),
    
    body('items.*.plannedQuantity')
        .isFloat({ min: 0 })
        .withMessage('Quantité prévue doit être positive'),
    
    body('items.*.distributedQuantity')
        .isFloat({ min: 0 })
        .withMessage('Quantité distribuée doit être positive'),
    
    body('isRepresentative')
        .optional()
        .isBoolean()
        .withMessage('isRepresentative doit être un booléen'),
    
    body('paymentMethod')
        .optional()
        .isIn(['free', 'cash', 'mobile_money', 'subsidized'])
        .withMessage('Méthode de paiement invalide'),
    
    body('paymentReference')
        .optional()
        .trim()
        .isLength({ max: 100 })
        .withMessage('Référence de paiement trop longue'),
    
    body('latitude')
        .optional()
        .isFloat({ min: -90, max: 90 })
        .withMessage('Latitude doit être entre -90 et 90'),
    
    body('longitude')
        .optional()
        .isFloat({ min: -180, max: 180 })
        .withMessage('Longitude doit être entre -180 et 180'),
    
    body('notes')
        .optional()
        .trim()
        .isLength({ max: 1000 })
        .withMessage('Notes trop longues (maximum 1000 caractères)'),
    
    handleValidationErrors
];

/**
 * Validations pour les paramètres d'URL
 */
const validateUUIDParam = (paramName = 'id') => [
    param(paramName)
        .isUUID()
        .withMessage(`${paramName} doit être un UUID valide`),
    
    handleValidationErrors
];

const validateQRParam = [
    param('identifier')
        .notEmpty()
        .withMessage('Identifiant requis')
        .trim()
        .escape()
        .isLength({ min: 1, max: 100 })
        .withMessage('Identifiant trop long'),
    
    handleValidationErrors
];

/**
 * Validations pour les paramètres de requête (pagination, filtres)
 */
const validatePagination = [
    query('page')
        .optional()
        .isInt({ min: 1 })
        .withMessage('Page doit être un entier positif')
        .toInt(),
    
    query('limit')
        .optional()
        .isInt({ min: 1, max: PAGINATION.MAX_LIMIT })
        .withMessage(`Limite doit être entre 1 et ${PAGINATION.MAX_LIMIT}`)
        .toInt(),
    
    query('search')
        .optional()
        .trim()
        .escape()
        .isLength({ max: 100 })
        .withMessage('Recherche trop longue (maximum 100 caractères)'),
    
    handleValidationErrors
];

const validateDateRange = [
    query('startDate')
        .optional()
        .isISO8601()
        .withMessage('Date de début doit être au format ISO8601')
        .toDate(),
    
    query('endDate')
        .optional()
        .isISO8601()
        .withMessage('Date de fin doit être au format ISO8601')
        .toDate(),
    
    // Validation personnalisée pour s'assurer que startDate < endDate
    query('endDate')
        .optional()
        .custom((endDate, { req }) => {
            if (req.query.startDate && endDate && new Date(req.query.startDate) >= new Date(endDate)) {
                throw new Error('Date de fin doit être postérieure à la date de début');
            }
            return true;
        }),
    
    handleValidationErrors
];

/**
 * Validation personnalisée pour les filtres
 */
const validateFilters = [
    query('status')
        .optional()
        .isIn(['active', 'inactive', 'pending', 'completed'])
        .withMessage('Statut invalide'),
    
    query('gender')
        .optional()
        .isIn(['male', 'female', 'other'])
        .withMessage('Genre invalide'),
    
    query('category')
        .optional()
        .isIn(['seeds', 'fertilizers', 'pesticides', 'equipment'])
        .withMessage('Catégorie invalide'),
    
    handleValidationErrors
];

module.exports = {
    handleValidationErrors,
    validateLogin,
    validateRegister,
    validateBeneficiary,
    validateDistribution,
    validateUUIDParam,
    validateQRParam,
    validatePagination,
    validateDateRange,
    validateFilters
};