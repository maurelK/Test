// =============================================================================

// middleware/upload.js - Middleware de gestion des uploads
const multer = require('multer');
const path = require('path');
const fs = require('fs');
const config = require('../config');
const { sendValidationError } = require('../utils/response');

// Configuration du stockage
const storage = multer.diskStorage({
    destination: (req, file, cb) => {
        const uploadDir = config.config.uploadDir || './uploads';
        
        // Créer le dossier s'il n'existe pas
        if (!fs.existsSync(uploadDir)) {
            fs.mkdirSync(uploadDir, { recursive: true });
        }
        
        // Organiser par type de fichier et date
        const today = new Date().toISOString().split('T')[0]; // YYYY-MM-DD
        const typeDir = path.join(uploadDir, file.fieldname, today);
        
        if (!fs.existsSync(typeDir)) {
            fs.mkdirSync(typeDir, { recursive: true });
        }
        
        cb(null, typeDir);
    },
    
    filename: (req, file, cb) => {
        // Générer un nom de fichier unique
        const uniqueSuffix = Date.now() + '-' + Math.round(Math.random() * 1E9);
        const sanitizedOriginalName = file.originalname.replace(/[^a-zA-Z0-9.-]/g, '_');
        const filename = `${file.fieldname}-${uniqueSuffix}-${sanitizedOriginalName}`;
        
        cb(null, filename);
    }
});

// Configuration multer
const upload = multer({
    storage: storage,
    limits: {
        fileSize: config.config.maxFileSize,
        files: 5, // Maximum 5 fichiers par requête
        fields: 10 // Maximum 10 champs de formulaire
    },
    fileFilter: (req, file, cb) => {
        // Vérifier le type de fichier
        const allowedTypes = config.config.allowedFileTypes;
        
        if (allowedTypes.includes(file.mimetype)) {
            cb(null, true);
        } else {
            const error = new Error(`Type de fichier non autorisé: ${file.mimetype}. Types autorisés: ${allowedTypes.join(', ')}`);
            error.code = 'INVALID_FILE_TYPE';
            cb(error, false);
        }
    }
});

/**
 * Middleware pour upload d'un seul fichier
 * @param {string} fieldName - Nom du champ de fichier
 * @param {boolean} required - Si le fichier est obligatoire
 */
const uploadSingle = (fieldName, required = false) => {
    return (req, res, next) => {
        const singleUpload = upload.single(fieldName);
        
        singleUpload(req, res, (err) => {
            if (err) {
                console.error('❌ Erreur upload:', err);
                
                if (err.code === 'LIMIT_FILE_SIZE') {
                    return sendValidationError(res, 'Fichier trop volumineux', [{
                        field: fieldName,
                        message: `Taille maximum autorisée: ${Math.round(config.config.maxFileSize / 1024 / 1024)}MB`
                    }]);
                }
                
                if (err.code === 'INVALID_FILE_TYPE') {
                    return sendValidationError(res, 'Type de fichier non autorisé', [{
                        field: fieldName,
                        message: err.message
                    }]);
                }
                
                return sendValidationError(res, 'Erreur lors de l\'upload', [{
                    field: fieldName,
                    message: err.message
                }]);
            }
            
            // Vérifier si le fichier est requis
            if (required && !req.file) {
                return sendValidationError(res, 'Fichier requis', [{
                    field: fieldName,
                    message: 'Ce fichier est obligatoire'
                }]);
            }
            
            // Ajouter des métadonnées sur le fichier
            if (req.file) {
                req.file.uploadedAt = new Date().toISOString();
                req.file.uploadedBy = req.user?.id || 'anonymous';
                
                console.log(`📎 Fichier uploadé: ${req.file.filename} (${Math.round(req.file.size / 1024)}KB)`);
            }
            
            next();
        });
    };
};

/**
 * Middleware pour upload de plusieurs fichiers
 * @param {string} fieldName - Nom du champ de fichiers
 * @param {number} maxCount - Nombre maximum de fichiers
 */
const uploadMultiple = (fieldName, maxCount = 5) => {
    return (req, res, next) => {
        const multipleUpload = upload.array(fieldName, maxCount);
        
        multipleUpload(req, res, (err) => {
            if (err) {
                console.error('❌ Erreur upload multiple:', err);
                
                if (err.code === 'LIMIT_FILE_COUNT') {
                    return sendValidationError(res, 'Trop de fichiers', [{
                        field: fieldName,
                        message: `Maximum ${maxCount} fichiers autorisés`
                    }]);
                }
                
                return sendValidationError(res, 'Erreur lors de l\'upload', [{
                    field: fieldName,
                    message: err.message
                }]);
            }
            
            // Ajouter des métadonnées
            if (req.files && req.files.length > 0) {
                req.files.forEach(file => {
                    file.uploadedAt = new Date().toISOString();
                    file.uploadedBy = req.user?.id || 'anonymous';
                });
                
                console.log(`📎 ${req.files.length} fichiers uploadés`);
            }
            
            next();
        });
    };
};

/**
 * Middleware pour upload de champs mixtes (texte + fichiers)
 */
const uploadFields = (fields) => {
    return (req, res, next) => {
        const fieldsUpload = upload.fields(fields);
        
        fieldsUpload(req, res, (err) => {
            if (err) {
                console.error('❌ Erreur upload fields:', err);
                return sendValidationError(res, 'Erreur lors de l\'upload', [{
                    field: 'files',
                    message: err.message
                }]);
            }
            
            next();
        });
    };
};

/**
 * Middleware pour nettoyer les fichiers uploadés en cas d'erreur
 */
const cleanupOnError = (req, res, next) => {
    const originalSend = res.send;
    
    res.send = function(data) {
        // Si c'est une erreur et qu'il y a des fichiers uploadés, les supprimer
        if (res.statusCode >= 400) {
            const filesToCleanup = [];
            
            if (req.file) {
                filesToCleanup.push(req.file.path);
            }
            
            if (req.files) {
                if (Array.isArray(req.files)) {
                    filesToCleanup.push(...req.files.map(f => f.path));
                } else {
                    Object.values(req.files).forEach(fileArray => {
                        filesToCleanup.push(...fileArray.map(f => f.path));
                    });
                }
            }
            
            // Supprimer les fichiers de manière asynchrone
            filesToCleanup.forEach(filePath => {
                fs.unlink(filePath, (err) => {
                    if (err) {
                        console.warn(`⚠️ Impossible de supprimer le fichier ${filePath}:`, err.message);
                    } else {
                        console.log(`🗑️ Fichier nettoyé: ${filePath}`);
                    }
                });
            });
        }
        
        originalSend.call(this, data);
    };
    
    next();
};

module.exports = {
    upload,
    uploadSingle,
    uploadMultiple,
    uploadFields,
    cleanupOnError
};