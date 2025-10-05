// src/pages/BeneficiaryPage.js
import React from 'react';
import { Typography, Box, Paper, Alert } from '@mui/material';
import DebugPage from '../components/common/DebugPage';

const BeneficiaryPage = () => {
  return (
    <Box sx={{ p: 3 }}>
      <DebugPage pageName="Bénéficiaires" />
      
      <Paper sx={{ p: 3, mt: 3, backgroundColor: 'white' }}>
        <Typography variant="h5" gutterBottom color="primary">
          Gestion des Bénéficiaires
        </Typography>
        <Alert severity="info" sx={{ mb: 2 }}>
          Cette page est en cours de développement. L'interface complète sera disponible bientôt.
        </Alert>
        <Typography variant="body1">
          Fonctionnalités à venir :
        </Typography>
        <ul>
          <li>Liste des bénéficiaires</li>
          <li>Scanner de cartes QR</li>
          <li>Ajout de nouveaux bénéficiaires</li>
          <li>Production de cartes</li>
        </ul>
      </Paper>
    </Box>
  );
};

export default BeneficiaryPage;