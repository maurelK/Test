// src/components/beneficiary/BeneficiaryList.js
import React, { useState, useEffect } from 'react';
import {
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Paper,
  Typography,
  Chip,
  Box,
  Alert,
  CircularProgress,
} from '@mui/material';
import api from '../../services/api';

const BeneficiaryList = () => {
  const [beneficiaries, setBeneficiaries] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');

  useEffect(() => {
    const fetchBeneficiaries = async () => {
      try {
        const response = await api.get('/beneficiaries/');
        // Gérer les différentes structures de réponse
        const data = response.data.results || response.data || [];
        setBeneficiaries(data);
      } catch (error) {
        console.error('Error fetching beneficiaries:', error);
        setError('Erreur lors du chargement des bénéficiaires');
      } finally {
        setLoading(false);
      }
    };

    fetchBeneficiaries();
  }, []);

  const getStatusColor = (status) => {
    switch (status) {
      case 'active': return 'success';
      case 'suspended': return 'warning';
      case 'inactive': return 'error';
      default: return 'default';
    }
  };

  const getStatusLabel = (status) => {
    switch (status) {
      case 'active': return 'Actif';
      case 'suspended': return 'Suspendu';
      case 'inactive': return 'Inactif';
      default: return status;
    }
  };

  if (loading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" py={4}>
        <CircularProgress />
      </Box>
    );
  }

  if (error) {
    return (
      <Alert severity="error" sx={{ mb: 2 }}>
        {error}
      </Alert>
    );
  }

  return (
    <Box>
      <Typography variant="h6" gutterBottom>
        Liste des bénéficiaires ({beneficiaries.length})
      </Typography>
      
      {beneficiaries.length === 0 ? (
        <Alert severity="info">
          Aucun bénéficiaire trouvé. Ajoutez des bénéficiaires pour commencer.
        </Alert>
      ) : (
        <TableContainer component={Paper}>
          <Table>
            <TableHead>
              <TableRow>
                <TableCell><strong>Nom complet</strong></TableCell>
                <TableCell><strong>Numéro de carte</strong></TableCell>
                <TableCell><strong>Village</strong></TableCell>
                <TableCell><strong>Téléphone</strong></TableCell>
                <TableCell><strong>Statut</strong></TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {beneficiaries.map((beneficiary) => (
                <TableRow 
                  key={beneficiary.id}
                  sx={{ '&:last-child td, &:last-child th': { border: 0 } }}
                >
                  <TableCell>
                    <Typography variant="body2" fontWeight="medium">
                      {beneficiary.first_name} {beneficiary.last_name}
                    </Typography>
                  </TableCell>
                  <TableCell>
                    <Chip 
                      label={beneficiary.card_number} 
                      size="small" 
                      variant="outlined"
                    />
                  </TableCell>
                  <TableCell>
                    {beneficiary.village_name || beneficiary.village?.name || 'Non spécifié'}
                  </TableCell>
                  <TableCell>
                    {beneficiary.phone || (
                      <Typography variant="body2" color="textSecondary">
                        Non renseigné
                      </Typography>
                    )}
                  </TableCell>
                  <TableCell>
                    <Chip 
                      label={getStatusLabel(beneficiary.status)} 
                      size="small"
                      color={getStatusColor(beneficiary.status)}
                    />
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </TableContainer>
      )}
    </Box>
  );
};

export default BeneficiaryList;