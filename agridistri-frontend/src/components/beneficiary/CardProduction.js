// src/components/beneficiary/CardProduction.js
import React, { useState, useEffect } from 'react';
import {
  Box,
  Button,
  Grid,
  Card,
  CardContent,
  Typography,
  TextField,
  FormControl,
  InputLabel,
  Select,
  MenuItem,
  Alert,
  Paper,
} from '@mui/material';
import { Print, Download, QrCode } from '@mui/icons-material';
import api from '../../services/api';

const CardProduction = ({ beneficiary, onSuccess, onCancel }) => {
  const [beneficiaries, setBeneficiaries] = useState([]);
  const [selectedBeneficiary, setSelectedBeneficiary] = useState(beneficiary);
  const [cardTemplate, setCardTemplate] = useState('standard');

  useEffect(() => {
    if (!beneficiary) {
      fetchBeneficiaries();
    }
  }, [beneficiary]);

  const fetchBeneficiaries = async () => {
    try {
      const response = await api.get('/beneficiaries/');
      setBeneficiaries(response.data.results || response.data || []);
    } catch (error) {
      console.error('Error fetching beneficiaries:', error);
    }
  };

  const handlePrint = () => {
    window.print();
  };

  const handleDownload = () => {
    // Simulation de téléchargement
    const link = document.createElement('a');
    link.href = '#';
    link.download = `carte_${selectedBeneficiary?.card_number || 'beneficiaire'}.pdf`;
    link.click();
  };

  const CardPreview = () => (
    <Paper 
      sx={{ 
        p: 3, 
        border: '2px solid #1976d2',
        borderRadius: 2,
        maxWidth: 400,
        mx: 'auto',
        background: 'linear-gradient(135deg, #f5f5f5 0%, #e3f2fd 100%)'
      }}
    >
      <Box sx={{ textAlign: 'center', mb: 2 }}>
        <Typography variant="h6" color="primary" gutterBottom>
          CARTE BÉNÉFICIAIRE
        </Typography>
        <Typography variant="body2" color="textSecondary">
          AgriDistriConnect
        </Typography>
      </Box>

      <Grid container spacing={2} alignItems="center">
        <Grid item xs={4}>
          <Box
            sx={{
              width: 80,
              height: 80,
              backgroundColor: '#e0e0e0',
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
              borderRadius: 1
            }}
          >
            <QrCode color="disabled" />
          </Box>
        </Grid>
        <Grid item xs={8}>
          <Typography variant="body1" fontWeight="bold">
            {selectedBeneficiary?.first_name} {selectedBeneficiary?.last_name}
          </Typography>
          <Typography variant="body2">
            Carte: {selectedBeneficiary?.card_number || 'BEN20240001'}
          </Typography>
          <Typography variant="body2">
            Village: {selectedBeneficiary?.village_name || selectedBeneficiary?.village?.name || 'Non spécifié'}
          </Typography>
        </Grid>
      </Grid>

      <Box sx={{ mt: 2, pt: 2, borderTop: '1px dashed #ccc' }}>
        <Typography variant="caption" color="textSecondary">
          Cette carte permet d'accéder aux distributions d'intrants agricoles
        </Typography>
      </Box>
    </Paper>
  );

  return (
    <Box>
      {!beneficiary && (
        <Card sx={{ mb: 3 }}>
          <CardContent>
            <Typography variant="h6" gutterBottom>
              Sélection du bénéficiaire
            </Typography>
            <FormControl fullWidth>
              <InputLabel>Bénéficiaire</InputLabel>
              <Select
                value={selectedBeneficiary?.id || ''}
                label="Bénéficiaire"
                onChange={(e) => {
                  const selected = beneficiaries.find(b => b.id === e.target.value);
                  setSelectedBeneficiary(selected);
                }}
              >
                {beneficiaries.map((benef) => (
                  <MenuItem key={benef.id} value={benef.id}>
                    {benef.first_name} {benef.last_name} - {benef.card_number}
                  </MenuItem>
                ))}
              </Select>
            </FormControl>
          </CardContent>
        </Card>
      )}

      {selectedBeneficiary ? (
        <Grid container spacing={3}>
          <Grid item xs={12} md={6}>
            <Card sx={{ mb: 3 }}>
              <CardContent>
                <Typography variant="h6" gutterBottom>
                  Options de production
                </Typography>
                <FormControl fullWidth sx={{ mb: 2 }}>
                  <InputLabel>Modèle de carte</InputLabel>
                  <Select
                    value={cardTemplate}
                    label="Modèle de carte"
                    onChange={(e) => setCardTemplate(e.target.value)}
                  >
                    <MenuItem value="standard">Standard</MenuItem>
                    <MenuItem value="premium">Premium</MenuItem>
                    <MenuItem value="economique">Économique</MenuItem>
                  </Select>
                </FormControl>
                
                <TextField
                  fullWidth
                  label="Quantité"
                  type="number"
                  defaultValue={1}
                  InputProps={{ inputProps: { min: 1, max: 10 } }}
                />
              </CardContent>
            </Card>

            <Box sx={{ display: 'flex', gap: 2 }}>
              <Button
                variant="outlined"
                startIcon={<Download />}
                onClick={handleDownload}
                fullWidth
              >
                Télécharger PDF
              </Button>
              <Button
                variant="contained"
                startIcon={<Print />}
                onClick={handlePrint}
                fullWidth
              >
                Imprimer
              </Button>
            </Box>
          </Grid>

          <Grid item xs={12} md={6}>
            <Typography variant="h6" gutterBottom>
              Aperçu de la carte
            </Typography>
            <CardPreview />
          </Grid>
        </Grid>
      ) : (
        <Alert severity="info">
          Veuillez sélectionner un bénéficiaire pour produire sa carte
        </Alert>
      )}

      <Box sx={{ display: 'flex', gap: 2, justifyContent: 'flex-end', mt: 3 }}>
        <Button onClick={onCancel}>
          Annuler
        </Button>
        <Button 
          variant="contained" 
          onClick={onSuccess}
          disabled={!selectedBeneficiary}
        >
          Terminer
        </Button>
      </Box>
    </Box>
  );
};

export default CardProduction;