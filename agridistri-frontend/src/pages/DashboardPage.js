// src/pages/DashboardPage.js
import React, { useState, useEffect } from 'react';
import {
  Grid,
  Typography,
  Box,
  Paper,
} from '@mui/material';
import {
  People,
  Inventory,
  Place,
  Campaign,
  Warning,
  Sync,
} from '@mui/icons-material';
import { useAuth } from '../contexts/AuthContext';
import StatsCard from '../components/dashboard/StatsCard';
import RecentDistributions from '../components/dashboard/RecentDistributions';
import api from '../services/api';

const DashboardPage = () => {
  const { user } = useAuth();
  const [stats, setStats] = useState(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const fetchDashboardData = async () => {
      try {
        const response = await api.get('/dashboard/overview/');
        setStats(response.data);
      } catch (error) {
        console.error('Error fetching dashboard data:', error);
      } finally {
        setLoading(false);
      }
    };

    fetchDashboardData();
  }, []);

  if (loading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <Typography>Chargement...</Typography>
      </Box>
    );
  }

  return (
    <Box sx={{ p: 3 }}>
      <Typography variant="h4" gutterBottom>
        Tableau de bord
      </Typography>
      <Typography variant="body1" color="textSecondary" sx={{ mb: 4 }}>
        Bonjour {user?.first_name} {user?.last_name}, voici l'aperçu de vos activités.
      </Typography>

      {/* Cartes de statistiques */}
      <Grid container spacing={3} sx={{ mb: 4 }}>
        <Grid item xs={12} sm={6} md={3}>
          <StatsCard
            title="Bénéficiaires"
            value={stats?.total_beneficiaries || 0}
            icon={<People />}
            color="#1976d2"
          />
        </Grid>
        <Grid item xs={12} sm={6} md={3}>
          <StatsCard
            title="Sites de distribution"
            value={stats?.total_sites || 0}
            icon={<Place />}
            color="#2e7d32"
          />
        </Grid>
        <Grid item xs={12} sm={6} md={3}>
          <StatsCard
            title="Campagnes actives"
            value={stats?.active_campaigns || 0}
            icon={<Campaign />}
            color="#ed6c02"
          />
        </Grid>
        <Grid item xs={12} sm={6} md={3}>
          <StatsCard
            title="Distributions aujourd'hui"
            value={stats?.today_distributions || 0}
            icon={<Inventory />}
            color="#9c27b0"
          />
        </Grid>
      </Grid>

      <Grid container spacing={3}>
        {/* Alertes */}
        <Grid item xs={12} md={6}>
          <Paper sx={{ p: 3 }}>
            <Typography variant="h6" gutterBottom>
              Alertes
            </Typography>
            <Box sx={{ display: 'flex', flexDirection: 'column', gap: 2 }}>
              {stats?.low_stock_count > 0 && (
                <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
                  <Warning color="warning" />
                  <Typography>
                    {stats.low_stock_count} produit(s) en stock faible
                  </Typography>
                </Box>
              )}
              {stats?.pending_sync > 0 && (
                <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
                  <Sync color="info" />
                  <Typography>
                    {stats.pending_sync} distribution(s) en attente de synchronisation
                  </Typography>
                </Box>
              )}
              {(!stats?.low_stock_count && !stats?.pending_sync) && (
                <Typography color="textSecondary">
                  Aucune alerte pour le moment
                </Typography>
              )}
            </Box>
          </Paper>
        </Grid>

        {/* Distributions récentes */}
        <Grid item xs={12} md={6}>
          <RecentDistributions />
        </Grid>
      </Grid>
    </Box>
  );
};

export default DashboardPage;