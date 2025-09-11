import React, { useState, useEffect } from 'react';
import './Catalogue.css';
import api from '../api';

const Catalogue = ({ lang = 'fr' }) => {
  const [currentPage, setCurrentPage] = useState(1);
  const [filteredProjects, setFilteredProjects] = useState([]);
  const [projectsData, setProjectsData] = useState([]);
  const [searchQuery, setSearchQuery] = useState('');
  const [activeFilter, setActiveFilter] = useState('all');
  const [sortBy, setSortBy] = useState('recent');
  const [viewMode, setViewMode] = useState('grid');
  const [showAdvancedFilters, setShowAdvancedFilters] = useState(false);
  const [sectorFilter, setSectorFilter] = useState('');
  const [locationFilter, setLocationFilter] = useState('');
  const [yearFilter, setYearFilter] = useState('');
  const [needsFilter, setNeedsFilter] = useState('');
  const [favorites, setFavorites] = useState([]);

  const itemsPerPage = 9;

  useEffect(() => {
    async function fetchStartups() {
      try {
        const data = await api.getStartups();
        const formattedData = data.map(startup => ({
          id: startup.id,
          title: startup.name,
          description: startup.description || "Description non disponible",
          status: startup.project_status || "early",
          sector: startup.sector || "tech",
          location: startup.address || "Non spécifié",
          year: startup.created_at ? new Date(startup.created_at).getFullYear().toString() : "2024",
          needs: startup.needs || "investment",
          founder: {
            name: "Fondateur",
            initials: "F",
            role: "CEO"
          },
          tags: ["Tech", "Innovation"],
          funding: "À déterminer",
          team: "Équipe en cours"
        }));
        setProjectsData(formattedData);
        setFilteredProjects(formattedData);
      } catch (error) {
        console.error("Erreur lors de la récupération des startups :", error);
      }
    }
    fetchStartups();
  }, []);

  useEffect(() => {
    const storedFavorites = JSON.parse(localStorage.getItem('innovhub_favorites') || '[]');
    setFavorites(storedFavorites);
  }, []);

  useEffect(() => {
    applyFilters();
  }, [searchQuery, activeFilter, sectorFilter, locationFilter, yearFilter, needsFilter, sortBy]);

  useEffect(() => {
    const handleKeyDown = (e) => {
      if ((e.ctrlKey || e.metaKey) && (e.key === 'f' || e.key === 'k')) {
        e.preventDefault();
        document.getElementById('searchInput')?.focus();
      }
      if (e.key === 'ArrowLeft' && currentPage > 1) {
        setCurrentPage(currentPage - 1);
      }
      if (e.key === 'ArrowRight') {
        const totalPages = Math.ceil(filteredProjects.length / itemsPerPage);
        if (currentPage < totalPages) {
          setCurrentPage(currentPage + 1);
        }
      }
    };

    document.addEventListener('keydown', handleKeyDown);
    return () => document.removeEventListener('keydown', handleKeyDown);
  }, [currentPage, filteredProjects.length]);

  const applyFilters = () => {
    let filtered = [...projectsData];

    if (searchQuery) {
      filtered = filtered.filter(project =>
        project.title.toLowerCase().includes(searchQuery.toLowerCase()) ||
        project.description.toLowerCase().includes(searchQuery.toLowerCase()) ||
        project.tags.some(tag => tag.toLowerCase().includes(searchQuery.toLowerCase())) ||
        project.founder.name.toLowerCase().includes(searchQuery.toLowerCase()) ||
        project.sector.toLowerCase().includes(searchQuery.toLowerCase())
      );
    }

    if (activeFilter !== 'all') {
      filtered = filtered.filter(project => project.status === activeFilter);
    }

    if (sectorFilter) {
      filtered = filtered.filter(project => project.sector === sectorFilter);
    }
    if (locationFilter) {
      filtered = filtered.filter(project => project.location === locationFilter);
    }
    if (yearFilter) {
      if (yearFilter === 'older') {
        filtered = filtered.filter(project => parseInt(project.year) < 2021);
      } else {
        filtered = filtered.filter(project => project.year === yearFilter);
      }
    }
    if (needsFilter) {
      filtered = filtered.filter(project => project.needs === needsFilter);
    }

    switch (sortBy) {
      case 'recent':
        filtered.sort((a, b) => b.year.localeCompare(a.year));
        break;
      case 'alphabetical':
        filtered.sort((a, b) => a.title.localeCompare(b.title));
        break;
      case 'funding':
        filtered.sort((a, b) => {
          const getFundingValue = (funding) => {
            const match = funding.match(/[\d.]+/);
            return match ? parseFloat(match[0]) : 0;
          };
          return getFundingValue(b.funding) - getFundingValue(a.funding);
        });
        break;
      case 'stage':
        const stageOrder = { mature: 4, growth: 3, early: 2, seed: 1 };
        filtered.sort((a, b) => stageOrder[b.status] - stageOrder[a.status]);
        break;
    }

    setFilteredProjects(filtered);
    setCurrentPage(1);
  };

  const toggleFavorite = (projectId) => {
    const newFavorites = favorites.includes(projectId)
      ? favorites.filter(id => id !== projectId)
      : [...favorites, projectId];
    
    setFavorites(newFavorites);
    localStorage.setItem('innovhub_favorites', JSON.stringify(newFavorites));
    
    const project = projectsData.find(p => p.id === projectId);
    showNotification(
      favorites.includes(projectId) 
        ? `${project.title} retiré des favoris` 
        : `${project.title} ajouté aux favoris`
    );
  };

  const showNotification = (message) => {
    const notification = document.createElement('div');
    notification.className = 'notification';
    notification.textContent = message;
    notification.style.cssText = `
      position: fixed;
      top: 20px;
      right: 20px;
      background: #FF6B35;
      color: white;
      padding: 12px 20px;
      border-radius: 8px;
      z-index: 10000;
      animation: slideInRight 0.3s ease;
      box-shadow: 0 4px 15px rgba(255, 107, 53, 0.3);
      font-weight: 600;
      max-width: 300px;
      word-wrap: break-word;
    `;
    document.body.appendChild(notification);

    setTimeout(() => {
      notification.style.animation = 'slideOutRight 0.3s ease';
      setTimeout(() => {
        if (notification.parentNode) {
          document.body.removeChild(notification);
        }
      }, 300);
    }, 3000);
  };

  const exportCatalog = () => {
    const csvContent = "data:text/csv;charset=utf-8," + 
      "Nom,Secteur,Phase,Équipe,Financement,Localisation,Fondateur\n" +
      filteredProjects.map(project => 
        `"${project.title}","${project.sector}","${project.status}","${project.team}","${project.funding}","${project.location}","${project.founder.name}"`
      ).join("\n");

    const encodedUri = encodeURI(csvContent);
    const link = document.createElement("a");
    link.setAttribute("href", encodedUri);
    link.setAttribute("download", "catalogue_startups_innovhub.csv");
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
  };

  const viewProject = (projectId) => {
    const project = projectsData.find(p => p.id === projectId);
    alert(`Affichage des détails pour :\n"${project.title}"\n\nFonctionnalité en développement...`);
  };

  const contactStartup = (projectId) => {
    const project = projectsData.find(p => p.id === projectId);
    const confirmed = window.confirm(`Contacter ${project.founder.name} de ${project.title} ?\n\nCeci ouvrira votre client email.`);
    
    if (confirmed) {
      window.location.href = `mailto:contact@${project.title.toLowerCase().replace(/\s+/g, '')}.com?subject=Contact depuis InnovHub&body=Bonjour ${project.founder.name},%0D%0A%0D%0AJe suis intéressé(e) par votre projet ${project.title}...`;
    }
  };

  const getStatusLabel = (status) => {
    const labels = {
      early: 'Phase Early',
      growth: 'Croissance',
      mature: 'Mature',
      seed: 'Seed'
    };
    return labels[status];
  };

  const startIndex = (currentPage - 1) * itemsPerPage;
  const endIndex = startIndex + itemsPerPage;
  const displayedProjects = filteredProjects.slice(startIndex, endIndex);
  const totalPages = Math.ceil(filteredProjects.length / itemsPerPage);

  const ProjectCard = ({ project }) => (
    <div className={`project-card ${viewMode === 'list' ? 'list-view' : ''}`}>
      <button 
        className="favorite-btn"
        onClick={() => toggleFavorite(project.id)}
        title={favorites.includes(project.id) ? 'Retirer des favoris' : 'Ajouter aux favoris'}
      >
        {favorites.includes(project.id) ? '❤️' : '🤍'}
      </button>

      <div className={`project-status status-${project.status}`}>
        {getStatusLabel(project.status)}
      </div>

      <h3 className="project-title">{project.title}</h3>
      <p className="project-description">{project.description}</p>

      <div className="project-info">
        <div className="info-row">
          <span className="info-label">Équipe:</span>
          <span className="info-value">{project.team}</span>
        </div>
        <div className="info-row">
          <span className="info-label">Financement:</span>
          <span className="info-value">{project.funding}</span>
        </div>
        <div className="info-row">
          <span className="info-label">Localisation:</span>
          <span className="info-value">{project.location.charAt(0).toUpperCase() + project.location.slice(1)}</span>
        </div>
      </div>

      <div className="project-tags">
        {project.tags.map((tag, index) => (
          <span key={index} className="project-tag">{tag}</span>
        ))}
      </div>

      <div className="project-footer">
        <div className="project-founder">
          <div className="founder-avatar">{project.founder.initials}</div>
          <div className="founder-info">
            <div className="founder-name">{project.founder.name}</div>
            <div className="founder-role">{project.founder.role}</div>
          </div>
        </div>
        <div className="project-actions">
          <button className="action-btn btn-primary" onClick={() => viewProject(project.id)}>
            Voir Plus
          </button>
          <button className="action-btn btn-secondary" onClick={() => contactStartup(project.id)}>
            Contact
          </button>
        </div>
      </div>
    </div>
  );

  const Pagination = () => {
    if (totalPages <= 1) return null;

    const pages = [];
    for (let i = 1; i <= totalPages; i++) {
      if (i === 1 || i === totalPages || (i >= currentPage - 2 && i <= currentPage + 2)) {
        pages.push(i);
      } else if (i === currentPage - 3 || i === currentPage + 3) {
        pages.push('...');
      }
    }

    return (
      <div className="pagination">
        <button 
          className="pagination-btn" 
          onClick={() => setCurrentPage(currentPage - 1)}
          disabled={currentPage === 1}
        >
          ← Précédent
        </button>
        
        {pages.map((page, index) => (
          page === '...' ? (
            <span key={index} className="pagination-btn disabled">...</span>
          ) : (
            <button
              key={index}
              className={`pagination-btn ${page === currentPage ? 'active' : ''}`}
              onClick={() => setCurrentPage(page)}
            >
              {page}
            </button>
          )
        ))}

        <button 
          className="pagination-btn" 
          onClick={() => setCurrentPage(currentPage + 1)}
          disabled={currentPage === totalPages}
        >
          Suivant →
        </button>
      </div>
    );
  };

  return (
    <div className="catalogue-page">
      <main className="main-content">
        <section className="catalogue-hero">
          <div className="hero-container">
            <h1 className="hero-title">Catalogue des Start-ups</h1>
            <p className="hero-subtitle">
              Découvrez l'ensemble des projets innovants incubés par InnovHub. 
              Des solutions technologiques qui transforment l'avenir.
            </p>
            
            <div className="hero-stats">
              <div className="stat-item">
                <span className="stat-number">{projectsData.length}</span>
                <span className="stat-label">Start-ups actives</span>
              </div>
              <div className="stat-item">
                <span className="stat-number">{new Set(projectsData.map(p => p.sector)).size}</span>
                <span className="stat-label">Secteurs d'activité</span>
              </div>
              <div className="stat-item">
                <span className="stat-number">€{projectsData.length * 0.5}M</span>
                <span className="stat-label">Levées de fonds</span>
              </div>
            </div>
          </div>
        </section>

        <section className="filters-section">
          <div className="filters-container">
            <div className="search-bar">
              <input 
                type="text" 
                className="search-input" 
                placeholder="Rechercher... (Ctrl+F pour focus rapide)"
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.target.value)}
                id="searchInput"
              />
              <select 
                className="sort-select" 
                value={sortBy}
                onChange={(e) => setSortBy(e.target.value)}
              >
                <option value="recent">Plus récentes</option>
                <option value="alphabetical">Alphabétique</option>
                <option value="funding">Financement</option>
                <option value="stage">Phase de développement</option>
              </select>
              <div className="view-toggle">
                <button 
                  className={`view-btn ${viewMode === 'grid' ? 'active' : ''}`}
                  onClick={() => setViewMode('grid')}
                  title="Vue grille"
                >
                  ⊞
                </button>
                <button 
                  className={`view-btn ${viewMode === 'list' ? 'active' : ''}`}
                  onClick={() => setViewMode('list')}
                  title="Vue liste"
                >
                  ☰
                </button>
              </div>
            </div>

            <div className="filter-tabs">
              {[
                { key: 'all', label: 'Toutes', count: projectsData.length },
                { key: 'early', label: 'Phase Early', count: projectsData.filter(p => p.status === 'early').length },
                { key: 'growth', label: 'Croissance', count: projectsData.filter(p => p.status === 'growth').length },
                { key: 'mature', label: 'Mature', count: projectsData.filter(p => p.status === 'mature').length },
                { key: 'seed', label: 'Seed', count: projectsData.filter(p => p.status === 'seed').length }
              ].map(filter => (
                <button
                  key={filter.key}
                  className={`filter-btn ${activeFilter === filter.key ? 'active' : ''}`}
                  onClick={() => setActiveFilter(filter.key)}
                >
                  {filter.label} ({filter.count})
                </button>
              ))}
              <button 
                className="toggle-advanced" 
                onClick={() => setShowAdvancedFilters(!showAdvancedFilters)}
              >
                {showAdvancedFilters ? 'Masquer filtres ↑' : 'Filtres avancés ↓'}
              </button>
            </div>

            {showAdvancedFilters && (
              <div className="advanced-filters show">
                <div className="advanced-grid">
                  <div className="filter-group">
                    <label className="filter-label">Secteur</label>
                    <select 
                      className="filter-select" 
                      value={sectorFilter}
                      onChange={(e) => setSectorFilter(e.target.value)}
                    >
                      <option value="">Tous les secteurs</option>
                      <option value="fintech">FinTech</option>
                      <option value="healthtech">HealthTech</option>
                      <option value="agritech">AgriTech</option>
                      <option value="edtech">EdTech</option>
                      <option value="greentech">GreenTech</option>
                      <option value="blockchain">Blockchain</option>
                    </select>
                  </div>
                  <div className="filter-group">
                    <label className="filter-label">Localisation</label>
                    <select 
                      className="filter-select"
                      value={locationFilter}
                      onChange={(e) => setLocationFilter(e.target.value)}
                    >
                      <option value="">Toutes les villes</option>
                      <option value="cotonou">Cotonou</option>
                      <option value="porto-novo">Porto-Novo</option>
                      <option value="parakou">Parakou</option>
                      <option value="abomey">Abomey</option>
                    </select>
                  </div>
                  <div className="filter-group">
                    <label className="filter-label">Année de création</label>
                    <select 
                      className="filter-select"
                      value={yearFilter}
                      onChange={(e) => setYearFilter(e.target.value)}
                    >
                      <option value="">Toutes les années</option>
                      <option value="2024">2024</option>
                      <option value="2023">2023</option>
                      <option value="2022">2022</option>
                      <option value="2021">2021</option>
                      <option value="older">Avant 2021</option>
                    </select>
                  </div>
                  <div className="filter-group">
                    <label className="filter-label">Recherche de</label>
                    <select 
                      className="filter-select"
                      value={needsFilter}
                      onChange={(e) => setNeedsFilter(e.target.value)}
                    >
                      <option value="">Tous les besoins</option>
                      <option value="investment">Investissement</option>
                      <option value="partnership">Partenariat</option>
                      <option value="talent">Recrutement</option>
                      <option value="market">Expansion marché</option>
                    </select>
                  </div>
                </div>
              </div>
            )}
          </div>
        </section>

        <div className="results-info">
          <div className="results-count">
            Affichage de <span className="highlight">{filteredProjects.length}</span> start-ups sur <span className="highlight">{projectsData.length}</span>
          </div>
          <button className="action-btn btn-secondary export-btn" onClick={exportCatalog}>
            Exporter CSV
          </button>
        </div>

        <section className="projects-container">
          <div className={`projects-grid ${viewMode === 'list' ? 'list-view' : ''}`}>
            {displayedProjects.map(project => (
              <ProjectCard key={project.id} project={project} />
            ))}
          </div>

          <Pagination />
        </section>
      </main>

      <footer className="footer">
        <div className="footer-container">
          <div className="footer-content">
            <div className="footer-section">
              <h3>InnovHub</h3>
              <p>Accélérateur d'innovation dédié aux start-ups technologiques en Afrique de l'Ouest.</p>
            </div>
            <div className="footer-section">
              <h3>Liens Rapides</h3>
              <p><a href="/">Accueil</a></p>
              <p><a href="/catalogue">Catalogue</a></p>
              <p><a href="/actualites">Actualités</a></p>
              <p><a href="/evenements">Événements</a></p>
            </div>
            <div className="footer-section">
              <h3>Contact</h3>
              <p>contact@innovhub.com</p>
              <p>+229 XX XX XX XX</p>
              <p>Cotonou, Bénin</p>
            </div>
          </div>
          <div className="footer-bottom">
            <p>&copy; 2024 InnovHub. Tous droits réservés.</p>
          </div>
        </div>
      </footer>
    </div>
  );
};

export default Catalogue;