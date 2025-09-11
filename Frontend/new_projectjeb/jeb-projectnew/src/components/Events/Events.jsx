import React, { useState, useMemo } from 'react';
import './Events.css';

const Events = () => {
  const [selectedCategory, setSelectedCategory] = useState('tous');
  const [sortBy, setSortBy] = useState('date');
  const [viewMode, setViewMode] = useState('grid');
  const [showAdvanced, setShowAdvanced] = useState(false);

  const events = [
    {
      id: 1,
      title: "Conférence IA & Innovation",
      description: "Découvrez les dernières innovations en intelligence artificielle et leur impact sur les startups.",
      date: new Date('2025-01-25'),
      type: "conference",
      location: "Cotonou Tech Hub",
      time: "14:00",
      icon: "🤖"
    },
    {
      id: 2,
      title: "Workshop Développement Mobile",
      description: "Atelier pratique sur le développement d'applications mobiles avec React Native.",
      date: new Date('2025-01-28'),
      type: "workshop",
      location: "JEB Incubator",
      time: "09:00",
      icon: "📱"
    },
    {
      id: 3,
      title: "Networking Entrepreneurs",
      description: "Rencontrez d'autres entrepreneurs et élargissez votre réseau professionnel.",
      date: new Date('2025-02-05'),
      type: "networking",
      location: "Marina Hotel",
      time: "18:30",
      icon: "🤝"
    },
    {
      id: 4,
      title: "Pitch Competition 2025",
      description: "Présentez votre startup devant un jury d'investisseurs et gagnez des prix.",
      date: new Date('2025-02-12'),
      type: "pitch",
      location: "Palais des Congrès",
      time: "16:00",
      icon: "🏆"
    },
    {
      id: 5,
      title: "Formation Marketing Digital",
      description: "Maîtrisez les stratégies de marketing digital pour votre entreprise.",
      date: new Date('2025-02-18'),
      type: "formation",
      location: "Centre de Formation",
      time: "10:00",
      icon: "📈"
    }
  ];

  const categories = [
    { id: 'tous', label: 'Tous', count: events.length },
    { id: 'conference', label: 'Conférences', count: events.filter(e => e.type === 'conference').length },
    { id: 'workshop', label: 'Ateliers', count: events.filter(e => e.type === 'workshop').length },
    { id: 'networking', label: 'Networking', count: events.filter(e => e.type === 'networking').length },
    { id: 'pitch', label: 'Pitch Days', count: events.filter(e => e.type === 'pitch').length },
    { id: 'formation', label: 'Formations', count: events.filter(e => e.type === 'formation').length }
  ];

  const filteredEvents = useMemo(() => {
    let filtered = events.filter(event => selectedCategory === 'tous' || event.type === selectedCategory);

    switch (sortBy) {
      case 'date':
        filtered.sort((a, b) => a.date - b.date);
        break;
      case 'title':
        filtered.sort((a, b) => a.title.localeCompare(b.title));
        break;
      case 'type':
        filtered.sort((a, b) => a.type.localeCompare(b.type));
        break;
      default:
        break;
    }

    return filtered;
  }, [selectedCategory, sortBy]);

  const formatDate = (date) => {
    const day = date.getDate();
    const month = date.toLocaleDateString('fr-FR', { month: 'short' });
    return { day, month };
  };

  const getCategoryLabel = (type) => {
    const category = categories.find(cat => cat.id === type);
    return category ? category.label : type;
  };

  return (
    <div className="events-page">
      {/* Hero Section */}
      <section className="events-hero">
        <div className="hero-container">
          <h1 className="hero-title">Événements JEB Incubator</h1>
          <p className="hero-subtitle">
            Participez à nos événements exclusifs : conférences, ateliers, sessions de networking et pitch days. Connectez-vous avec l'écosystème entrepreneurial africain.
          </p>
          <div className="hero-cta">
            <button className="cta-btn">Voir les événements</button>
            <button className="cta-btn secondary">Notifications</button>
          </div>
        </div>
      </section>

      {/* Filters Section */}
      <section className="filters-section">
        <div className="filters-container">
          <select
            className="sort-select"
            value={sortBy}
            onChange={(e) => setSortBy(e.target.value)}
            aria-label="Trier les événements"
          >
            <option value="date">Trier par date</option>
            <option value="title">Trier par titre</option>
            <option value="type">Trier par type</option>
          </select>

          <div className="filter-tabs">
            {categories.map(category => (
              <button
                key={category.id}
                className={`filter-btn ${selectedCategory === category.id ? 'active' : ''}`}
                onClick={() => setSelectedCategory(category.id)}
              >
                {category.label} {category.count}
              </button>
            ))}
          </div>

          <button
            className="toggle-advanced"
            onClick={() => setShowAdvanced(!showAdvanced)}
          >
            {showAdvanced ? 'Masquer' : 'Filtres avancés'} ▼
          </button>

          {showAdvanced && (
            <div className="advanced-filters show">
              <div className="advanced-grid">
                <div className="filter-group">
                  <label className="filter-label">Mois</label>
                  <select className="filter-select">
                    <option>Tous les mois</option>
                    <option>Janvier 2025</option>
                    <option>Février 2025</option>
                    <option>Mars 2025</option>
                  </select>
                </div>
                <div className="filter-group">
                  <label className="filter-label">Lieu</label>
                  <select className="filter-select">
                    <option>Tous les lieux</option>
                    <option>JEB Incubator</option>
                    <option>Cotonou Tech Hub</option>
                    <option>Marina Hotel</option>
                  </select>
                </div>
              </div>
            </div>
          )}
        </div>
      </section>

      {/* Events Display */}
      <div className="events-container">
        <p className="results-count">Affichage de {filteredEvents.length} événements</p>
        
        <div className={`events-grid ${viewMode}`}>
          {filteredEvents.map(event => {
            const { day, month } = formatDate(event.date);
            return (
              <div key={event.id} className="event-card">
                <div className="event-image">
                  <span className="event-icon">{event.icon}</span>
                  <div className="event-date-badge">
                    <span className="badge-day">{day}</span>
                    <span className="badge-month">{month}</span>
                  </div>
                  <div className={`event-type-badge type-${event.type}`}>
                    {getCategoryLabel(event.type)}
                  </div>
                </div>
                <div className="event-content">
                  <h3 className="event-title">{event.title}</h3>
                  <p className="event-description">{event.description}</p>
                  <div className="event-meta">
                    <div className="event-location">📍 {event.location}</div>
                    <div className="event-time">🕒 {event.time}</div>
                  </div>
                </div>
              </div>
            );
          })}
        </div>

        {filteredEvents.length === 0 && (
          <div className="no-results">
            <h3>Aucun événement trouvé</h3>
            <p>Essayez de modifier vos critères de recherche</p>
          </div>
        )}
      </div>

      {/* Newsletter Section */}
      <section className="newsletter-section">
        <div className="newsletter-container">
          <h2 className="newsletter-title">Restez informé</h2>
          <p className="newsletter-subtitle">
            Recevez les dernières actualités de nos événements directement dans votre boîte mail
          </p>
          <form className="newsletter-form">
            <input
              type="email"
              className="newsletter-input"
              placeholder="Votre adresse email..."
              required
            />
            <button type="submit" className="newsletter-btn">
              S'abonner
            </button>
          </form>
        </div>
      </section>
    </div>
  );
};

export default Events;