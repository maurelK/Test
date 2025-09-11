import React, { useState } from 'react';
import './NewsContents.css';

const NewsContent = ({ 
  featuredArticle, 
  newsArticles, 
  filterCategories, 
  activeFilter, 
  onFilterChange,
  viewMode,
  onViewModeChange 
}) => {
  const [email, setEmail] = useState('');
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [isSuccess, setIsSuccess] = useState(false);

  const formatDate = (dateString) => {
    if (dateString === "Aujourd'hui") return { day: "09", month: "Sep" };
    
    const date = new Date(dateString);
    const day = date.getDate().toString().padStart(2, '0');
    const month = date.toLocaleDateString('fr-FR', { month: 'short' });
    return { day, month };
  };

  const getCategoryClass = (category) => {
    const categoryClasses = {
      startup: 'category-startup',
      innovation: 'category-innovation',
      financement: 'category-financement',
      tech: 'category-tech',
      evenement: 'category-evenement',
      partenariat: 'category-partenariat'
    };
    return categoryClasses[category] || '';
  };

  const getCategoryLabel = (category) => {
    const categoryLabels = {
      startup: 'Start-up',
      innovation: 'Innovation',
      financement: 'Financement',
      tech: 'Tech',
      evenement: 'Événement',
      partenariat: 'Partenariat'
    };
    return categoryLabels[category] || category;
  };

  const handleNewsletterSubmit = (e) => {
    e.preventDefault();
    if (!email) return;

    setIsSubmitting(true);
    
    setTimeout(() => {
      setIsSuccess(true);
      setIsSubmitting(false);
      
      setTimeout(() => {
        setIsSuccess(false);
        setEmail('');
      }, 2000);
    }, 1000);
  };

  return (
    <>
      {/* Quick Filters */}
      <section className="quick-filters">
        <div className="filters-container">
          {filterCategories.map((category) => (
            <div
              key={category.key}
              className={`filter-chip ${activeFilter === category.key ? 'active' : ''}`}
              onClick={() => onFilterChange(category.key)}
            >
              {category.label} <span className="count">{category.count}</span>
            </div>
          ))}
        </div>
      </section>

      {/* View Toggle */}
      <div className="view-toggle-section">
        <div className="news-count">
          Affichage de <span className="highlight">{newsArticles.length}</span> actualités
        </div>
        <div className="view-toggle">
          <button 
            className={`view-btn ${viewMode === 'grid' ? 'active' : ''}`}
            onClick={() => onViewModeChange('grid')}
          >
            <span>📰 Articles</span>
          </button>
          <button 
            className={`view-btn ${viewMode === 'timeline' ? 'active' : ''}`}
            onClick={() => onViewModeChange('timeline')}
          >
            <span>📅 Timeline</span>
          </button>
        </div>
      </div>

      {/* News Container */}
      <div className="news-container" id="news">
        {/* Featured Article */}
        <div className="featured-section">
          <h2 className="featured-title">À la une</h2>
          <article className="featured-article">
            <div className="featured-image">
              <div className="news-icon" style={{ fontSize: '72px' }}>{featuredArticle.icon}</div>
            </div>
            <div className="featured-content">
              <div className="featured-badge">À la une</div>
              <h2 className="featured-article-title">{featuredArticle.title}</h2>
              <p className="featured-excerpt">{featuredArticle.excerpt}</p>
              <div className="featured-meta">
                <span>📅 {featuredArticle.date}</span>
                <span>⏱️ {featuredArticle.readTime}</span>
                <span>👤 {featuredArticle.author}</span>
              </div>
              <a href="#" className="read-more-btn">Lire l'article</a>
            </div>
          </article>
        </div>

        {/* News Grid */}
        <div className="news-grid">
          {newsArticles.map((article) => {
            const { day, month } = formatDate(article.date);
            return (
              <article key={article.id} className="news-card fade-in" data-category={article.category}>
                <div className="news-image">
                  <div className="news-icon">{article.icon}</div>
                  <div className="news-date-badge">
                    <span className="badge-day">{day}</span>
                    <span className="badge-month">{month}</span>
                  </div>
                  <div className={`news-category-badge ${getCategoryClass(article.category)}`}>
                    {getCategoryLabel(article.category)}
                  </div>
                </div>
                <div className="news-content">
                  <h3 className="news-title">{article.title}</h3>
                  <p className="news-excerpt">{article.excerpt}</p>
                  <div className="news-meta">
                    <div className="author-info">
                      <div className="author-avatar">{article.avatar}</div>
                      <span>{article.author}</span>
                    </div>
                    <span className="read-time">{article.readTime}</span>
                  </div>
                </div>
              </article>
            );
          })}
        </div>
      </div>

      {/* Newsletter Section */}
      <section className="newsletter-section" id="newsletter">
        <div className="newsletter-container">
          <h2 className="newsletter-title">Newsletter InnovHub</h2>
          <p className="newsletter-subtitle">
            Recevez chaque semaine les actualités les plus importantes de l'écosystème 
            entrepreneurial africain directement dans votre boîte mail.
          </p>
          
          <form className="newsletter-form" onSubmit={handleNewsletterSubmit}>
            <input 
              type="email" 
              placeholder="Votre adresse email" 
              className="newsletter-input"
              value={email}
              onChange={(e) => setEmail(e.target.value)}
              required
              disabled={isSubmitting || isSuccess}
            />
            <button 
              type="submit" 
              className="newsletter-btn"
              disabled={isSubmitting || isSuccess}
              style={isSuccess ? { background: '#10B981' } : {}}
            >
              {isSubmitting ? 'Inscription...' : isSuccess ? '✓ Inscrit !' : 'S\'abonner'}
            </button>
          </form>
        </div>
      </section>
    </>
  );
};

export default NewsContent;