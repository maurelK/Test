import React, { useState, useEffect } from 'react';
import { Link } from 'react-router-dom';
import api from '../api';

const NewsFeed = ({ lang = 'fr' }) => {
  const [newsArticles, setNewsArticles] = useState([]);

  useEffect(() => {
    async function fetchNews() {
      try {
        const data = await api.getNews();
        setNewsArticles(data.slice(0, 3)); // On ne prend que les 3 premiers
      } catch (error) {
        console.error("Erreur lors de la récupération des actualités :", error);
      }
    }
    fetchNews();
  }, []);

  const formatDate = (dateString) => {
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
      evenement: 'category-evenement'
    };
    return categoryClasses[category] || '';
  };

  const getCategoryLabel = (category) => {
    const categoryLabels = {
      startup: 'Start-up',
      innovation: 'Innovation',
      financement: 'Financement',
      tech: 'Tech',
      evenement: 'Événement'
    };
    return categoryLabels[category] || category;
  };

  return (
    <section className="news-section">
      {/* Header */}
      <div className="news-section-header">
        <h2 className="news-section-title">Actualités</h2>
        <p className="news-section-subtitle">
          Les dernières nouvelles de notre écosystème
        </p>
      </div>

      {/* Grid avec les 3 premiers articles */}
      <div className="news-grid">
        {newsArticles.map((article) => {
          const { day, month } = formatDate(article.news_date);
          return (
            <div key={article.id} className="news-card">
              <div className="news-image">
                <div className="news-icon">📰</div>
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
                <p className="news-excerpt">{article.description || "Description non disponible"}</p>
                <div className="news-meta">
                  <div className="author-info">
                    <div className="author-avatar">A</div>
                    <span>Admin</span>
                  </div>
                  <span className="read-time">3 min</span>
                </div>
              </div>
            </div>
          );
        })}
      </div>

      {/* Bouton Voir plus */}
      <div className="news-see-more">
        <Link to="/actualites" className="news-see-more-btn">
          Voir plus
        </Link>
      </div>
    </section>
  );
};

export default NewsFeed;