import React, { useState, useEffect } from 'react';
import NewsContent from './NewsContents';
import './News.css';
import api from '../../api';

const News = ({ lang = 'fr' }) => {
  const [activeFilter, setActiveFilter] = useState('all');
  const [viewMode, setViewMode] = useState('grid');
  const [newsArticles, setNewsArticles] = useState([]);

  useEffect(() => {
    async function fetchNews() {
      try {
        const data = await api.getNews();
        const formattedData = data.map(article => ({
          id: article.id,
          title: article.title,
          excerpt: article.description || "Description non disponible",
          date: article.news_date,
          category: article.category,
          author: "Admin",
          avatar: "A",
          readTime: "3 min",
          icon: "📰",
          featured: false
        }));
        setNewsArticles(formattedData);
      } catch (error) {
        console.error("Erreur lors de la récupération des actualités :", error);
      }
    }
    fetchNews();
  }, []);

  // Article à la une (séparé des autres)
  const featuredArticle = {
    id: 'featured',
    title: "TechAfrica lève 5M$ pour révolutionner l'agriculture en Afrique de l'Ouest",
    excerpt: "La start-up béninoise TechAfrica vient de boucler une levée de fonds de 5 millions de dollars pour développer sa plateforme d'agriculture intelligente destinée aux petits producteurs africains.",
    date: "Aujourd'hui",
    author: "Sarah Koné",
    readTime: "4 min de lecture",
    icon: "🚀",
    featured: true
  };

  const filterCategories = [
    { key: 'all', label: 'Toutes', count: newsArticles.length },
    { key: 'startup', label: 'Start-ups', count: newsArticles.filter(n => n.category === 'startup').length },
    { key: 'innovation', label: 'Innovation', count: newsArticles.filter(n => n.category === 'innovation').length },
    { key: 'financement', label: 'Financement', count: newsArticles.filter(n => n.category === 'financement').length },
    { key: 'tech', label: 'Tech', count: newsArticles.filter(n => n.category === 'tech').length },
    { key: 'evenement', label: 'Événements', count: newsArticles.filter(n => n.category === 'evenement').length }
  ];

  const filteredNews = activeFilter === 'all' 
    ? newsArticles 
    : newsArticles.filter(article => article.category === activeFilter);

  const scrollToNews = () => {
    const newsSection = document.getElementById('news');
    if (newsSection) {
      newsSection.scrollIntoView({ behavior: 'smooth' });
    }
  };

  const scrollToNewsletter = () => {
    const newsletterSection = document.getElementById('newsletter');
    if (newsletterSection) {
      newsletterSection.scrollIntoView({ behavior: 'smooth' });
    }
  };

  return (
    <div className="news-page">
      {/* Hero Section */}
      <section className="news-hero">
        <div className="hero-container">
          <h1 className="hero-title">Actualités InnovHub</h1>
          <p className="hero-subtitle">
            Restez informé des dernières nouvelles de l'écosystème entrepreneurial africain, 
            des success stories aux nouvelles tendances technologiques.
          </p>
          
          <div className="hero-cta">
            <button onClick={scrollToNews} className="cta-btn">
              Dernières actualités
            </button>
            <button onClick={scrollToNewsletter} className="cta-btn secondary">
              Newsletter
            </button>
          </div>
        </div>
      </section>

      {/* Contenu principal */}
      <NewsContent 
        featuredArticle={featuredArticle}
        newsArticles={filteredNews}
        filterCategories={filterCategories}
        activeFilter={activeFilter}
        onFilterChange={setActiveFilter}
        viewMode={viewMode}
        onViewModeChange={setViewMode}
      />
    </div>
  );
};

export default News;