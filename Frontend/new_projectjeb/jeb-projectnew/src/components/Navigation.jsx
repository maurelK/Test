import React, { useState, useEffect } from 'react';
import './Navigation.css';

const Navigation = () => {
  const [isScrolled, setIsScrolled] = useState(false);

  useEffect(() => {
    const handleScroll = () => {
      setIsScrolled(window.scrollY > 100);
    };

    window.addEventListener('scroll', handleScroll);
    return () => window.removeEventListener('scroll', handleScroll);
  }, []);

  return (
    <header className={`header ${isScrolled ? 'scrolled' : ''}`}>
      <div className="nav-container">
        <div className="logo">
          <div className="logo-icon"></div>
          <span className="logo-text">InnovHub</span>
        </div>
        
        <nav>
          <ul className="nav-menu">
            <li><a href="/index.html" className="nav-link">Accueil</a></li>
            <li><a href="/catalogue.html" className="nav-link active">Catalogue</a></li>
            <li><a href="#actualites" className="nav-link">Actualités</a></li>
            <li><a href="#evenements" className="nav-link">Événements</a></li>
            <li><a href="/login.html" className="nav-link">Connexion</a></li>
          </ul>
        </nav>
      </div>
    </header>
  );
};

export default Navigation;