import React, { useState, useEffect } from 'react';
import { Link, useLocation } from 'react-router-dom';
import logo from '../assets/logo-login.jpeg';
import './Navbar.css';

const Navbar = ({ lang, setLang }) => {
  const location = useLocation();
  const [isMenuOpen, setIsMenuOpen] = useState(false);
  const [isDarkMode, setIsDarkMode] = useState(false);

  useEffect(() => {
    const savedTheme = localStorage.getItem('theme');
    if (savedTheme === 'dark') {
      setIsDarkMode(true);
      document.body.classList.add('dark-mode');
    }
  }, []);

  const toggleDarkMode = () => {
    if (isDarkMode) {
      document.body.classList.remove('dark-mode');
      localStorage.setItem('theme', 'light');
    } else {
      document.body.classList.add('dark-mode');
      localStorage.setItem('theme', 'dark');
    }
    setIsDarkMode(!isDarkMode);
  };

  const isActive = (path) => location.pathname === path;

  const toggleMenu = () => setIsMenuOpen(!isMenuOpen);

  const closeMenu = () => setIsMenuOpen(false);

  return (
    <nav className="navbar">
      <div className="navbar-container">
        {/* Logo */}
        <Link to="/" className="navbar-logo" onClick={closeMenu}>
          <div className="logo-icon">
            <img src={logo} alt="JEB Logo" className="logo-image" />
          </div>
          <span className="logo-text">JEB Incubator</span>
        </Link>

        {/* Burger Menu Button */}
        <button
          className={`burger-menu ${isMenuOpen ? 'active' : ''}`}
          onClick={toggleMenu}
          aria-label="Toggle menu"
        >
          <span></span>
          <span></span>
          <span></span>
        </button>

        {/* Menu de navigation */}
        <ul className={`navbar-menu ${isMenuOpen ? 'active' : ''}`}>
          <li className="navbar-item">
            <Link
              to="/"
              className={`navbar-link ${isActive('/') ? 'active' : ''}`}
              onClick={closeMenu}
            >
              Accueil
            </Link>
          </li>
          <li className="navbar-item">
            <Link
              to="/catalog"
              className={`navbar-link ${isActive('/catalog') ? 'active' : ''}`}
              onClick={closeMenu}
            >
              Catalogue
            </Link>
          </li>
          <li className="navbar-item">
            <Link
              to="/evenements"
              className={`navbar-link ${isActive('/evenements') ? 'active' : ''}`}
              onClick={closeMenu}
            >
              Événements
            </Link>
          </li>
          <li className="navbar-item">
            <Link
              to="/startup"
              className={`navbar-link ${isActive('/startup') ? 'active' : ''}`}
              onClick={closeMenu}
            >
              Startup
            </Link>
          </li>
          <li className="navbar-item">
            <Link
              to="/actualites"
              className={`navbar-link ${isActive('/actualites') ? 'active' : ''}`}
              onClick={closeMenu}
            >
              Actualités
            </Link>
          </li>

          {/* Section droite mobile uniquement */}
          <li className="navbar-item navbar-right-mobile">
            <div className="language-selector">
              <button
                className={`lang-btn ${lang === 'fr' ? 'active' : ''}`}
                onClick={() => setLang('fr')}
              >
                🇫🇷 FR
              </button>
              <button
                className={`lang-btn ${lang === 'en' ? 'active' : ''}`}
                onClick={() => setLang('en')}
              >
                🇬🇧 EN
              </button>
            </div>

            <button
              className="theme-toggle-btn"
              onClick={toggleDarkMode}
              aria-label="Toggle dark/light mode"
              title={isDarkMode ? 'Passer en mode clair' : 'Passer en mode sombre'}
            >
              {isDarkMode ? '☀️' : '🌙'}
            </button>

            <div className="action-buttons">
              <Link to="/register" className="btn-secondary" onClick={closeMenu}>
                S'inscrire
              </Link>
              <Link to="/login" className="btn-primary" onClick={closeMenu}>
                Connexion
              </Link>
            </div>
          </li>
        </ul>

        {/* Section droite desktop uniquement */}
        <div className="navbar-right">
          <div className="language-selector">
            <button
              className={`lang-btn ${lang === 'fr' ? 'active' : ''}`}
              onClick={() => setLang('fr')}
            >
              🇫🇷 FR
            </button>
            <button
              className={`lang-btn ${lang === 'en' ? 'active' : ''}`}
              onClick={() => setLang('en')}
            >
              🇬🇧 EN
            </button>
          </div>

          <button
            className="theme-toggle-btn"
            onClick={toggleDarkMode}
            aria-label="Toggle dark/light mode"
            title={isDarkMode ? 'Passer en mode clair' : 'Passer en mode sombre'}
          >
            {isDarkMode ? '☀️' : '🌙'}
          </button>

          <div className="action-buttons">
            <Link to="/register" className="btn-secondary">
              S'inscrire
            </Link>
            <Link to="/login" className="btn-primary">
              Connexion
            </Link>
          </div>
        </div>

        {/* Overlay pour fermer le menu sur mobile */}
        {isMenuOpen && <div className="menu-overlay" onClick={closeMenu}></div>}
      </div>
    </nav>
  );
};

export default Navbar;