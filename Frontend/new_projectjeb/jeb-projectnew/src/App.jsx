import { useState } from 'react'
import { BrowserRouter, Routes, Route } from 'react-router-dom'

// Composants de layout
import Navbar from './components/Navbar'
import Footer from './components/Footer'

// Composants de pages
import Hero from './components/Hero'
import NewsFeed from './components/Newsfeed'
import Catalog from './components/Catalog'
import Catalogue from './components/Catalogue'
import Login from './components/Login'
import Register from './components/Register'
import Dashboard from './components/Dashboard'

// Pages spécialisées
import Events from './components/Events/Events'
import News from './components/News/News' // IMPORT CORRIGÉ
import Startup from './components/Startup/Startup';

import './index.css'

function App() {
  const [lang, setLang] = useState('fr') // Langue par défaut
  
  return (
    <BrowserRouter>
      <Routes>
        {/* Page d'accueil */}
        <Route
          path="/"
          element={
            <>
              <Navbar lang={lang} setLang={setLang} />
              <Hero lang={lang} />
              <Catalog lang={lang} />
              <NewsFeed lang={lang} />
              <Footer lang={lang} />
            </>
          }
        />
        
        {/* Page catalogue complète */}
        <Route
          path="/catalog"
          element={
            <>
              <Navbar lang={lang} setLang={setLang} />
              <Catalogue lang={lang} />
              <Footer lang={lang} />
            </>
          }
        />
        
        {/* Page des événements */}
        <Route
          path="/evenements"
          element={
            <>
              <Navbar lang={lang} setLang={setLang} />
              <Events lang={lang} />
              <Footer lang={lang} />
            </>
          }
        />
        
        {/* Page startup */}
        <Route
          path="/startup"
          element={
            <>
              <Navbar lang={lang} setLang={setLang} />
              <Startup lang={lang} />
              <Footer lang={lang} />
            </>
          }
        />
        
        {/* Page des actualités */}
        <Route
          path="/actualites"
          element={
            <>
              <Navbar lang={lang} setLang={setLang} />
              <News lang={lang} />
              <Footer lang={lang} />
            </>
          }
        />
        
        {/* Page de connexion */}
        <Route
          path="/login"
          element={<Login lang={lang} />}
        />

        {/* Page d'inscription */}
        <Route
          path="/register"
          element={<Register lang={lang} />}
        />

        {/* Tableau de bord */}
        <Route
          path="/dashboard"
          element={<Dashboard lang={lang} />}
        />
      </Routes>
    </BrowserRouter>
  )
}

export default App