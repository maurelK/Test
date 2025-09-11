import './Catalog.css'
import { Link } from 'react-router-dom'
import { useState, useEffect } from 'react'
import api from '../api'

function Catalog({ lang }) {
  const t = {
    fr: {
      title: "Nos Start-ups Incubées",
      subtitle: "Découvrez les projets innovants qui façonnent l'avenir du digital en Afrique",
      more: "Voir plus →"
    },
    en: {
      title: "Our Incubated Startups",
      subtitle: "Discover innovative projects shaping the future of digital in Africa",
      more: "See more →"
    }
  }

  const [projects, setProjects] = useState([])

  useEffect(() => {
    async function fetchStartups() {
      try {
        const data = await api.getStartups()
        setProjects(data)
      } catch (error) {
        console.error("Erreur lors de la récupération des startups :", error)
      }
    }
    fetchStartups()
  }, [])

  return (
    <section className="catalog-preview">
      <div className="section-container">
        <header className="section-header">
          <h2 className="section-title fade-in">{t[lang].title}</h2>
          <p className="section-subtitle">{t[lang].subtitle}</p>
        </header>

        <div className="projects-grid">
          {projects.map((project, index) => (
            <div className="project-card" key={index}>
              <span className={`project-status status-${project.project_status}`}>
                {project.project_status}
              </span>
              <h3 className="project-title">{project.name}</h3>
              <p className="project-description">{project.description}</p>
              <div className="project-tags">
                {/* Assuming tags are not available from API, so skipping */}
              </div>
              <footer className="project-footer">
                <div className="project-founder">
                  <div className="founder-avatar">{/* No initials from API */}</div>
                  <span className="founder-name">{/* No founder name from API */}</span>
                </div>
              </footer>
            </div>
          ))}
        </div>

        {/* Flèche vers la page catalogue */}
        <div className="catalog-see-more">
          <Link to="/catalog" className="catalog-see-more-btn">
            {t[lang].more}
          </Link>
        </div>
      </div>
    </section>
  )
}

export default Catalog
