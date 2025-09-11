import './NewsAndFooter.css'

function Footer({ lang }) {
  const t = {
    fr: {
      name: "JEB Incubator",
      mission: "Accélérateur d'innovation dédié aux start-ups technologiques en Afrique de l'Ouest.",
      vision: "Notre mission est d'accompagner les entrepreneurs dans leur parcours de croissance.",
      links: "Liens Rapides",
      services: "Services",
      contact: "Contact",
      items: {
        startups: "Nos Start-ups",
        news: "Actualités",
        events: "Événements",
        contact: "Contact"
      },
      serviceList: ["Incubation", "Financement", "Mentoring", "Networking"],
      copyright: "Tous droits réservés."
    },
    en: {
      name: "JEB Incubator",
      mission: "Innovation accelerator dedicated to tech startups in West Africa.",
      vision: "Our mission is to support entrepreneurs on their growth journey.",
      links: "Quick Links",
      services: "Services",
      contact: "Contact",
      items: {
        startups: "Our Startups",
        news: "News",
        events: "Events",
        contact: "Contact"
      },
      serviceList: ["Incubation", "Funding", "Mentoring", "Networking"],
      copyright: "All rights reserved."
    }
  }

  return (
    <footer className="footer">
      <div className="footer-container">
        <div className="footer-content">
          <div className="footer-section">
            <h3>{t[lang].name}</h3>
            <p>{t[lang].mission}</p>
            <p>{t[lang].vision}</p>
          </div>

          <div className="footer-section">
            <h3>{t[lang].links}</h3>
            <p><a href="#projets">{t[lang].items.startups}</a></p>
            <p><a href="#actualites">{t[lang].items.news}</a></p>
            <p><a href="#evenements">{t[lang].items.events}</a></p>
            <p><a href="#contact">{t[lang].items.contact}</a></p>
          </div>

          <div className="footer-section">
            <h3>{t[lang].services}</h3>
            {t[lang].serviceList.map((service, i) => (
              <p key={i}><a href="#">{service}</a></p>
            ))}
          </div>

          <div className="footer-section">
            <h3>{t[lang].contact}</h3>
            <p>📧 contact@jebincubator.com</p>
            <p>📱 +229 xx xx xx xx</p>
            <p>📍 Cotonou, Bénin</p>
          </div>
        </div>

        <div className="footer-bottom">
          <p>&copy; 2025 JEB Incubator. {t[lang].copyright}</p>
        </div>
      </div>
    </footer>
  )
}

export default Footer
