import './Hero.css'
import Stats from './Stats'


function Hero({ lang }) {
  const t = {
    fr: {
      title: (
        <>
          Les <span className="highlight">meilleures solutions digitales</span><br />
          au service de votre <span className="highlight">croissance</span>.
        </>
      ),
      description:
        "Notre incubateur accompagne les start-ups technologiques dans leur développement, en leur offrant un écosystème complet pour augmenter leur productivité et leur chiffre d'affaires.",
      button: "Découvrir nos solutions"
    },
    en: {
      title: (
        <>
          The <span className="highlight">best digital solutions</span><br />
          to boost your <span className="highlight">growth</span>.
        </>
      ),
      description:
        "Our incubator supports tech startups in their development by offering a complete ecosystem to increase productivity and revenue.",
      button: "Discover our solutions"
    }
  }

  return (
    <section id="accueil" className="hero">
      <div className="hero-container">
        <div className="hero-content animate-fade-up">
          <h1 className="hero-title">{t[lang].title}</h1>
          <p className="hero-description">{t[lang].description}</p>
          <a href="#projets" className="cta-button">{t[lang].button}</a>
          <Stats />
        </div>
      </div>
    </section>
  )
}

export default Hero
