import './Dashboard.css'
import { useEffect, useState } from 'react'
import { useNavigate } from 'react-router-dom'

function Dashboard({ lang }) {
  const [user, setUser] = useState(null)
  const navigate = useNavigate()

  const t = {
    fr: {
      welcome: "Bienvenue sur votre tableau de bord",
      email: "Connecté en tant que",
      logout: "Se déconnecter"
    },
    en: {
      welcome: "Welcome to your dashboard",
      email: "Logged in as",
      logout: "Log out"
    }
  }

  useEffect(() => {
    const stored = localStorage.getItem('innovhub_user') || sessionStorage.getItem('innovhub_user')
    if (stored) {
      try {
        const parsed = JSON.parse(stored)
        setUser(parsed)
      } catch {
        setUser(null)
      }
    }
  }, [])

  const handleLogout = () => {
    localStorage.removeItem('innovhub_user')
    sessionStorage.removeItem('innovhub_user')
    navigate('/login')
  }

  return (
    <div className="dashboard-container">
      <h1>{t[lang].welcome} 🚀</h1>
      {user && (
        <p>
          {t[lang].email}: <strong>{user.email}</strong>
        </p>
      )}
      <button className="home-button" onClick={() => navigate('/')}>
        Accueil
      </button>
      <button className="logout-button" onClick={handleLogout}>
        {t[lang].logout}
      </button>
    </div>
  )
}

export default Dashboard
