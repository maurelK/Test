import './Login.css'
import { useState, useEffect } from 'react'
import { useNavigate, Link } from 'react-router-dom'

function Login({ lang }) {
  const [email, setEmail] = useState('')
  const [password, setPassword] = useState('')
  const [remember, setRemember] = useState(false)
  const [showPassword, setShowPassword] = useState(false)
  const [loading, setLoading] = useState(false)
  const [message, setMessage] = useState(null)
  const navigate = useNavigate()

  const t = {
    fr: {
      title: "Connexion",
      subtitle: "Accédez à votre espace InnovHub",
      emailPlaceholder: "Votre adresse email",
      passwordPlaceholder: "Votre mot de passe",
      remember: "Se souvenir de moi",
      forgot: "Mot de passe oublié ?",
      forgotEmpty: "Veuillez entrer votre email d'abord.",
      forgotSent: "Instructions envoyées à votre adresse email.",
      login: "Se connecter",
      loading: "Connexion en cours...",
      success: "Connexion réussie ! Redirection...",
      error: "Email ou mot de passe incorrect.",
      empty: "Veuillez remplir tous les champs.",
      back: "← Accueil",
      divider: "ou continuez avec",
      socialStart: "Connexion avec",
      socialSuccess: "Connexion réussie avec",
      alreadyLogged: "Vous êtes déjà connecté. Redirection..."
    },
    en: {
      title: "Login",
      subtitle: "Access your InnovHub space",
      emailPlaceholder: "Your email address",
      passwordPlaceholder: "Your password",
      remember: "Remember me",
      forgot: "Forgot password?",
      forgotEmpty: "Please enter your email first.",
      forgotSent: "Instructions sent to your email.",
      login: "Log in",
      loading: "Logging in...",
      success: "Login successful! Redirecting...",
      error: "Invalid email or password.",
      empty: "Please fill in all fields.",
      back: "← Home",
      divider: "or continue with",
      socialStart: "Logging in with",
      socialSuccess: "Successfully logged in with",
      alreadyLogged: "You are already logged in. Redirecting..."
    }
  }

  useEffect(() => {
    document.body.classList.add('login-page')

    if (window.location.pathname === '/login') {
      const userData = localStorage.getItem('innovhub_user') || sessionStorage.getItem('innovhub_user')
      if (userData) {
        try {
          const user = JSON.parse(userData)
          const loginTime = new Date(user.loginTime)
          const now = new Date()
          const maxAge = user.remember ? 7 * 24 * 60 * 60 * 1000 : 2 * 60 * 60 * 1000

          if (now - loginTime < maxAge) {
            setMessage({ type: 'success', text: t[lang].alreadyLogged })
            setTimeout(() => navigate('/dashboard'), 1500)
          }
        } catch {
          localStorage.removeItem('innovhub_user')
          sessionStorage.removeItem('innovhub_user')
        }
      }
    }

    return () => {
      document.body.classList.remove('login-page')
    }
  }, [lang, navigate])

  const handleSubmit = async (e) => {
    e.preventDefault()
    setMessage(null)

    if (!email || !password) {
      setMessage({ type: 'error', text: t[lang].empty })
      return
    }

    setLoading(true)

    try {
      const response = await fetch("http://localhost:8000/JEB/login/", {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify({ email, password })
      })

      if (response.ok) {
        setMessage({ type: 'success', text: t[lang].success })

        const userData = {
          email,
          loginTime: new Date().toISOString(),
          remember
        }

        if (remember) {
          localStorage.setItem('innovhub_user', JSON.stringify(userData))
        } else {
          sessionStorage.setItem('innovhub_user', JSON.stringify(userData))
        }

        setTimeout(() => navigate('/'), 2000)
      } else {
        const errorData = await response.json()
        setMessage({ type: 'error', text: errorData.error || t[lang].error })
      }
    } catch (error) {
      setMessage({ type: 'error', text: t[lang].error })
    }

    setLoading(false)
  }

  const handleForgotPassword = () => {
    if (!email) {
      setMessage({ type: 'error', text: t[lang].forgotEmpty })
      return
    }
    setMessage({ type: 'success', text: t[lang].forgotSent })
  }

  const handleSocialLogin = (provider) => {
    setMessage({ type: 'success', text: `${t[lang].socialStart} ${provider}...` })
    setTimeout(() => {
      setMessage({ type: 'success', text: `${t[lang].socialSuccess} ${provider}` })
      setTimeout(() => navigate('/dashboard'), 1500)
    }, 1500)
  }

  return (
    <>
      <Link to="/" className="back-home">{t[lang].back}</Link>

      <div className="login-wrapper">
        <div className="login-container">
          <div className="login-header">
            <h1 className="login-title">{t[lang].title}</h1>
            <p className="login-subtitle">{t[lang].subtitle}</p>
          </div>

          {message && <div className={`message ${message.type}`}>{message.text}</div>}

          <form className="login-form" onSubmit={handleSubmit}>
            <div className="form-group">
              <input
                type="email"
                className="form-input"
                placeholder={t[lang].emailPlaceholder}
                value={email}
                onChange={e => setEmail(e.target.value)}
                required
              />
            </div>

            <div className="form-group password-container">
              <input
                type={showPassword ? 'text' : 'password'}
                className="form-input"
                placeholder={t[lang].passwordPlaceholder}
                value={password}
                onChange={e => setPassword(e.target.value)}
                required
              />
              <button
                type="button"
                className="password-toggle"
                onClick={() => setShowPassword(prev => !prev)}
              >
                {showPassword ? '🙈' : '👁️'}
              </button>
            </div>

            <div className="form-options">
              <label className="remember-me">
                <input
                  type="checkbox"
                  checked={remember}
                  onChange={e => setRemember(e.target.checked)}
                />
                <span>{t[lang].remember}</span>
              </label>
              <button type="button" className="forgot-password" onClick={handleForgotPassword}>
                {t[lang].forgot}
              </button>
            </div>

            <button type="submit" className={`login-button ${loading ? 'loading' : ''}`}>
              {loading ? t[lang].loading : t[lang].login}
            </button>
          </form>

          <div className="divider"><span>{t[lang].divider}</span></div>

          <div className="social-login">
            <button type="button" className="social-btn" onClick={() => handleSocialLogin('Google')}>
              <div className="social-icon google-icon">G</div>
              Google
            </button>
            <button type="button" className="social-btn" onClick={() => handleSocialLogin('Microsoft')}>
              <div className="social-icon microsoft-icon">M</div>
              Microsoft
            </button>
          </div>
        </div>
      </div>
    </>
  )
}

export default Login
