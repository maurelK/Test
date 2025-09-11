import React, { useState } from 'react';
import { Link, useNavigate } from 'react-router-dom';
import './Register.css';

const Register = ({ lang = 'fr' }) => {
  const navigate = useNavigate();

  const [formData, setFormData] = useState({
    username: '',
    first_name: '',
    last_name: '',
    email: '',
    password: '',
    confirmPassword: '',
    role: '',
    name: ''
  });

  const [errors, setErrors] = useState({});
  const [isLoading, setIsLoading] = useState(false);
  const [progress, setProgress] = useState(0);
  const [showSuccess, setShowSuccess] = useState(false);

  const handleInputChange = (e) => {
    const { name, value } = e.target;
    setFormData(prev => ({ ...prev, [name]: value }));

    if (errors[name]) {
      setErrors(prev => ({ ...prev, [name]: '' }));
    }
  };

  const validateForm = () => {
    const newErrors = {};

    if (!formData.username) newErrors.username = 'Le nom d’utilisateur est requis';
    if (!formData.first_name) newErrors.first_name = 'Le prénom est requis';
    if (!formData.last_name) newErrors.last_name = 'Le nom est requis';
    if (!formData.email) newErrors.email = 'L’email est requis';
    if (!formData.password) newErrors.password = 'Le mot de passe est requis';
    if (formData.password !== formData.confirmPassword) newErrors.confirmPassword = 'Les mots de passe ne correspondent pas';
    if (!formData.role) newErrors.role = 'Veuillez sélectionner un rôle';

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = async (e) => {
    e.preventDefault();

    if (!validateForm()) return;

    setIsLoading(true);
    setProgress(30);

    try {
      setTimeout(() => setProgress(60), 800);

      setTimeout(async () => {
        setProgress(100);

        const accountData = {
          username: formData.username,
          first_name: formData.first_name,
          last_name: formData.last_name,
          email: formData.email,
          password: formData.password,
          role: formData.role,
          name: formData.name
        };

        try {
          const response = await fetch("http://localhost:8000/JEB/user/", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(accountData)
          });

          if (!response.ok) {
            const text = await response.text();
            let errorMessage = "Erreur lors de l'inscription";
            try {
              const errorData = JSON.parse(text);
              errorMessage = errorData.detail || errorMessage;
            } catch {
              errorMessage = text || errorMessage;
            }
            throw new Error(errorMessage);
          }

          await response.json();

          const loginResponse = await fetch("http://localhost:8000/JEB/login/", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ email: formData.email, password: formData.password })
          });

          if (loginResponse.ok) {
            setShowSuccess(true);
            setTimeout(() => {
              navigate('/');
            }, 2000);
          } else {
            setErrors({ submit: "Email ou mot de passe erroné" });
            setIsLoading(false);
            setProgress(0);
          }

        } catch (error) {
          setErrors({ submit: error.message });
          setIsLoading(false);
          setProgress(0);
        }
      }, 1500);

    } catch (error) {
      setErrors({ submit: 'Une erreur est survenue. Veuillez réessayer.' });
      setIsLoading(false);
      setProgress(0);
    }
  };

  return (
    <div className="register-page">
      <div className="progress-bar" style={{ width: `${progress}%` }}></div>

      <div className="register-container">
        <div className="register-header">
          <h1>Création de compte</h1>
          <p>Rejoignez notre plateforme professionnelle en quelques étapes</p>
        </div>

        <form onSubmit={handleSubmit} className="register-form">

          <div className="form-group">
            <label htmlFor="username">Nom d’utilisateur</label>
            <input
              type="text"
              id="username"
              name="username"
              value={formData.username}
              onChange={handleInputChange}
              required
            />
            {errors.username && <div className="error-message">{errors.username}</div>}
          </div>

          <div className="form-group">
            <label htmlFor="first_name">Prénom</label>
            <input
              type="text"
              id="first_name"
              name="first_name"
              value={formData.first_name}
              onChange={handleInputChange}
              required
            />
            {errors.first_name && <div className="error-message">{errors.first_name}</div>}
          </div>

          <div className="form-group">
            <label htmlFor="last_name">Nom</label>
            <input
              type="text"
              id="last_name"
              name="last_name"
              value={formData.last_name}
              onChange={handleInputChange}
              required
            />
            {errors.last_name && <div className="error-message">{errors.last_name}</div>}
          </div>

          <div className="form-group">
            <label htmlFor="email">Adresse email</label>
            <input
              type="email"
              id="email"
              name="email"
              value={formData.email}
              onChange={handleInputChange}
              required
            />
            {errors.email && <div className="error-message">{errors.email}</div>}
          </div>

          <div className="form-group">
            <label htmlFor="password">Mot de passe</label>
            <input
              type="password"
              id="password"
              name="password"
              value={formData.password}
              onChange={handleInputChange}
              required
            />
            {errors.password && <div className="error-message">{errors.password}</div>}
          </div>

          <div className="form-group">
            <label htmlFor="confirmPassword">Confirmer le mot de passe</label>
            <input
              type="password"
              id="confirmPassword"
              name="confirmPassword"
              value={formData.confirmPassword}
              onChange={handleInputChange}
              required
            />
            {errors.confirmPassword && <div className="error-message">{errors.confirmPassword}</div>}
          </div>

          <div className="form-group">
            <label htmlFor="role">Rôle</label>
            <select
              id="role"
              name="role"
              value={formData.role}
              onChange={handleInputChange}
              required
            >
              <option value="">-- Sélectionner --</option>
              <option value="investor">Investisseur</option>
              <option value="client">Client</option>
            </select>
            {errors.role && <div className="error-message">{errors.role}</div>}
          </div>

          <div className="form-group">
            <label htmlFor="name">Nom affiché</label>
            <input
              type="text"
              id="name"
              name="name"
              value={formData.name}
              onChange={handleInputChange}
            />
          </div>

          {errors.submit && <div className="error-message submit-error">{errors.submit}</div>}

          <button type="submit" className="submit-btn" disabled={isLoading}>
            {isLoading ? "Création en cours..." : "Créer mon compte"}
          </button>
        </form>

        <div className="register-footer">
          <p>
            Déjà un compte ?{" "}
            <Link to="/login" className="login-link">
              Se connecter
            </Link>
          </p>
        </div>
      </div>

      {showSuccess && (
        <div className="success-notification show">
          <span>✅</span>
          <div>
            <strong>Inscription réussie !</strong><br />
            Redirection vers votre espace personnel...
          </div>
        </div>
      )}
    </div>
  );
};

export default Register;