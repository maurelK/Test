import React, { useState } from "react";
import Accueil from "../../src_start/components_start/Accueil/Accueil";
import "./Startup.css";

const Startup = ({ lang }) => {
  const [showForm, setShowForm] = useState(false);
  const [startupList, setStartupList] = useState([]);

  const [formData, setFormData] = useState({
    name: "",
    legal_status: "",
    address: "",
    email: "",
    telephone: "",
    created: "",
    description: "",
    website_url: "",
    social_media_url: "",
    project_status: "",
    needs: "",
    sector: "",
    maturity: "",
    founders: "",
  });

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.id]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    if (!formData.name || !formData.email || !formData.telephone || !formData.address || !formData.created || !formData.description || !formData.project_status || !formData.sector || !formData.founders) {
      alert("Veuillez remplir tous les champs obligatoires.");
      return;
    }
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(formData.email)) {
      alert("Veuillez entrer une adresse email valide.");
      return;
    }
    setStartupList([...startupList, formData]);
    setShowForm(false);
    setFormData({
      name: "",
      legal_status: "",
      address: "",
      email: "",
      telephone: "",
      created: "",
      description: "",
      website_url: "",
      social_media_url: "",
      project_status: "",
      needs: "",
      sector: "",
      maturity: "",
      founders: "",
    })
  };

  const handleEdit = (index) => {
    setFormData(startupList[index]);
    setShowForm(true);
  };

  return (
    <div className="app">
      <section className="startup-accueil">
        <div className="accueil">
          <Accueil />
          <button type="button" onClick={() => setShowForm(!showForm)}>
            Créer Profil
          </button>
          {showForm && (
            <div className="form-container">
              <h2>Créer votre startup</h2>

              <form onSubmit={handleSubmit} className="startup-form">

                {/* Section Infos générales */}
                <h3>Informations générales</h3>
                <div className="form-grid">
                  <div>
                    <label htmlFor="name">Nom de la startup</label>
                    <input id="name" type="text" value={formData.name} onChange={handleChange} placeholder="Entrez le nom.." />
                  </div>
                  <div>
                    <label htmlFor="legal_status">Statut légal</label>
                    <input id="legal_status" type="text" value={formData.legal_status} onChange={handleChange} placeholder="SARL, SAS..." />
                  </div>

                  <div className="full-width">
                    <label htmlFor="founders">Fondateurs</label>
                    <input id="founders" type="text" value={formData.founders} onChange={handleChange} placeholder="Nom des créateurs..." />
                  </div>

                  <div className="full-width">
                    <label htmlFor="description">Description</label>
                    <textarea id="description" value={formData.description} onChange={handleChange} placeholder="Décrivez votre projet..." />
                  </div>
                </div>

                {/* Section Contacts */}
                <h3>Contacts</h3>
                <div className="form-grid">
                  <div>
                    <label htmlFor="email">Adresse mail</label>
                    <input id="email" type="email" value={formData.email} onChange={handleChange} placeholder="example@gmail.com" />
                  </div>
                  <div>
                    <label htmlFor="telephone">Téléphone</label>
                    <input id="telephone" type="text" value={formData.telephone} onChange={handleChange} placeholder="+22901XXXXXXXX" />
                  </div>

                  <div className="full-width">
                    <label htmlFor="address">Adresse</label>
                    <input id="address" type="text" value={formData.address} onChange={handleChange} placeholder="Adresse complète..." />
                  </div>

                  <div>
                    <label htmlFor="website_url">Site web</label>
                    <input id="website_url" type="text" value={formData.website_url} onChange={handleChange} placeholder="www.monsite.com" />
                  </div>
                  <div>
                    <label htmlFor="social_media_url">Réseaux sociaux</label>
                    <input id="social_media_url" type="text" value={formData.social_media_url} onChange={handleChange} placeholder="Lien LinkedIn, Twitter..." />
                  </div>
                </div>

                {/* Section Projet */}
                <h3>Projet</h3>
                <div className="form-grid">
                  <div>
                    <label htmlFor="created">Date de création</label>
                    <input id="created" type="date" value={formData.created} onChange={handleChange} />
                  </div>
                  <div>
                    <label htmlFor="maturity">Maturité</label>
                    <input id="maturity" type="text" value={formData.maturity} onChange={handleChange} placeholder="Idée, Prototype, Produit..." />
                  </div>

                  <div>
                    <label htmlFor="sector">Secteur</label>
                    <input id="sector" type="text" value={formData.sector} onChange={handleChange} placeholder="Santé, Agro-alimentaire, Tech..." />
                  </div>
                  <div>
                    <label htmlFor="project_status">Statut du projet</label>
                    <input id="project_status" type="text" value={formData.project_status} onChange={handleChange} placeholder="En cours, Terminé..." />
                  </div>

                  <div className="full-width">
                    <label htmlFor="needs">Besoins</label>
                    <input id="needs" type="text" value={formData.needs} onChange={handleChange} placeholder="Financement, Partenaires, etc." />
                  </div>
                </div>

                <button type="submit" className="submit-btn">Valider</button>
              </form>

            </div>
          )}
        </div>
      </section>
      <section className="startup_list_selection">
        <div className="startup_list">
          {startupList.map((startup, index) => (
            <div key={index} className="startup_card" onClick={() => handleEdit(index)}>
              <h3>Nom : {startup.name}</h3>
              <p>Email : {startup.email}</p>
              <p>Telephone : {startup.telephone}</p>
              <p>Url du site : {startup.website_url}</p>
              <p>Secteur : {startup.sector}</p>
              <span>Statut : {startup.project_status}</span>
            </div>
          ))}
        </div>
      </section>
    </div>
  );
};

export default Startup;
