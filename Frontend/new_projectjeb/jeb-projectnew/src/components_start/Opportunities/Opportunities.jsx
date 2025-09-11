import React, { useState } from "react";
import "./Opportunities.scss";

function Opportunities() {
  const [notifications, setNotifications] = useState([
    {
      id: 1,
      investor: "Jean Dupont",
      startup: "GreenTech",
      message: "Un investisseur est intéressé par ton projet GreenTech 🌱",
      time: "10:45",
    },
    {
      id: 2,
      investor: "Marie Curie",
      startup: "HealthAI",
      message: "Marie veut discuter de ton projet HealthAI 🧬",
      time: "11:30",
    },
  ]);

  return (
    <div className="opportunities-container">
      <h2>Opportunités</h2>
      {notifications.length === 0 ? (
        <p>Aucune opportunité pour l’instant.</p>
      ) : (
        <div className="notifications-list">
          {notifications.map((notif) => (
            <div key={notif.id} className="notification-card">
              <h3>{notif.startup}</h3>
              <p><strong>{notif.investor}</strong> : {notif.message}</p>
              <span className="time">{notif.time}</span>
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

export default Opportunities;