import React, { useState } from "react";
import "./Messaging.scss";

function Messaging() {
  const [conversations] = useState([
    {
      id: 1, name: "Alice",
      messages: [
        { text: "Salut ! Comment avance ta startup ?", sender: "Alice", time: "09:30" },
        { text: "Ça va bien ! Je bosse sur le pitch deck 🚀", sender: "Moi", time: "09:32" },
      ]
    },
    {
      id: 2, name: "Bob",
      messages: [
        { text: "Tu as vu les dernières tendances tech ?", sender: "Bob", time: "10:00" },
        { text: "Oui, l'IA est partout maintenant !", sender: "Moi", time: "10:05" },
      ]
    },
    {
      id: 3, name: "Charlie",
      messages: [
        { text: "On se fait un café cette semaine ?", sender: "Charlie", time: "11:15" },
      ]
    },
  ]);

  const [activeConv, setActiveConv] = useState(conversations[0]);
  const [newMessage, setNewMessage] = useState("");

  const handleSend = (e) => {
    e.preventDefault();
    if (!newMessage.trim()) return;

    const newMsg = {
      text: newMessage,
      sender: "Moi", // plus tard tu pourras gérer plusieurs utilisateurs
      time: new Date().toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" }),
    };

    const updateConv = {
      ...activeConv,
      messages: [...activeConv.messages, newMsg],
    };
    setActiveConv(updateConv)
    setNewMessage("");
  };

  return (
    <div className="messaging-container">
      <aside className="conversation-list">
        <h3>Conversations</h3>
        <ul>
          {conversations.map((conv) => (
            <li key={conv.id} className={conv.id === activeConv.id ? "active" : ""} onClick={() => setActiveConv(conv)}>
              {conv.name}
            </li>
          ))}
        </ul>
      </aside>
      <div className="chat-area">
        <h2>{activeConv.name}</h2>
        <div className="messages-list">
          {activeConv.messages.map((msg, i) => (
            <div key={i} className={`message-bubble ${msg.sender === "Moi" ? "sent" : "received"}`}>
              <p>{msg.text}</p>
              <span className="time">{msg.time}</span>
            </div>
          ))}
        </div>
        <form onSubmit={handleSend} className="message-form">
          <input type="text" value={newMessage} onChange={(e) => setNewMessage(e.target.value)} placeholder="Écris ton message..." />
          <button type="submit">Envoyer</button>
        </form>
      </div>
    </div>
  );
}

export default Messaging;