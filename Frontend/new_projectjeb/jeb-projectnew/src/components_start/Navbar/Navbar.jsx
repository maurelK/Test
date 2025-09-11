import React from "react";
import "./Navbar.scss";
import { Link } from "react-router-dom";
import { FiMenu } from "react-icons/fi";

const Navbar = () => {
    return (
        <div className="navbar">
            <div className="laptop">
                <h2 className="logo">Startup Area</h2>
                <div className="navigation">
                    <ul className="links-conteneur">
                        <li className="link">
                            <Link to="/" end>
                            Créer Startup
                            </Link>
                        </li>
                        <li className="link">
                            <Link to={"/messagerie"}>Messagerie</Link>
                        </li>
                        <li className="link">
                            <Link to ={"/opportunities"}>Opportunités</Link>
                        </li>
                    </ul>
                    <FiMenu className="menu-icon"/>
                </div>
            </div>
            <div className="mobile"></div>
        </div>
    );
};

export default Navbar;