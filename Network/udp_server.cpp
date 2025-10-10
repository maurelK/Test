#include "udp_server.hpp"
#include <iostream>
#include <algorithm>

UDPServer::UDPServer(unsigned short port, ConcurrentQueue& inputQueue) 
    : m_port(port)
    , m_socket(m_contexteIO, udp::endpoint(udp::v4(), port))
    , m_inputQueue(inputQueue)
{
    std::cout << "Serveur UDP créé sur le port " << port << std::endl;
}

void UDPServer::demarrer()
{
    std::cout << "Démarrage du serveur UDP..." << std::endl;
    demarrerReception();
    
    std::thread([this]() {
        m_contexteIO.run();
    }).detach();
}

void UDPServer::arreter()
{
    std::cout << "Arrêt du serveur UDP..." << std::endl;
    m_contexteIO.stop();
    m_socket.close();
}

void UDPServer::demarrerReception()
{
    auto buffer = std::make_shared<std::vector<char>>(1024);
    
    m_socket.async_receive_from(
        boost::asio::buffer(*buffer), m_endpointDistant,
        [this, buffer](boost::system::error_code erreur, size_t taille) {
            if (!erreur && taille >= sizeof(InputPacket)) {
                InputPacket* input = reinterpret_cast<InputPacket*>(buffer->data());
                
                if (input->header.type == PacketType::INPUT) {
                    std::cout << "Input reçu - Joueur: " << input->player_id 
                              << " Move: (" << input->move_x << "," << input->move_y 
                              << ") Tir: " << input->shoot << std::endl;
                    
                    m_inputQueue.ajouter(*input);
                    
                    if (std::find(m_clients.begin(), m_clients.end(), m_endpointDistant) == m_clients.end()) {
                        m_clients.push_back(m_endpointDistant);
                        std::cout << "Nouveau client UDP: " << m_endpointDistant.address().to_string() 
                                  << ":" << m_endpointDistant.port() << std::endl;
                    }
                }
            }
            
            demarrerReception();
        });
}

void UDPServer::envoyerSnapshot(const SnapshotPacket& snapshot)
{
    for (const auto& client : m_clients) {
        try {
            m_socket.send_to(boost::asio::buffer(&snapshot, sizeof(snapshot)), client);
        } catch (const std::exception& e) {
            std::cout << "Erreur envoi UDP: " << e.what() << std::endl;
        }
    }
}