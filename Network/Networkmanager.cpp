#include "Networkmanager.hpp"
#include <iostream>
#include <thread>

NetworkManager& NetworkManager::getInstance() {
    static NetworkManager instance;
    return instance;
}

NetworkManager::NetworkManager() 
    : m_serveurTCP(9090)
    , m_serveurUDP(9091, m_inputQueue)
{}

bool NetworkManager::initialize(uint16_t tcp_port, uint16_t udp_port) {
    try {
        std::cout << "Initialisation réseau - TCP:" << tcp_port << " UDP:" << udp_port << std::endl;
        
        std::thread tcpThread([this]() {
            m_serveurTCP.demarrer();
        });
        tcpThread.detach();
        
        std::thread udpThread([this]() {
            m_serveurUDP.demarrer();
        });
        udpThread.detach();
        
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
        std::cout << "Réseau démarré!" << std::endl;
        return true;
        
    } catch (const std::exception& e) {
        std::cerr << "Erreur réseau: " << e.what() << std::endl;
        return false;
    }
}

void NetworkManager::shutdown() {
    std::cout << "Arrêt réseau..." << std::endl;
    m_serveurTCP.arreter();
    m_serveurUDP.arreter();
}

bool NetworkManager::hasInputs() const { 
    return !m_inputQueue.estVide(); 
}

bool NetworkManager::popInput(InputPacket& input) { 
    return m_inputQueue.recuperer(input); 
}


void NetworkManager::sendSnapshot(const SnapshotPacket& snapshot) { 
    m_serveurUDP.envoyerSnapshot(snapshot); 
}

uint32_t NetworkManager::getConnectedPlayersCount() const {
    return 1;
}


uint32_t NetworkManager::login(const std::string& username) {
    return m_lobbyManager.login(username);
}

std::vector<Lobby> NetworkManager::getLobbyList() {
    return m_lobbyManager.getLobbyList();
}

std::pair<bool, uint32_t> NetworkManager::joinLobby(uint32_t player_id, uint32_t lobby_id) {
    return m_lobbyManager.joinLobby(player_id, lobby_id);
}

uint32_t NetworkManager::createLobby(uint8_t max_players) {
    return m_lobbyManager.createLobby(max_players);
}
