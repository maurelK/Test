#pragma once

#include <asio.hpp>
#include "concurent_queue.hpp"
#include "protocol.hpp"
#include <vector>

using asio::ip::udp;

class UDPServer {
public:
    UDPServer(unsigned short port, ConcurrentQueue& inputQueue);
    
    void demarrer();
    void arreter();
    void envoyerSnapshot(const SnapshotPacket& snapshot);
    size_t getClientCount() const { return m_clients.size(); }

private:
    void demarrerReception();

    unsigned short m_port;
    asio::io_context m_contexteIO;
    udp::socket m_socket;
    udp::endpoint m_endpointDistant;
    
    ConcurrentQueue& m_inputQueue;
    std::vector<udp::endpoint> m_clients;
};
