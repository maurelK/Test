#pragma once

#include <boost/asio.hpp>
#include "concurent_queue.hpp"
#include "protocol.hpp"
#include <vector>

using boost::asio::ip::udp;

class UDPServer {
public:
    UDPServer(unsigned short port, ConcurrentQueue& inputQueue);
    
    void demarrer();
    void arreter();
    void envoyerSnapshot(const SnapshotPacket& snapshot);

private:
    void demarrerReception();

    unsigned short m_port;
    boost::asio::io_context m_contexteIO;
    udp::socket m_socket;
    udp::endpoint m_endpointDistant;
    
    ConcurrentQueue& m_inputQueue;
    std::vector<udp::endpoint> m_clients;
};
