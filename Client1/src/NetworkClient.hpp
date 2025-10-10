#pragma once

#include <atomic>
#include <thread>
#include  "protocol.hpp"
#include <mutex>
#include <queue>
#include <boost/asio.hpp>

using boost::asio::ip::udp;


class NetworkClient {
public:
    NetworkClient(const std::string & host, unsigned short port);
    ~NetworkClient();

    bool start();
    void stop();

    void sendInput(const InputPacket & input);

bool pollSnapshot(SnapshotPacket& snapshot);

private:
     void receiveLoop();

    boost::asio::io_context m_context;
    udp::socket m_socket;
    udp::endpoint m_serverEndpoint;

    std::thread m_thread;
    std::atomic<bool> m_running{false};

    std::queue<SnapshotPacket> m_snapshots;
    std::mutex m_mutex;
};