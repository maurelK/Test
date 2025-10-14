#pragma once

#include <queue>
#include <mutex>
#include "protocol.hpp"

class ConcurrentQueue {
private:
    std::queue<InputPacket> m_file;
    mutable std::mutex m_verrou;

public:
    void ajouter(const InputPacket& element) {
        std::lock_guard<std::mutex> verrouillage(m_verrou);
        m_file.push(element);
    }

    bool recuperer(InputPacket& element) {
        std::lock_guard<std::mutex> verrouillage(m_verrou);
        
        if (m_file.empty()) {
            return false;
        }
        
        element = m_file.front();
        m_file.pop();
        return true;
    }

    bool estVide() const {
        std::lock_guard<std::mutex> verrouillage(m_verrou);
        return m_file.empty();
    }

    int taille() const {
        std::lock_guard<std::mutex> verrouillage(m_verrou);
        return m_file.size();
    }
};
