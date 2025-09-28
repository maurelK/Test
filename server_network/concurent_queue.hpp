#pragma once

#include <queue>
#include <mutex>
#include <condition_variable>

template<typename T>
class ConcurrentQueue {
private:
    std::queue<T> m_queue;
    std::mutex m_mutex;
    std::condition_variable m_condition;

public:
    void push(const T& item) {
        {
            std::unique_lock<std::mutex> lock(m_mutex);
            m_queue.push(item);
        }
        m_condition.notify_one();
    }

    bool pop(T& item) {
        std::unique_lock<std::mutex> lock(m_mutex);
        m_condition.wait(lock, [this]() { return !m_queue.empty(); });
        
        item = m_queue.front();
        m_queue.pop();
        return true;
    }

    bool try_pop(T& item) {
        std::unique_lock<std::mutex> lock(m_mutex);
        if (m_queue.empty()) {
            return false;
        }
        
        item = m_queue.front();
        m_queue.pop();
        return true;
    }

    bool empty() const {
        std::unique_lock<std::mutex> lock(m_mutex);
        return m_queue.empty();
    }

    size_t size() const {
        std::unique_lock<std::mutex> lock(m_mutex);
        return m_queue.size();
    }
};