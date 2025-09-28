/*
** EPITECH PROJECT, 2025
** dzd
** File description:
** zdzd
*/

#include "GameServer.hpp"

Snapshot GameServer::generateSnapshot() const {
    Snapshot snap;

    for (EntityID id : em.getEntities()) {
        Snapshot::EntityData e;
        e.id = id;

        if (const Position* p = storage.getPosition(id)) e.pos = *p;
        if (const Velocity* v = storage.getVelocity(id)) e.vel = *v;
        if (const Health* h = storage.getHealth(id)) e.hp = *h;

        snap.entities.push_back(e);
    }

    return snap;
}
