#include "include/my.h"

int generate_egg_id(game_t *game) 
{
    return game->egg_count++;
}

void execute_fork(int client_fd, int i, char *buffer, info_t *info) 
{
    (void)buffer;
    player_t *player = &info->game.players[i];
    tile_t *tile = &info->game.map[player->y][player->x];
    team_t *team = &info->game.teams[player->team_id];

    // Limites max d'oeufs pour l'équipe et la case
    if (team->egg_count >= CLIENTS || tile->egg_count >= CLIENTS)
        return;

    egg_t *egg = &team->eggs[team->egg_count];

    // Si la struct a le champ id, décommente la ligne suivante
    // int egg_id = generate_egg_id(&info->game);
    // egg->id = egg_id;
    dprintf(1, "Player pos: x=%d y=%d\n", player->x, player->y);
    egg->x = player->x;
    egg->y = player->y;
    egg->team_id = player->team_id;
    egg->is_hatched = 0;
    egg->hatch_time = 42.0 / info->game.freq;

    // Ici, on utilise egg_count comme id implicite
    int egg_id = team->egg_count;

    // On enregistre l'oeuf sur la case
    tile->egg_ids[tile->egg_count++] = egg_id;

    // On incrémente le nombre d'oeufs de l'équipe
    team->egg_count++;

    // Envoie "ok" au client
    dprintf(client_fd, "ok\n");

    // Affiche dans la sortie serveur la création d'oeuf (id, joueur, x, y)
    dprintf(1, "enw %d %d %d %d\n", egg_id, player->id, egg->x, egg->y);
}



egg_t *get_available_egg(team_t *team) 
{
    for (int i = 0; i < team->egg_count; i++) {
        if (!team->eggs[i].is_hatched) {
            team->eggs[i].is_hatched = 1;
            return &team->eggs[i];
        }
    }
    return NULL;
}

void spawn_player_from_egg(player_t *player, team_t *team, game_t *game) 
{
    egg_t *egg = get_available_egg(team);

    if (egg) {
        player->x = egg->x;
        player->y = egg->y;
        dprintf(1, "ebo %d\n", game->egg_count);
    } else {
        player->x = rand() % game->width;
        player->y = rand() % game->height;
    }
}
