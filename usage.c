/*
** EPITECH PROJECT, 2025
** Help implementation
** File description:
** -help
*/
#include "include/my.h"

void print_help(void)
{
    printf("USAGE: ./myftp port path\n");
    printf("       port: port number on which the server socket listens\n");
    printf("       path: path to the home directory for the Anonymous user\n");
}
