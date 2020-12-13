#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "train.h"

void train(char *filename) {

}

param_state_t **init_param_array() {
    param_state_t **array = calloc(GENERATION_SIZE, sizeof(param_state_t *));
    for (int i = 0; i < GENERATION_SIZE; i++) {
        array[i] = generate_random_param();
    }

    return array;
}

param_state_t *generate_random_param() {
    param_state_t *param = malloc(sizeof(param_state_t));
    param->growth_w = randomDouble(0, 1);
    param->pagerank_w = randomDouble(0, 1);
    param->turns_w = randomDouble(0, 1);
    param->danger_w = randomDouble(0, 1);
    param->gain = 0;

    return param;
}

void compute_gain(param_state_t *param) {
    for (int i = 0; i < GAME_ITERATION_COUNT; i++) {
        
        system("../Main clever-ai clever-ai --strategy1 PlanetRankRush --strategy2 Skeynet");
    }
}

param_state_t **select_fittest(param_state_t **param_array);
void generate_children(param_state_t *fittest, param_state_t **param_array);
