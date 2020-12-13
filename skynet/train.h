#ifndef SKYNET_TRAIN_H
#define SKYNET_TRAIN_H

#define GENERATION_SIZE 100
#define FITTEST_SIZE 30
#define GAME_ITERATION_COUNT 10

/**
 * This struct represents a configuration of parameter and its gain
 */

typedef struct {
    double growth_w;
    double pagerank_w;
    double turns_w;
    double danger_w;
    /* gain is defined as how many ships are left at the end of the game */
    double gain;
} param_state_t;

/**
 * 1. generate random params
 * 2. run the game for certain number of times and get result
 * 3. order the result, then produce the next generation
 */

void train(char *filename);
param_state_t **init_param_array();
param_state_t *generate_random_param();
void compute_gain(param_state_t *param);
param_state_t **select_fittest(param_state_t **param_array);
void generate_children(param_state_t *fittest, param_state_t **param_array);

#endif