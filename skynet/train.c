#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include "train.h"

static int _sort_param_array_cmp(const void *a, const void *b);
static void sort_param_array(param_state_t **param_array, int array_size);
static void normalize(param_state_t *param);
static double cap_to_nonnegative(double n);
static void print_param(const param_state_t *param);
static void print_param_array(param_state_t **param_array, int array_size);

void train(char *filename) {
    param_state_t **param_array = init_param_array(GENERATION_SIZE);
    // param_state_t **offspring_array = calloc(FITTEST_SIZE, sizeof(param_state_t *));
    sort_param_array(param_array, GENERATION_SIZE);

    printf("Computing the loss of initial population...\n");
    for (int i = 0; i < GENERATION_SIZE; i++) {
        replace_parameter(param_array[i]);
        compute_gain(param_array[i]);
        printf("Gain successfully computed for param vec %d\n", i);
    }
    sort_param_array(param_array, GENERATION_SIZE);
    print_param_array(param_array, GENERATION_SIZE);
    //print_and_save_result(param_array, 1);

    int count = 0;
    while(count < 1000) {
        for (int i = 0; i < GAME_ITERATION_COUNT; i++) {
            param_state_t **fittest = select_fittest(param_array);
            param_state_t *child = generate_children(fittest);
            free(param_array[GENERATION_SIZE - 1]);
            param_array[GENERATION_SIZE - 1] = child;
            /* delete the worst 30% of the population and add offsprings */
            // for (int i = GENERATION_SIZE - FITTEST_SIZE; i < GENERATION_SIZE; i++) {
            //     assert (offspring_array[i - GENERATION_SIZE + FITTEST_SIZE] != NULL);
            //     param_array[i] = offspring_array[i - GENERATION_SIZE + FITTEST_SIZE];
            // }
            // memset(offspring_array, 0, FITTEST_SIZE * sizeof(param_state_t *));
        }
        printf("Computing gain of new parameter vectors...\n");
        for (int i = 0; i < GENERATION_SIZE; i++) {
            // printf("generate i = %d, param_array[i] = %p\n", i, param_array[i]);
            replace_parameter(param_array[i]);
            compute_gain(param_array[i]);
        }

        sort_param_array(param_array, GENERATION_SIZE);

        double total_loss = 0;
        for (int i = 0; i < GENERATION_SIZE; i++) {
            total_loss += param_array[i]->gain;
        }
        printf("Average Gain= %f\n", total_loss);
        //print_and_save_result(param_array, 1);
        count++;
    }
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
    FILE *fp;
    char path[20];

    for (int i = 0; i < GAME_ITERATION_COUNT; i++) {
        system("./Main clever-ai clever-ai --strategy1 Skynet --strategy2 PlanetRankRush --headless --no-recomp > out.log");
        fp = popen("/usr/bin/tail -n2 ./out/log1.txt", "r");
        fgets(path, sizeof(path), fp);
        param->gain = atoi(path);
        printf("path = %s, gain = %d\n", path, param->gain);
        printf("The gain of this parameter is %d\n", param->gain);
    }
}

param_state_t **select_fittest(param_state_t **param_array) {
    int sample_size = FITTEST_SIZE;
    param_state_t **sample = calloc(sample_size, sizeof(param_state_t *));
    bool *used = calloc(GENERATION_SIZE, sizeof(bool));
    for (int i = 0; i < sample_size; i++) {
        int random = -1;
        while (random == -1 || used[random]) random = randomInteger(0, GENERATION_SIZE);
        used[random] = true;
        sample[i] = param_array[random];
    }
    sort_param_array(sample, sample_size);
    sample = realloc(sample, 3 * sizeof(param_state_t *));
    assert (sample != NULL);
    sample[2] = NULL;

    free(used);
    return sample;
}

static int _sort_param_array_cmp(const void *a, const void *b) {
    return ((*(param_state_t **) b)->gain - (*(param_state_t **) a)->gain);
}

static void sort_param_array(param_state_t **param_array, int array_size) {
  qsort(param_array, array_size, sizeof(param_state_t*), _sort_param_array_cmp);
}

void replace_parameter(param_state_t *param) {
  char command[400];
  sprintf(command, "sed -i ''  '56s/^.*$/  , params = [%lf, %lf, %lf, %lf, %lf]/g' clever-ai/Submission2.hs",
          param->growth_w, param->pagerank_w, param->turns_w, param->danger_w, param->offensive_w);
  system(command);
}

param_state_t *generate_children(param_state_t **fittest) {
    param_state_t *param = malloc(sizeof(param_state_t));
    param->growth_w = 0;
    param->pagerank_w = 0;
    param->turns_w = 0;
    param->danger_w = 0;
    param->offensive_w = 0;
    for (int i = 0; fittest[i] != NULL; i++) {
        param_state_t *temp = fittest[i];
        param->growth_w += temp->growth_w * temp->gain;
        param->pagerank_w += temp->pagerank_w * temp->gain;
        param->turns_w += temp->turns_w * temp->gain;
        param->danger_w += temp->danger_w * temp->gain;
        param->offensive_w += temp->offensive_w * temp->gain;
    }
    normalize(param);

    // mutate
    if (randomInteger(0, 100) < 5) {
        int choice = randomInteger(0, 5);
        switch (choice) {
        case 0:
            param->growth_w = cap_to_nonnegative(param->growth_w + randomDouble(-0.2, 0.2));
            break;
        case 1:
            param->pagerank_w = cap_to_nonnegative(param->pagerank_w + randomDouble(-0.2, 0.2));
            break;
        case 2:
            param->turns_w = cap_to_nonnegative(param->turns_w + randomDouble(-0.2, 0.2));
            break;
        case 3:
            param->danger_w = cap_to_nonnegative(param->danger_w + randomDouble(-0.2, 0.2));
            break;
        case 4:
            param->offensive_w = cap_to_nonnegative(param->danger_w + randomDouble(-0.2, 0.2));
            break;
        }
    }
    normalize(param);

    return param;
}

static void normalize(param_state_t *param) {
    /* temporary variables set to register in order to run faster */
  register double hw = param->growth_w;
  hw *= hw;
  register double lw = param->pagerank_w;
  lw *= lw;
  register double nw = param->turns_w;
  nw *= nw;
  register double bw = param->danger_w;
  bw *= bw;
  register double ow = param->offensive_w;
  ow *= ow;
  double magnitude = sqrt(hw + lw + nw + bw + ow);

  param->growth_w /= magnitude;
  param->pagerank_w /= magnitude;
  param->turns_w /= magnitude;
  param->danger_w /= magnitude;
  param->offensive_w /= magnitude;
}

static double cap_to_nonnegative(double n) {
    if (n < 0) return 0.0;
    else return n;
}

static void print_param(const param_state_t *param) {
    printf("param_state_t(growth_w=%f, pagerank_w=%f, turns_w=%f, danger_w=%f, gain=%d)\n",
            param->growth_w, param->pagerank_w, param->turns_w, param->danger_w, param->gain);
}

static void print_param_array(param_state_t **param_array, int array_size) {
    printf("==================\n");
    for (int i = 0; i < array_size; i++) {
        print_param(param_array[i]);
    }
}
