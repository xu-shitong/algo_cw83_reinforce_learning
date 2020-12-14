#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "train.h"

char *command;
FILE *summary;

static double randomDouble(int lower, int upper);
int read_last_danger_point();
void evolve_typeA(param_state_t**array, int father_index, int mother_index, int child_index);
void evolve_typeB(param_state_t**array, int father_index, int mother_index, int child_index);
void swap_parameters(param_state_t **array);
void free_array(param_state_t **array);
void show_fittest(param_state_t **array);

// void train(char *filename) {
void train() {
  param_state_t **array = init_param_array();
  command = malloc(150 * sizeof(char));
  summary = fopen("summary.txt", "w");
  // 1. give GENERATION_SIZE number of ramdom parameters sets, calculate dr of each
  for (int i = 0; i < GENERATION_SIZE; i++) {
    compute_gain(array[i]);
  }

  // 4. repeat 2.3. GAME_ITERATION_COUNT times
  for (int j = 0; j < GAME_ITERATION_COUNT; j++) {
      // 2. choose top FITTEST_SIZE number of parameters sets according to gain, 
      select_fittest(array);

      show_fittest(array);

      //    2.1 pairwise swap parameters between these fittest parameters
      //    2.2 swapped result are stored as new parameters but not substitute the old parameter
      swap_parameters(array);

      for (int i = 2 * FITTEST_SIZE; i < GENERATION_SIZE;i++) {
        array[i] = generate_random_param();
      }

      // 3. choose remaning parameters random again and calculate 
      //    calculate only those new parameters
      for (int i = FITTEST_SIZE; i < GENERATION_SIZE; i++) {
        compute_gain(array[i]);
      }
  }

  // 5. choose the one with highest as the result parameter of training
  param_state_t *result = array[0];
  for (int i = 1; i < GENERATION_SIZE; i++) {
    if (result->gain < array[i]->gain) {
      result = array[i];
    }
  }
  printf("pGro = %lf, pPr = %lf, pT = %lf, pDr = %lf, offensive = %lf\n", result->growth_w, result->pagerank_w, result->turns_w, result->danger_w, result->offensive_w);

  free_array(array);
  free(command);
  fclose(summary);
}

/* PRE: FITTEST_SIZE should smaller than HALF of generation size */
void swap_parameters(param_state_t **array) {
  int index = FITTEST_SIZE;
  for (int i = 0; i < FITTEST_SIZE; i+=2) {
    evolve_typeA(array, i, i + 1, index);
    index++;
    evolve_typeB(array, i, i + 1, index);
    index++;
  }
}

void evolve_typeA(param_state_t**array, int father_index, int mother_index, int child_index) {
  param_state_t *param = array[child_index], *father = array[father_index], *mother = array[mother_index];
  param->growth_w = father->growth_w;
  param->pagerank_w = father->pagerank_w;
  param->turns_w = mother->turns_w;
  param->danger_w = mother->danger_w;
  param->offensive_w = father->offensive_w;
  param->gain = 0;
}

void evolve_typeB(param_state_t**array, int father_index, int mother_index, int child_index) {
  param_state_t *param = array[child_index], *father = array[father_index], *mother = array[mother_index];
  param->growth_w = mother->growth_w;
  param->pagerank_w = mother->pagerank_w;
  param->turns_w = father->turns_w;
  param->danger_w = father->danger_w;
  param->offensive_w = mother->offensive_w;
  param->gain = 0;
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
    param->growth_w = randomDouble(0, 100);
    param->pagerank_w = randomDouble(0, 100);
    param->turns_w = randomDouble(0, 100);
    param->danger_w = randomDouble(0, 100);
    param->offensive_w = randomDouble(0, 1);
    param->gain = 0;
    printf("using para %lf, %lf, %lf, %lf, %lf",
          param->growth_w, param->pagerank_w, param->turns_w, param->danger_w, param->offensive_w);
    return param;
}

static double randomDouble(int lower, int upper) {
  return ((double) rand()) / RAND_MAX * (upper - lower);
}

void free_array(param_state_t **array) {
  for (int i=0;i<GENERATION_SIZE;i++) {
    free(array[i]);
  }
  free(array);
}

void replace_parameter(param_state_t *param) {
  sprintf(command, "sed -i ''  '56s/^.*$/  , params = [%lf, %lf, %lf, %lf, %lf]/g' clever-ai/Submission2.hs",
          param->growth_w, param->pagerank_w, param->turns_w, param->danger_w, param->offensive_w);
  system(command);  
}

/* compute the danger point for one parameter of parameter combination */
void compute_gain(param_state_t *param) {
    // for (int i = 0; i < GAME_ITERATION_COUNT; i++) {
    //system(**change the params in ai initial state**)
        replace_parameter(param);
        system("./Main clever-ai clever-ai --strategy1 PlanetRankRush --strategy2 Skynet --headless");
        param->gain = read_last_danger_point();
    // }
}

/* read the danger point calculated and stored by haskell program in log2.txt
   haskell will always start the value by #, so search from end to #, return such value */
int read_last_danger_point() {
  FILE *file = fopen("out/log2.txt", "r");
  if (file == NULL) {
    printf("fail to open output file\n");
    return 0;
  }
  fseek(file, -2, SEEK_END);
  char c[2];
  fread(c, sizeof(char), 1, file);
  while (ftell(file) > 1 && c[0] != '#') {
    fseek(file, -3, SEEK_CUR);
    fread(c, sizeof(char) + 1, 1, file);
  }
  char string[20];
  fseek(file, -1, SEEK_CUR);
  fread(string, 20, 1, file);
  fclose(file);
  return atoi(string);
}

/* start from the 31th parameter, scan the whole generation
   if its gain is larger than any of the firth 30 parameter's gain, substitute that one
   does not delete the parameter, only overwrite but not swap */
void select_fittest(param_state_t **param_array) {
  for (int i = FITTEST_SIZE; i < GENERATION_SIZE; i++) {
    for (int j = 0;j < FITTEST_SIZE;j++) {
      if (param_array[j]->gain < param_array[i]->gain) {
        param_state_t *lower_param = param_array[j];
        param_state_t *higher_param = param_array[i];
        lower_param->danger_w = higher_param->danger_w;
        lower_param->gain = higher_param->gain;
        lower_param->growth_w = higher_param->growth_w;
        lower_param->pagerank_w = higher_param->pagerank_w;
        lower_param->turns_w = higher_param->turns_w;
        lower_param->offensive_w = higher_param->offensive_w;
      }
    }
  }
}

void show_fittest(param_state_t **array) {
  for (int i = FITTEST_SIZE; i < GENERATION_SIZE; i++) {
    fprintf(summary, "%d ", array[i]->gain);
  }
  fprintf(summary, "\n");
}


void generate_children(param_state_t *fittest, param_state_t **param_array) {
  
}
