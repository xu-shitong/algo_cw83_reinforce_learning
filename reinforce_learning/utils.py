import os 
import ast
import torch

LOG_FILE_PATH = "out/log2.txt"
DISCOUNT_RATIO = 0.9


# update network parameter in haskell file
# input: net is a network with 8, 8, 5 neurons each layer
def update_model_param(net):
  output_w = net[0].weight.data.tolist()
  output_b = net[0].bias.data.tolist()
  
  # use ssd to change value in haskell file
  param_names = ['output_w', 'output_b']
  for i, param in enumerate([output_w, output_b]):
    os.system(f"sed -i ''  '{463 + i}s/^.*$/{param_names[i]} = {param}/g' clever-ai/Submission2.hs")
  return

# get training result from log.2 output file
# output: features, rewards, values of type list
#   features.shape = (step_num, adj_planet_num, FEATURE_NUM)
#   rewards.shape = (step_num)
#   values.shape = (step_num, adj_planet_num, VAL_NUM)
def read_trial_result():
  features = []
  rewards = []
  explored_vals = []
  
  # read data in dictionary from output file
  file = open(LOG_FILE_PATH)
  
  line = file.readline()
  set_size = 0
  while line:
    if line[0] == '-' or line[0] == '\n':
      # line start with '-' or is empty, ignore
      line = file.readline()
      continue
    data_dict = ast.literal_eval(line)
    
    features.append(torch.tensor(data_dict['features']))
    rewards.append(data_dict['reward'])
    explored_vals.append(torch.tensor(data_dict['vals']))
    line = file.readline()
    set_size += 1
  
  # process sets so that reward, features, vals with same index belong to the same step 
  rewards.pop(0)
  features.pop(set_size - 1)
  explored_vals.pop(set_size - 1)
  
  return features, rewards, explored_vals


# get discounted reward, by using discount ratio R = DISCOUNTED_RATIO
# input: list of reward, represent rewards from one trial
# output: list of reward, each discounted dr[i] = r[i] + r[i+1] * R + r[i+2] * R*R + ...
def get_discounted_reward(rewards):
  l = len(rewards)
  for i in range(l-2, -1, -1):
    # change rewards from the second last reward to the first
    rewards[i] += rewards[i+1] * DISCOUNT_RATIO
  return rewards
