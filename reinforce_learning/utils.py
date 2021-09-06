import os 
import ast
import torch

LOG_FILE_PATH = "out/log2.txt"


# update network parameter in haskell file
# input: net is a network with 8, 8, 5 neurons each layer
def update_model_param(net):
  hidden1_w = net[0].weight.data.tolist()
  hidden1_b = net[0].bias.data.tolist()
  output_w = net[2].weight.data.tolist()
  output_b = net[2].bias.data.tolist()
  
  # use ssd to change value in haskell file
  param_names = ['hidden1_w', 'hidden1_b', 'output_w', 'output_b']
  for i, param in enumerate([hidden1_w, hidden1_b, output_w, output_b]):
    os.system(f"sed -i ''  '{463 + i}s/^.*$/{param_names[i]} = {param}/g' clever-ai/Submission2.hs")
  return

# get training result from log.2 output file
# output: features, rewards, values of type torch.tensor
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
    
    features.append(data_dict['features'])
    rewards.append(data_dict['reward'])
    explored_vals.append(data_dict['vals'])
    line = file.readline()
    set_size += 1
  
  # process sets so that reward, features, vals with same index belong to the same step 
  rewards.pop(0)
  features.pop(set_size - 1)
  explored_vals.pop(set_size - 1)
  
  return torch.tensor(features), torch.tensor(rewards), torch.tensor(explored_vals)
