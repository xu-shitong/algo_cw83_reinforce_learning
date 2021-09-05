import torch 
from torch import nn, optim
import ast
import os

# define consts and hyperparameters
LOG_FILE_PATH = "out/log2.txt"
DISCOUNT_RATIO = 0.95
LEARNING_RATE = 0.1
EPOCH_NUM = 30
FEATURE_NUM = 5 # each planet takes 5 features
VAL_NUM = 2 # each planet generate 2 values after forwarded through network


# define network used
net = nn.Sequential(
  nn.Linear(in_features=6, out_features=8), 
  nn.ReLU(inplace=True),
  nn.Linear(in_features=8, out_features=8),
  nn.ReLU(inplace=True),
  nn.Linear(in_features=8, out_features=2),
  nn.Sigmoid()
  )

# update network parameter in haskell file
# input: net is a network with 8, 8, 5 neurons each layer
def update_model_param(net):
  hidden1_w = net[0].weight.data.tolist()
  hidden1_b = net[0].bias.data.tolist()
  hidden2_w = net[2].weight.data.tolist()
  hidden2_b = net[2].bias.data.tolist()
  output_w = net[4].weight.data.tolist()
  output_b = net[4].bias.data.tolist()
  
  # use ssd to change value in haskell file
  param_names = ['hidden1_w', 'hidden1_b', 'hidden2_w', 'hidden2_b', 'output_w', 'output_b']
  for i, param in enumerate([hidden1_w, hidden1_b, hidden2_w, hidden2_b, output_w, output_b]):
    os.system(f"sed -i ''  '{463 + i}s/^.*$/{param_names[i]} = {param}/g' clever-ai/Submission2.hs")
  return

# get discounted reward, by using discount ratio R = DISCOUNTED_RATIO
# input: list of reward, represent rewards from one trial
# output: list of reward, each discounted dr[i] = r[i] + r[i+1] * R + r[i+2] * R*R + ...
def get_discounted_reward(rewards):
  l = len(rewards)
  for i in range(l-2, -1, -1):
    # change rewards from the second last reward to the first
    rewards[i] += rewards[i+1] * DISCOUNT_RATIO
  return rewards

# get discounted normalized reward
# input: list of reward read from log file
# output: tensor list of discounted normalized rewards
def get_discounted_normalized_reward(rewards):
  discounted_rewards = torch.tensor(get_discounted_reward(rewards))
  
  mean_reward = discounted_rewards.mean()
  std_reward = discounted_rewards.std()
  return (discounted_rewards - mean_reward) / std_reward

# get training result from log.2 output file
# output: features, discounted rewards, values of type torch.tensor
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
  
  return torch.tensor(features), get_discounted_normalized_reward(rewards), torch.tensor(explored_vals)

# forward features through network, get unexplored vals
# input: features of type torch.tensor
#   features.shape = (adj_planet_num, FEATURE_NUM)
# outputs: vals of type torch.tensor
#   vals.shape = (adj_planet_num, VAL_NUM)
def forward(net, features):
  adj_planet_num, feature_num = features.shape
  features = features.reshape((-1, feature_num))
  vals_hat = net(features)
  return vals_hat.reshape((adj_planet_num, VAL_NUM))

loss = nn.MSELoss()

trainer = optim.Adam(net.parameters(), lr=LEARNING_RATE)

# 1 initialize network (skipped, use default initialization)

for epoch_index in range(EPOCH_NUM):
  # 2 write network parameters to haskell file
  update_model_param(net)

  # 3 run haskell program, ignore terminal information
  os.system(f"./Main clever-ai clever-ai --strategy1 ZergRush --strategy2 Skynet --seed 229367113-1 --no-recomp --headless >> tmp")

  # 4 get result 
  features, rewards, explore_vals = read_trial_result()

  acc_loss = 0

  # 5 train and update parameter
  # grads is a list of (group of gradients), each group of gradient corrispond to gradients calculated from different trial step
  #   example: [[gradients of net[0].weight, 
  #              gradients of net[0].bias,
  #              ... gradients from other layers], 
  #             ... gradient group from other training steps]
  grads = []
  for i, feature_batch in enumerate(features):
    # for each step, calculate loss and gradient, record the gradient, apply after getting all discounted rewards
    vals_hat = forward(net, feature_batch)
    l = loss(vals_hat, explore_vals[i])
    trainer.zero_grad()
    l.backward()

    # get gradients of different trial steps
    grad_group = []
    for param in net.parameters():
      grad_group.append(param.grad)
    grads.append(grad_group)

    acc_loss += l

  # get weighted mean of gradients
  acc_grad_group = [0] * len(grad_group[0])
  for step_num, grad_group in enumerate(grads):
    step_reward = rewards[step_num]
    for grad_index, grad in enumerate(grad_group):
      acc_grad_group[grad_index] += step_reward * grad

  # apply gradient back to net
  for grad_index, param in enumerate(net.parameters()):
    # print(acc_grad_group[grad_index])
    param.grad[:] = acc_grad_group[grad_index]

  # train use gradient setup in the last step
  trainer.step()
  print(f"epoch {epoch_index+1} acc loss: {acc_loss}")

print("=========== training finished ===========")
print("latest parameters saved in haskell file")