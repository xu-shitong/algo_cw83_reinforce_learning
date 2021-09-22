# Thie program is based on reinforce learning with neural network policy, 
# training result: ai do no move, but delay for long time in order to get reward from having ships
# code no longer useable, if wish to reuse, need to change:
#   1. adapt network structure to fit the net used in Submission.hs file
#   2. result get from utils' read_trial_result function are list of tensors, need to adapt forward funciton for that
#   3. features from haskell file has changed, require adaptation
#   4. haskell file no longer use explore, if use neural net policy, need adaptation

import torch 
from torch import nn, optim
import ast
import os
import numpy
from utils import update_model_param, read_trial_result, get_discounted_reward

# define consts and hyperparameters
LEARNING_RATE = 1
EPOCH_NUM = 30
FEATURE_NUM = 5 # each planet takes 5 features
VAL_NUM = 2 # each planet generate 2 values after forwarded through network


# define network used
net = nn.Sequential(
  nn.Linear(in_features=6, out_features=8), 
  nn.ReLU(inplace=True),
  nn.Linear(in_features=8, out_features=2),
  nn.Sigmoid()
  )

# get discounted normalized reward
# input: list of reward read from log file
# output: tensor list of discounted normalized rewards
def get_discounted_normalized_reward(rewards):
  discounted_rewards = torch.tensor(get_discounted_reward(rewards))
  
  mean_reward = discounted_rewards.mean()
  std_reward = discounted_rewards.std()
  return (discounted_rewards - mean_reward) / std_reward

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
  discount_normal_reward = get_discounted_normalized_reward(rewards.numpy())

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

  # get weighted mean of gradients
  acc_grad_group = [0] * len(grad_group[0])
  for step_num, grad_group in enumerate(grads):
    step_reward = discount_normal_reward[step_num]
    for grad_index, grad in enumerate(grad_group):
      acc_grad_group[grad_index] += step_reward * grad

  # apply gradient back to net
  for grad_index, param in enumerate(net.parameters()):
    # print(acc_grad_group[grad_index])
    param.grad[:] = acc_grad_group[grad_index]

  # train use gradient setup in the last step
  trainer.step()
  print(f"epoch {epoch_index+1} turns: {len(features)} mean rewards: {numpy.mean(numpy.mean(rewards))}")

print("=========== training finished ===========")
print("latest parameters saved in haskell file")