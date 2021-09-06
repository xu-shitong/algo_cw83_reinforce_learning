from torch import optim
from utils import read_trial_result, update_model_param, get_discounted_reward
import torch
from torch import nn
import os
from numpy import random

# define hyperparameters
EPOCH_NUM = 3
# net number consts
GENERATION_NET_NUM = 7 # number of net in one generation
PARENT_NET_NUM = 4 # number of net chosen as parent nets
MUTATE_NET_NUM = 5 # number of child nets that mutated

# parameter mutate consts
MUTATE_PARAM_NUM = 2 # number of parameters mutate in one mutated net
MUTATE_RANGE = 7 # mutated value is allowed to change in [-MUTATE_RANGE, MUTATE_RANGE)
PARAM_UPPER_BOUND = 10 # maximim value weight and bias can take
PARAM_LOWER_BOUND = -10 # minimum value weight and bias can take

# network structure consts
LINEAR_NET_INDEX = [0] # index of neural net layer of type nn.Linear
def get_raw_net():
  return nn.Sequential(
            nn.Linear(in_features=6, out_features=2),
          )

# return one nets, in structure (Linear 6x8) -> relu -> (Linear 8x2) -> sigmoid
# weights initialized with uniform, bias initialized as 0 
def get_uniform_net():
  # init weight in a layer as uniform in (PARAM_UPPER_BOUND, PARAM_LOWER_BOUND)
  def uniform_weight_init(layer):
    if isinstance(layer, nn.Linear):
      nn.init.uniform_(layer.weight, PARAM_LOWER_BOUND, PARAM_UPPER_BOUND)
      layer.bias.data.fill_(0)

  net = get_raw_net()
  net.apply(uniform_weight_init)
  return net

# get distance between 2 nets, calculated as mean square difference in weight and bias
# two nets have to have the same structure
def net_distance(net1, net2):
  assert len(net1) == len(net2) # assert two nets have same layer num
  acc_loss = 0
  for i in LINEAR_NET_INDEX:
    net1_layer = net1[i]
    net2_layer = net2[i]
    acc_loss += ((net1_layer.weight.data - net2_layer.weight.data) **2).mean()
    acc_loss += ((net1_layer.bias.data - net2_layer.bias.data) **2).mean()
  return acc_loss

# randomly take values from two given tensors
def crossover_tensor(tensor1, tensor2):
  flatten_tensor1 = tensor1.reshape((-1,))
  flatten_tensor2 = tensor2.reshape((-1,))

  result = flatten_tensor1.detach().clone()
  for i in range(flatten_tensor1.numel()):
    # for each element in result, 0.5 chance of change to tensor2's result
    if random.randint(0, 1) == 0:
      result[i] = flatten_tensor2[i]

  # print(tensor1, tensor2, result)
  return result.reshape((tensor1.shape))

# generate one net that randomly take each prameter from two given nets
def crossover(net1, net2):
  child_net = get_raw_net()
  for i in range(len(net1)):
    net1_layer = net1[i]
    net2_layer = net2[i]
    child_layer = child_net[i]
    # if layer is a linear layer, crossover its parameters, 
    # otherwise, layer is an activation layer, leave unchaged 
    if isinstance(net1_layer, nn.Linear):
      # print("corssover: ", net1_layer.weight.data, net2_layer.weight.data)
      child_layer.weight = nn.Parameter(crossover_tensor(net1_layer.weight.data, net2_layer.weight.data))
      child_layer.bias = nn.Parameter(crossover_tensor(net1_layer.bias.data, net2_layer.bias.data))

  return child_net

# clamp value in range [low, high]
def clamp(v, low, high):
  return max(low, min(v, high))

# get mutated net from the given net
# for code convinience, change one bias and one weight parameter
def mutate(net):
  for i in range(MUTATE_PARAM_NUM):
    layer_num = LINEAR_NET_INDEX[random.randint(0, len(LINEAR_NET_INDEX))] # which net layer get mutated
    layer = net[layer_num]
    
    # select one weight and one bias index to be mutated
    weight_index = random.randint(0, layer.weight.data.numel())
    bias_index = random.randint(0, layer.bias.data.numel())
    
    # mutate weight
    weight = layer.weight.data
    mutated_weight = weight.reshape((-1, ))
    mutated_weight[weight_index] += random.uniform(low=-MUTATE_RANGE, high=MUTATE_RANGE)
    # cast mutated value in permitted range
    mutated_weight[weight_index] = clamp(mutated_weight[weight_index], PARAM_LOWER_BOUND, PARAM_UPPER_BOUND)
    
    # mutate bias
    bias = layer.bias.data
    bias[bias_index] += random.uniform(low=-MUTATE_RANGE, high=MUTATE_RANGE)
    # cast mutated value in permitted range
    bias[bias_index] = clamp(bias[bias_index], PARAM_LOWER_BOUND, PARAM_UPPER_BOUND)
    
    # write parameter back
    net[layer_num].weight = nn.Parameter(mutated_weight.reshape((weight.shape)))
    net[layer_num].bias = nn.Parameter(bias)
  return net


# 1.generate initial generation
#   dict initialised as 1 since it times reward in selection step
nets = [ {"net": get_uniform_net(), "reward" : 0, "dist": 1} for i in range(GENERATION_NET_NUM)]

for epoch_num in range(EPOCH_NUM):

  # 2.forward through network, get rewards
  for i, net_dict in enumerate(nets):
    update_model_param(net=net_dict["net"])
    os.system(f"./Main clever-ai clever-ai --strategy1 ZergRush --strategy2 Skynet --seed 229367113-1 --no-recomp --headless >> tmp")
    _, rewards, _= read_trial_result()
    # rewards = torch.tensor(get_discounted_reward(rewards))
    rewards = torch.tensor(rewards)
    net_dict["reward"] = rewards.max().exp() + 0.01 * rewards.numel()
    print(f"epoch {epoch_num} dict {i} reward: {rewards.max().exp() + 0.1 * rewards.numel()}")

  # 3.select parameters
  #   select according to getting net that maximise product of rewards and diversity
  parent_nets = []
  for parent_num in range(1, PARENT_NET_NUM+1):
    # take dict with maximum reward * dist
    optimum_net = max(nets, key=(lambda net_dict: net_dict["reward"] * net_dict["dist"] / parent_num)) 
    print(f"taking parameter with rating {optimum_net['dist'] / parent_num}")
    # pop the selected net
    nets.remove(optimum_net)

    # insert the net in parent net list
    parent_nets.append(optimum_net)

    # update all the remaining nets' distance
    for net_dict in nets:
      net_dict["dist"] += net_distance(net_dict["net"], optimum_net["net"])

  # for net_dict in parent_nets:
  #   print(net_dict["net"][0].weight.data)   
  
  # 4.generate one generation of child parameter exchanged nets
  nets.clear()
  for child_index in range(GENERATION_NET_NUM):
    net1_index = net2_index = 0
    # prevent the same parent produce child
    while net1_index == net2_index:
      net1_index, net2_index = random.randint(0, PARENT_NET_NUM), random.randint(0, PARENT_NET_NUM)
    # print(f"parent1 = {net1_index}, parent2 = {net2_index}")
    # print(f"layer 1 = {parent_nets[net1_index]['net'][0].weight.data}, layer2 = {parent_nets[net2_index]['net'][0].weight.data}")
    child_net = crossover(parent_nets[net1_index]["net"], parent_nets[net2_index]["net"])
    nets.append({"net": child_net, "reward": 0, "dist": 1})

  # for net_dict in nets:
  #   print(net_dict["net"][0].weight.data)   

  # 5.generate mutated childs
  for i in range(MUTATE_NET_NUM):
    mutate_index = random.randint(0, GENERATION_NET_NUM)
    mutated_child_dict = nets[mutate_index]
    mutated_child_dict["net"] = mutate(mutated_child_dict["net"])

    

