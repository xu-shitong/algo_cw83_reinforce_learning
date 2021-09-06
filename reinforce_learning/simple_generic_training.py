from utils import read_trial_result, update_model_param
from torch import nn
import os
from numpy import random

# define hyperparameters
EPOCH_NUM = 10
GENERATION_SIZE = 20 # number of net in one generation
PARENT_SIZE = 10 # number of net chosen as parent nets
MUTATE_SIZE = 5 # number of child nets that mutated
MUTATE_PARAM_NUM = 2 # number of parameters mutate in one mutated net
PARAM_UPPER_BOUND = 10 # largest value weight and bias can take
PARAM_LOWER_BOUND = -10 # largest value weight and bias can take

# return one nets, in structure (Linear 6x8) -> relu -> (Linear 8x2) -> sigmoid
# weights initialized with uniform, bias initialized as 0 
def get_uniform_net():
  # init weight in a layer as uniform in (PARAM_UPPER_BOUND, PARAM_LOWER_BOUND)
  def uniform_weight_init(layer):
    if isinstance(layer, nn.Linear):
      nn.init.uniform_(layer.weight, PARAM_LOWER_BOUND, PARAM_UPPER_BOUND)
      layer.bias.data.fill_(0)

  net = nn.Sequential(
    nn.Linear(in_features=6, out_features=8), 
    nn.ReLU(inplace=True),
    nn.Linear(in_features=8, out_features=2),
    nn.Sigmoid()
  )
  net.apply(uniform_weight_init)
  return net

# get distance between 2 nets, calculated as mean square difference in weight and bias
# two nets have to have the same structure
def net_distance(net1, net2):
  assert len(net1) == len(net2) # assert two nets have same layer num
  acc_loss = 0
  for i in range(len(net1)):
    net1_layer = net1[i]
    net2_layer = net2[i]
    if isinstance(net1_layer):
      assert isinstance(net2_layer) # assert two nets' layer have same type
      acc_loss += ((net1_layer.weight.data - net2_layer.weight.data) **2).mean()
      acc_loss += ((net1_layer.bias.data - net2_layer.bias.data) **2).mean()
  return acc_loss

# randomly take values from two given tensors
def crossover_tensor(tensor1, tensor2):
  flatten_tensor1 = tensor1.reshape((-1,))
  flatten_tensor2 = tensor2.reshape((-1,))

  result = flatten_tensor1
  for i in range(flatten_tensor1.numel()):
    # for each element in result, 0.5 chance of change to tensor2's result
    if random.rand(0, 1) == 0:
      result[i] = flatten_tensor2[i]

  return result.reshape((tensor1.shape))

# generate one net that randomly take each prameter from two given nets
def crossover(net1, net2):
  child_net = nn.Sequential()
  for i in range(len(net1)):
    net1_layer = net1[i]
    net2_layer = net2[i]
    child_layer = net1_layer
    # if layer is a linear layer, crossover its parameters, 
    # otherwise, layer is an activation layer, leave unchaged 
    if isinstance(net1_layer, nn.Linear):
      child_layer.weight = nn.Parameter(crossover_tensor(net1_layer.weight.data, net2_layer.weight.data))
      child_layer.bias = nn.Parameter(crossover_tensor(net1_layer.bias.data, net2_layer.bias.data))

    child_net.add_module(name=f"layer {i}", module=child_layer)
  return 

# get mutated net from the given net
def mutate(net):
  
  return


# 1.generate initial generation
#   dict initialised as 1 since it times reward in selection step
nets = [ {"id": i, "net": get_uniform_net(), "reward" : 0, "dist": 1} for i in range(GENERATION_SIZE)]

for epoch_num in range(EPOCH_NUM):

  # 2.forward through network, get rewards
  for net_dict in nets:
    update_model_param(net=net_dict["net"])
    os.system(f"./Main clever-ai clever-ai --strategy1 ZergRush --strategy2 Skynet --seed 229367113-1 --no-recomp --headless >> tmp")
    _, rewards, _= read_trial_result()
    net_dict["reward"] = rewards.mean()

  # 3.select parameters
  #   select according to getting net that maximise product of rewards and diversity
  parent_nets = []
  for parent_num in range(1, PARENT_SIZE+1):
    # take dict with maximum reward * dist
    optimum_net = max(nets, key=(lambda net_dict: net_dict["reward"] * net_dict["dist"] / parent_num)) 
    
    # pop the selected net
    nets.pop(optimum_net["id"])

    # insert the net in parent net list
    parent_nets.append(optimum_net)

    # update all the remaining nets' distance
    for net_dict in nets:
      net_dict["dist"] += net_distance(net_dict["net"], optimum_net)

  
  # 4.generate one generation of child parameter exchanged nets
  nets.clear()
  for child_index in range(GENERATION_SIZE):
    net1_index, net2_index = random.randint(0, PARENT_SIZE - 1), random.randint(0, PARENT_SIZE - 1)
    child_net = crossover(parent_nets[net1_index]["net"], parent_nets[net2_index]["net"])
    nets.append({"id": child_index, "net": child_net, "reward": 0, "dist": 1})
    
  # 5.generate mutated childs
  for i in range(MUTATE_SIZE):
    mutate_index = random.randint(0, GENERATION_SIZE - 1)
    mutated_child_dict = nets[mutate_index]
    mutate(mutated_child_dict["net"])

    

