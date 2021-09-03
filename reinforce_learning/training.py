import torch 
from torch import nn

# define network used
net = nn.Sequential(
  nn.Linear(in_features=5, out_features=8), 
  nn.ReLU(inplace=True),
  nn.Linear(in_features=8, out_features=8),
  nn.ReLU(inplace=True),
  nn.Linear(in_features=8, out_features=2))

# define parameters 
hidden1_w = [[1,2,3,4,5,6], [1,2,3,4,5,6], [1,2,3,4,5,6], [1,2,3,4,5,6], [1,2,3,4,5,6], [1,2,3,4,5,6], [1,2,3,4,5,6], [1,2,3,4,5,6]]
hidden1_b = [1,2,3,4,5,6,7,8]
hidden2_w = [[1,2,3,4,5,6,7,8], [1,2,3,4,5,6,7,8], [1,2,3,4,5,6,7,8], [1,2,3,4,5,6,7,8], [1,2,3,4,5,6,7,8], [1,2,3,4,5,6,7,8], [1,2,3,4,5,6,7,8], [1,2,3,4,5,6,7,8]]
hidden2_b = [1,2,3,4,5,6,7,8]
output_w = [[1,2,3,4,5,6,7,8], [1,2,3,4,5,6,7,8]]
output_b = [1,2]

# update network parameters
def update_net_param(net, hidden1_w, hidden1_b, hidden2_w, hidden2_b, output_w, output_b):
  net[0].weight = nn.Parameter(torch.tensor(hidden1_w, dtype=torch.float))
  net[0].bias = nn.Parameter(torch.tensor(hidden1_b, dtype=torch.float))
  net[1].weight = nn.Parameter(torch.tensor(hidden2_w, dtype=torch.float))
  net[1].bias = nn.Parameter(torch.tensor(hidden2_b, dtype=torch.float))
  net[2].weight = nn.Parameter(torch.tensor(output_w, dtype=torch.float))
  net[2].bias = nn.Parameter(torch.tensor(output_b, dtype=torch.float))

X = torch.tensor(
  [[2.0, 55.0,  0.0, 98.0, 0.0, 0.0],
   [5.0, 98.0,  1.0, 0.0,  0.0, 0.0],
   [1.0, 120.0, 0.0, 0.0,  0.0, 0.0],
   [2.0, 55.0,  0.0, 98.0, 0.0, 0.0],
   [2.0, 142.0, 0.0, 0.0,  0.0, 0.0],
   [5.0, 123.0, 0.0, 0.0,  0.0, 0.0],
   [2.0, 111.0, 0.0, 0.0,  0.0, 0.0]])

print(net(X))