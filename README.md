# algo_cw83_reinforce_learning

This project is based on Imperial College London 2nd year Imperial-Conquest coursework. All code apart from reinforce_learning directory and clever-ai/Submission2.hs are provided by Imperial College London. 

Code in reinforce_learning directory and clever-ai/Submission2.hs file are code originated by Shitong Xu sx119, and Tom Zhao xz1919. 

## background introduction
Imperial-Conquest is a famous real-time computer game. The game can be generalized to a directed graph. Each vertex has certain amount of ships and can generate ships if is been occupied by one player. Each edge can transport ships to attack the target vertex, or help the target vertex to defence enemy attack. The game is played until one player has no ships and planets, or turns number exceed 1000.

In this project there are only 2 players, both are AI that have access to the whole graph's data, including enemy's ship number. In each step each AI decide which planet to move how many ships to which other planet. 

## usage
To see if file that takes reinfrocement learning parameters compile, run `ghci -iclever-ai clever-ai/Submission2.hs` in root directory.

To see 2 AI fighting each other, run `make server` followed by `./Main clever-ai clever-ai --strategy1 ZergRush --strategy2 Skynet {--seed 229367113-1} {--no-recomp} {--headless}` in root direcotry. The `Skynet` means the AI that use result of reinforcement learning. The `--seed` field is the seed of the graph, not using it means use a graph randomly generated. The `--headless` field indicates if you want to see dynamic ships moving in terminal, or just want to see final result of the fight. The `--no-recomp` indicate if you wish to recompile the `Submission.hs` file, it is recommended to include it in the first run and not in the later runs. 

## project structure
    /algo_cw83_reinforce_learning
    | 
    |__clever-ai          
    |  |
    |  |__Submission2.hs  (code of AI that play based on the result of reinforcement learning)
    |  |
    |  |__Train.hs        (first version of reinforcement learning by Tom Zhao)
    |
    |__reinforce_learning (directory contains reinforcement learning programs, in python)
    |
    |__(all other directories) (directories that provided by Imperial College London, for the proporse of supporting the game system)