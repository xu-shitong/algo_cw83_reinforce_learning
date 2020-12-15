#!/bin/bash

for i in {1..1000}
do
    echo "The $i round starts to run!"
    ./Main clever-ai clever-ai --strategy1 Skynet --strategy2 ZergRush --headless --no-recomp >> against_zergrush.output
    ./Main clever-ai clever-ai --strategy1 Skynet --strategy2 PlanetRankRush --headless --no-recomp >> against_planetrank.output
done
