#!/bin/bash

for i in {1..1000}
do
    echo "The $i round starts to run!"
    ./Main clever-ai clever-ai --strategy1 PlanetRankRush --strategy2 ZergRush --headless >> stats.output
done
