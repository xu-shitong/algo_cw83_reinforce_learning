#!/bin/bash

for i in {1..1000}
do
    echo "The $i round starts to run!"
    ./Main clever-ai clever-ai --strategy1 Skynet --strategy2 ZergRush --headless --no-recomp >> against_zergrush.output
    ./Main clever-ai clever-ai --strategy1 Skynet --strategy2 PlanetRankRush --headless --no-recomp >> against_planetrank.output
done

echo "The stats for Skynet playing against ZergRush"
echo "Number of time we won: "
grep "Player 1" against_zergrush.output | wc -l
echo "Number of time they won: "
grep "Player 2" against_zergrush.output | wc -l
echo "Number of draws: "
grep "Draw" against_zergrush.output | wc -l

echo "The stats for Skynet playing against PlanetRankRush"
echo "Number of time we won: "
grep "Player 1" against_planetrank.output | wc -l
echo "Number of time they won: "
grep "Player 2" against_planetrank.output | wc -l
echo "Number of draws: "
grep "Draw" against_planetrank.output | wc -l
