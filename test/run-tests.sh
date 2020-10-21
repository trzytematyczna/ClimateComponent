#!/bin/bash

printf "PING climate-data\n"
curl -s http://localhost:8000/climate-data/ping
printf "\n\n"

printf "PING topic-analyzer\n"
curl -s http://localhost:8000/topic-analyzer/ping
printf "\n\n"

printf "CALL climate-data FOR topics\n"
curl -s http://localhost:8000/climate-data/topics -d '{}' | head -c1000
printf "\n\n"

printf "PIPE climate-data AND topic-analyzer FOR topic similarities\n"
curl -s http://localhost:8000/climate-data/topics -d '{}' | curl -s http://localhost:8000/topic-analyzer/similarity -d '@-' | head -c1000
printf "\n\n"

printf "PIPE climate-data AND topic-analyzer FOR topic specificities\n"
curl -s http://localhost:8000/climate-data/topics -d '{"topic":["G1","T2","G4","P8"]}' | curl -s http://localhost:8000/topic-analyzer/specificity -d '@-' | head -c1000
printf "\n\n"
