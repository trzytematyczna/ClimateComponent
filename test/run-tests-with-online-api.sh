#!/bin/bash

printf "PING climate-data\n"
curl -s https://penelope.huma-num.fr/tools/climate-data/ping
printf "\n\n"

printf "PING topic-analyzer\n"
curl -s https://penelope.huma-num.fr/tools/topic-analyzer/ping
printf "\n\n"

printf "CALL climate-data FOR topics\n"
curl -s https://penelope.huma-num.fr/tools/climate-data/topics -d '{}' \
    | head -c1000
printf "\n\n"

printf "PIPE climate-data AND topic-analyzer FOR topic similarities\n"
curl -H "Content-Type: application/json" -s https://penelope.huma-num.fr/tools/climate-data/topics -d '{}' \
    | curl  -H "Content-Type: application/json" -s https://penelope.huma-num.fr/tools/topic-analyzer/similarity -d '@-' \
    | head -c1000
printf "\n\n"

printf "PIPE climate-data AND topic-analyzer FOR topic specificities\n"
curl  -H "Content-Type: application/json" -s https://penelope.huma-num.fr/tools/climate-data/topics -d '{"topic":["G1","T2","G4","P8"]}' \
    | curl  -H "Content-Type: application/json" -s https://penelope.huma-num.fr/tools/topic-analyzer/specificity -d '@-' \
    | head -c1000
printf "\n\n"

printf "CALL climate-data FOR timeline\n"
curl  -H "Content-Type: application/json" -s https://penelope.huma-num.fr/tools/climate-data/timeline -d '{"corpus":"twitter", "timescale":"month", "by_topic":"TRUE"}' \
    | head -c1000
printf "\n\n"

printf "PIPE climate-data AND event-analyzer FOR event discovery\n"
curl  -H "Content-Type: application/json" -s https://penelope.huma-num.fr/tools/climate-data/timeline -d '{"corpus":"twitter", "timescale":"month", "by_topic":"TRUE", "sample":"TRUE"}' \
    | curl  -H "Content-Type: application/json" -s https://penelope.huma-num.fr/tools/event-analyzer/events -d '@-' \
    | head -c1000
printf "\n\n"

printf "CALL climate-data FOR network\n"
curl  -H "Content-Type: application/json" -s https://penelope.huma-num.fr/tools/climate-data/network -d '{}' \
    | head -c1000
printf "\n\n"
