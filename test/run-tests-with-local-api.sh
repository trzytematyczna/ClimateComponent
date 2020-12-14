#!/bin/bash

printf "PING climate-data\n"
curl -s http://localhost:8000/climate-data/ping
printf "\n\n"

printf "PING topic-analyzer\n"
curl -s http://localhost:8000/topic-analyzer/ping
printf "\n\n"

printf "CALL climate-data FOR topics\n"
curl -H "Content-Type: application/json" -s http://localhost:8000/climate-data/topics -d '{"sample":"TRUE"}' | head -c1000
printf "\n\n"

printf "PIPE climate-data AND topic-analyzer FOR topic similarities\n"
curl -H "Content-Type: application/json" -s http://localhost:8000/climate-data/topics -d '{"sample":"TRUE"}' | curl -s http://localhost:8000/topic-analyzer/similarity -d '@-' | head -c1000
printf "\n\n"

printf "PIPE climate-data AND topic-analyzer FOR topic specificities\n"
curl -H "Content-Type: application/json" -s http://localhost:8000/climate-data/topics -d '{"topic":["G1","T2","G4","P8"], "sample":"TRUE"}' | curl -H "Content-Type: application/json" -s http://localhost:8000/topic-analyzer/specificity -d '@-' | head -c1000
printf "\n\n"

printf "CALL climate-data FOR timeline\n"
curl -H "Content-Type: application/json" -s http://localhost:8000/climate-data/timeline -d '{"corpus":"twitter", "timescale":"month", "by_topic":"TRUE", "sample":"TRUE"}' | head -c1000
printf "\n\n"

printf "PIPE climate-data AND event-analyzer FOR event discovery\n"
curl -H "Content-Type: application/json" -s http://localhost:8000/climate-data/timeline -d '{"corpus":"twitter", "timescale":"month", "by_topic":"TRUE", "sample":"TRUE"}' | curl -H "Content-Type: application/json" -s http://localhost:8000/event-analyzer/events -d '@-' | head -c1000
printf "\n\n"
