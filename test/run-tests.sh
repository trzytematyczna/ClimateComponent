#!/bin/bash

printf "PING climate-data\n"
curl -s http://localhost:8000/climate-data/ping
printf "\n\n"

printf "PING topic-analyzer\n"
curl -s http://localhost:8000/topic-analyzer/ping
printf "\n\n"

printf "CALL cliamte-data FOR topics-probs\n"
curl -s http://localhost:8000/climate-data/topics-probs -d '{"dataname": "guardian", "k": 10}' | head -c1000
printf "\n\n"

printf "PIPE climate-data AND topic-analyzer\n"
curl -s http://localhost:8000/climate-data/topics-probs -d '{"dataname": "guardian", "k": 10}' | curl -s http://localhost:8000/topic-analyzer/similarity -d '@-'
printf "\n\n"
