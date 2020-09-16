#!/bin/bash

printf "PING climate-data\n"
curl -s http://localhost:8000/climate-data/ping
printf "\n\n"

printf "PING topic-analyzer\n"
curl -s http://localhost:8001/topic-analyzer/ping
printf "\n\n"

printf "PIPE climate-data AND topic-analyzer\n"
curl -s http://localhost:8001/climate-data/topics-probs -d '{"dataname": "guardian", "k": 10}' | curl -s http://localhost:8001/topic-analyzer/similarity -d '@-'
printf "\n\n"



