#!/bin/bash

printf "PING climate-data\n"
curl -X GET http://127.0.0.1:8001/climate-data/ping
printf "\n\n"

printf "PING topic-analyzer\n"
curl -X GET http://127.0.0.1:8001/topic-analyzer/ping
printf "\n\n"

printf "Topic-Probs climate-data\n"
curl -X GET "http://127.0.0.1:8001/climate-data/topics-probs?DataName=guardian&K=10&JustWordsDF=true" -H  "accept: */*" -d ""

printf "PIPE climate-data with threshold for filtering AND topic-analyzer\n"
curl -X GET "http://127.0.0.1:8001/climate-data/topics-probs?DataName=guardian&K=10&JustWordsDF=true" -H  "accept: */*" -d "" |
curl -X GET http://127.0.0.1:8001/topic-analyzer/similarity -d '@-'

printf "\n\n"

curl -X POST "http://127.0.0.1:8001/climate-data/topics-probs?DataName=guardian&K=10&JustWordsDF=true" -H  "accept: */*" -d "" |
curl -d @data.json http://127.0.0.1:8001/topic-analyzer/similarity




