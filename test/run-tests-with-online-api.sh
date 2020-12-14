#!/bin/bash

printf "PING climate-data\n"
curl -s https://penelope.huma-num.fr/tools/climate-data/ping
printf "\n\n"

printf "PING topic-analyzer\n"
curl -s https://penelope.huma-num.fr/tools/topic-analyzer/ping
printf "\n\n"

printf "CALL climate-data FOR topics\n"
curl -s https://penelope.huma-num.fr/tools/climate-data/topics -d '{}' | head -c1000
printf "\n\n"

printf "PIPE climate-data AND topic-analyzer FOR topic similarities\n"
curl -s https://penelope.huma-num.fr/tools/climate-data/topics -d '{}' | curl -s https://penelope.huma-num.fr/tools/topic-analyzer/similarity -d '@-' | head -c1000
printf "\n\n"

printf "PIPE climate-data AND topic-analyzer FOR topic specificities\n"
curl -s https://penelope.huma-num.fr/tools/climate-data/topics -d '{"topic":["G1","T2","G4","P8"]}' | curl -s https://penelope.huma-num.fr/tools/topic-analyzer/specificity -d '@-' | head -c1000
printf "\n\n"

printf "CALL climate-data FOR timeline\n"
curl -s https://penelope.huma-num.fr/tools/climate-data/timeline -d '{"corpus":"twitter", "timescale":"month", "by_topic":"TRUE"}' | head -c1000
printf "\n\n"


printf "PIPE climate-data AND event-analyzer FOR event discovery\n"
curl -X POST -H "Content-type: application/json" -H "Accept: application/json" https://penelope.huma-num.fr/tools/climate-data/timeline -d '{"corpus":"twitter", "timescale":"month", "by_topic":"TRUE", "sample":"TRUE"}' | curl  -X POST -H "Content-type: application/json" -H "Accept: application/json" -d '@-' "https://penelope.huma-num.fr/tools/event-analyzer/events"
printf "\n\n"


printf "PIPE event-analyzer/texts\n"
curl -X POST "https://penelope.huma-num.fr/tools/event-analyzer/texts?ids=%7B%20%20%20%20%20%22timeline%22%3A%20%5B%20%20%20%20%20%20%20%7B%20%20%20%20%20%20%20%20%20%22corpus%22%3A%20%22guardian%22%2C%20%20%20%20%20%20%20%20%20%22date%22%3A%20%222016-01-25%22%2C%20%20%20%20%20%20%20%20%20%22topic%22%3A%20%22G1%22%2C%20%20%20%20%20%20%20%20%20%22doc_nb%22%3A%200.0972%2C%20%20%20%20%20%20%20%20%20%22word_nb%22%3A%20101.2313%2C%20%20%20%20%20%20%20%20%20%22doc_ids%22%3A%20%225cc17807a2c3615169989a39%22%20%20%20%20%20%20%20%7D%20%20%20%20%20%5D%20%7D%20%20&corpus=guardian" -H  "accept: */*" -d ""
printf "\n\n"
