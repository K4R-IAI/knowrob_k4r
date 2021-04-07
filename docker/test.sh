#!/usr/bin/env bash

SERVICE=$(rosservice list | grep query)
REST=$(curl --request POST   --url http://localhost:62226/knowrob/api/v1.0/query   --header 'content-type: application/json'   --data '{ "query": "member(A, [2]), B is A+2" , "maxSolutionCount": 100}')

[ -z "${SERVICE}" ] && [ -z "${REST}" ] && echo false
