import requests
import json
import sys

link = 'http://localhost:62226/knowrob/api/v1.0/query'
data = {}
data["query"] = "member(A, [2]), B is A+2"
data["maxSolutionCount"] = 10

try:
  resp = requests.post(link, data=json.dumps(data), headers={'Content-Type': 'application/json'})
  if resp.status_code == 200:
    sys.exit(0)
  else:
    sys.exit(1)
except requests.exceptions.ConnectionError:
  sys.exit(2)