import requests
import json
import sys
import os

# Set KnowRob version and KnowRob Port from environment variables
KNOWROB_VERSION = os.getenv('KNOWROB_VERSION')
if KNOWROB_VERSION is None:
    KNOWROB_VERSION = 'v1.0'
else:
    KNOWROB_VERSION = str(KNOWROB_VERSION)

KNOWROB_PORT = os.getenv('KNOWROB_PORT')
if KNOWROB_PORT is None:
    KNOWROB_PORT = 62226
else:
    KNOWROB_PORT = int(KNOWROB_PORT)

link = 'http://localhost:' + KNOWROB_PORT + '/knowrob/api/' + KNOWROB_VERSION + '/query'
data = {}
data['query'] = 'member(A, [2]), B is A+2'
data['maxSolutionCount'] = 10

try:
  print(link)
  resp = requests.post(link, data=json.dumps(data), headers={'Content-Type': 'application/json'})
  print(resp.text)
  if resp.status_code == 200:
    sys.exit(0)
  else:
    sys.exit(1)
except requests.exceptions.ConnectionError:
  sys.exit(2)