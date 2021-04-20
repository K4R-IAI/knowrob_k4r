#!/usr/bin/python

import pymongo
import subprocess
import sys
import os

KNOWROB_MONGODB_PKG_URI = os.getenv('KNOWROB_MONGODB_PKG_URI')
if KNOWROB_MONGODB_PKG_URI is None:
  sys.exit(1)
else:
  KNOWROB_MONGODB_PKG_URI = str(KNOWROB_MONGODB_PKG_URI)

client = pymongo.MongoClient(KNOWROB_MONGODB_PKG_URI)
db = client.roslog
collection_names = list(db.list_collection_names())

if u'tf' in collection_names and u'annotations' in collection_names and u'triples' in collection_names and u'inferred' in collection_names:
  sys.exit(0)
else:
  subprocess.call('/mongorestore.bash')
  sys.exit(0)