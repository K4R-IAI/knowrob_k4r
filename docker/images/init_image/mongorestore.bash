#!/bin/bash
mongorestore --uri=$KNOWROB_MONGODB_PKG_URI /home/data/triples/
mongorestore --uri=$KNOWROB_MONGODB_PKG_URI /home/data/ros_tf/
mongorestore --uri=$KNOWROB_MONGODB_PKG_URI /home/data/annotations/
mongorestore --uri=$KNOWROB_MONGODB_PKG_URI /home/data/inferred/