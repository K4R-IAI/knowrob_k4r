#!/bin/bash
source /home/ros/devel/setup.bash
mongorestore --uri=$KNOWROB_MONGODB_URI/roslog /home/data/triples/
mongorestore --uri=$KNOWROB_MONGODB_URI/roslog /home/data/ros_tf/
mongorestore --uri=$KNOWROB_MONGODB_URI/roslog /home/data/annotations/
mongorestore --uri=$KNOWROB_MONGODB_URI/roslog /home/data/inferred/

exec "$@"
