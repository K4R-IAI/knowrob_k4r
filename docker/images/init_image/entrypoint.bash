#!/bin/bash
mongorestore --uri=mongodb://mongo:27017/roslog /home/data/triples/
mongorestore --uri=mongodb://mongo:27017/roslog /home/data/ros_tf/
mongorestore --uri=mongodb://mongo:27017/roslog /home/data/annotations/
mongorestore --uri=mongodb://mongo:27017/roslog /home/data/inferred/

exec "$@"