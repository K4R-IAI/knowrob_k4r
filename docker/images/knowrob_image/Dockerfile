FROM openease/knowrob:latest

ENV ROS_DISTRO=melodic
ENV ROS_PYTHON_VERSION=2

USER root

# Initialize the catkin workspace
RUN cd /home/ros/src/knowrob \
    && git remote add k4r https://github.com/K4R-IAI/knowrob \
    && git pull k4r v0.2 \
    && git checkout v0.2

RUN cd /home/ros/src \
    && git clone -b v0.3 https://github.com/K4R-IAI/knowrob_k4r \
    && git clone -b v0.2 https://github.com/K4R-IAI/knowrob_refills

RUN cd /home/ros/src/rosprolog \
    && git remote add k4r https://github.com/K4R-IAI/rosprolog \
    && git pull k4r v0.1 \
    && git checkout v0.1

COPY test.py /home/ros
RUN chmod +x test.py

RUN wget https://fastdl.mongodb.org/tools/db/mongodb-database-tools-amazon-x86_64-100.3.1.tgz \
    && tar -zxvf mongodb-database-tools-*-100.3.1.tgz

RUN apt-get update && apt-get install -y \
    apt-utils \
    net-tools \
    iputils-ping \
    python3-pip \
    python3-yaml \
    libcurlpp-dev \
    libjsoncpp-dev \
    && rosdep install --from-paths src --ignore-src -r -y \
    && rm -rf /var/lib/apt/lists/*

RUN sudo -H pip3 install flask flask_restplus gevent rospkg requests

RUN sed -i 's/werkzeug import cached_property/werkzeug.utils import cached_property/' /usr/local/lib/python*/dist-packages/flask_restplus/fields.py \
    && sed -i 's/werkzeug import cached_property/werkzeug.utils import cached_property/' /usr/local/lib/python*/dist-packages/flask_restplus/api.py

# Build the catkin workspace
WORKDIR /home/ros
RUN . /opt/ros/${ROS_DISTRO}/setup.sh && catkin_make

SHELL ["/bin/bash", "-c"]
COPY ./entrypoint.bash /entrypoint.bash
RUN chmod +x /entrypoint.bash

# USER ros

HEALTHCHECK --interval=10s --timeout=3s --start-period=60s \
    CMD python3 test.py
