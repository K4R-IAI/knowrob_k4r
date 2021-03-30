FROM openease/knowrob:latest

ENV ROS_DISTRO=melodic
ENV ROS_PYTHON_VERSION=2

USER root

# Initialize the catkin workspace
RUN cd /home/ros/src/knowrob \
    && git remote add k4r https://github.com/K4R-IAI/knowrob \
    && git pull k4r v0.0

RUN cd /home/ros/src \
    && git clone -b master https://github.com/K4R-IAI/knowrob_k4r \
    && git clone -b v0.0 https://github.com/K4R-IAI/knowrob_refills

RUN cd /home/ros/src/rosprolog \
    && git remote add k4r https://github.com/K4R-IAI/rosprolog \
    && git pull k4r v0.0

RUN mkdir /home/data
COPY simulation_data/ /home/data/

WORKDIR /home/ros
RUN apt-get update && apt-get install -y \
    apt-utils \
    python3-pip \
    python3-yaml \
    libcurlpp-dev \
    libjsoncpp-dev \
    && rosdep install --from-paths src --ignore-src -r -y \
    && rm -rf /var/lib/apt/lists/*

RUN sudo -H pip3 install flask flask_restplus gevent rospkg

RUN sed -i 's/werkzeug import cached_property/werkzeug.utils import cached_property/' /usr/local/lib/python3.6/dist-packages/flask_restplus/fields.py \
 && sed -i 's/werkzeug import cached_property/werkzeug.utils import cached_property/' /usr/local/lib/python3.6/dist-packages/flask_restplus/api.py

# Build the catkin workspace
RUN . /opt/ros/${ROS_DISTRO}/setup.sh && catkin_make

SHELL ["/bin/bash", "-c"]
COPY ./entrypoint.bash /entrypoint.bash
RUN chmod +x /entrypoint.bash
USER ros
