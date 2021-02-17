FROM openease/knowrob:latest

USER root

# Initialize the catkin workspace
RUN cd /home/ros/src \
    && git clone https://github.com/K4R-IAI/knowrob_k4r.git \
    && git clone -b neems https://github.com/refills-project/knowrob_refills

RUN cd /home/ros/src/rosprolog \
    && git remote add sascha https://github.com/sasjonge/rosprolog.git \
    && git pull sascha master

WORKDIR /home/ros
RUN apt-get update && apt-get install -y \
    apt-utils \
    libcurlpp-dev \
    libjsoncpp-dev \
    && rosdep install --from-paths src --ignore-src -r -y \
    && rm -rf /var/lib/apt/lists/*

RUN pip install flask flask_restful

# Build the catkin workspace
RUN . /opt/ros/melodic/setup.sh && catkin_make

SHELL ["/bin/bash", "-c"]
COPY ./entrypoint.bash /entrypoint.bash
RUN chmod +x /entrypoint.bash