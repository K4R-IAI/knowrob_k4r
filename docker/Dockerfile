FROM openease/knowrob:latest

USER root

# Initialize the catkin workspace
RUN cd /home/ros/src \
    && git clone https://github.com/K4R-IAI/knowrob_k4r.git \
    && git clone -b neems https://github.com/refills-project/knowrob_refills

RUN cd /home/ros/src/rosprolog \
    && git remote add giang https://github.com/HoangGiang93/rosprolog.git \
    && git pull giang master

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

RUN pip3 install flask flask_restplus gevent rospkg catkin_pkg

RUN sed -i 's/werkzeug import cached_property/werkzeug.utils import cached_property/' /usr/local/lib/python3.6/dist-packages/flask_restplus/fields.py \
 && sed -i 's/werkzeug import cached_property/werkzeug.utils import cached_property/' /usr/local/lib/python3.6/dist-packages/flask_restplus/api.py

# Build the catkin workspace
RUN . /opt/ros/melodic/setup.sh && catkin_make

SHELL ["/bin/bash", "-c"]
COPY ./entrypoint.bash /entrypoint.bash
RUN chmod +x /entrypoint.bash