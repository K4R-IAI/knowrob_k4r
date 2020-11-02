From openease/knowrob:latest
#FROM ubuntu:18.04
MAINTAINER gautam dange, gdange@uni-bremen.de

ARG HOST_IP=172.17.0.1
ARG ROS_SOURCES="http://packages.ros.org/ros/ubuntu"

ARG USE_APT_CACHE=0
ARG LOCAL_BUILD=0
USER root

# Use apt-cacher container
RUN if [ "x$USE_APT_CACHE" = "x0" ] ; then \
	echo "no apt-cacher is used." ; else \
	echo 'Acquire::http { Proxy "http://172.17.42.1:3142"; };' >> /etc/apt/apt.conf.d/01proxy \
	echo 'Acquire::http { Proxy "http://172.17.0.1:3142"; };' >> /etc/apt/apt.conf.d/02proxy ; \
	fi

RUN apt-get -qq update && \
    DEBIAN_FRONTEND=noninteractive apt-get -qq -y install wget apt-utils gnupg tzdata

# && apt-get upgrade -y

# Initialize the catkin workspace
WORKDIR /home/ros/src
RUN /usr/bin/python /opt/ros/melodic/bin/catkin_init_workspace

RUN if [ "x$LOCAL_BUILD" = "x0" ] ; then \
    rm -rf /home/ros/src/* && \
    git clone https://github.com/K4R-IAI/knowrob_k4r.git;\
    fi

WORKDIR /home/ros/src/knowrob_k4r
WORKDIR /home/ros
# Build the catkin workspace
RUN /opt/ros/melodic/bin/catkin_make

COPY knowrob_k4r.sh /knowrob_k4r.sh

ENTRYPOINT ["/knowrob_k4r.sh"]
