FROM mongo:4.4.3-bionic

RUN mkdir /home/data
COPY simulation_data/ /home/data/
WORKDIR /home/data

SHELL ["/bin/bash", "-c"]
COPY ./entrypoint.bash /entrypoint.bash
RUN chmod +x /entrypoint.bash
