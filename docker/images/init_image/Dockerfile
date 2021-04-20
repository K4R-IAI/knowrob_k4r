FROM mongo:4.4.3-bionic

RUN apt-get update && apt-get install -y python-pymongo

RUN mkdir /home/data
COPY simulation_data/ /home/data/
WORKDIR /home/data

SHELL ["/bin/bash", "-c"]
COPY ./mongorestore.bash /mongorestore.bash
COPY ./init.py /init.py
RUN chmod +x /mongorestore.bash

CMD python /init.py