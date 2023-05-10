#ifndef __Kafka_Event_Producer__
#define __Kafka_Event_Producer__

#include <iostream>
// kafka
#include "librdkafka/rdkafkacpp.h"
class DeliverCallback : public RdKafka::DeliveryReportCb
{
public:
    DeliverCallback();
    ~DeliverCallback();

    void dr_cb(RdKafka::Message &message);
};

class KafkaEventProducer 
{
public:
	KafkaEventProducer(std::string& broker,
                       std::string& topic);
	~KafkaEventProducer();

    void send_data(std::string);

private:

RdKafka::Conf* set_producer_config(RdKafka::Conf *);
RdKafka::Producer* get_producer();

std::string broker_;
std::string topic_;
const int32_t partition_;
const int64_t offset_;
RdKafka::Conf *conf_;
DeliverCallback deliverCallback_;
};


#endif //__Kafka_Event_Producer__