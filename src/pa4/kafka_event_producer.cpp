#include "knowrob_k4r/kafka_event_producer.h"

KafkaEventProducer::KafkaEventProducer(std::string &broker,
                                       std::string &topic) : broker_(broker),
                                                             topic_(topic),
                                                             partition_(RdKafka::Topic::PARTITION_UA),
                                                             offset_(RdKafka::Topic::OFFSET_BEGINNING),
                                                             conf_(RdKafka::Conf::create(RdKafka::Conf::CONF_GLOBAL))
{
}

KafkaEventProducer::~KafkaEventProducer() {}

RdKafka::Conf* KafkaEventProducer::set_producer_config(RdKafka::Conf *conf)
{   
    std::string errstr;

     if (conf->set("metadata.broker.list", this->broker_, errstr) != RdKafka::Conf::CONF_OK)
    {
        std::cerr << "Error in setting broker" << std::endl;
        exit(1);
    }

     if (conf->set("dr_cb", &this->deliverCallback_, errstr) != RdKafka::Conf::CONF_OK)
    {
        std::cerr << "Error in setting a message callback" << std::endl;
        exit(1);
    }

    return conf;
}


RdKafka::Producer* KafkaEventProducer::get_producer()
{
    RdKafka::Conf* config = set_producer_config(this->conf_);
    std::string errstr;

    RdKafka::Producer *producer = RdKafka::Producer::create(config, errstr);
    if (!producer)
    {
        std::cerr << "Failed to create producer: " << errstr << std::endl;
        exit(1);
    }
    else
    {
        std::cout << "Created producer " << producer->name() << std::endl;
    }
    return producer;
}

void KafkaEventProducer::send_data(const std::string event_data)
{
    RdKafka::Producer *producer = get_producer();
    retry:
        RdKafka::ErrorCode error_code = producer->produce(topic_,
                                               partition_,
                                               RdKafka::Producer::RK_MSG_COPY,
                                               const_cast<char* >(event_data.c_str()), event_data.size(),
                                               NULL, 0,
                                               0,
                                               NULL);

    if (error_code != RdKafka::ERR_NO_ERROR)
    {
        std::cerr << "Failed to produce to topic " << topic_ << ": " << RdKafka::err2str(error_code) << std::endl;
        if (error_code == RdKafka::ERR__QUEUE_FULL)
        {
            producer->poll(1000); /*block for max 1000ms*/
            goto retry;
        }
    }
    else
    {
        std::cout << "Enqueued message  " << "for topic " << topic_ << std::endl;
    }

    producer->poll(0);

    std::cout << "Flushing final messages" << std::endl;
    producer->flush(10 * 1000); /* wait for max 10 seconds */
    // outq_len -> Returns the current out queue length.
    if (producer->outq_len() > 0)
        std::cerr << "% " << producer->outq_len() << " messages were not delivered" << std::endl;
   
    delete producer;
}

DeliverCallback::DeliverCallback() {}

void DeliverCallback::dr_cb(RdKafka::Message &message)
{   
    if (message.err())
        std::cerr << "Message delivery failed: " << message.errstr() << std::endl;
    else
        std::cerr << "Message delivered to topic " << message.topic_name() << " [" << message.partition() << "] at offset " << message.offset() << std::endl;
    
}

DeliverCallback::~DeliverCallback() {}