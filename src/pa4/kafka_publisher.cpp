// To compile : g++ -std=c++11 kafka_event_producer.cpp -o kaf -lrdkafka++ -lrdkafka

#include <iostream>
#include "librdkafka/rdkafkacpp.h"


class ExampleDeliveryReportCb : public RdKafka::DeliveryReportCb
{
public:
    void dr_cb(RdKafka::Message &message)
    {
        /* If message.err() is non-zero the message delivery failed permanently
     * for the message. */
        if (message.err())
            std::cerr << "% Message delivery failed: " << message.errstr() << std::endl;
        else
            std::cerr << "% Message delivered to topic " << message.topic_name() << " [" << message.partition() << "] at offset " << message.offset() << std::endl;
    }
};

int main()
{

    ExampleDeliveryReportCb ex_dr_cb;

    std::string brokers = "localhost:9092";
    std::string errstr;
    std::string topic = "test_kafka";
    int32_t partition = RdKafka::Topic::PARTITION_UA;
    int64_t start_offset = RdKafka::Topic::OFFSET_BEGINNING;
    

    RdKafka::Conf *conf = RdKafka::Conf::create(RdKafka::Conf::CONF_GLOBAL);
    // RdKafka::Conf *tconf = RdKafka::Conf::create(RdKafka::Conf::CONF_TOPIC);

    conf->set("metadata.broker.list", brokers, errstr);

    if (conf->set("dr_cb", &ex_dr_cb, errstr) != RdKafka::Conf::CONF_OK)
    {
        std::cerr << errstr << std::endl;
        exit(1);
    }
    RdKafka::Producer *producer = RdKafka::Producer::create(conf, errstr);
    if (!producer)
    {
        std::cerr << "Failed to create producer: " << errstr << std::endl;
        exit(1);
    }

    std::cout << "% Created producer " << producer->name() << std::endl;
    std::string text = "hello_world makkale";

retry:
    RdKafka::ErrorCode err = producer->produce(topic,
                                               partition,
                                               RdKafka::Producer::RK_MSG_COPY,
                                               const_cast<char *>(text.c_str()), text.size(),
                                               NULL, 0,
                                               0,
                                               NULL);

    if (err != RdKafka::ERR_NO_ERROR)
    {
        std::cerr << "% Failed to produce to topic " << topic << ": " << RdKafka::err2str(err) << std::endl;

        if (err == RdKafka::ERR__QUEUE_FULL)
        {
            /* If the internal queue is full, wait for
         * messages to be delivered and then retry.
         * The internal queue represents both
         * messages to be sent and messages that have
         * been sent or failed, awaiting their
         * delivery report callback to be called.
         *
         * The internal queue is limited by the
         * configuration property
         * queue.buffering.max.messages */
            producer->poll(1000 /*block for max 1000ms*/);
            goto retry;
        }
    }
    else
    {
        std::cerr << "Enqueued message  "
                  << "for topic " << topic << std::endl;
    }

    producer->poll(0);

    /* Wait for final messages to be delivered or fail.
   * flush() is an abstraction over poll() which
   * waits for all messages to be delivered. */
    std::cerr << "% Flushing final messages..." << std::endl;
    producer->flush(10 * 1000 /* wait for max 10 seconds */);

    if (producer->outq_len() > 0)
        std::cerr << "% " << producer->outq_len() << " message(s) were not delivered" << std::endl;

    delete producer;

    return 0;
}
