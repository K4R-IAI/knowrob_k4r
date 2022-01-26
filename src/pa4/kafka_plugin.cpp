// SWI Prolog
#define PL_SAFE_ARG_MACROS
#include <SWI-cpp.h>

#include <jsoncpp/json/json.h>
#include "include/kafka_event_producer.h"
#include <iostream>
#include <ctime>

KafkaEventProducer get_producer_object()  
{   
    std::string broker_("localhost:9092");
    std::string topic_("test_kafka");
    // const KafkaEventProducer 
    return KafkaEventProducer(broker_, topic_);
}

const std::string convert_time(const std::string input_time)
{
    std::time_t in_time(std::stoll(input_time));
    std::tm current_time = *localtime(&in_time);
    char date[20];
    // Date time Format 
    strftime(date, sizeof(date), "%F %T", &current_time);
    std::cout << date << '\n';
    return date;
}

// publish_log_in(time, [CustomerId, StoreId])
PREDICATE(publish_log_in, 2)
{
    std::cout << "Hola holaa" << std::endl;
    Json::Value event_data;

    event_data["eventType"] = "LOG_IN";
    event_data["timestamp"] = convert_time((std::string)PL_A1);

    std::cout << event_data << std::endl;

    Json::Value data;
    PlTail data_list(PL_A2);
    PlTerm traversal_term;

    data_list.next(traversal_term);
    data["customerId"] = (int)traversal_term;
    data_list.next(traversal_term);
    data["storeId"] = (int)traversal_term;

    event_data["data"] = data;

    std::cout << event_data["data"] << std::endl;
    std::cout << event_data["timestamp"] << std::endl;
    std::cout << event_data["eventType"] << std::endl;

    get_producer_object().send_data(event_data.toStyledString());
    return true;
}

// publish_log_out(time, [CustomerId, StoreId])
PREDICATE(publish_log_out, 2)
{
    Json::Value event_data;

    event_data["eventType"] = "LOG_OUT";
    event_data["timestamp"] = convert_time((std::string)PL_A1);

    Json::Value data;
    PlTail data_list(PL_A2);
    PlTerm traversal_term;

    data_list.next(traversal_term);
    data["customerId"] = (int)traversal_term;
    data_list.next(traversal_term);
    data["storeId"] = (int)traversal_term;

    event_data["data"] = data;

    std::cout << event_data << std::endl;

    get_producer_object().send_data(event_data.toStyledString());
    return true;
}

// publish_pick_event(time, [CustomerId, StoreId, Product])
PREDICATE(publish_pick_event, 2)
{
    Json::Value event_data;

    event_data["eventType"] = "PRODUCT_REMOVED";
    event_data["timestamp"] = convert_time((std::string)PL_A1);

    Json::Value data;
    PlTail data_list(PL_A2);
    PlTerm traversal_term;

    data_list.next(traversal_term);
    data["customerId"] = (int)traversal_term;
    data_list.next(traversal_term);
    data["storeId"] = (int)traversal_term;
    data_list.next(traversal_term);
    data["product"] = (std::string)traversal_term;
    data_list.next(traversal_term);
    data["quantity"] = 1.0; // Not sure if this will stay as 1.0

    event_data["data"] = data;

    std::cout << event_data << std::endl;

    get_producer_object().send_data(event_data.toStyledString());
    return true;
}

// publish_return_event(time, [CustomerId, StoreId, Product])
PREDICATE(publish_put_back, 2)
{
    Json::Value event_data;

    event_data["eventType"] = "PRODUCT_RETURNED";
    event_data["timestamp"] = convert_time((std::string)PL_A1);

    Json::Value data;
    PlTail data_list(PL_A2);
    PlTerm traversal_term;

    data_list.next(traversal_term);
    data["customerId"] = (int)traversal_term;
    data_list.next(traversal_term);
    data["storeId"] = (int)traversal_term;
    data_list.next(traversal_term);
    data["product"] = (std::string)traversal_term;
    data_list.next(traversal_term);
    data["quantity"] = 1.0; // Not sure if this will stay as 1.0

    event_data["data"] = data;

    std::cout << event_data << std::endl;

    get_producer_object().send_data(event_data.toStyledString());
    return true;
}