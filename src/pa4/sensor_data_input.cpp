#include <jsoncpp/json/json.h>

#define PL_SAFE_ARG_MACROS
#include <SWI-cpp.h>

#include "../db/k4r_db/Entities/DataController.cpp"


class SensorDataInput : DataController
{  
    private:
        static constexpr char* rest_link = (char*)"http://ked.informatik.uni-bremen.de:3678/getLastEvent";


    public:
        SensorDataInput();
        const Json::Value get_sensor_data();
        //void assert_event_data(Json::Value)
};


SensorDataInput::SensorDataInput() : DataController::DataController(rest_link, "")
{
}

/* void SensorDataInput::assert_event_data(Json::Value event_data)
{
    if(event_data["event_type"] == "object-in")

} */

const Json::Value SensorDataInput::get_sensor_data()
{
    Json::Value data = this->get_data();
    std::cout << data["event_type"] << std::endl;
    return data;
}

PREDICATE(get_put_data, 6)
{   
    SensorDataInput sensorDataInput;
    const Json::Value event_data =  sensorDataInput.get_sensor_data();
    if(event_data["event_type"] == "object-in")
    {
        PL_A1 = event_data["user_id"].toStyledString().c_str();
        PL_A2 = event_data["object_info"]["object_id"].toStyledString().c_str();
        PL_A3 = event_data["object_info"]["object_type"].toStyledString().c_str();
        PL_A4 = event_data["timestamp"].toStyledString().c_str();

        PlTail item_position(PL_A5);
        item_position.append(event_data["Shelf"].asFloat());
        item_position.append(event_data["ShelfLayer"].asFloat());
        item_position.append(event_data["object_info"]["facing"].asFloat());
        item_position.close();


        PlTail item_coordinates(PL_A6);
        item_coordinates.append(event_data["object_info"]["location"][0].asFloat());
        item_coordinates.append(event_data["object_info"]["location"][1].asFloat());
        item_coordinates.close();
    } 

    return true;
}