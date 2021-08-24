#include </home/kaviya/ros_ws/src/knowrob_k4r/src/db/k4r_db/Entities/DataController.cpp>


class SensorDataInput : DataController
{  
    private:
        static constexpr char* rest_link = (char*)"http://ked.informatik.uni-bremen.de:3678/getLastEvent";
    
    public:
        SensorDataInput();
        void get_sensor_data();
};

//

SensorDataInput::SensorDataInput()
{   
    
}


void SensorDataInput::get_sensor_data()
{
    Json::Value data = this->get_data(rest_link);
    std::cout << data << std::endl;
}
