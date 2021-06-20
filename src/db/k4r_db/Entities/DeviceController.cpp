#pragma once

#include "DataController.cpp"

class DeviceController : public DataController
{
public:
  DeviceController();

public:
  const Json::Value get_devices();

  const bool post_device(const Json::Value &, Json::Value &);
  const bool post_device(const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool delete_device(const std::string &);
};

DeviceController::DeviceController() : DataController::DataController("devices/")
{
}

const Json::Value DeviceController::get_devices()
{
  return this->get_data();
}

const bool DeviceController::post_device(const Json::Value &in_device, Json::Value &out_device)
{
  return this->post_data(in_device, out_device);
}

const bool DeviceController::post_device(const std::string &in_device_id, const std::string &in_store_id, const Json::Value &in_device, Json::Value &out_device)
{
  Json::Value in_device_tmp = in_device;
  in_device_tmp["id"] = in_device_id;
  in_device_tmp["storeId"] = in_store_id;
  return this->post_device(in_device_tmp, out_device);
}

const bool DeviceController::delete_device(const std::string &device_id)
{
  return this->delete_data(device_id);
}