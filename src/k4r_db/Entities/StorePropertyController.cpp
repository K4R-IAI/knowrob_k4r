#pragma once

#include "DataController.cpp"

class StorePropertyController : public DataController
{
public:
  StorePropertyController();

public:
  const Json::Value get_store_properties(const std::string &);

  const bool post_store_property(const std::string &, const std::string &, const std::string &, const std::string &, Json::Value &);

  const bool delete_store_property(const std::string &, const std::string &);
};

StorePropertyController::StorePropertyController() : DataController::DataController("stores/")
{
}

const Json::Value StorePropertyController::get_store_properties(const std::string &store_id)
{
  return this->get_data(store_id + "/storeproperties");
}

const bool StorePropertyController::post_store_property(const std::string &in_store_id, const std::string &in_store_characteristic_id, const std::string &in_value_low, const std::string &in_value_high, Json::Value &out_store_property)
{
  Json::Value in_store_property;
  in_store_property["characteristicId"] = in_store_characteristic_id;
  in_store_property["storeId"] = in_store_id;
  in_store_property["valueLow"] = in_value_low;
  in_store_property["valueHigh"] = in_value_high;
  return this->post_data(in_store_property, out_store_property, in_store_id + "/storeproperties/" + in_store_characteristic_id);
}

const bool StorePropertyController::delete_store_property(const std::string &store_id, const std::string &store_characteristic_id)
{
  return this->delete_data(store_id + "/storeproperties/" + store_characteristic_id);
}