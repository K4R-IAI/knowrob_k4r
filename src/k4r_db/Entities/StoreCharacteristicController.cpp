#pragma once

#include "DataController.cpp"

class StoreCharacteristicController : public DataController
{
public:
  StoreCharacteristicController();

public:
  const Json::Value get_store_characteristics();

  const bool post_store_characteristic(const std::string &, Json::Value &);

  const bool delete_store_characteristic(const std::string &);
};

StoreCharacteristicController::StoreCharacteristicController() : DataController::DataController("storecharacteristics/")
{
}

const Json::Value StoreCharacteristicController::get_store_characteristics()
{
  return this->get_data();
}

const bool StoreCharacteristicController::post_store_characteristic(const std::string &in_name, Json::Value &out_store_characteristic)
{
  Json::Value in_store_characteristic;
  in_store_characteristic["name"] = in_name;
  return this->post_data(in_store_characteristic, out_store_characteristic);
}

const bool StoreCharacteristicController::delete_store_characteristic(const std::string &store_characteristic_id)
{
  return this->delete_data(store_characteristic_id);
}