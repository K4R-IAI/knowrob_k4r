#pragma once

#include "DataController.cpp"

class ProductCharacteristicController : public DataController
{
public:
  ProductCharacteristicController();

public:
  const Json::Value get_product_characteristics();

  const bool post_product_characteristic(const std::string &, const std::string &, Json::Value &);

  const bool delete_product_characteristic(const std::string &);
};

ProductCharacteristicController::ProductCharacteristicController() : DataController::DataController("productcharacteristics/")
{
}

const Json::Value ProductCharacteristicController::get_product_characteristics()
{
  return this->get_data();
}

const bool ProductCharacteristicController::post_product_characteristic(const std::string &in_code, const std::string &in_name, Json::Value &out_product_characteristic)
{
  Json::Value in_product_characteristic;
  in_product_characteristic["code"] = in_code;
  in_product_characteristic["name"] = in_name;
  return this->post_data(in_product_characteristic, out_product_characteristic);
}

const bool ProductCharacteristicController::delete_product_characteristic(const std::string &product_characteristic_id)
{
  return this->delete_data(product_characteristic_id);
}