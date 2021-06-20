#pragma once

#include "DataController.cpp"

class ProductPropertyController : public DataController
{
public:
  ProductPropertyController();

public:
  const Json::Value get_product_properties(const std::string &, const std::string &);

  const bool post_product_property(const std::string &, const std::string &, const std::string &, const std::string &, Json::Value &);

  const bool delete_product_property(const std::string &, const std::string &, const std::string &);
};

ProductPropertyController::ProductPropertyController() : DataController::DataController("stores/")
{
}

const Json::Value ProductPropertyController::get_product_properties(const std::string &store_id, const std::string &product_id)
{
  return this->get_data(store_id + "/products/" + product_id + "/productproperties");
}

const bool ProductPropertyController::post_product_property(const std::string &in_store_id, const std::string &in_product_id, const std::string &in_product_characteristic_id, const std::string &in_value, Json::Value &out_product_property)
{
  Json::Value in_product_property;
  in_product_property["characteristicId"] = in_product_characteristic_id;
  in_product_property["productId"] = in_product_id;
  in_product_property["storeId"] = in_store_id;
  in_product_property["value"] = in_value;
  return this->post_data(in_product_property, out_product_property, in_store_id + "/products/" + in_product_id + "/productproperties/" + in_product_characteristic_id);
}

const bool ProductPropertyController::delete_product_property(const std::string &store_id, const std::string &product_id, const std::string &product_characteristic_id)
{
  return this->delete_data(store_id + "/products/" + product_id + "/productproperties/" + product_characteristic_id);
}