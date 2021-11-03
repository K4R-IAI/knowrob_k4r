#pragma once

#include "DataController.cpp"

class ProductUnitController : public DataController
{
public:
  ProductUnitController();

public:
  const Json::Value get_product_unit(const std::string &);
  const Json::Value get_product_units();

  const bool post_product_unit(const std::string &, const Json::Value &, Json::Value &);

  const bool put_product_unit(const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool delete_product_unit(const std::string &);
};

ProductUnitController::ProductUnitController() : DataController::DataController("productunits/")
{
}

const Json::Value ProductUnitController::get_product_unit(const std::string &product_unit_id)
{
  return this->get_data(product_unit_id);
}

const Json::Value ProductUnitController::get_product_units()
{
  return this->get_data();
}

const bool ProductUnitController::post_product_unit(const std::string &in_product_id, const Json::Value &in_product_unit, Json::Value &out_product_unit)
{
  Json::Value in_product_unit_tmp = in_product_unit;
  in_product_unit_tmp["productId"] = in_product_id;
  return this->post_data(in_product_unit_tmp, out_product_unit);
}

const bool ProductUnitController::put_product_unit(const std::string &in_product_unit_id, const std::string &in_product_id, const Json::Value &in_product_unit, Json::Value &out_product_unit)
{
  Json::Value in_product_unit_tmp = in_product_unit;
  in_product_unit_tmp["productId"] = in_product_id;
  return this->put_data(in_product_unit_tmp, out_product_unit, in_product_unit_id);
}

const bool ProductUnitController::delete_product_unit(const std::string &product_unit_id)
{
  return this->delete_data(product_unit_id);
}