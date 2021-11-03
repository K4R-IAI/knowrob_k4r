#pragma once

#include "DataController.cpp"

class ProductGtinController : public DataController
{
public:
  ProductGtinController();

public:
  const Json::Value get_product_gtin(const std::string &);
  const Json::Value get_product_gtins();

  const bool post_product_gtin(const std::string &, const Json::Value &, Json::Value &);

  const bool put_product_gtin(const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool delete_product_gtin(const std::string &);
};

ProductGtinController::ProductGtinController() : DataController::DataController("productgtins/")
{
}

const Json::Value ProductGtinController::get_product_gtin(const std::string &product_gtin_id)
{
  return this->get_data(product_gtin_id);
}

const Json::Value ProductGtinController::get_product_gtins()
{
  return this->get_data();
}

const bool ProductGtinController::post_product_gtin(const std::string &in_product_unit_id, const Json::Value &in_product_gtin, Json::Value &out_product_gtin)
{
  Json::Value product_gtin_tmp = in_product_gtin;
  product_gtin_tmp["productUnitId"] = in_product_unit_id;
  return this->post_data(product_gtin_tmp, out_product_gtin);
}

const bool ProductGtinController::put_product_gtin(const std::string &in_product_gtin_id, const std::string &in_product_unit_id, const Json::Value &in_product_gtin, Json::Value &out_product_gtin)
{
  Json::Value product_gtin_tmp = in_product_gtin;
  product_gtin_tmp["productUnitId"] = in_product_unit_id;
  return this->put_data(product_gtin_tmp, out_product_gtin, in_product_gtin_id);
}

const bool ProductGtinController::delete_product_gtin(const std::string &product_gtin_id)
{
  return this->delete_data(product_gtin_id);
}