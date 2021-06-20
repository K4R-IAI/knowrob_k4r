#pragma once

#include "ProductController.cpp"
#include "StoreController.cpp"

class ProductGroupController : public DataController
{
public:
  ProductGroupController();

public:
  const Json::Value get_product_group(const std::string &);
  const Json::Value get_product_groups(const std::string &);

  const bool post_product_to_product_group(const std::string &, const std::string &, Json::Value &);
  const bool post_product_group(const std::string &, const Json::Value &, Json::Value &);

  const bool delete_product_from_product_group(const std::string &, const std::string &);
  const bool delete_product_group(const std::string &);
};

ProductGroupController::ProductGroupController() : DataController::DataController()
{
}

const Json::Value ProductGroupController::get_product_group(const std::string &product_group_id)
{
  return this->get_data("productgroups/" + product_group_id);
}

const Json::Value ProductGroupController::get_product_groups(const std::string &store_id)
{
  return this->get_data("stores/" + store_id + "/productgroups");
}

const bool ProductGroupController::post_product_to_product_group(const std::string &in_product_group_id, const std::string &in_product_id, Json::Value &out_product_group)
{
  return this->post_data(Json::Value(), out_product_group, "productgroups/" + in_product_group_id + "/products/" + in_product_id);
}

const bool ProductGroupController::post_product_group(const std::string &in_store_id, const Json::Value &in_product_group, Json::Value &out_product_group)
{
  return this->post_data(in_product_group, out_product_group, "stores/" + in_store_id + "/productgroups");
}

const bool ProductGroupController::delete_product_group(const std::string &product_group_id)
{
  return this->delete_data("productgroups/" + product_group_id);
}

const bool ProductGroupController::delete_product_from_product_group(const std::string &product_group_id, const std::string &product_id)
{
  return this->delete_data("productgroups/" + product_group_id + "/products/" + product_id);
}