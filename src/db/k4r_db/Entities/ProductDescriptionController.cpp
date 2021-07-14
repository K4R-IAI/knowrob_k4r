#pragma once

#include "DataController.cpp"

class ProductDescriptionController : public DataController
{
public:
  ProductDescriptionController();

public:
  const Json::Value get_product_description(const std::string &);
  const Json::Value get_product_descriptions();

  const bool post_product_description(const std::string &, const Json::Value &, Json::Value &);

  const bool put_product_description(const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool delete_product_description(const std::string &);
};

ProductDescriptionController::ProductDescriptionController() : DataController::DataController("productdescriptions/")
{
}

const Json::Value ProductDescriptionController::get_product_description(const std::string &product_description_id)
{
  return this->get_data(product_description_id);
}

const Json::Value ProductDescriptionController::get_product_descriptions()
{
  return this->get_data();
}

const bool ProductDescriptionController::post_product_description(const std::string &in_product_id, const Json::Value &in_product_description, Json::Value &out_product_description)
{
  Json::Value product_description_tmp = in_product_description;
  product_description_tmp["productId"] = in_product_id;
  return this->post_data(product_description_tmp, out_product_description);
}

const bool ProductDescriptionController::put_product_description(const std::string &in_product_description_id, const std::string &in_product_id, const Json::Value &in_product_description, Json::Value &out_product_description)
{
  Json::Value product_description_tmp = in_product_description;
  product_description_tmp["productId"] = in_product_id;
  return this->put_data(product_description_tmp, out_product_description, in_product_description_id);
}

const bool ProductDescriptionController::delete_product_description(const std::string &product_description_id)
{
  return this->delete_data(product_description_id);
}