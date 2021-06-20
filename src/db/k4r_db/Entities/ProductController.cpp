#pragma once

#include "DataController.cpp"

class ProductController : public DataController
{
public:
  ProductController();

public:
  const Json::Value get_product(const std::string &);
  const Json::Value get_products();

  const bool post_product(const std::string &, const Json::Value &, Json::Value &);
  const bool post_product(const std::string &, const std::string &, const Json::Value &, Json::Value &);
  const bool post_products(const Json::Value &, Json::Value &);

  const bool put_product(const std::string &, const Json::Value &, Json::Value &);
  const bool put_product(const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool delete_product(const std::string &);
};

ProductController::ProductController() : DataController::DataController("products/")
{
}

const Json::Value ProductController::get_product(const std::string &product_id)
{
  return this->get_data(product_id);
}

const Json::Value ProductController::get_products()
{
  return this->get_data();
}

const bool ProductController::post_product(const std::string &in_product_id, const Json::Value &in_product, Json::Value &out_product)
{
  std::string in_material_group_id = in_product["materialGroupId"].toStyledString();
  if (in_material_group_id.compare("null"))
  {
    return this->post_data(in_product, out_product, in_product_id);
  }
  else
  {
    return this->post_product(in_product_id, in_material_group_id, in_product, out_product);
  }
}

const bool ProductController::post_product(const std::string &in_product_id, const std::string &in_material_group_id, const Json::Value &in_product, Json::Value &out_product)
{
  Json::Value in_product_tmp = in_product;
  in_product_tmp["materialGroupId"] = in_material_group_id;
  return this->post_data(in_product_tmp, out_product, in_product_id);
}

const bool ProductController::post_products(const Json::Value &in_products, Json::Value &out_products)
{
  return this->post_data(in_products, out_products, "list");
}

const bool ProductController::put_product(const std::string &in_product_id, const Json::Value &in_product, Json::Value &out_product)
{
  return this->put_data(in_product, out_product, in_product_id);
}

const bool ProductController::put_product(const std::string &in_product_id, const std::string &in_material_group_id, const Json::Value &in_product, Json::Value &out_product)
{
  Json::Value in_product_tmp = in_product;
  in_product_tmp["materialGroupId"] = in_material_group_id;
  return this->put_data(in_product_tmp, out_product, in_product_id);
}

const bool ProductController::delete_product(const std::string &product_id)
{
  return this->delete_data(product_id);
}