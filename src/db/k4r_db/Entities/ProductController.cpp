#pragma once

#include "MaterialGroupController.cpp"

class ProductController : public DataController
{
public:
  ProductController();

public:
  const bool check_product_id(const std::string &);

  const Json::Value get_product(const std::string &);
  const Json::Value get_products();

  const bool post_product(const std::string &, const Json::Value &, Json::Value &);
  const bool post_product(const std::string &, const std::string &, const Json::Value &, Json::Value &);
  const bool post_products(const Json::Value &, Json::Value &);

  const bool put_product(const std::string &, const Json::Value &, Json::Value &);
  const bool put_product(const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool delete_product(const std::string &);

private:
  MaterialGroupController material_group_controller;
};

ProductController::ProductController() : DataController::DataController("products/")
{
}

const bool ProductController::check_product_id(const std::string &product_id)
{
  Json::Value product = this->get_product(product_id);
  if (product["id"].asString() == product_id)
  {
    return true;
  }
  else
  {
    std::cout << "Product with id " << product_id << " not found" << std::endl;
    return false;
  }
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
  return this->post_data(in_product_tmp, out_product, in_product_id) ||
         this->material_group_controller.check_material_group_id(in_material_group_id);
}

const bool ProductController::post_products(const Json::Value &in_products, Json::Value &out_products)
{
  if (!in_products["products"].isNull())
  {
    return this->post_data(in_products, out_products, "list");
  }
  else
  {
    std::cout << "Invalid products" << std::endl;
    return false;
  }
}

const bool ProductController::put_product(const std::string &in_product_id, const Json::Value &in_product, Json::Value &out_product)
{
  return this->put_data(in_product, out_product, in_product_id) ||
         this->check_product_id(in_product_id);
}

const bool ProductController::put_product(const std::string &in_product_id, const std::string &in_material_group_id, const Json::Value &in_product, Json::Value &out_product)
{
  Json::Value in_product_tmp = in_product;
  in_product_tmp["materialGroupId"] = in_material_group_id;
  return this->put_data(in_product_tmp, out_product, in_product_id) ||
         (this->check_product_id(in_product_id) &&
          this->material_group_controller.check_material_group_id(in_material_group_id));
}

const bool ProductController::delete_product(const std::string &product_id)
{
  return this->delete_data(product_id) || this->check_product_id(product_id);
}