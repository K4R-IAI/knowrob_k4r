#pragma once

#include "EntityController.cpp"

class ProductController : public EntityController
{
public:
  ProductController(const char*);

  Json::Value get_product(const std::string &);
  bool post_product(const Json::Value &, const std::string &);
  bool delete_product(const std::string &);

  Json::Value get_products();
  bool post_products(const Json::Value &);
};

ProductController::ProductController(const char* link) : EntityController::EntityController((std::string(link) + "products/").c_str())
{
}

Json::Value ProductController::get_product(const std::string &product_id)
{
  return this->get_entity(product_id);
}

bool ProductController::post_product(const Json::Value &product, const std::string &product_id)
{
  if (product["depth"].isInt() &&
      product["description"].isString() &&
      product["gtin"].isString() &&
      product["height"].isInt() &&
      product["length"].isInt() &&
      product["name"].isString() &&
      product["weight"].isInt())
  {
    return this->post_entity(product, product_id);
  }
  else
  {
    std::cout << "Invalid products" << std::endl;
    return false;
  }
}

bool ProductController::delete_product(const std::string &product_id)
{
  return this->delete_entity(product_id);
}

Json::Value ProductController::get_products()
{
  return this->get_entity();
}

bool ProductController::post_products(const Json::Value &products)
{
  if (!products["products"].isNull())
  {
    return this->post_entity(products, "list");
  }
  else
  {
    std::cout << "Invalid products" << std::endl;
    return false;
  }
}