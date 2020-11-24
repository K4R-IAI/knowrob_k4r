#pragma once

#include "EntityController.cpp"

class ProductController : public EntityController
{
public:
  ProductController(const char*);

  bool check_product(const Json::Value &);

  Json::Value get_product(const std::string &);
  Json::Value get_products();

  bool post_product(const std::string &, const Json::Value &);
  bool post_products(const Json::Value&);

  bool put_product(const std::string &, const Json::Value &);

  bool delete_product(const std::string &);
};

ProductController::ProductController(const char* link) : EntityController::EntityController((std::string(link) + "products/").c_str())
{
}

Json::Value ProductController::get_product(const std::string &product_id)
{
  return this->get_entity(product_id);
}

bool ProductController::check_product(const Json::Value &product)
{
  if (product["depth"].isInt() &&
      product["description"].isString() &&
      product["gtin"].isString() &&
      product["height"].isInt() &&
      product["length"].isInt() &&
      product["name"].isString() &&
      product["weight"].isInt())
  {
    return true;
  }
  else
  {
    std::cout << "Invalid products" << std::endl;
    return false;
  }
}

bool ProductController::post_product(const std::string &product_id, const Json::Value &product)
{
  return this->check_product(product) && this->post_entity(product, product_id);
}

bool ProductController::put_product(const std::string &product_id, const Json::Value &product)
{
  return this->check_product(product) && this->put_entity(product, product_id);
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