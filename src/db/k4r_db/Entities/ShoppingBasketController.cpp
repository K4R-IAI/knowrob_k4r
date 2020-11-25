#pragma once

#include "CustomerController.cpp"
#include "ProductController.cpp"
#include "StoreController.cpp"

class ShoppingBasketController : public EntityController
{
private:
  StoreController* store_controller;
  ProductController* product_controller;
  CustomerController* customer_controller;

  std::string store_id;
  std::string product_id;
  std::string customer_id;

public:
  ShoppingBasketController(const char*);
  ShoppingBasketController(const char*, const std::string, const std::string);
  ShoppingBasketController(const char*, const std::string, const std::string, const std::string);

  bool set_store(const std::string&);
  bool set_product(const std::string&);
  bool set_customer(const std::string&);

  Json::Value get_shopping_basket(const std::string&);
  Json::Value get_shopping_basket();
  Json::Value get_shopping_baskets();

  bool post_shopping_basket(const std::string&, const std::string&, const Json::Value&);
  bool post_shopping_basket(const Json::Value&);
  
  bool delete_shopping_basket(const std::string&, const std::string&, const std::string&);
  bool delete_shopping_basket(const std::string&);
  bool delete_shopping_basket();
  bool delete_shopping_baskets();
};

ShoppingBasketController::ShoppingBasketController(const char* link) : EntityController::EntityController((std::string(link) + "stores/").c_str())
{
  store_controller = new StoreController(link);
  product_controller = new ProductController(link);
  customer_controller = new CustomerController(link);
}

ShoppingBasketController::ShoppingBasketController(const char* link, const std::string store_id, const std::string customer_id) : ShoppingBasketController::ShoppingBasketController(link)
{
  this->set_store(store_id);
  this->set_customer(customer_id);
}

ShoppingBasketController::ShoppingBasketController(const char* link, const std::string store_id, const std::string customer_id, const std::string product_id) : ShoppingBasketController::ShoppingBasketController(link, store_id, customer_id)
{
  this->set_product(product_id);
}

bool ShoppingBasketController::set_store(const std::string& store_id)
{
  if (store_id.empty())
  {
    std::cout << "Store with empty Id does not exist" << std::endl;
    return false;
  }
  
  Json::Value store = this->store_controller->get_store(store_id);
  if (store["id"].asString() == store_id)
  {
    this->store_id = store_id;
    return true;
  }
  else
  {
    std::cout << "Store with given Id does not exist" << std::endl;
    return false;
  }
}

bool ShoppingBasketController::set_product(const std::string& product_id)
{
  Json::Value product = this->product_controller->get_product(product_id);
  if (product["id"].asString() == product_id)
  {
    this->product_id = product_id;
    return true;
  }
  else
  {
    std::cout << "Product with given Id does not exist" << std::endl;
    return false;
  }
}

bool ShoppingBasketController::set_customer(const std::string& customer_id)
{
  Json::Value customers = this->customer_controller->get_customers();
  for (const Json::Value customer : customers)
  {
    if (customer["id"].asString() == customer_id)
    {
      this->customer_id = customer_id;
      return true;
    }
  }
  std::cout << "customer with given Id does not exist" << std::endl;
  return false;
}

bool ShoppingBasketController::post_shopping_basket(const std::string& store_id, const std::string& customer_id, const Json::Value& shopping_basket)
{
  return this->set_store(store_id) && this->set_customer(customer_id) && this->post_shopping_basket(shopping_basket);
}

bool ShoppingBasketController::post_shopping_basket(const Json::Value& shopping_basket)
{

  if (shopping_basket["productId"].isString() &&
      shopping_basket["quantity"].isInt() &&
      shopping_basket["sellingPrice"].isNumeric())
  {
    std::string link_tail = this->store_id + "/customers/" + this->customer_id + "/shoppingbasketpositions";
    return this->post_entity(shopping_basket, link_tail);
  }
  else
  {
    std::cout << "Invalid product" << std::endl;
    return false;
  }
}

bool ShoppingBasketController::delete_shopping_basket(const std::string& store_id, const std::string& customer_id, const std::string& product_id)
{
  return this->set_store(store_id) && this->set_customer(customer_id) && this->set_product(product_id) && this->delete_shopping_basket();
}

bool ShoppingBasketController::delete_shopping_basket(const std::string& product_id)
{
  return this->set_product(product_id) && this->delete_shopping_basket();
}

bool ShoppingBasketController::delete_shopping_basket()
{
  std::string link_tail = this->store_id + "/customers/" + this->customer_id + "/shoppingbasketpositions/" + this->product_id;
  return this->delete_entity(link_tail);
}

bool ShoppingBasketController::delete_shopping_baskets()
{
  std::string link_tail = this->store_id + "/customers/" + this->customer_id + "/shoppingbasketpositions";
  return this->delete_entity(link_tail);
}

Json::Value ShoppingBasketController::get_shopping_basket(const std::string& product_id)
{
  return this->set_product(product_id) ? this->get_shopping_basket() : Json::Value();
}

Json::Value ShoppingBasketController::get_shopping_basket()
{
  std::string link_tail = this->store_id + "/customers/" + this->customer_id + "/shoppingbasketpositions/" + this->product_id;
  return this->get_entity(link_tail);
}

Json::Value ShoppingBasketController::get_shopping_baskets()
{
  std::string link_tail = this->store_id + "/customers/" + this->customer_id + "/shoppingbasketpositions";
  return this->get_entity(link_tail);
}