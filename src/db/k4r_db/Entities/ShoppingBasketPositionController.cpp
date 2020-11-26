#pragma once

#include "CustomerController.cpp"
#include "StoreController.cpp"

class ShoppingBasketPositionController : public EntityController
{
private:
  StoreController* store_controller;
  CustomerController* customer_controller;

  std::string store_id;
  std::string customer_id;

public:
  ShoppingBasketPositionController(const char*);
  ShoppingBasketPositionController(const char*, const std::string, const std::string);

  bool set_store(const std::string&);
  bool set_customer(const std::string&);

  Json::Value get_shopping_basket_position(const std::string&);
  Json::Value get_shopping_basket_positions(const std::string&, const std::string&);
  Json::Value get_shopping_basket_positions();

  bool post_shopping_basket_position(const std::string&, const std::string&, const Json::Value&);
  bool post_shopping_basket_position(const Json::Value&);

  bool delete_shopping_basket_position(const std::string&);
  bool delete_shopping_basket_positions(const std::string&, const std::string&);
  bool delete_shopping_basket_positions();
};

ShoppingBasketPositionController::ShoppingBasketPositionController(const char* link) : EntityController::EntityController((std::string(link)).c_str())
{
  store_controller = new StoreController(link);
  customer_controller = new CustomerController(link);
}

ShoppingBasketPositionController::ShoppingBasketPositionController(const char* link, const std::string store_id, const std::string customer_id) : ShoppingBasketPositionController::ShoppingBasketPositionController(link)
{
  this->set_store(store_id);
  this->set_customer(customer_id);
}

bool ShoppingBasketPositionController::set_store(const std::string& store_id)
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

bool ShoppingBasketPositionController::set_customer(const std::string& customer_id)
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

Json::Value ShoppingBasketPositionController::get_shopping_basket_position(const std::string& shopping_basket_position_id)
{
  std::string link_tail = "shoppingbasketpositions/" + shopping_basket_position_id;
  return this->get_entity(link_tail);
}

Json::Value ShoppingBasketPositionController::get_shopping_basket_positions(const std::string& store_id, const std::string& customer_id)
{
  if (this->set_store(store_id) && this->set_customer(customer_id))
  {
    return this->get_shopping_basket_positions();
  }
  else
  {
    std::cout << "Invalid shopping basket" << std::endl;
    return false;
  }
}

Json::Value ShoppingBasketPositionController::get_shopping_basket_positions()
{
  std::string link_tail = "stores/" + this->store_id + "/customers/" + this->customer_id + "/shoppingbasketpositions";
  return this->get_entity(link_tail);
}

bool ShoppingBasketPositionController::post_shopping_basket_position(const std::string& store_id, const std::string& customer_id, const Json::Value& shopping_basket_position)
{
  return this->set_store(store_id) && this->set_customer(customer_id) && this->post_shopping_basket_position(shopping_basket_position);
}

bool ShoppingBasketPositionController::post_shopping_basket_position(const Json::Value& shopping_basket_position)
{

  if (shopping_basket_position["productId"].isString() &&
      shopping_basket_position["quantity"].isInt() &&
      shopping_basket_position["sellingPrice"].isNumeric() &&
      shopping_basket_position["currency"].isString())
  {
    std::string link_tail = "stores/" + this->store_id + "/customers/" + this->customer_id + "/shoppingbasketpositions";
    return this->post_entity(shopping_basket_position, link_tail);
  }
  else
  {
    std::cout << "Invalid product" << std::endl;
    return false;
  }
}

bool ShoppingBasketPositionController::delete_shopping_basket_position(const std::string& shopping_basket_position_id)
{
  std::string link_tail = "/shoppingbasketpositions/" + shopping_basket_position_id;
  return this->delete_entity(link_tail);
}

bool ShoppingBasketPositionController::delete_shopping_basket_positions(const std::string& store_id, const std::string& customer_id)
{
  return this->set_store(store_id) && this->set_customer(customer_id) && this->delete_shopping_basket_positions();
}

bool ShoppingBasketPositionController::delete_shopping_basket_positions()
{
  std::string link_tail = "stores/" + this->store_id + "/customers/" + this->customer_id + "/shoppingbasketpositions";
  return this->delete_entity(link_tail);
}