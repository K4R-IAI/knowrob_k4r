#pragma once

#include "StoreController.cpp"
#include "ProductController.cpp"
#include "CharacteristicController.cpp"

class PropertyController : public EntityController
{
private:
  StoreController* store_controller;
  ProductController* product_controller;
  CharacteristicController* characteristic_controller;

  std::string store_id;
  std::string product_id;
  std::string characteristic_id;

public:
  PropertyController(const char*);

  bool set_store(const std::string&);
  bool set_product(const std::string&);
  bool set_characteristic(const std::string&);

  bool post_property(const std::string&, const std::string&, const std::string&, const std::string&);
  bool post_property(const std::string&);
  bool delete_property(const std::string&, const std::string&, const std::string&);
  bool delete_property();

  Json::Value get_properties();
};

PropertyController::PropertyController(const char* link) : EntityController::EntityController((std::string(link) + "stores/").c_str())
{
  store_controller = new StoreController(link);
  product_controller = new ProductController(link);
  characteristic_controller = new CharacteristicController(link);
}

bool PropertyController::set_store(const std::string& store_id)
{
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

bool PropertyController::set_product(const std::string& product_id)
{
  Json::Value product = this->product_controller->get_product(product_id);
  if(product["id"].asString() == product_id)
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

bool PropertyController::set_characteristic(const std::string& characteristic_id)
{
  Json::Value characteristics = this->characteristic_controller->get_characteristics();
  for (const Json::Value characteristic : characteristics)
  {
    if (characteristic["id"].asString() == characteristic_id)
    {
      this->characteristic_id = characteristic_id;
      return true;
    }
  }
  std::cout << "Characteristic with given Id does not exist" << std::endl;
  return false;
}

bool PropertyController::post_property(const std::string& store_id, const std::string& product_id, const std::string& characteristic_id, const std::string& value)
{
  return this->set_store(store_id) && this->set_product(product_id) && this->set_characteristic(characteristic_id) && this->post_property(value);
}

bool PropertyController::post_property(const std::string& value)
{
  Json::Value property;
  property["characteristicId"] = this->characteristic_id;
  property["productId"] = this->product_id;
  property["storeId"] = this->store_id;
  property["value"] = value;
  std::string link_tail = this->store_id + "/products/" + this->product_id + "/properties/" + this->characteristic_id;
  return this->post_entity(property, link_tail);
}

bool PropertyController::delete_property(const std::string& store_id, const std::string& product_id, const std::string& characteristic_id)
{
  return this->set_store(store_id) && this->set_product(product_id) && this->set_characteristic(characteristic_id) && this->delete_property();
}

bool PropertyController::delete_property()
{
  std::string link_tail = this->store_id + "/products/" + this->product_id + "/properties/" + this->characteristic_id;
  return this->delete_entity(link_tail);
}

Json::Value PropertyController::get_properties()
{
  std::string link_tail = this->store_id + "/products/" + this->product_id + "/properties";
  return this->get_entity(link_tail);
}