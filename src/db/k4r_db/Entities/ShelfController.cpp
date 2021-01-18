#pragma once

#include "StoreController.cpp"
#include "ProductGroupController.cpp"

class ShelfController : public EntityController
{
private:
  StoreController* store_controller;
  ProductGroupController* product_group_controller;

  std::string store_id;
  std::string product_group_id;

public:
  ShelfController(const char*);
  ShelfController(const char*, const std::string);
  ShelfController(const char*, const std::string, const std::string);

  bool check_shelf(const Json::Value&);

  bool set_store(const std::string&);
  bool set_product_group(const std::string&);

  Json::Value get_shelf(const std::string&);
  Json::Value get_shelves(const std::string&);
  Json::Value get_shelves();

  bool post_shelf(const std::string&, const std::string&, const Json::Value&);
  bool post_shelf(const std::string&, const Json::Value&);
  bool post_shelf(const Json::Value&);

  bool put_shelf(const std::string&, const std::string&, const std::string&, const Json::Value&);
  bool put_shelf(const std::string&, const std::string&, const Json::Value&);
  bool put_shelf(const std::string&, const Json::Value&);

  bool delete_shelf(const std::string&, const std::string&, const std::string&);
  bool delete_shelf(const std::string&);
};

ShelfController::ShelfController(const char* link) : EntityController::EntityController(link)
{
  this->store_controller = new StoreController(link);
  this->product_group_controller = new ProductGroupController(link);
}

ShelfController::ShelfController(const char* link, const std::string store_id) : ShelfController::ShelfController(link)
{
  this->set_store(store_id);
}

ShelfController::ShelfController(const char* link, const std::string store_id, const std::string product_group_id) : ShelfController::ShelfController(link, store_id)
{
  this->set_product_group(product_group_id);
}

bool ShelfController::set_store(const std::string& store_id)
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

bool ShelfController::set_product_group(const std::string& product_group_id)
{
  Json::Value product_group = this->product_group_controller->get_product_group(product_group_id);
  if (product_group["id"].asString() == product_group_id)
  {
    this->product_group_id = product_group_id;
    return true;
  }
  else
  {
    std::cout << "Product group with given Id does not exist" << std::endl;
    return false;
  }
}

Json::Value ShelfController::get_shelf(const std::string& shelf_id)
{
  std::string link_tail = "/shelves/" + shelf_id;
  return this->get_entity(link_tail);
}

bool ShelfController::post_shelf(const std::string& store_id, const std::string& product_group_id, const Json::Value& shelf)
{
  return this->set_product_group(product_group_id) && this->post_shelf(store_id, shelf);
}

bool ShelfController::post_shelf(const std::string& store_id, const Json::Value& shelf)
{
  return this->set_store(store_id) && this->post_shelf(shelf);
}

bool ShelfController::check_shelf(const Json::Value& shelf)
{
  if (shelf["cadPlanId"].isString() &&
      shelf["depth"].isNumeric() &&
      shelf["externalReferenceId"].isString() &&
      shelf["height"].isNumeric() &&
      shelf["orientationW"].isNumeric() &&
      shelf["orientationX"].isNumeric() &&
      shelf["orientationY"].isNumeric() &&
      shelf["orientationZ"].isNumeric() &&
      shelf["positionX"].isNumeric() &&
      shelf["positionY"].isNumeric() &&
      shelf["positionZ"].isNumeric() &&
      shelf["width"].isNumeric())
  {
    return true;
  }
  else
  {
    std::cout << "Invalid Shelf" << std::endl;
    return false;
  }
}

bool ShelfController::post_shelf(const Json::Value& shelf)
{
  std::string link_tail = "stores/" + this->store_id + "/shelves";
  if (this->check_shelf(shelf))
  {
    Json::Value shelf_in = shelf;
    shelf_in["productGroupId"] = std::stoi(this->product_group_id);
    return this->post_entity(shelf_in, link_tail);
  }
}

bool ShelfController::put_shelf(const std::string& store_id, const std::string& product_group_id, const std::string& shelf_id, const Json::Value& shelf)
{
  return this->set_store(store_id) && this->put_shelf(product_group_id, shelf_id, shelf);
}

bool ShelfController::put_shelf(const std::string& product_group_id, const std::string& shelf_id, const Json::Value& shelf)
{
  return this->set_product_group(product_group_id) && this->put_shelf(shelf_id, shelf);
}

bool ShelfController::put_shelf(const std::string& shelf_id, const Json::Value& shelf)
{
  std::string link_tail = "/shelves/" + shelf_id;
  if (this->check_shelf(shelf))
  {
    Json::Value shelf_in = shelf;
    shelf_in["storeId"] = std::stoi(this->store_id);
    shelf_in["productGroupId"] = std::stoi(this->product_group_id);
    return this->put_entity(shelf_in, link_tail);
  }
}

bool ShelfController::delete_shelf(const std::string& shelf_id)
{
  std::string link_tail = "/shelves/" + shelf_id;
  return this->delete_entity(link_tail);
}

Json::Value ShelfController::get_shelves(const std::string& store_id)
{
  return this->set_store(store_id) ? this->get_shelves() : Json::Value();
}

Json::Value ShelfController::get_shelves()
{
  std::string link_tail = "stores/" + this->store_id + "/shelves";
  return this->get_entity(link_tail);
}