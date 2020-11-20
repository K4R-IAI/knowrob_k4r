#pragma once

#include "StoreController.cpp"

class ShelfController : public EntityController
{
private:
  StoreController* store_controller;

  std::string store_id;

public:
  ShelfController(const char*);

  bool set_store(const std::string&);

  Json::Value get_shelf(const std::string&);

  bool post_shelf(const std::string &, const Json::Value&);
  bool post_shelf(const Json::Value&);
  bool delete_shelf(const std::string&, const std::string&, const std::string&);
  bool delete_shelf(const std::string&);

  Json::Value get_shelves(const std::string&);
  Json::Value get_shelves();
};

ShelfController::ShelfController(const char* link) : EntityController::EntityController(link)
{
  store_controller = new StoreController(link);
}

bool ShelfController::set_store(const std::string& store_id)
{
  Json::Value store = this->store_controller->get_store(store_id);
  if(store["id"].asString() == store_id)
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

Json::Value ShelfController::get_shelf(const std::string& shelf_id)
{
  std::string link_tail = "/shelves/" + shelf_id;
  return this->get_entity(link_tail);
}

bool ShelfController::post_shelf(const std::string& store_id, const Json::Value& shelf)
{
  return this->set_store(store_id) && this->post_shelf(shelf);
}

bool ShelfController::post_shelf(const Json::Value& shelf)
{
  if (shelf["cadPlanId"].isString() &&
      shelf["depth"].isInt() &&
      shelf["externalReferenceId"].isString() &&
      shelf["height"].isInt() &&
      shelf["orientationY"].isNumeric() &&
      shelf["orientationYaw"].isNumeric() &&
      shelf["orientationZ"].isNumeric() &&
      shelf["orientationx"].isNumeric() &&
      shelf["positionX"].isNumeric() &&
      shelf["positionY"].isNumeric() &&
      shelf["positionZ"].isNumeric() &&
      shelf["productGroupId"].isInt() &&
      shelf["width"].isNumeric())
  {
    std::string link_tail = "/stores/" + this->store_id + "/shelves";
    return this->post_entity(shelf, link_tail);
  }
  else
  {
    std::cout << "Invalid Shelf" << std::endl;
    return false;
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
  std::string link_tail = "/stores/" + this->store_id + "/shelves";
  return this->get_entity(link_tail);
}