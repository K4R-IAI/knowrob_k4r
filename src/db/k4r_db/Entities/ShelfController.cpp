#pragma once

#include "StoreController.cpp"

class ShelfController : public EntityController
{
private:
  StoreController* store_controller;

  std::string store_id;

public:
  ShelfController(const char*);
  ShelfController(const char*, const std::string);

  bool check_shelf(const Json::Value&);

  bool set_store(const std::string&);

  Json::Value get_shelf(const std::string&);
  Json::Value get_shelves(const std::string&);
  Json::Value get_shelves();

  bool post_shelf(const std::string &, const Json::Value&);
  bool post_shelf(const Json::Value&);

  bool put_shelf(const std::string &, const Json::Value&);

  bool delete_shelf(const std::string&, const std::string&, const std::string&);
  bool delete_shelf(const std::string&);
};

ShelfController::ShelfController(const char* link) : EntityController::EntityController(link)
{
  store_controller = new StoreController(link);
}

ShelfController::ShelfController(const char* link, const std::string store_id) : ShelfController::ShelfController(link)
{
  this->set_store(store_id);
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

bool ShelfController::check_shelf(const Json::Value& shelf)
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
  std::string link_tail = "/stores/" + this->store_id + "/shelves";
  return this->check_shelf(shelf) && this->post_entity(shelf, link_tail);
}

bool ShelfController::put_shelf(const std::string& shelf_id, const Json::Value& shelf)
{
  std::string link_tail = "/shelves/" + shelf_id;
  return this->check_shelf(shelf) && this->put_entity(shelf, link_tail);
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