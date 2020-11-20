#pragma once

#include "ShelfController.cpp"

class ShelfLayerController : public EntityController
{
private:
  ShelfController* shelf_controller;

  std::string shelf_id;

public:
  ShelfLayerController(const char*);

  bool set_shelf(const std::string&);

  Json::Value get_shelf_layer(const std::string&);

  bool post_shelf_layer(const std::string &, const Json::Value&);
  bool post_shelf_layer(const Json::Value&);
  bool delete_shelf_layer(const std::string&);

  Json::Value get_shelf_layers(const std::string&);
  Json::Value get_shelf_layers();
};

ShelfLayerController::ShelfLayerController(const char* link) : EntityController::EntityController(link)
{
  shelf_controller = new ShelfController(link);
}

bool ShelfLayerController::set_shelf(const std::string& shelf_id)
{
  Json::Value shelf = this->shelf_controller->get_shelf(shelf_id);
  if(shelf["id"].asString() == shelf_id)
  {
    this->shelf_id = shelf_id;
    return true;
  }
  else
  {
    std::cout << "Shelf with given Id does not exist" << std::endl;
    return false;
  }
}

Json::Value ShelfLayerController::get_shelf_layer(const std::string& shelf_layer_id)
{
  std::string link_tail = "shelflayers/" + shelf_layer_id;
  return this->get_entity(link_tail);
}

bool ShelfLayerController::post_shelf_layer(const std::string& shelf_id, const Json::Value& shelf_layer)
{
  return this->set_shelf(shelf_id) && this->post_shelf_layer(shelf_layer);
}

bool ShelfLayerController::post_shelf_layer(const Json::Value& shelf_layer)
{
  if (shelf_layer["depth"].isInt() &&
      shelf_layer["externalReferenceId"].isString() &&
      shelf_layer["height"].isInt() &&
      shelf_layer["level"].isNumeric() &&
      shelf_layer["positionZ"].isNumeric() &&
      shelf_layer["type"].isString() &&
      shelf_layer["width"].isNumeric())
  {
    std::string link_tail = "shelves/" + this->shelf_id + "/shelflayers";
    return this->post_entity(shelf_layer, link_tail);
  }
  else
  {
    std::cout << "Invalid Shelf layer" << std::endl;
    return false;
  }
}

bool ShelfLayerController::delete_shelf_layer(const std::string& shelf_layer_id)
{
  std::string link_tail = "/shelflayers/" + shelf_layer_id;
  return this->delete_entity(link_tail);
}

Json::Value ShelfLayerController::get_shelf_layers(const std::string& shelf_id)
{
  return this->set_shelf(shelf_id) ? this->get_shelf_layers() : Json::Value();
}

Json::Value ShelfLayerController::get_shelf_layers()
{
  std::string link_tail = "/shelves/" + this->shelf_id + "/shelflayers";
  return this->get_entity(link_tail);
}