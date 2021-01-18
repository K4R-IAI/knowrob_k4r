#pragma once

#include "ShelfLayerController.cpp"
#include "ProductController.cpp"

class FacingController : public EntityController
{
private:
  ShelfLayerController* shelf_layer_controller;
  ProductController* product_controller;

  std::string shelf_layer_id;
  std::string product_id;

public:
  FacingController(const char*);
  FacingController(const char*, const std::string);
  FacingController(const char*, const std::string, const std::string);

  bool check_facing(const Json::Value&);

  bool set_shelf_layer(const std::string&);
  bool set_product(const std::string&);

  Json::Value get_facing(const std::string&);
  Json::Value get_facings(const std::string&);
  Json::Value get_facings();

  bool post_facing(const std::string&, const std::string&, const Json::Value&);
  bool post_facing(const std::string&, const Json::Value&);
  bool post_facing(const Json::Value&);

  bool put_facing(const std::string&, const std::string&, const std::string&, const Json::Value&);
  bool put_facing(const std::string&, const std::string&, const Json::Value&);
  bool put_facing(const std::string&, const Json::Value&);

  bool delete_facing(const std::string&);
};

FacingController::FacingController(const char* link) : EntityController::EntityController(link)
{
  this->shelf_layer_controller = new ShelfLayerController(link);
  this->product_controller = new ProductController(link);
}

FacingController::FacingController(const char* link, const std::string shelf_layer_id) : FacingController::FacingController(link)
{
  this->set_shelf_layer(shelf_layer_id);
}

FacingController::FacingController(const char* link, const std::string shelf_layer_id, const std::string product_id) : FacingController::FacingController(link, shelf_layer_id)
{
  this->set_product(product_id);
}

bool FacingController::set_shelf_layer(const std::string& shelf_layer_id)
{
  Json::Value shelf_layer = this->shelf_layer_controller->get_shelf_layer(shelf_layer_id);
  if (shelf_layer["id"].asString() == shelf_layer_id)
  {
    this->shelf_layer_id = shelf_layer_id;
    return true;
  }
  else
  {
    std::cout << "ShelfLayer with given Id does not exist" << std::endl;
    return false;
  }
}

bool FacingController::set_product(const std::string& product_id)
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

Json::Value FacingController::get_facing(const std::string& facing_id)
{
  std::string link_tail = "facings/" + facing_id;
  return this->get_entity(link_tail);
}

Json::Value FacingController::get_facings(const std::string& shelf_layer_id)
{
  if (this->set_shelf_layer(shelf_layer_id))
  {
    return this->get_facings();
  }
  else
  {
    return Json::Value();
  }
}

Json::Value FacingController::get_facings()
{
  std::string link_tail = "shelflayers/" + this->shelf_layer_id + "/facings";
  return this->get_entity(link_tail);
}

bool FacingController::check_facing(const Json::Value& facing)
{
  if (facing["layerRelativePosition"].isInt() &&
      facing["quantity"].isInt())
  {
    return true;
  }
  else
  {
    std::cout << "Invalid facing" << std::endl;
    return false;
  }
}

bool FacingController::post_facing(const std::string& shelf_layer_id, const std::string& product_id, const Json::Value& facing)
{
  return this->set_product(product_id) && this->post_facing(shelf_layer_id, facing);
}

bool FacingController::post_facing(const std::string& shelf_layer_id, const Json::Value& facing)
{
  return this->set_shelf_layer(shelf_layer_id) && this->post_facing(facing);
}

bool FacingController::post_facing(const Json::Value& facing)
{
  if (this->check_facing(facing))
  {
    Json::Value facing_in = facing;
    facing_in["productId"] = this->product_id;
    std::string link_tail = "shelflayers/" + this->shelf_layer_id + "/facings";
    return this->post_entity(facing_in, link_tail);
  }
  else
  {
    return false;
  }
}

bool FacingController::put_facing(const std::string& shelf_layer_id, const std::string& product_id, const std::string& facing_id, const Json::Value& facing)
{
  return this->set_shelf_layer(shelf_layer_id) && this->put_facing(product_id, facing_id, facing);
}

bool FacingController::put_facing(const std::string& product_id, const std::string& facing_id, const Json::Value& facing)
{
  return this->set_product(product_id) && this->put_facing(facing_id, facing);
}

bool FacingController::put_facing(const std::string& facing_id, const Json::Value& facing)
{
  if (this->get_facing(facing_id).isNull())
  {
    std::cout << "Facing with id " << facing_id << " not found" << std::endl;
    return false;
  }
  else
  {
    Json::Value facing_in = facing;
    facing_in["productId"] = this->product_id;
    facing_in["shelfLayerId"] = std::stoi(this->shelf_layer_id);
    std::string link_tail = "facings/" + facing_id;
    return this->put_entity(facing_in, link_tail);
  }
}

bool FacingController::delete_facing(const std::string& facing_id)
{
  std::string link_tail = "/facings/" + facing_id;
  return this->delete_entity(link_tail);
}