#pragma once

#include "EntityController.cpp"
#include "ProductController.cpp"
#include "ShelfLayerController.cpp"

class PlanogramController : public EntityController
{
private:
  ProductController* product_controller;
  ShelfLayerController* shelf_layer_controller;

  std::string product_id;
  std::string shelf_layer_id;

public:
  PlanogramController(const char*);
  PlanogramController(const char*, const std::string);
  PlanogramController(const char*, const std::string, const std::string);

  bool set_product(const std::string&);
  bool set_shelf_layer(const std::string&);

  bool check_planogram(const Json::Value&);

  Json::Value get_planograms();

  bool post_planogram(const std::string&, const std::string&, const Json::Value&);
  bool post_planogram(const std::string&, const Json::Value&);
  bool post_planogram(const Json::Value&);

  bool put_planogram(const std::string&, const std::string&, const std::string&, const Json::Value&);
  bool put_planogram(const std::string&, const std::string&, const Json::Value&);
  bool put_planogram(const std::string&, const Json::Value&);

  bool delete_planogram(const std::string&);
};

PlanogramController::PlanogramController(const char* link) : EntityController::EntityController((std::string(link) + "planograms/").c_str())
{
  this->product_controller = new ProductController(link);
  this->shelf_layer_controller = new ShelfLayerController(link);
}

PlanogramController::PlanogramController(const char* link, const std::string shelf_layer_id) : PlanogramController::PlanogramController(link)
{
  this->shelf_layer_id = shelf_layer_id;
}

PlanogramController::PlanogramController(const char* link, const std::string product_id, const std::string shelf_layer_id) : PlanogramController::PlanogramController(link, shelf_layer_id)
{
  this->product_id = product_id;
}

bool PlanogramController::set_product(const std::string& product_id)
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

bool PlanogramController::set_shelf_layer(const std::string& shelf_layer_id)
{
  Json::Value shelf_layer = this->shelf_layer_controller->get_shelf_layer(shelf_layer_id);
  if (shelf_layer["id"].asString() == shelf_layer_id)
  {
    this->shelf_layer_id = shelf_layer_id;
    return true;
  }
  else
  {
    std::cout << "Shelf layer with given Id does not exist" << std::endl;
    return false;
  }
}

Json::Value PlanogramController::get_planograms()
{
  return this->get_entity();
}

bool PlanogramController::check_planogram(const Json::Value& planogram)
{
  if (planogram["numberOfFacings"].isInt() &&
      planogram["orientationYaw"].isNumeric() &&
      planogram["positionX"].isInt() &&
      planogram["versionTimestamp"].isInt())
  {
    return true;
  }
  else
  {
    std::cout << "Invalid planogram" << std::endl;
    return false;
  }
}

bool PlanogramController::post_planogram(const std::string& product_id, const std::string& shelf_layer_id, const Json::Value& planogram)
{
  return this->set_product(product_id) && this->post_planogram(shelf_layer_id, planogram);
}

bool PlanogramController::post_planogram(const std::string& shelf_layer_id, const Json::Value& planogram)
{
  return this->set_shelf_layer(shelf_layer_id) && this->post_planogram(planogram);
}

bool PlanogramController::post_planogram(const Json::Value& planogram)
{
  if (this->check_planogram(planogram))
  {
    Json::Value planogram_in = planogram;
    planogram_in["productId"] = this->product_id;
    planogram_in["shelfLayerId"] = std::stoi(this->shelf_layer_id);
    return this->post_entity(planogram_in);
  }
  else
  {
    return false;
  }
}

bool PlanogramController::put_planogram(const std::string& product_id, const std::string& shelf_layer_id, const std::string& planogram_id, const Json::Value& planogram)
{
  return this->set_product(product_id) && this->post_planogram(shelf_layer_id, planogram_id, planogram);
}

bool PlanogramController::put_planogram(const std::string& shelf_layer_id, const std::string& planogram_id, const Json::Value& planogram)
{
  return this->set_shelf_layer(shelf_layer_id) && this->post_planogram(planogram_id, planogram);
}

bool PlanogramController::put_planogram(const std::string& planogram_id, const Json::Value& planogram)
{
  if (this->check_planogram(planogram))
  {
    Json::Value planogram_in = planogram;
    planogram_in["productId"] = this->product_id;
    planogram_in["shelfLayerId"] = std::stoi(this->shelf_layer_id);
    return this->put_entity(planogram_in, planogram_id);
  }
  else
  {
    return false;
  }
}

bool PlanogramController::delete_planogram(const std::string& planogram_id)
{
  return this->delete_entity(planogram_id);
}