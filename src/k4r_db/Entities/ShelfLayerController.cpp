#pragma once

#include "DataController.cpp"

class ShelfLayerController : public DataController
{
public:
  ShelfLayerController();

public:
  const Json::Value get_shelf_layer(const std::string &);
  const Json::Value get_shelf_layers(const std::string &);

  const bool post_shelf_layer(const std::string &, const Json::Value &, Json::Value &);

  const bool delete_shelf_layer(const std::string &);
};

ShelfLayerController::ShelfLayerController() : DataController::DataController()
{
}

const Json::Value ShelfLayerController::get_shelf_layer(const std::string &shelf_layer_id)
{
  return this->get_data("shelflayers/" + shelf_layer_id);
}

const Json::Value ShelfLayerController::get_shelf_layers(const std::string &shelf_id)
{
  return this->get_data("shelves/" + shelf_id + "/shelflayers");
}

const bool ShelfLayerController::post_shelf_layer(const std::string &in_shelf_id, const Json::Value &in_shelf_layer, Json::Value &out_shelf_layer)
{
  return this->post_data(in_shelf_layer, out_shelf_layer, "shelves/" + in_shelf_id + "/shelflayers");
}

const bool ShelfLayerController::delete_shelf_layer(const std::string &shelf_layer_id)
{
  return this->delete_data("shelflayers/" + shelf_layer_id);
}