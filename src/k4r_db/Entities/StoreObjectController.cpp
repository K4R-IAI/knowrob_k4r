#pragma once

#include "DataController.cpp"

class StoreObjectController : public DataController
{
public:
  StoreObjectController();

public:
  const Json::Value get_store_objects(const std::string &);

  const bool post_store_object(const std::string &, const Json::Value &, Json::Value &);

  const bool delete_store_object(const std::string &);
};

StoreObjectController::StoreObjectController() : DataController::DataController()
{
}

const Json::Value StoreObjectController::get_store_objects(const std::string &store_id)
{
  return this->get_data("stores/" + store_id + "/storeobjects");
}

const bool StoreObjectController::post_store_object(const std::string &in_store_id, const Json::Value &in_store_object, Json::Value &out_store_object)
{
  return this->post_data(in_store_object, out_store_object, "stores/" + in_store_id + "/storeobjects");
}

const bool StoreObjectController::delete_store_object(const std::string &store_object_id)
{
  return this->delete_data("storeobjects/" + store_object_id);
}