#pragma once

#include "DataController.cpp"

class StoreController : public DataController
{
public:
  StoreController();

public:
  const Json::Value get_store(const std::string&);
  const Json::Value get_stores();

  const bool post_store(const Json::Value&, Json::Value&);

  const bool put_store(const std::string&, const Json::Value&, Json::Value&);

  const bool delete_store(const std::string&);
};

StoreController::StoreController() : DataController::DataController("stores/")
{
}

const Json::Value StoreController::get_store(const std::string& store_id)
{
  return this->get_data(store_id);
}

const Json::Value StoreController::get_stores()
{
  return this->get_data();
}

const bool StoreController::post_store(const Json::Value& in_store, Json::Value& out_store)
{
  return this->post_data(in_store, out_store);
}

const bool StoreController::put_store(const std::string& in_store_id, const Json::Value& in_store, Json::Value& out_store)
{
  return this->put_data(in_store, out_store, in_store_id);
}

const bool StoreController::delete_store(const std::string& store_id)
{
  return this->delete_data(store_id);
}