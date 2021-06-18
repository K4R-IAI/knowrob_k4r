#pragma once

#include "DataController.cpp"

class StoreController : public DataController
{
public:
  StoreController();

public:
  const bool check_store_id(const std::string&);

  const bool check_store(const Json::Value&);

  const Json::Value get_store(const std::string&);
  const Json::Value get_stores();

  const bool post_store(const Json::Value&, Json::Value&);

  const bool put_store(const std::string&, const Json::Value&, Json::Value&);

  const bool delete_store(const std::string&);
};

StoreController::StoreController() : DataController::DataController("stores/")
{
}

const bool StoreController::check_store_id(const std::string& store_id)
{
  Json::Value store = this->get_store(store_id);
  if (store["id"].asString() == store_id)
  {
    return true;
  }
  else
  {
    std::cout << "Store with id " << store_id << " not found" << std::endl;
    return false;
  }
}

const bool StoreController::check_store(const Json::Value& store)
{
  if (store["addressAdditional"].isString() &&
      store["addressCity"].isString() &&
      store["addressCountry"].isString() &&
      store["addressPostcode"].isString() &&
      store["addressState"].isString() &&
      store["addressStreet"].isString() &&
      store["addressStreetNumber"].isString() &&
      store["cadPlanId"].isString() &&
      store["latitude"].isNumeric() &&
      store["longitude"].isNumeric() &&
      store["storeName"].isString() &&
      store["storeNumber"].isString())
  {
    return true;
  }
  else
  {
    std::cout << "Invalid Store" << std::endl;
    return false;
  }
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
  return this->post_data(in_store, out_store) || this->check_store(in_store);
}

const bool StoreController::put_store(const std::string& in_store_id, const Json::Value& in_store, Json::Value& out_store)
{
  return this->put_data(in_store, out_store, in_store_id) || (this->check_store_id(in_store_id) && this->check_store(in_store));
}

const bool StoreController::delete_store(const std::string& store_id)
{
  return this->delete_data(store_id) || this->check_store_id(store_id);
}