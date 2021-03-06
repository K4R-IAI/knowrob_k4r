#pragma once

#include "EntityController.cpp"

class StoreController : public EntityController
{
public:
  StoreController(const char*);

  bool check_store(const Json::Value&);

  Json::Value get_store(const std::string&);
  Json::Value get_stores();

  bool post_store(const Json::Value&);
  bool post_stores(const Json::Value&);

  bool put_store(const std::string&, const Json::Value&);

  bool delete_store(const std::string&);
};

StoreController::StoreController(const char* link) : EntityController::EntityController((std::string(link) + "stores/").c_str())
{
}

Json::Value StoreController::get_store(const std::string& store_id)
{
  return this->get_entity(store_id);
}

bool StoreController::check_store(const Json::Value& store)
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

bool StoreController::post_store(const Json::Value& store)
{
  return this->check_store(store) && this->post_entity(store);
}

bool StoreController::put_store(const std::string& store_id, const Json::Value& store)
{
  return this->check_store(store) && this->put_entity(store, store_id);
}

bool StoreController::delete_store(const std::string& store_id)
{
  return this->delete_entity(store_id);
}

Json::Value StoreController::get_stores()
{
  return this->get_entity();
}

bool StoreController::post_stores(const Json::Value& stores)
{
  if (!stores["products"].isNull())
  {
    return this->post_entity(stores);
  }
  else
  {
    std::cout << "Invalid stores" << std::endl;
    return false;
  }
}