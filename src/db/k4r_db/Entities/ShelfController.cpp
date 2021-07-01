#pragma once

#include "DataController.cpp"

class ShelfController : public DataController
{
public:
  ShelfController();

  const Json::Value get_shelf(const std::string &);
  const Json::Value get_shelves(const std::string &);

  const bool post_shelf(const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool put_shelf(const std::string &, const std::string &, const std::string &, const Json::Value &, Json::Value &);
  
  const bool delete_shelf(const std::string &);
};

ShelfController::ShelfController() : DataController::DataController()
{
}

const Json::Value ShelfController::get_shelf(const std::string &shelf_id)
{
  return this->get_data("/shelves/" + shelf_id);
}

const Json::Value ShelfController::get_shelves(const std::string &store_id)
{
  return this->get_data("stores/" + store_id + "/shelves");
}

const bool ShelfController::post_shelf(const std::string &in_store_id, const std::string &in_product_group_id, const Json::Value &in_shelf, Json::Value &out_shelf)
{
  Json::Value in_shelf_tmp = in_shelf;
  in_shelf_tmp["productGroupId"] = in_product_group_id;
  return this->post_data(in_shelf_tmp, out_shelf, "stores/" + in_store_id + "/shelves");
}

const bool ShelfController::put_shelf(const std::string &in_shelf_id, const std::string &in_store_id, const std::string &in_product_group_id, const Json::Value &in_shelf, Json::Value &out_shelf)
{
  Json::Value in_shelf_tmp = in_shelf;
  in_shelf_tmp["storeId"] = in_store_id;
  in_shelf_tmp["productGroupId"] = in_product_group_id;
  return this->put_data(in_shelf_tmp, out_shelf, "shelves/" + in_shelf_id);
}

const bool ShelfController::delete_shelf(const std::string &shelf_id)
{
  return this->delete_data("/shelves/" + shelf_id);
}