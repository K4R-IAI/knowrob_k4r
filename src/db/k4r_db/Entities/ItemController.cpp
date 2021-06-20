#pragma once

#include "DataController.cpp"

class ItemController : public DataController
{
public:
  ItemController();

public:
  const Json::Value get_item(const std::string &);
  const Json::Value get_items();

  const bool post_item(const Json::Value &, Json::Value &);
  const bool post_item(const std::string &, const Json::Value &, Json::Value &);

  const bool put_item(const std::string &, const Json::Value &, Json::Value &);
  const bool put_item(const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool delete_item(const std::string &);
};

ItemController::ItemController() : DataController::DataController("items/")
{
}

const Json::Value ItemController::get_item(const std::string &item_id)
{
  return this->get_data(item_id);
}

const Json::Value ItemController::get_items()
{
  return this->get_data();
}

const bool ItemController::post_item(const Json::Value &in_item, Json::Value &out_item)
{
  return this->post_data(in_item, out_item);
}

const bool ItemController::post_item(const std::string &in_item_group_id, const Json::Value &in_item, Json::Value &out_item)
{
  Json::Value in_item_tmp = in_item;
  in_item_tmp["itemGroupId"] = in_item_group_id;
  return this->post_item(in_item_tmp, out_item);
}

const bool ItemController::put_item(const std::string &in_item_id, const Json::Value &in_item, Json::Value &out_item)
{
  return this->put_data(in_item, out_item, in_item_id);
}

const bool ItemController::put_item(const std::string &in_item_id, const std::string &in_item_group_id, const Json::Value &in_item, Json::Value &out_item)
{
  Json::Value in_item_tmp = in_item;
  in_item_tmp["itemGroupId"] = in_item_group_id;
  return this->put_item(in_item_id, in_item_tmp, out_item);
}

const bool ItemController::delete_item(const std::string &item_id)
{
  return this->delete_data(item_id);
}