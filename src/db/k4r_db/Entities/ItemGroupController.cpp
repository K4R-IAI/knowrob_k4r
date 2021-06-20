#pragma once

#include "DataController.cpp"

class ItemGroupController : public DataController
{
public:
  ItemGroupController();

public:
  const Json::Value create_item_group(const std::string &, const std::string &, const std::string &);

  const Json::Value get_item_group(const std::string &);
  const Json::Value get_item_groups();

  const bool post_item_group(const std::string &, const std::string &, const std::string &, Json::Value &);

  const bool put_item_group(const std::string &, const std::string &, const std::string &, Json::Value &);

  const bool delete_item_group(const std::string &);
};

ItemGroupController::ItemGroupController() : DataController::DataController("itemgroups/")
{
}

const Json::Value ItemGroupController::create_item_group(const std::string &facing_id, const std::string &logisticcal_unit_id, const std::string &stock)
{
  Json::Value item_group;
  item_group["facingId"] = facing_id;
  item_group["logisticalUnitId"] = facing_id;
  item_group["stock"] = facing_id;
  return item_group;
}

const Json::Value ItemGroupController::get_item_group(const std::string &item_group_id)
{
  return this->get_data(item_group_id);
}

const Json::Value ItemGroupController::get_item_groups()
{
  return this->get_data();
}

const bool ItemGroupController::post_item_group(const std::string &in_facing_id, const std::string &in_logisticcal_unit_id, const std::string &in_stock, Json::Value &out_item_group)
{
  return this->post_data(this->create_item_group(in_facing_id, in_logisticcal_unit_id, in_stock), out_item_group);
}

const bool ItemGroupController::put_item_group(const std::string &in_item_group_id, const std::string &in_anonymised_name, Json::Value &out_item_group)
{
  return this->put_data(this->create_item_group(in_anonymised_name), out_item_group, in_item_group_id);
}

const bool ItemGroupController::delete_item_group(const std::string &item_group_id)
{
  return this->delete_data(item_group_id);
}