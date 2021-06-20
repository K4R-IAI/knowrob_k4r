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

  const bool post_item_group(const Json::Value &, Json::Value &);
  const bool post_item_group(const std::string &, const std::string &, const std::string &, Json::Value &);

  const bool put_item_group(const std::string &, const Json::Value &, Json::Value &);
  const bool put_item_group(const std::string &, const std::string &, const std::string &, const std::string &, Json::Value &);

  const bool delete_item_group(const std::string &);
};

ItemGroupController::ItemGroupController() : DataController::DataController("itemgroups/")
{
}

const Json::Value ItemGroupController::create_item_group(const std::string &facing_id, const std::string &logistical_unit_id, const std::string &stock)
{
  Json::Value item_group;
  item_group["facingId"] = facing_id;
  item_group["logisticalUnitId"] = logistical_unit_id;
  item_group["stock"] = stock;
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

const bool ItemGroupController::post_item_group(const Json::Value &in_item_group, Json::Value &out_item_group)
{
  return this->post_data(in_item_group, out_item_group);
}

const bool ItemGroupController::post_item_group(const std::string &in_facing_id, const std::string &logistical_unit_id, const std::string &in_stock, Json::Value &out_item_group)
{
  return this->post_item_group(this->create_item_group(in_facing_id, logistical_unit_id, in_stock), out_item_group);
}

const bool ItemGroupController::put_item_group(const std::string &in_item_group_id, const Json::Value &in_item_group, Json::Value &out_item_group)
{
  return this->put_data(in_item_group, out_item_group, in_item_group_id);
}

const bool ItemGroupController::put_item_group(const std::string &in_item_group_id, const std::string &in_facing_id, const std::string &logistical_unit_id, const std::string &in_stock, Json::Value &out_item_group)
{
  return this->put_item_group(in_item_group_id, this->create_item_group(in_facing_id, logistical_unit_id, in_stock), out_item_group);
}

const bool ItemGroupController::delete_item_group(const std::string &item_group_id)
{
  return this->delete_data(item_group_id);
}