#pragma once

#include "DataController.cpp"

class MaterialGroupController : public DataController
{
public:
  MaterialGroupController();

public:
  const Json::Value get_material_group(const std::string &);
  const Json::Value get_material_groups();

  const bool post_material_group(const std::string &, const Json::Value &, Json::Value &);

  const bool put_material_group(const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool delete_material_group(const std::string &);
};

MaterialGroupController::MaterialGroupController() : DataController::DataController("materialgroups/")
{
}

const Json::Value MaterialGroupController::get_material_group(const std::string &material_group_id)
{
  return this->get_data(material_group_id);
}

const Json::Value MaterialGroupController::get_material_groups()
{
  return this->get_data();
}

const bool MaterialGroupController::post_material_group(const std::string &in_parent_id, const Json::Value &in_material_group, Json::Value &out_material_group)
{
  Json::Value in_material_group_tmp = in_material_group;
  in_material_group_tmp["parentId"] = in_parent_id;
  return this->post_data(in_material_group_tmp, out_material_group);
}

const bool MaterialGroupController::put_material_group(const std::string &in_material_group_id, const std::string &in_parent_id, const Json::Value &in_material_group, Json::Value &out_material_group)
{
  Json::Value in_material_group_tmp = in_material_group;
  in_material_group_tmp["parentId"] = in_parent_id;
  return this->put_data(in_material_group_tmp, out_material_group, in_material_group_id);
}

const bool MaterialGroupController::delete_material_group(const std::string &material_group_id)
{
  return this->delete_data(material_group_id);
}