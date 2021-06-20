#pragma once

#include "DataController.cpp"

class MaterialGroupController : public DataController
{
public:
  MaterialGroupController();

public:
  const Json::Value create_material_group(const std::string &);
  const Json::Value create_material_group(const std::string &, const std::string &);

  const Json::Value get_material_group(const std::string &);
  const Json::Value get_material_groups();

  const bool post_material_group(const std::string &, Json::Value &);
  const bool post_material_group(const std::string &, const std::string &, Json::Value &);

  const bool put_material_group(const std::string &, const std::string &, Json::Value &);
  const bool put_material_group(const std::string &, const std::string &, const std::string &, Json::Value &);

  const bool delete_material_group(const std::string &);
};

MaterialGroupController::MaterialGroupController() : DataController::DataController("materialgroups/")
{
}

const Json::Value MaterialGroupController::create_material_group(const std::string &name)
{
  Json::Value material_group;
  material_group["name"] = name;
  return material_group;
}

const Json::Value MaterialGroupController::create_material_group(const std::string &parent_id, const std::string &name)
{
  Json::Value material_group = create_material_group(name);
  material_group["parentId"] = parent_id;
  return material_group;
}

const Json::Value MaterialGroupController::get_material_group(const std::string &material_group_id)
{
  return this->get_data(material_group_id);
}

const Json::Value MaterialGroupController::get_material_groups()
{
  return this->get_data();
}

const bool MaterialGroupController::post_material_group(const std::string &in_name, Json::Value &out_material_group)
{
  return this->post_data(this->create_material_group(in_name), out_material_group);
}

const bool MaterialGroupController::post_material_group(const std::string &in_parent_id, const std::string &in_name, Json::Value &out_material_group)
{
  return this->post_data(this->create_material_group(in_parent_id, in_name), out_material_group);
}

const bool MaterialGroupController::put_material_group(const std::string &in_material_group_id, const std::string &in_name, Json::Value &out_material_group)
{
  return this->put_data(this->create_material_group(in_name), out_material_group, in_material_group_id);
}

const bool MaterialGroupController::put_material_group(const std::string &in_material_group_id, const std::string &in_parent_id, const std::string &in_name, Json::Value &out_material_group)
{
  return this->put_data(this->create_material_group(in_parent_id, in_name), out_material_group, in_material_group_id);
}

const bool MaterialGroupController::delete_material_group(const std::string &material_group_id)
{
  return this->delete_data(material_group_id);
}