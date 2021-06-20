#pragma once

#include "DataController.cpp"

class FacingController : public DataController
{
public:
  FacingController();

public:
  const Json::Value get_facing(const std::string &);
  const Json::Value get_facings(const std::string &);
  const Json::Value get_facings();

  const bool post_facing(const std::string &, const Json::Value &, Json::Value &);

  const bool put_facing(const std::string &, const Json::Value &, Json::Value &);
  const bool put_facing(const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool delete_facing(const std::string &);
};

FacingController::FacingController() : DataController::DataController()
{
}

const Json::Value FacingController::get_facing(const std::string &facing_id)
{
  return this->get_data("facings/" + facing_id);
}

const Json::Value FacingController::get_facings(const std::string &shelf_layer_id)
{
  return this->get_data("shelflayers/" + shelf_layer_id + "/facings");
}

const bool FacingController::post_facing(const std::string &in_shelf_layer_id, const Json::Value &in_facing, Json::Value &out_facing)
{
  return this->post_data(in_facing, out_facing, "shelflayers/" + in_shelf_layer_id + "/facings");
}

const bool FacingController::put_facing(const std::string &in_facing_id, const Json::Value &in_facing, Json::Value &out_facing)
{
  return this->put_data(in_facing, out_facing,"facings/" + in_facing_id);
}

const bool FacingController::put_facing(const std::string &in_facing_id, const std::string &in_shelf_layer_id, const Json::Value &in_facing, Json::Value &out_facing)
{
  Json::Value in_facing_tmp = in_facing;
  in_facing_tmp["shelfLayerId"] = in_shelf_layer_id;
  return this->put_facing(in_facing_id, in_facing, out_facing);
}

const bool FacingController::delete_facing(const std::string &facing_id)
{
  return this->delete_data("facings/" + facing_id);
}