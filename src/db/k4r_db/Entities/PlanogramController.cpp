#pragma once

#include "DataController.cpp"

class PlanogramController : public DataController
{
public:
  PlanogramController();

public:
  const Json::Value get_planograms();

  const bool post_planogram(const Json::Value &, Json::Value &);
  const bool post_planogram(const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool put_planogram(const std::string &, const Json::Value &, Json::Value &);
  const bool put_planogram(const std::string &, const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool delete_planogram(const std::string &);
};

PlanogramController::PlanogramController() : DataController::DataController("planograms/")
{
}

const Json::Value PlanogramController::get_planograms()
{
  return this->get_data();
}

const bool PlanogramController::post_planogram(const Json::Value &in_planogram, Json::Value &out_planogram)
{
  return this->post_data(in_planogram, out_planogram);
}

const bool PlanogramController::post_planogram(const std::string &in_logistical_unit_id, const std::string &shelf_layer_id, const Json::Value &in_planogram, Json::Value &out_planogram)
{
  Json::Value in_planogram_tmp = in_planogram;
  in_planogram_tmp["logisticalUnitId"] = in_logistical_unit_id;
  in_planogram_tmp["shelfLayerId"] = shelf_layer_id;
  return this->post_planogram(in_planogram_tmp, out_planogram);
}

const bool PlanogramController::put_planogram(const std::string &in_planogram_id, const Json::Value &in_planogram, Json::Value &out_planogram)
{
  return this->put_data(in_planogram, out_planogram, in_planogram_id);
}

const bool PlanogramController::put_planogram(const std::string &in_planogram_id, const std::string &in_logistical_unit_id, const std::string &shelf_layer_id, const Json::Value &in_planogram, Json::Value &out_planogram)
{
  Json::Value in_planogram_tmp = in_planogram;
  in_planogram_tmp["logisticalUnitId"] = in_logistical_unit_id;
  in_planogram_tmp["shelfLayerId"] = shelf_layer_id;
  return this->put_planogram(in_planogram_id, in_planogram_tmp, out_planogram);
}

const bool PlanogramController::delete_planogram(const std::string &planogram_id)
{
  return this->delete_data(planogram_id);
}