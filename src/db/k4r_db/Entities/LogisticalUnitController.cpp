#pragma once

#include "DataController.cpp"

class LogisticalUnitController : public DataController
{
public:
  LogisticalUnitController();

public:
  const Json::Value create_logistical_unit(const std::string &, const std::string &, const Json::Value &);

  const Json::Value get_logistical_unit(const std::string &);
  const Json::Value get_logistical_units();

  const bool post_logistical_unit(const Json::Value &, Json::Value &);
  const bool post_logistical_unit(const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool put_logistical_unit(const std::string &, const Json::Value &, Json::Value &);
  const bool put_logistical_unit(const std::string &, const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool delete_logistical_unit(const std::string &);
};

LogisticalUnitController::LogisticalUnitController() : DataController::DataController("logisticalunits/")
{
}

const Json::Value LogisticalUnitController::create_logistical_unit(const std::string &predecessor_id, const std::string &product_id, const Json::Value &logistical_unit)
{
  Json::Value logistical_unit_tmp = logistical_unit;
  logistical_unit_tmp["predecessorId"] = predecessor_id;
  logistical_unit_tmp["productId"] = product_id;
  return logistical_unit_tmp;
}

const Json::Value LogisticalUnitController::get_logistical_unit(const std::string &logistical_unit_id)
{
  return this->get_data(logistical_unit_id);
}

const Json::Value LogisticalUnitController::get_logistical_units()
{
  return this->get_data();
}

const bool LogisticalUnitController::post_logistical_unit(const Json::Value &in_logistical_unit, Json::Value &out_logistical_unit)
{
  return this->post_data(in_logistical_unit, out_logistical_unit);
}

const bool LogisticalUnitController::post_logistical_unit(const std::string &in_predecessor_id, const std::string &in_product_id, const Json::Value &in_logistical_unit, Json::Value &out_logistical_unit)
{
  return this->post_data(this->create_logistical_unit(in_predecessor_id, in_product_id, in_logistical_unit), out_logistical_unit);
}

const bool LogisticalUnitController::put_logistical_unit(const std::string &in_logistical_unit_id, const Json::Value &in_logistical_unit, Json::Value &out_logistical_unit)
{
  return this->put_data(in_logistical_unit, out_logistical_unit, in_logistical_unit_id);
}

const bool LogisticalUnitController::put_logistical_unit(const std::string &in_logistical_unit_id, const std::string &in_predecessor_id, const std::string &in_product_id, const Json::Value &in_logistical_unit, Json::Value &out_logistical_unit)
{
  return this->put_data(this->create_logistical_unit(in_predecessor_id, in_product_id, in_logistical_unit), out_logistical_unit, in_logistical_unit_id);
}

const bool LogisticalUnitController::delete_logistical_unit(const std::string &logistical_unit_id)
{
  return this->delete_data(logistical_unit_id);
}