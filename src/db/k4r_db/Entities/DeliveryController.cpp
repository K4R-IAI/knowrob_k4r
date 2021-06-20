#pragma once

#include "DataController.cpp"

class DeliveryController : public DataController
{
public:
  DeliveryController();

public:
  const Json::Value get_delivery(const std::string &);
  const Json::Value get_deliveries();

  const bool post_delivery(const Json::Value &, Json::Value &);
  const bool post_delivery(const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool put_delivery(const std::string &, const Json::Value &, Json::Value &);
  const bool put_delivery(const std::string &, const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool delete_delivery(const std::string &);
};

DeliveryController::DeliveryController() : DataController::DataController("deliveries/")
{
}

const Json::Value DeliveryController::get_delivery(const std::string &delivery_id)
{
  return this->get_data(delivery_id);
}

const Json::Value DeliveryController::get_deliveries()
{
  return this->get_data();
}

const bool DeliveryController::post_delivery(const Json::Value &in_delivery, Json::Value &out_delivery)
{
  return this->post_data(in_delivery, out_delivery);
}

const bool DeliveryController::post_delivery(const std::string &in_logistical_unit_id, const std::string &in_store_id, const Json::Value &in_delivery, Json::Value &out_delivery)
{
  Json::Value in_delivery_tmp = in_delivery;
  in_delivery_tmp["logisticalUnitId"] = in_logistical_unit_id;
  in_delivery_tmp["storeId"] = in_store_id;
  return this->post_data(in_delivery_tmp, out_delivery);
}

const bool DeliveryController::put_delivery(const std::string &in_delivery_id, const Json::Value &in_delivery, Json::Value &out_delivery)
{
  return this->put_data(in_delivery, out_delivery, in_delivery_id);
}

const bool DeliveryController::put_delivery(const std::string &in_delivery_id, const std::string &in_logistical_unit_id, const std::string &in_store_id, const Json::Value &in_delivery, Json::Value &out_delivery)
{
  Json::Value in_delivery_tmp = in_delivery;
  in_delivery_tmp["logisticalUnitId"] = in_logistical_unit_id;
  in_delivery_tmp["storeId"] = in_store_id;
  return this->put_delivery(in_delivery_id, in_delivery_tmp, out_delivery);
}

const bool DeliveryController::delete_delivery(const std::string &delivery_id)
{
  return this->delete_data(delivery_id);
}