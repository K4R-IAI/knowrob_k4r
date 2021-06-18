#pragma once

#include "LogisticalUnitController.cpp"

class ProductGtinController : public DataController
{
public:
  ProductGtinController();

public:
  const bool check_product_gtin_id(const std::string &);

  const bool check_product_gtin(const Json::Value &);

  const Json::Value create_product_gtin(const std::string &, const std::string &, const std::string &, const Json::Value &);

  const Json::Value get_product_gtin(const std::string &);
  const Json::Value get_product_gtins();

  const bool post_product_gtin(const Json::Value &, Json::Value &);
  const bool post_product_gtin(const std::string &, const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool put_product_gtin(const std::string &, const Json::Value &, Json::Value &);
  const bool put_product_gtin(const std::string &, const std::string &, const std::string &, const Json::Value &, Json::Value &);

  const bool delete_product_gtin(const std::string &);

private:
  LogisticalUnitController logistical_unit_controller;
  ProductController product_controller;
};

ProductGtinController::ProductGtinController() : DataController::DataController("productgtins/")
{
}

const bool ProductGtinController::check_product_gtin_id(const std::string &product_gtin_id)
{
  Json::Value product_gtin = this->get_product_gtin(product_gtin_id);

  if (product_gtin["id"].asString() == product_gtin_id)
  {
    return true;
  }
  else
  {
    std::cerr << "ProductGtin with id " << product_gtin_id << " not found" << std::endl;
    return false;
  }
}

const bool ProductGtinController::check_product_gtin(const Json::Value &product_gtin)
{
  std::string product_gtin_id = product_gtin["id"].asString();
  std::string logistical_unit_id = product_gtin["logisticalUnitId"].asString();
  std::string product_id = product_gtin["productId"].asString();
  if (this->logistical_unit_controller.check_logistical_unit_id(logistical_unit_id) &&
      this->product_controller.check_product_id(product_id))
  {
    for (const Json::Value &product_gtin_existed : this->get_product_gtins())
    {
      if (product_gtin_existed["id"].asString() == product_gtin_id ||
          (product_gtin_existed["logisticalUnitId"].asString() == logistical_unit_id &&
            product_gtin_existed["productId"].asString() == product_id))
      {
        std::cerr << "Product gtin \n" << product_gtin << "\nalready existed" << std::endl;
        return false;
      }
    }
    return false;
  }
  else
  {
    return false;
  }
}

const Json::Value ProductGtinController::create_product_gtin(const std::string &product_gtin_id, const std::string &logistical_unit_id, const std::string &product_id, const Json::Value &product_gtin)
{
  Json::Value product_gtin_tmp = product_gtin;
  product_gtin_tmp["id"] = product_gtin_id;
  product_gtin_tmp["logisticalUnitId"] = logistical_unit_id;
  product_gtin_tmp["productId"] = product_id;
  return product_gtin_tmp;
}

const Json::Value ProductGtinController::get_product_gtin(const std::string &product_gtin_id)
{
  return this->get_data(product_gtin_id);
}

const Json::Value ProductGtinController::get_product_gtins()
{
  return this->get_data();
}

const bool ProductGtinController::post_product_gtin(const Json::Value &in_product_gtin, Json::Value &out_product_gtin)
{
  return this->post_data(in_product_gtin, out_product_gtin) ||
         this->check_product_gtin(in_product_gtin);
}

const bool ProductGtinController::post_product_gtin(const std::string &in_product_gtin_id, const std::string &in_logistical_unit_id, const std::string &in_product_id, const Json::Value &in_product_gtin, Json::Value &out_product_gtin)
{
  return this->post_product_gtin(this->create_product_gtin(in_product_gtin_id, in_logistical_unit_id, in_product_id, in_product_gtin), out_product_gtin);
}

const bool ProductGtinController::put_product_gtin(const std::string &in_product_gtin_id, const Json::Value &in_product_gtin, Json::Value &out_product_gtin)
{
  // return this->check_product_gtin_id(in_product_gtin_id) && 
  // this->check_product_gtin(in_product_gtin) && 
  // this->put_data(in_product_gtin, out_product_gtin, in_product_gtin_id);
  this->check_product_gtin_id(in_product_gtin_id);
  this->put_data(in_product_gtin, out_product_gtin, in_product_gtin_id) ||
         (this->check_product_gtin(in_product_gtin) && 
         this->check_product_gtin_id(in_product_gtin_id));
         this->check_product_gtin_id(in_product_gtin_id);
  return false;
}

const bool ProductGtinController::put_product_gtin(const std::string &in_product_gtin_id, const std::string &in_logistical_unit_id, const std::string &in_product_id, const Json::Value &in_product_gtin, Json::Value &out_product_gtin)
{
  return this->put_product_gtin(in_product_gtin_id, this->create_product_gtin(in_product_gtin_id, in_logistical_unit_id, in_product_id, in_product_gtin), out_product_gtin);
}

const bool ProductGtinController::delete_product_gtin(const std::string &product_gtin_id)
{
  return this->delete_data(product_gtin_id) || this->check_product_gtin_id(product_gtin_id);
}