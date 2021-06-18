#pragma once

#include "DataController.cpp"

class CustomerController : public DataController
{
public:
  CustomerController();

public:
  const Json::Value create_customer(const std::string &);

  const Json::Value get_customer(const std::string &);
  const Json::Value get_customers();

  const bool post_customer(const std::string &, Json::Value &);

  const bool put_customer(const std::string &, const std::string &, Json::Value &);

  const bool delete_customer(const std::string &);
};

CustomerController::CustomerController() : DataController::DataController("customers/")
{
}

const Json::Value CustomerController::create_customer(const std::string &anonymised_name)
{
  Json::Value costumer;
  costumer["anonymisedName"] = anonymised_name;
  return costumer;
}

const Json::Value CustomerController::get_customer(const std::string &customer_id)
{
  return this->get_data(customer_id);
}

const Json::Value CustomerController::get_customers()
{
  return this->get_data();
}

const bool CustomerController::post_customer(const std::string &in_anonymised_name, Json::Value &out_customer)
{
  return this->post_data(this->create_customer(in_anonymised_name), out_customer);
}

const bool CustomerController::put_customer(const std::string &in_customer_id, const std::string &in_anonymised_name, Json::Value &out_customer)
{
  return this->put_data(this->create_customer(in_anonymised_name), out_customer, in_customer_id) || this->check_customer_id(in_customer_id);
}

const bool CustomerController::delete_customer(const std::string &customer_id)
{
  return this->delete_data(customer_id) || this->check_customer_id(customer_id);
}