#pragma once

#include "EntityController.cpp"

class CustomerController : public EntityController
{
public:
  CustomerController(const char*);

  Json::Value create_customer(const std::string&);

  Json::Value get_customer(const std::string&);
  Json::Value get_customers();

  bool post_customer(const std::string&);

  bool put_customer(const std::string&, const std::string&); // Not working, will be fixed

  bool delete_customer(const std::string&);
};

CustomerController::CustomerController(const char* link) : EntityController::EntityController((std::string(link) + "customers/").c_str())
{
}

Json::Value CustomerController::get_customer(const std::string& customer_id)
{
  return this->get_entity(customer_id);
}

Json::Value CustomerController::create_customer(const std::string& customer_name)
{
  Json::Value costumer;
  costumer["anonymisedName"] = customer_name;
  return costumer;
}

bool CustomerController::post_customer(const std::string& customer_name)
{
  return this->post_entity(this->create_customer(customer_name));
}

bool CustomerController::put_customer(const std::string& customer_id, const std::string& customer_name)
{
  if (this->get_customer(customer_id).isNull())
  {
    std::cout << "Customer with id " << customer_id << " not found" << std::endl;
    return false;
  }
  else
  {
    return this->put_entity(this->create_customer(customer_name), customer_id);
  }
}

bool CustomerController::delete_customer(const std::string& customer_id)
{
  return this->delete_entity(customer_id);
}

Json::Value CustomerController::get_customers()
{
  return this->get_entity();
}