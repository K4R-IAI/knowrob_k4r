#pragma once

#include "ProductController.cpp"
#include "StoreController.cpp"

class ProductGroupController : public EntityController
{
private:
  StoreController* store_controller;
  ProductController* product_controller;

  std::string store_id;
  std::string product_id;
  std::string product_group_id;

public:
  ProductGroupController(const char*);
  ProductGroupController(const char*, const std::string, const std::string, const std::string);

  bool set_store(const std::string&);
  bool set_product(const std::string&);
  bool set_product_group(const std::string&);

  Json::Value get_product_group(const std::string&);
  Json::Value get_product_groups(const std::string&);
  Json::Value get_product_groups();

  bool post_product_to_product_group(const std::string&, const std::string&);
  bool post_product_to_product_group();
  bool post_product_group(const std::string&, const std::string&);
  bool post_product_group(const std::string&);

  bool delete_product_group(const std::string&);
  bool delete_product_from_product_group(const std::string&, const std::string&);
  bool delete_product_from_product_group(const std::string&);
  bool delete_product_from_product_group();
};

ProductGroupController::ProductGroupController(const char* link) : EntityController::EntityController(link)
{
  store_controller = new StoreController(link);
  product_controller = new ProductController(link);
}

ProductGroupController::ProductGroupController(const char* link, const std::string store_id, const std::string product_group_id, const std::string product_id) : ProductGroupController::ProductGroupController(link)
{
  this->set_store(store_id);
  this->set_product_group(product_group_id);
  this->set_product(product_id);
}

bool ProductGroupController::set_store(const std::string& store_id)
{
  Json::Value store = this->store_controller->get_store(store_id);
  if (store["id"].asString() == store_id)
  {
    this->store_id = store_id;
    return true;
  }
  else
  {
    std::cout << "Store with given Id does not exist" << std::endl;
    return false;
  }
}

bool ProductGroupController::set_product_group(const std::string& product_group_id)
{
  Json::Value product_group = this->get_product_group(product_group_id);
  if (product_group["id"].asString() == product_group_id)
  {
    this->product_group_id = product_group_id;
    return true;
  }
  else
  {
    std::cout << "Product group with given Id on given Store id does not exist" << std::endl;
    return false;
  }
}

bool ProductGroupController::set_product(const std::string& product_id)
{
  Json::Value product = this->product_controller->get_product(product_id);
  if (product["id"].asString() == product_id)
  {
    this->product_id = product_id;
    return true;
  }
  else
  {
    std::cout << "Product with given Id does not exist" << std::endl;
    return false;
  }
}

Json::Value ProductGroupController::get_product_group(const std::string& product_group_id)
{
  std::string link_tail = "productgroups/" + product_group_id;
  return this->get_entity(link_tail);
}

Json::Value ProductGroupController::get_product_groups(const std::string& store_id)
{
  if (this->set_store(store_id))
  {
    return this->get_product_groups();
  }
  else
  {
    return false;
  }
}

Json::Value ProductGroupController::get_product_groups()
{
  std::string link_tail = "stores/" + this->store_id + "/productgroups";
  return this->get_entity(link_tail);
}

bool ProductGroupController::post_product_to_product_group(const std::string& product_group_id, const std::string& product_id)
{
  return this->set_product_group(product_group_id) && this->set_product(product_id) && this->post_product_to_product_group();
}

bool ProductGroupController::post_product_to_product_group()
{
  std::string link_tail = "productgroups/" + this->product_group_id + "/products/" + this->product_id;
  return this->post_entity(Json::Value(), link_tail);
}

bool ProductGroupController::post_product_group(const std::string& store_id, const std::string& product_group_name)
{
  return this->set_store(store_id) && this->post_product_group(product_group_name);
}

bool ProductGroupController::post_product_group(const std::string& product_group_name)
{
  Json::Value product_group;
  product_group["name"] = product_group_name;
  std::string link_tail = "stores/" + this->store_id + "/productgroups";
  return this->post_entity(product_group, link_tail);
}

bool ProductGroupController::delete_product_group(const std::string& product_group_id)
{
  std::string link_tail = "productgroups/" + product_group_id;
  return this->delete_entity(link_tail);
}

bool ProductGroupController::delete_product_from_product_group(const std::string& product_group_id, const std::string& product_id)
{
  return this->set_product(product_id) && this->set_product_group(product_group_id) && this->delete_product_from_product_group();
}

bool ProductGroupController::delete_product_from_product_group(const std::string& product_id)
{
  return this->set_product(product_id) && this->delete_product_from_product_group();
}

bool ProductGroupController::delete_product_from_product_group()
{
  std::string link_tail = "productgroups/" + this->product_group_id + "/products/" + this->product_id;
  return this->delete_entity(link_tail);
}