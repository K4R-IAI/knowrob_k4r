/* 
 * Copyright (c) 2020, Sascha Jongebloed
 * All rights reserved.
 * 
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#define PL_SAFE_ARG_MACROS
#include <SWI-cpp.h>

// For K4R
#include "Entities/CharacteristicController.cpp"
#include "Entities/CustomerController.cpp"
#include "Entities/ProductController.cpp"
#include "Entities/PropertyController.cpp"
#include "Entities/ShelfController.cpp"
#include "Entities/ShelfLayerController.cpp"
#include "Entities/StoreController.cpp"

PREDICATE(k4r_get_link, 1) {
  PL_A1 = "http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/";
}

Json::Value char_to_json(const char* entity_in)
{
  Json::Value entity_out;
  Json::Reader reader;
  reader.parse(entity_in, entity_out);
  return entity_out;
}

PREDICATE(k4r_get_value_from_key, 3)
{
  Json::Value entity = char_to_json((char*)PL_A1);
  Json::Value value = entity[std::string(PL_A2)];
  if (value.isNull())
  {
    return false;
  }
  else
  {
    PL_A3 = value.toStyledString().c_str();
    return true;
  }
}

PREDICATE(k4r_check_key_value, 3)
{
  Json::Value entity = char_to_json((char*)PL_A1);
  return (entity[std::string(PL_A2)].asString() == std::string(PL_A3));
}

PREDICATE(k4r_get_customers, 2)
{
  CustomerController customers(PL_A1);

  PlTail values(PL_A2);
  for (const Json::Value& customer : customers.get_customers())
  {
    values.append(customer.toStyledString().c_str());
  }
  return values.close();
}

PREDICATE(k4r_get_customer_by_id, 3)
{
  CustomerController customers(PL_A1);

  Json::Value customer = customers.get_customer(std::string(PL_A2));
  if (std::stoi(std::string(PL_A2)) == customer["id"].asInt())
  {
    PL_A3 = customer.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

PREDICATE(k4r_post_customer, 2)
{
  CustomerController customers(PL_A1);
  return customers.post_customer(std::string(PL_A2));
}

PREDICATE(k4r_delete_customer, 2)
{
  CustomerController customers(PL_A1);
  return customers.delete_customer(std::string(PL_A2));
}

PREDICATE(k4r_get_stores, 2) {
  StoreController stores(PL_A1);

  PlTail values(PL_A2);
  for (const Json::Value& store : stores.get_stores())
  {
    values.append(store.toStyledString().c_str());
  }
  return values.close();
}

PREDICATE(k4r_get_store_by_id, 3)
{
  StoreController stores(PL_A1);

  Json::Value store = stores.get_store(std::string(PL_A2));
  if (std::stoi(std::string(PL_A2)) == store["id"].asInt())
  {
    PL_A3 = store.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

PREDICATE(k4r_post_store, 3)
{
  StoreController stores(PL_A1);
  Json::Value entity = char_to_json((char*)PL_A2);
  return stores.post_store(entity);
}

PREDICATE(k4r_delete_store, 2)
{
  StoreController stores(PL_A1);
  return stores.delete_store(std::string(PL_A2));
}

PREDICATE(k4r_get_products, 2) {
  ProductController products(PL_A1);

  PlTail values(PL_A2);
  for (const Json::Value& product : products.get_products())
  {
    values.append(product.toStyledString().c_str());
  }
  return values.close();
}

PREDICATE(k4r_get_product_by_id, 3)
{
  ProductController products(PL_A1);

  Json::Value product = products.get_product(std::string(PL_A2));
  if (std::stoi(std::string(PL_A2)) == product["id"].asInt())
  {
    PL_A3 = product.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

PREDICATE(k4r_post_products, 3)
{
  ProductController products(PL_A1);
  Json::Value entity = char_to_json((char*)PL_A2);
  return products.post_product(entity, std::string(PL_A3));
}

PREDICATE(k4r_post_product, 3)
{
  ProductController products(PL_A1);
  Json::Value entity = char_to_json((char*)PL_A2);
  return products.post_product(entity, std::string(PL_A3));
}

PREDICATE(k4r_delete_product, 2)
{
  ProductController products(PL_A1);
  return products.delete_product(std::string(PL_A2));
}

