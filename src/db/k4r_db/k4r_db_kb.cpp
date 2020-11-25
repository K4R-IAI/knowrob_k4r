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
#include "Entities/ShoppingBasketController.cpp"
#include "Entities/StoreController.cpp"
#include "Entities/FacingController.cpp"

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

// Customer

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
  std::string customer_id = customer["id"].asString();
  remove_new_line(customer_id);
  if (std::stoi(std::string(PL_A2)) == std::stoi(customer_id))
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

PREDICATE(k4r_put_customer, 3)
{
  CustomerController customers(PL_A1);
  return customers.put_customer(std::string(PL_A2), std::string(PL_A3));
}

PREDICATE(k4r_delete_customer, 2)
{
  CustomerController customers(PL_A1);
  return customers.delete_customer(std::string(PL_A2));
}

// Store

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
  std::string store_id = store["id"].asString();
  remove_new_line(store_id);
  if (std::stoi(std::string(PL_A2)) == std::stoi(store_id))
  {
    PL_A3 = store.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

PREDICATE(k4r_post_store, 2)
{
  StoreController stores(PL_A1);
  Json::Value store = char_to_json((char*)PL_A2);
  return stores.post_store(store);
}

PREDICATE(k4r_put_store, 3)
{
  StoreController stores(PL_A1);
  Json::Value store = char_to_json((char*)PL_A3);
  return stores.put_store(std::string(PL_A2), store);
}

PREDICATE(k4r_delete_store, 2)
{
  StoreController stores(PL_A1);
  return stores.delete_store(std::string(PL_A2));
}

// Product

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
  std::string product_id = product["id"].asString();
  remove_new_line(product_id);
  if (std::string(PL_A2) == product_id)
  {
    PL_A3 = product.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

PREDICATE(k4r_post_products, 2)
{
  ProductController products(PL_A1);
  std::cout << std::string(PL_A2) << std::endl;
  Json::Value product = char_to_json((char*)PL_A2);
  return products.post_products(product);
}

PREDICATE(k4r_put_product, 3)
{
  ProductController products(PL_A1);
  Json::Value product = char_to_json((char*)PL_A2);
  return products.put_product(std::string(PL_A3), product);
}

PREDICATE(k4r_post_product, 3)
{
  ProductController products(PL_A1);
  Json::Value product = char_to_json((char*)PL_A2);
  return products.post_product(std::string(PL_A3), product);
}

PREDICATE(k4r_delete_product, 2)
{
  ProductController products(PL_A1);
  return products.delete_product(std::string(PL_A2));
}

// Characteristic

PREDICATE(k4r_get_characteristics, 2) {
  CharacteristicController characteristics(PL_A1);

  PlTail values(PL_A2);
  for (const Json::Value& characteristic : characteristics.get_characteristics())
  {
    values.append(characteristic.toStyledString().c_str());
  }
  return values.close();
}

PREDICATE(k4r_post_characteristic, 2)
{
  CharacteristicController characteristics(PL_A1);
  return characteristics.post_characteristic(std::string(PL_A2));
}

PREDICATE(k4r_delete_characteristic, 2)
{
  CharacteristicController characteristics(PL_A1);
  return characteristics.delete_characteristic(std::string(PL_A2));
}

// Property

PREDICATE(k4r_get_properties, 4) {
  PropertyController properties(PL_A1, std::string(PL_A2), std::string(PL_A3));

  PlTail values(PL_A4);
  for (const Json::Value& property : properties.get_properties())
  {
    values.append(property.toStyledString().c_str());
  }
  return values.close();
}

PREDICATE(k4r_post_property, 5)
{
  std::string store_id(PL_A2);
  remove_new_line(store_id);
  std::string product_id(PL_A3);
  remove_new_line(product_id);
  std::string characteristic_id(PL_A4);
  remove_new_line(characteristic_id);
  PropertyController properties(PL_A1, store_id, product_id, characteristic_id);
  return properties.post_property(std::string(PL_A5));
}

PREDICATE(k4r_delete_property, 4)
{
  PropertyController properties(PL_A1, std::string(PL_A2), std::string(PL_A3));
  return properties.delete_property(std::string(PL_A4));
}

// Shelf

PREDICATE(k4r_get_shelves, 3) {
  ShelfController shelves(PL_A1, std::string(PL_A2));

  PlTail values(PL_A3);
  for (const Json::Value& shelf : shelves.get_shelves())
  {
    values.append(shelf.toStyledString().c_str());
  }
  return values.close();
}

PREDICATE(k4r_get_shelf_by_id, 3)
{
  ShelfController shelves(PL_A1);

  Json::Value shelf = shelves.get_shelf(std::string(PL_A2));
  std::string shelf_id = shelf["id"].asString();
  remove_new_line(shelf_id);
  if (std::stoi(std::string(PL_A2)) == std::stoi(shelf_id))
  {
    PL_A3 = shelf.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

PREDICATE(k4r_post_shelf, 3)
{
  ShelfController shelves(PL_A1, std::string(PL_A2));
  Json::Value shelf = char_to_json((char*)PL_A3);
  return shelves.post_shelf(shelf);
}

PREDICATE(k4r_put_shelf, 3)
{
  ShelfController shelves(PL_A1);
  Json::Value shelf = char_to_json((char*)PL_A3);
  return shelves.put_shelf(std::string(PL_A2), shelf);
}

PREDICATE(k4r_delete_shelf, 2)
{
  ShelfController shelves(PL_A1);
  return shelves.delete_shelf(std::string(PL_A2));
}

PREDICATE(k4r_get_shelf_location, 8)
{
  Json::Value shelf = char_to_json((char*)PL_A1);
  PL_A2 = shelf["positionX"].asString().c_str();
  PL_A3 = shelf["positionY"].asString().c_str();
  PL_A4 = shelf["positionZ"].asString().c_str();
  PL_A5 = shelf["orientationx"].asString().c_str();
  PL_A6 = shelf["orientationY"].asString().c_str();
  PL_A7 = shelf["orientationZ"].asString().c_str();
  PL_A8 = shelf["orientationYaw"].asString().c_str();
  return true;
}

// Shelf layer

PREDICATE(k4r_get_shelf_layers, 3) {
  ShelfLayerController shelf_layers(PL_A1, std::string(PL_A2));

  PlTail values(PL_A3);
  for (const Json::Value& shelf_layer : shelf_layers.get_shelf_layers())
  {
    values.append(shelf_layer.toStyledString().c_str());
  }
  return values.close();
}

PREDICATE(k4r_get_shelf_layer_by_id, 3)
{
  ShelfLayerController shelf_layers(PL_A1);

  Json::Value shelf_layer = shelf_layers.get_shelf_layer(std::string(PL_A2));
  std::string shelf_layer_id = shelf_layer["id"].asString();
  remove_new_line(shelf_layer_id);
  if (std::stoi(std::string(PL_A2)) == std::stoi(shelf_layer_id))
  {
    PL_A3 = shelf_layer.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

PREDICATE(k4r_post_shelf_layer, 3)
{
  ShelfLayerController shelf_layers(PL_A1, std::string(PL_A2));
  Json::Value shelf_layer = char_to_json((char*)PL_A3);
  return shelf_layers.post_shelf_layer(shelf_layer);
}

PREDICATE(k4r_delete_shelf_layer, 2)
{
  ShelfLayerController shelf_layers(PL_A1);
  return shelf_layers.delete_shelf_layer(std::string(PL_A2));
}

// Shopping basket

PREDICATE(k4r_get_shopping_baskets, 4)
{
  ShoppingBasketController shopping_baskets(PL_A1, std::string(PL_A2), std::string(PL_A3));

  PlTail values(PL_A4);
  for (const Json::Value& shopping_basket : shopping_baskets.get_shopping_baskets())
  {
    values.append(shopping_basket.toStyledString().c_str());
  }
  return values.close();
}

PREDICATE(k4r_get_shopping_basket_by_id, 5)
{
  ShoppingBasketController shopping_baskets(PL_A1, std::string(PL_A2), std::string(PL_A3), std::string(PL_A4));

  Json::Value shopping_basket = shopping_baskets.get_shopping_basket();
  std::string product_id = shopping_basket["productId"].asString();
  remove_new_line(product_id);
  if (std::string(PL_A4) == product_id)
  {
    PL_A5 = shopping_basket.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

PREDICATE(k4r_post_shopping_basket, 4)
{
  ShoppingBasketController shopping_baskets(PL_A1, std::string(PL_A2), std::string(PL_A3));
  std::cout << (char*)PL_A4 << std::endl;
  Json::Value shopping_basket = char_to_json((char*)PL_A4);
  return shopping_baskets.post_shopping_basket(shopping_basket);
}

PREDICATE(k4r_delete_shopping_baskets, 3)
{
  ShoppingBasketController shopping_baskets(PL_A1, std::string(PL_A2), std::string(PL_A3));
  return shopping_baskets.delete_shopping_baskets();
}

PREDICATE(k4r_delete_shopping_basket, 4)
{
  ShoppingBasketController shopping_baskets(PL_A1, std::string(PL_A2), std::string(PL_A3), std::string(PL_A4));
  return shopping_baskets.delete_shopping_basket();
}

// Facing

PREDICATE(k4r_get_facings, 3)
{
  FacingController facings(PL_A1, std::string(PL_A2));

  PlTail values(PL_A3);
  for (const Json::Value& facing : facings.get_facings())
  {
    values.append(facing.toStyledString().c_str());
  }
  return values.close();
}

PREDICATE(k4r_get_facing_by_id, 3)
{
  FacingController facings(PL_A1);

  Json::Value facing = facings.get_facing(std::string(PL_A2));
  std::string facing_id = facing["id"].asString();
  remove_new_line(facing_id);
  if (std::stoi(std::string(PL_A2)) == std::stoi(facing_id))
  {
    PL_A3 = facing.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

PREDICATE(k4r_post_facing, 3)
{
  FacingController facings(PL_A1, std::string(PL_A2));
  Json::Value facing = char_to_json((char*)PL_A3);
  return facings.post_facing(facing);
}

PREDICATE(k4r_put_facing, 3)
{
  FacingController facings(PL_A1);
  Json::Value facing = char_to_json((char*)PL_A3);
  return facings.put_facing(std::string(PL_A2), facing);
}

PREDICATE(k4r_delete_facing, 2)
{
  FacingController facings(PL_A1);
  return facings.delete_facing(std::string(PL_A2));
}