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
#include "Entities/ShoppingBasketPositionController.cpp"
#include "Entities/StoreController.cpp"
#include "Entities/FacingController.cpp"
#include "Entities/PlanogramController.cpp"
#include "Entities/ProductGroupController.cpp"

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
    std::string entity_value(value.toStyledString());
    remove_new_line(entity_value);
    PL_A3 = entity_value.c_str();
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

// k4R_get_shelves(Link, StoreId, Shelves)
PREDICATE(k4r_get_shelves, 3) {
  ShelfController shelves(PL_A1, std::string(PL_A2));

  PlTail values(PL_A3);
  for (const Json::Value& shelf : shelves.get_shelves())
  {
    values.append(shelf.toStyledString().c_str());
  }
  return values.close();
}

// k4r_get_shelf_by_id(Link, ShelfId, StoreId, ShapeData, Pose)
PREDICATE(k4r_get_shelf_by_id, 5)
{
  ShelfController shelves(PL_A1);
  PlTerm pose_term;
  PlTail pose_list(pose_term);

  Json::Value shelf = shelves.get_shelf(std::string(PL_A2));
  std::string shelf_id = shelf["id"].asString();
  remove_new_line(shelf_id);
  if (std::stoi(std::string(PL_A2)) == std::stoi(shelf_id))
  { 
    PlTerm shape_term;
    PlTail shape(shape_term);

    shape.append(shelf["depth"].asFloat());
    shape.append(shelf["width"].asFloat());
    shape.append(shelf["height"].asFloat());

    PL_A3 = shelf["storeId"].asInt();

    PlTerm pos_term;

    PlTail position_list(pos_term);

    position_list.append(shelf["positionX"].asDouble());
    position_list.append(shelf["positionY"].asDouble());
    position_list.append(shelf["positionZ"].asDouble());
    position_list.close();

    PlTerm rotation_term;

    PlTail rotation_list(rotation_term);

    // rotation_list.append(shelf["orientationX"].asDouble());
    rotation_list.append(shelf["orientationY"].asDouble());
    rotation_list.append(shelf["orientationZ"].asDouble());
    //rotation_list.append(shelf["orientationW"].asDouble());

    pose_list.append(pos_term);
    pose_list.append(rotation_term);

    PL_A4 = shape_term;
    PL_A5 = pose_term;

    return true;
  }
  else
  {
    return false;
  }
}

// k4r_post_shelf(Link, StoreId, ShelfId, [Translation, Rotation], [D,W,H])
PREDICATE(k4r_post_shelf, 5)
{
  ShelfController shelves(PL_A1, std::string(PL_A2));

  Json::Value shelf_data = shelves.get_shelf(std::string(PL_A3));
  shelf_data["storeId"] = (int)PL_A2;

  PlTail pose_list(PL_A4);
  PlTerm temp_term, traversal_term;
  
  pose_list.next(temp_term);

  PlTail translation(temp_term);

  translation.next(traversal_term);
  shelf_data["positionX"] = (double)traversal_term;
  translation.next(traversal_term);
  shelf_data["positionY"] = (double)traversal_term;
  translation.next(traversal_term);
  shelf_data["positionZ"] = (double)traversal_term;

  pose_list.next(temp_term);

  PlTail rotation(temp_term);

  rotation.next(traversal_term);
  shelf_data["orientationX"] = (double)traversal_term;
  rotation.next(traversal_term);
  shelf_data["orientationY"] = (double)traversal_term;
  rotation.next(traversal_term);
  shelf_data["orientationZ"] = (double)traversal_term;
  rotation.next(traversal_term);
  shelf_data["orientationW"] = (double)traversal_term;

  PlTail dimension(PL_A5);
  dimension.next(traversal_term);
  shelf_data["depth"] = (int)traversal_term;
  dimension.next(traversal_term);
  shelf_data["width"] = (int)traversal_term;
  dimension.next(traversal_term);
  shelf_data["height"] = (int)traversal_term;

  return shelves.post_shelf(shelf_data);
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
  PL_A5 = shelf["orientationX"].asString().c_str();
  PL_A6 = shelf["orientationY"].asString().c_str();
  PL_A7 = shelf["orientationZ"].asString().c_str();
  PL_A8 = shelf["orientationW"].asString().c_str();
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

PREDICATE(k4r_get_shopping_basket_positions, 4)
{
  ShoppingBasketPositionController shopping_basket_positions(PL_A1, std::string(PL_A2), std::string(PL_A3));

  PlTail values(PL_A4);
  for (const Json::Value& shopping_basket_position : shopping_basket_positions.get_shopping_basket_positions())
  {
    values.append(shopping_basket_position.toStyledString().c_str());
  }
  return values.close();
}

PREDICATE(k4r_get_shopping_basket_position_by_id, 3)
{
  ShoppingBasketPositionController shopping_basket_positions(PL_A1);

  Json::Value shopping_basket_position = shopping_basket_positions.get_shopping_basket_position(std::string(PL_A2));
  std::string shopping_basket_position_id = shopping_basket_position["id"].asString();
  remove_new_line(shopping_basket_position_id);
  if (std::stoi(std::string(PL_A2)) == std::stoi(shopping_basket_position_id))
  {
    PL_A3 = shopping_basket_position.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

PREDICATE(k4r_post_shopping_basket_position, 4)
{
  ShoppingBasketPositionController shopping_basket_positions(PL_A1, std::string(PL_A2), std::string(PL_A3));
  Json::Value shopping_basket_position = char_to_json((char*)PL_A4);
  return shopping_basket_positions.post_shopping_basket_position(shopping_basket_position);
}

PREDICATE(k4r_delete_shopping_basket_position, 2)
{
  ShoppingBasketPositionController shopping_basket_positions(PL_A1);
  return shopping_basket_positions.delete_shopping_basket_position(std::string(PL_A2));
}

PREDICATE(k4r_delete_shopping_basket_positions, 3)
{
  ShoppingBasketPositionController shopping_basket_positions(PL_A1, std::string(PL_A2), std::string(PL_A3));
  return shopping_basket_positions.delete_shopping_basket_positions();
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

// Planogram

PREDICATE(k4r_get_planograms, 2)
{
  PlanogramController planograms(PL_A1);

  PlTail values(PL_A2);
  for (const Json::Value& planogram : planograms.get_planograms())
  {
    values.append(planogram.toStyledString().c_str());
  }
  return values.close();
}

PREDICATE(k4r_post_planogram, 2)
{
  PlanogramController planograms(PL_A1);
  Json::Value planogram = char_to_json((char*)PL_A2);
  return planograms.post_planogram(planogram);
}

PREDICATE(k4r_put_planogram, 3)
{
  PlanogramController planograms(PL_A1);
  Json::Value planogram = char_to_json((char*)PL_A3);
  return planograms.put_planogram(std::string(PL_A2), planogram);
}

PREDICATE(k4r_delete_planogram, 2)
{
  PlanogramController planograms(PL_A1);
  return planograms.delete_planogram(std::string(PL_A2));
}

// Product group

PREDICATE(k4r_get_product_groups, 3)
{
  ProductGroupController product_groups(PL_A1);
  product_groups.set_store(std::string(PL_A2));

  PlTail values(PL_A3);
  for (const Json::Value& product_group : product_groups.get_product_groups())
  {
    values.append(product_group.toStyledString().c_str());
  }
  return values.close();
}

PREDICATE(k4r_get_product_group_by_id, 3)
{
  ProductGroupController product_groups(PL_A1);

  Json::Value product_group = product_groups.get_product_group(std::string(PL_A2));
  std::string product_group_id = product_group["id"].asString();
  remove_new_line(product_group_id);
  if (std::stoi(std::string(PL_A2)) == std::stoi(product_group_id))
  {
    PL_A3 = product_group.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

PREDICATE(k4r_post_product_to_product_group, 3)
{
  ProductGroupController product_groups(PL_A1);
  return product_groups.post_product_to_product_group(std::string(PL_A2), std::string(PL_A3));
}

PREDICATE(k4r_post_product_group, 3)
{
  ProductGroupController product_groups(PL_A1);
  return product_groups.post_product_group(std::string(PL_A2), std::string(PL_A3));
}

PREDICATE(k4r_delete_product_group, 2)
{
  ProductGroupController product_groups(PL_A1);
  return product_groups.delete_product_group(std::string(PL_A2));
}

PREDICATE(k4r_delete_product_from_product_group, 3)
{
  ProductGroupController product_groups(PL_A1);
  return product_groups.delete_product_from_product_group(std::string(PL_A2), std::string(PL_A3));
}