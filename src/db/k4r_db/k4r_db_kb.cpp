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

Json::Value char_to_json(const char *entity_in)
{
  Json::Value entity_out;
  Json::Reader reader;
  reader.parse(entity_in, entity_out);
  return entity_out;
}

PREDICATE(k4r_get_value_from_key, 3)
{
  Json::Value entity = char_to_json((char *)PL_A1);
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
  Json::Value entity = char_to_json((char *)PL_A1);
  return (entity[std::string(PL_A2)].asString() == std::string(PL_A3));
}

// Customer

PREDICATE(k4r_get_customers, 2)
{
  CustomerController customer_controller(PL_A1);

  PlTail customers(PL_A2);
  for (const Json::Value &customer : customer_controller.get_customers())
  {
    customers.append(customer.toStyledString().c_str());
  }
  return customers.close();
}

PREDICATE(k4r_get_customer, 3)
{
  CustomerController customer_controller(PL_A1);

  Json::Value customer = customer_controller.get_customer(std::string(PL_A2));
  std::string customer_id = customer["id"].asString();
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
  CustomerController customer_controller(PL_A1);
  return customer_controller.post_customer(std::string(PL_A2));
}

PREDICATE(k4r_put_customer, 3)
{
  CustomerController customer_controller(PL_A1);
  return customer_controller.put_customer(std::string(PL_A2), std::string(PL_A3));
}

PREDICATE(k4r_delete_customer, 2)
{
  CustomerController customer_controller(PL_A1);
  return customer_controller.delete_customer(std::string(PL_A2));
}

// Store

PREDICATE(k4r_get_stores, 2)
{
  StoreController store_controller(PL_A1);

  PlTail stores(PL_A2);
  for (const Json::Value &store : store_controller.get_stores())
  {
    stores.append(store.toStyledString().c_str());
  }
  return stores.close();
}

PREDICATE(k4r_get_store, 3)
{
  StoreController store_controller(PL_A1);

  Json::Value store = store_controller.get_store(std::string(PL_A2));
  std::string store_id = store["id"].asString();

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

Json::Value store_array_to_store_json(const Json::Value& store_array)
{
  Json::Value store_json;
  if (store_array.size() == 12)
  {
    store_json["addressAdditional"] = store_array[0];
    store_json["addressCity"] = store_array[1];
    store_json["addressCountry"] = store_array[2];
    store_json["addressPostcode"] = store_array[3];
    store_json["addressState"] = store_array[4];
    store_json["addressStreet"] = store_array[5];
    store_json["addressStreetNumber"] = store_array[6];
    store_json["cadPlanId"] = store_array[7];
    store_json["latitude"] = store_array[8];
    store_json["longitude"] = store_array[9];
    store_json["storeName"] = store_array[10];
    store_json["storeNumber"] = store_array[11];
  }
  else
  {
    std::cout << "Invalid store array (length = " << store_array.size() << ")" << std::endl;
  }
  return store_json;
}

PREDICATE(k4r_post_store, 2)
{
  StoreController store_controller(PL_A1);
  Json::Value store = char_to_json((char *)PL_A2);
  if (store.isArray())
  {
    return store_controller.post_store(store_array_to_store_json(store));
  }
  else
  {
    return store_controller.post_store(store);
  }
}

PREDICATE(k4r_put_store, 3)
{
  StoreController store_controller(PL_A1);
  Json::Value store = char_to_json((char *)PL_A3);
  if (store.isArray())
  {
    return store_controller.put_store(std::string(PL_A2), store_array_to_store_json(store));
  }
  else
  {
    return store_controller.put_store(std::string(PL_A2), store);
  }
}

PREDICATE(k4r_delete_store, 2)
{
  StoreController store_controller(PL_A1);
  return store_controller.delete_store(std::string(PL_A2));
}

// Product

// k4r_get_products(Link, Products)
PREDICATE(k4r_get_products, 2)
{
  ProductController product_controller(PL_A1);

  PlTail products(PL_A2);
  for (const Json::Value &product : product_controller.get_products())
  {
    products.append(product.toStyledString().c_str());
  }
  return products.close();
}

// k4r_get_product(Link, ProductId, Product)
PREDICATE(k4r_get_product, 3)
{
  ProductController product_controller(PL_A1);
  Json::Value product = product_controller.get_product(std::string(PL_A2));
  std::string product_id = product["id"].asString();

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

Json::Value product_array_to_product_json(const Json::Value& product_array)
{
  Json::Value product_json;
  if (product_array.size() == 7)
  {
    product_json["depth"] = product_array[0];
    product_json["description"] = product_array[1];
    product_json["gtin"] = product_array[2];
    product_json["height"] = product_array[3];
    product_json["length"] = product_array[4];
    product_json["name"] = product_array[5];
    product_json["weight"] = product_array[6];
  }
  else if (product_array.size() == 8)
  {
    product_json["depth"] = product_array[0];
    product_json["description"] = product_array[1];
    product_json["gtin"] = product_array[2];
    product_json["height"] = product_array[3];
    product_json["id"] = product_array[4];
    product_json["length"] = product_array[5];
    product_json["name"] = product_array[6];
    product_json["weight"] = product_array[7];
  }
  else
  {
    std::cout << "Invalid product array (length = " << product_array.size() << ")" << std::endl;
  }
  return product_json;
}

// k4r_post_product(Link, ProductId, Product)
PREDICATE(k4r_post_product, 3)
{
  ProductController product_controller(PL_A1);
  Json::Value product = char_to_json((char *)PL_A2);
  if (product.isArray())
  {
    
    return product_controller.post_product(std::string(PL_A3), product_array_to_product_json(product));
  }
  else
  {
    return product_controller.post_product(std::string(PL_A3), product);
  }
}

// k4r_put_product(Link, ProductId, Product)
PREDICATE(k4r_put_product, 3)
{
  ProductController product_controller(PL_A1);
  Json::Value product = char_to_json((char *)PL_A2);
  if (product.isArray())
  {
    return product_controller.put_product(std::string(PL_A3), product_array_to_product_json(product));
  }
  else
  {
    return product_controller.put_product(std::string(PL_A3), product);
  }
}

Json::Value products_array_to_products_json(const Json::Value& products_array)
{
  Json::Value products_json;
  products_json["products"] = {};
  for (const Json::Value& product_array : products_array)
  {
    products_json["products"].append(product_array_to_product_json(product_array));
  }
  return products_json;
}

// k4r_post_products(Link, Products)
PREDICATE(k4r_post_products, 2)
{
  ProductController product_controller(PL_A1);
  Json::Value products = char_to_json((char *)PL_A2);
  if (products.isArray())
  {
    return product_controller.post_products(products_array_to_products_json(products));
  }
  else
  {
    return product_controller.post_products(products);
  }
}

// k4r_delete_product(Link, ProductId)
PREDICATE(k4r_delete_product, 2)
{
  ProductController product_controller(PL_A1);
  return product_controller.delete_product(std::string(PL_A2));
}

// Characteristic

// k4r_get_characteristics(Link, Characteristics)
PREDICATE(k4r_get_characteristics, 2)
{
  CharacteristicController characteristic_controller(PL_A1);

  PlTail characteristics(PL_A2);
  for (const Json::Value &characteristic : characteristic_controller.get_characteristics())
  {
    characteristics.append(characteristic.toStyledString().c_str());
  }
  return characteristics.close();
}

// k4r_post_characteristic(Link, CharacteristicName)
PREDICATE(k4r_post_characteristic, 2)
{
  CharacteristicController characteristic_controller(PL_A1);
  return characteristic_controller.post_characteristic(std::string(PL_A2));
}

// k4r_delete_characteristic(Link, CharacteristicId)
PREDICATE(k4r_delete_characteristic, 2)
{
  CharacteristicController characteristic_controller(PL_A1);
  return characteristic_controller.delete_characteristic(std::string(PL_A2));
}

// Property

// k4r_get_properties(Link, StoreId, ProductId, Properties)
PREDICATE(k4r_get_properties, 4)
{
  PropertyController property_controller(PL_A1, std::string(PL_A2), std::string(PL_A3));

  PlTail properties(PL_A4);
  for (const Json::Value &property : property_controller.get_properties())
  {
    properties.append(property.toStyledString().c_str());
  }
  return properties.close();
}

// k4r_post_property(Link, StoreId, ProductId, CharacteristicId, Value)
PREDICATE(k4r_post_property, 5)
{
  PropertyController property_controller(PL_A1, std::string(PL_A2), std::string(PL_A3), std::string(PL_A4));
  return property_controller.post_property(std::string(PL_A5));
}

// k4r_delete_property(Link, StoreId, ProductId, CharacteristicId)
PREDICATE(k4r_delete_property, 4)
{
  PropertyController property_controller(PL_A1, std::string(PL_A2), std::string(PL_A3), std::string(PL_A4));
  return property_controller.delete_property();
}

// Shelf

// k4r_get_shelves(Link, StoreId, Shelves)
PREDICATE(k4r_get_shelves, 3)
{
  ShelfController shelf_controller(PL_A1, std::string(PL_A2));

  PlTail shelves(PL_A3);
  for (const Json::Value &shelf : shelf_controller.get_shelves())
  {
    shelves.append(shelf.toStyledString().c_str());
  }
  return shelves.close();
}

// k4r_get_shelf(Link, ShelfId, Shelf)
PREDICATE(k4r_get_shelf, 3)
{
  ShelfController shelves(PL_A1);

  Json::Value shelf = shelves.get_shelf(std::string(PL_A2));
  std::string shelf_id = shelf["id"].asString();

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

Json::Value shelf_array_to_shelf_json(const Json::Value& shelf_array)
{
  Json::Value shelf_json;
  if (shelf_array.size() == 12)
  {
    shelf_json["cadPlanId"] = shelf_array[0];
    shelf_json["depth"] = shelf_array[1];
    shelf_json["externalReferenceId"] = shelf_array[2];
    shelf_json["height"] = shelf_array[3];
    shelf_json["orientationW"] = shelf_array[4];
    shelf_json["orientationX"] = shelf_array[5];
    shelf_json["orientationY"] = shelf_array[6];
    shelf_json["orientationZ"] = shelf_array[7];
    shelf_json["positionX"] = shelf_array[8];
    shelf_json["positionY"] = shelf_array[9];
    shelf_json["positionZ"] = shelf_array[10];
    shelf_json["width"] = shelf_array[11];
  }
  else
  {
    std::cout << "Invalid shelf array (length = " << shelf_array.size() << ")" << std::endl;
  }
  return shelf_json;
}

// k4r_post_shelf(Link, StoreId, ProductGroupId, Shelf)
PREDICATE(k4r_post_shelf, 4)
{
  ShelfController shelf_controller(PL_A1, std::string(PL_A2), std::string(PL_A3));
  Json::Value shelf = char_to_json((char*)PL_A4);
  if (shelf.isArray())
  {
    return shelf_controller.post_shelf(shelf_array_to_shelf_json(shelf));
  }
  else
  {
    return shelf_controller.post_shelf(shelf);
  }
}

// k4r_put_shelf(Link, StoreId, ProductGroupId, ShelfId, Shelf)
PREDICATE(k4r_put_shelf, 5)
{
  ShelfController shelf_controller(PL_A1, std::string(PL_A2), std::string(PL_A3));
  Json::Value shelf = char_to_json((char *)PL_A5);
  if (shelf.isArray())
  {
    return shelf_controller.put_shelf(std::string(PL_A4), shelf_array_to_shelf_json(shelf));
  }
  else
  {
    return shelf_controller.put_shelf(std::string(PL_A4), shelf);
  }
}

// k4r_delete_shelf(Link, ShelfId)
PREDICATE(k4r_delete_shelf, 2)
{
  ShelfController shelves(PL_A1);
  return shelves.delete_shelf(std::string(PL_A2));
}

// k4r_get_shelf_data(Shelf, ShelfPosition, ShelfOrientation, ShelfDimension)
PREDICATE(k4r_get_shelf_data, 4)
{
  Json::Value shelf = char_to_json((char *)PL_A1);
  PlTail shelf_position(PL_A2);
  shelf_position.append(shelf["positionX"].asDouble());
  shelf_position.append(shelf["positionY"].asDouble());
  shelf_position.append(shelf["positionZ"].asDouble());
  shelf_position.close();
  PlTail shelf_orientation(PL_A3);
  shelf_orientation.append(shelf["orientationX"].asDouble());
  shelf_orientation.append(shelf["orientationY"].asDouble());
  shelf_orientation.append(shelf["orientationZ"].asDouble());
  shelf_orientation.append(shelf["orientationW"].asDouble());
  shelf_orientation.close();
  PlTail shelf_dimension(PL_A4);
  shelf_dimension.append(shelf["depth"].asFloat());
  shelf_dimension.append(shelf["width"].asFloat());
  shelf_dimension.append(shelf["height"].asFloat());
  shelf_dimension.close();
  return true;
}

// Shelf layer

// k4r_get_layers(Link, ShelfId, ShelfLayers)
PREDICATE(k4r_get_shelf_layers, 3)
{
  ShelfLayerController shelf_layer_controller(PL_A1, std::string(PL_A2));

  PlTail shelf_layers(PL_A3);
  for (const Json::Value &shelf_layer : shelf_layer_controller.get_shelf_layers())
  {
    shelf_layers.append(shelf_layer.toStyledString().c_str());
  }
  return shelf_layers.close();
}

// k4r_get_layers(Link, ShelfLayerId, ShelfLayer)
PREDICATE(k4r_get_shelf_layer, 3)
{
  ShelfLayerController shelf_layer_controller(PL_A1);

  Json::Value shelf_layer = shelf_layer_controller.get_shelf_layer(std::string(PL_A2));
  std::string shelf_layer_id = shelf_layer["id"].asString();

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

Json::Value shelf_layer_array_to_shelf_layer_json(const Json::Value& shelf_layer_array)
{
  Json::Value shelf_layer_json;
  if (shelf_layer_array.size() == 7)
  {
    shelf_layer_json["depth"] = shelf_layer_array[0];
    shelf_layer_json["externalReferenceId"] = shelf_layer_array[1];
    shelf_layer_json["height"] = shelf_layer_array[2];
    shelf_layer_json["level"] = shelf_layer_array[3];
    shelf_layer_json["positionZ"] = shelf_layer_array[4];
    shelf_layer_json["type"] = shelf_layer_array[5];
    shelf_layer_json["width"] = shelf_layer_array[6];
  }
  else
  {
    std::cout << "Invalid shelf layer array (length = " << shelf_layer_array.size() << ")" << std::endl;
  }
  return shelf_layer_json;
}

// k4r_post_shelf_layer(Link, ShelfId, ShelfLayer)
PREDICATE(k4r_post_shelf_layer, 3)
{
  ShelfLayerController shelf_layer_controller(PL_A1, std::string(PL_A2));
  Json::Value shelf_layer = char_to_json((char *)PL_A3);
  if (shelf_layer.isArray())
  {
    return shelf_layer_controller.post_shelf_layer(shelf_layer_array_to_shelf_layer_json(shelf_layer));
  }
  else
  {
    return shelf_layer_controller.post_shelf_layer(shelf_layer);
  }
}

// k4r_post_shelf_layer(Link, ShelfLayerId)
PREDICATE(k4r_delete_shelf_layer, 2)
{
  ShelfLayerController shelf_layer_controller(PL_A1);
  return shelf_layer_controller.delete_shelf_layer(std::string(PL_A2));
}

// Shopping basket

PREDICATE(k4r_get_shopping_basket_positions, 4)
{
  ShoppingBasketPositionController shopping_basket_position_controller(PL_A1, std::string(PL_A2), std::string(PL_A3));

  PlTail shopping_basket_positions(PL_A4);
  for (const Json::Value &shopping_basket_position : shopping_basket_position_controller.get_shopping_basket_positions())
  {
    shopping_basket_positions.append(shopping_basket_position.toStyledString().c_str());
  }
  return shopping_basket_positions.close();
}

PREDICATE(k4r_get_shopping_basket_position, 3)
{
  ShoppingBasketPositionController shopping_basket_position_controller(PL_A1);

  Json::Value shopping_basket_position = shopping_basket_position_controller.get_shopping_basket_position(std::string(PL_A2));
  std::string shopping_basket_position_id = shopping_basket_position["id"].asString();

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

Json::Value shopping_basket_position_array_to_shopping_basket_position_json(const Json::Value& shopping_basket_position_array)
{
  Json::Value shopping_basket_position_json;
  if (shopping_basket_position_array.size() == 3)
  {
    shopping_basket_position_json["currency"] = shopping_basket_position_array[0];
    shopping_basket_position_json["quantity"] = shopping_basket_position_array[1];
    shopping_basket_position_json["sellingPrice"] = shopping_basket_position_array[2];
  }
  else
  {
    std::cout << "Invalid shopping basket position array (length = " << shopping_basket_position_array.size() << ")" << std::endl;
  }
  return shopping_basket_position_json;
}

PREDICATE(k4r_post_shopping_basket_position, 5)
{
  ShoppingBasketPositionController shopping_basket_position_controller(PL_A1, std::string(PL_A2), std::string(PL_A3), std::string(PL_A4));
  Json::Value shopping_basket_position = char_to_json((char *)PL_A5);
  if (shopping_basket_position.isArray())
  {
    return shopping_basket_position_controller.post_shopping_basket_position(shopping_basket_position_array_to_shopping_basket_position_json(shopping_basket_position));
  }
  else
  {
    return shopping_basket_position_controller.post_shopping_basket_position(shopping_basket_position);
  }
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

// k4r_get_facings(Link, ShelfLayerId, Facings)
PREDICATE(k4r_get_facings, 3)
{
  FacingController facing_controller(PL_A1, std::string(PL_A2));

  PlTail facings(PL_A3);
  for (const Json::Value &facing : facing_controller.get_facings())
  {
    facings.append(facing.toStyledString().c_str());
  }
  return facings.close();
}

// k4r_get_facings(Link, FacingId, Facing)
PREDICATE(k4r_get_facing, 3)
{
  FacingController facing_controller(PL_A1);

  Json::Value facing = facing_controller.get_facing(std::string(PL_A2));
  std::string facing_id = facing["id"].asString();

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

// k4r_post_facing(Link, ShelfLayerId, ProductId, LayerRelativePosition, Quantity)
PREDICATE(k4r_post_facing, 5)
{
  FacingController facing_controller(PL_A1, std::string(PL_A2), std::string(PL_A3));
  Json::Value facing;
  facing["layerRelativePosition"] = std::stoi(std::string(PL_A4));
  facing["quantity"] = std::stoi(std::string(PL_A5));
  return facing_controller.post_facing(facing);
}

// k4r_put_facing(Link, ShelfLayerId, ProductId, FacingId, LayerRelativePosition, Quantity)
PREDICATE(k4r_put_facing, 6)
{
  FacingController facing_controller(PL_A1, std::string(PL_A2), std::string(PL_A3));
  Json::Value facing;
  facing["layerRelativePosition"] = std::stoi(std::string(PL_A5));
  facing["quantity"] = std::stoi(std::string(PL_A6));
  return facing_controller.put_facing(std::string(PL_A4), facing);
}

// k4r_delete_facing(Link, FacingId)
PREDICATE(k4r_delete_facing, 2)
{
  FacingController facing_controller(PL_A1);
  return facing_controller.delete_facing(std::string(PL_A2));
}

// Planogram

// k4r_get_planograms(Link, Planograms)
PREDICATE(k4r_get_planograms, 2)
{
  PlanogramController planogram_controller(PL_A1);

  PlTail planograms(PL_A2);
  for (const Json::Value &planogram : planogram_controller.get_planograms())
  {
    planograms.append(planogram.toStyledString().c_str());
  }
  return planograms.close();
}

Json::Value planogram_array_to_planogram_json(const Json::Value& planogram_array)
{
  Json::Value planogram_json;
  if (planogram_array.size() == 4)
  {
    planogram_json["numberOfFacings"] = planogram_array[0];
    planogram_json["orientationYaw"] = planogram_array[1];
    planogram_json["positionX"] = planogram_array[2];
    planogram_json["versionTimestamp"] = planogram_array[3];
  }
  else
  {
    std::cout << "Invalid planogram array (length = " << planogram_array.size() << ")" << std::endl;
  }
  return planogram_json;
}

// k4r_post_planogram(Link, ProductId, ShelfLayerId, Planogram)
PREDICATE(k4r_post_planogram, 4)
{
  PlanogramController planogram_controller(PL_A1, std::string(PL_A2), std::string(PL_A3));
  Json::Value planogram = char_to_json((char *)PL_A4);
  if (planogram.isArray())
  {
    return planogram_controller.post_planogram(planogram_array_to_planogram_json(planogram));
  }
  else
  {
    return planogram_controller.post_planogram(planogram);
  }
}

// k4r_put_planogram(Link, ProductId, ShelfLayerId, PlanogramId, Planogram)
PREDICATE(k4r_put_planogram, 5)
{
  PlanogramController planogram_controller(PL_A1, std::string(PL_A2), std::string(PL_A3));
  Json::Value planogram = char_to_json((char *)PL_A5);
  if (planogram.isArray())
  {
    return planogram_controller.put_planogram(std::string(PL_A4), planogram_array_to_planogram_json(planogram));
  }
  else
  {
    return planogram_controller.put_planogram(std::string(PL_A4), planogram);
  }
}

// k4r_delete_planogram(Link, PlanogramId)
PREDICATE(k4r_delete_planogram, 2)
{
  PlanogramController planogram_controller(PL_A1);
  return planogram_controller.delete_planogram(std::string(PL_A2));
}

// Product group

// k4r_get_product_groups(Link, ProductGroups, StoreId)
PREDICATE(k4r_get_product_groups, 3)
{
  ProductGroupController product_group_controller(PL_A1);

  PlTail product_groups(PL_A3);
  for (const Json::Value &product_group : product_group_controller.get_product_groups(std::string(PL_A2)))
  {
    product_groups.append(product_group.toStyledString().c_str());
  }
  return product_groups.close();
}

// k4r_get_product_group(Link, ProductGroupId, ProductGroup)
PREDICATE(k4r_get_product_group, 3)
{
  ProductGroupController product_group_controller(PL_A1);

  Json::Value product_group = product_group_controller.get_product_group(std::string(PL_A2));
  std::string product_group_id = product_group["id"].asString();

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

// k4r_post_product_to_product_group(Link, ProductId, ProductGroupId)
PREDICATE(k4r_post_product_to_product_group, 3)
{
  ProductGroupController product_group_controller(PL_A1, std::string(PL_A2));
  return product_group_controller.post_product_to_product_group(std::string(PL_A3));
}

// k4r_post_product_group(Link, StoreId, ProductGroupName)
PREDICATE(k4r_post_product_group, 3)
{
  ProductGroupController product_group_controller(PL_A1);
  return product_group_controller.post_product_group(std::string(PL_A2), std::string(PL_A3));
}

// k4r_delete_product_group(Link, ProductGroupId)
PREDICATE(k4r_delete_product_group, 2)
{
  ProductGroupController product_group_controller(PL_A1);
  return product_group_controller.delete_product_group(std::string(PL_A2));
}

// k4r_delete_product_from_product_group(Link, ProductId, ProductGroupId)
PREDICATE(k4r_delete_product_from_product_group, 3)
{
  ProductGroupController product_group_controller(PL_A1, std::string(PL_A2));
  return product_group_controller.delete_product_from_product_group(std::string(PL_A3));
}