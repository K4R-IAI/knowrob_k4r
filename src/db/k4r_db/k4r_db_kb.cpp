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
#include "Entities/CustomerController.cpp"
#include "Entities/LogisticalUnitController.cpp"
#include "Entities/MaterialGroupController.cpp"
#include "Entities/ProductCharacteristicController.cpp"
#include "Entities/ProductController.cpp"
#include "Entities/ProductGtinController.cpp"
#include "Entities/ProductPropertyController.cpp"
#include "Entities/ShelfController.cpp"
#include "Entities/ShelfLayerController.cpp"
// #include "Entities/ShoppingBasketPositionController.cpp"
#include "Entities/FacingController.cpp"
#include "Entities/StoreController.cpp"
// #include "Entities/PlanogramController.cpp"
#include "Entities/ProductGroupController.cpp"

// convert_string_to_int(StringVal, IntegerVal)
PREDICATE(convert_string_to_int, 2)
{
  int integer_val = std::stoi(std::string(PL_A1));
  PL_A2 = integer_val;
}

const Json::Value PlTerm_to_json(PlTerm in_entity_PlTerm)
{
  Json::Value out_entity;
  switch (in_entity_PlTerm.type())
  {
    case 10:
      if (out_entity.isNull())
      {
        PlTail tail(in_entity_PlTerm);
        PlTerm term;
        while(tail.next(term))
        {
          out_entity.append((char *)term);
        }
      }
      break;

    default:
      Json::Reader reader;
      reader.parse((char *)in_entity_PlTerm, out_entity);
      break;
  }
  
  return out_entity;
}

PREDICATE(get_value_from_key, 3)
{
  Json::Value entity = PlTerm_to_json(PL_A1);
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

PREDICATE(check_key_value, 3)
{
  Json::Value entity = PlTerm_to_json(PL_A1);
  return (entity[std::string(PL_A2)].asString() == std::string(PL_A3));
}

// Customer

// get_customers(CustomerList)
PREDICATE(get_customers, 1)
{
  CustomerController customer_controller;

  PlTail customers(PL_A1);
  for (const Json::Value &customer : customer_controller.get_customers())
  {
    customers.append(customer.toStyledString().c_str());
  }
  return customers.close();
}

// get_customer(CustomerId, Customer)
PREDICATE(get_customer, 2)
{
  CustomerController customer_controller;

  Json::Value customer = customer_controller.get_customer(std::string(PL_A1));
  std::string customer_id = customer["id"].asString();
  if (std::stoi(std::string(PL_A1)) == std::stoi(customer_id))
  {
    PL_A2 = customer.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_customer(InAnonymisedName, OutCustomer)
PREDICATE(post_customer, 2)
{
  CustomerController customer_controller;
  Json::Value out_customer;
  if (customer_controller.post_customer(std::string(PL_A1), out_customer))
  {
    PL_A2 = out_customer.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_customer(InCustomerId, InAnonymisedName, OutCustomer)
PREDICATE(put_customer, 3)
{
  CustomerController customer_controller;
  Json::Value out_customer;
  if (customer_controller.put_customer(std::string(PL_A1), std::string(PL_A2), out_customer))
  {
    PL_A3 = out_customer.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_customer(CustomerId)
PREDICATE(delete_customer, 1)
{
  CustomerController customer_controller;
  return customer_controller.delete_customer(std::string(PL_A1));
}

// Store

const Json::Value store_array_to_store_json(const Json::Value &store_array)
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
    std::cerr << "Invalid store array (length = " << store_array.size() << ")" << std::endl;
  }
  return store_json;
}

// get_stores(StoreList)
PREDICATE(get_stores, 1)
{
  StoreController store_controller;

  PlTail stores(PL_A1);
  for (const Json::Value &store : store_controller.get_stores())
  {
    stores.append(store.toStyledString().c_str());
  }
  return stores.close();
}

// get_store(StoreId, Store)
PREDICATE(get_store, 2)
{
  StoreController store_controller;

  Json::Value store = store_controller.get_store(std::string(PL_A1));
  std::string store_id = store["id"].asString();
  if (std::stoi(std::string(PL_A1)) == std::stoi(store_id))
  {
    PL_A2 = store.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_store(InStore, OutStore)
PREDICATE(post_store, 2)
{
  StoreController store_controller;
  Json::Value in_store = PlTerm_to_json(PL_A1);
  Json::Value out_store;
  if (in_store.isArray())
  {
    store_controller.post_store(store_array_to_store_json(in_store), out_store);
  }
  else
  {
    store_controller.post_store(in_store, out_store);
  }
  if (!out_store.isNull())
  {
    PL_A2 = out_store.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_store(InStoreId, InStore, OutStore)
PREDICATE(put_store, 3)
{
  StoreController store_controller;
  Json::Value in_store = PlTerm_to_json(PL_A2);
  Json::Value out_store;
  if (in_store.isArray())
  {
    store_controller.put_store(std::string(PL_A1), store_array_to_store_json(in_store), out_store);
  }
  else
  {
    store_controller.put_store(std::string(PL_A1), in_store, out_store);
  }
  if (!out_store.isNull())
  {
    PL_A3 = out_store.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_store(StoreId)
PREDICATE(delete_store, 1)
{
  StoreController store_controller;
  return store_controller.delete_store(std::string(PL_A1));
}

// MaterialGroup

// get_material_groups(MaterialGroupList)
PREDICATE(get_material_groups, 1)
{
  MaterialGroupController material_group_controller;

  PlTail material_groups(PL_A1);
  for (const Json::Value &material_group : material_group_controller.get_material_groups())
  {
    material_groups.append(material_group.toStyledString().c_str());
  }
  return material_groups.close();
}

// get_material_group(MaterialGroupId, MaterialGroup)
PREDICATE(get_material_group, 2)
{
  MaterialGroupController material_group_controller;

  Json::Value material_group = material_group_controller.get_material_group(std::string(PL_A1));
  std::string material_group_id = material_group["id"].asString();
  if (std::stoi(std::string(PL_A1)) == std::stoi(material_group_id))
  {
    PL_A2 = material_group.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_material_group(InName, OutMaterialGroup)
PREDICATE(post_material_group, 2)
{
  MaterialGroupController material_group_controller;
  Json::Value out_material_group;
  if (material_group_controller.post_material_group(std::string(PL_A1), out_material_group))
  {
    PL_A2 = out_material_group.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_material_group(InParentId, InName, OutMaterialGroup)
PREDICATE(post_material_group, 3)
{
  MaterialGroupController material_group_controller;
  Json::Value out_material_group;
  if (material_group_controller.post_material_group(std::string(PL_A1), std::string(PL_A2), out_material_group))
  {
    PL_A3 = out_material_group.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_material_group(InMaterialGroupId, InName, OutMaterialGroup)
PREDICATE(put_material_group, 3)
{
  MaterialGroupController material_group_controller;
  Json::Value out_material_group;
  if (material_group_controller.put_material_group(std::string(PL_A1), std::string(PL_A2), out_material_group))
  {
    PL_A3 = out_material_group.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_material_group(InMaterialGroupId, InParentId, InName, OutMaterialGroup)
PREDICATE(put_material_group, 4)
{
  MaterialGroupController material_group_controller;
  Json::Value out_material_group;
  if (material_group_controller.put_material_group(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), out_material_group))
  {
    PL_A4 = out_material_group.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_material_group(MaterialGroupId)
PREDICATE(delete_material_group, 1)
{
  MaterialGroupController material_group_controller;
  return material_group_controller.delete_material_group(std::string(PL_A1));
}

// Product

// get_products(Products)
PREDICATE(get_products, 1)
{
  ProductController product_controller;

  PlTail products(PL_A1);
  for (const Json::Value &product : product_controller.get_products())
  {
    products.append(product.toStyledString().c_str());
  }
  return products.close();
}

// get_product(ProductId, Product)
PREDICATE(get_product, 2)
{
  ProductController product_controller;
  Json::Value product = product_controller.get_product(std::string(PL_A1));
  std::string product_id = product["id"].asString();

  if (std::string(PL_A1) == product_id)
  {
    PL_A2 = product.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

const Json::Value product_array_to_product_json(const Json::Value &product_array)
{
  if (product_array.size() == 0)
  {
    Json::Value product_array_tmp;
    Json::Reader reader;
    reader.parse((char *)product_array.asCString(), product_array_tmp);
    return product_array_to_product_json(product_array_tmp);
  }
  Json::Value product_json;
  if (product_array.size() == 4)
  {
    product_json["description"] = product_array[0];
    product_json["name"] = product_array[1];
    product_json["productType"] = product_array[2];
    product_json["productUnit"] = product_array[3];
  }
  else if (product_array.size() == 5)
  {
    product_json["description"] = product_array[0];
    product_json["id"] = product_array[1];
    product_json["name"] = product_array[2];
    product_json["productType"] = product_array[3];
    product_json["productUnit"] = product_array[4];
  }
  else if (product_array.size() == 6)
  {
    product_json["description"] = product_array[0];
    product_json["id"] = product_array[1];
    product_json["materialGroupId"] = product_array[2];
    product_json["name"] = product_array[3];
    product_json["productType"] = product_array[4];
    product_json["productUnit"] = product_array[5];
  }
  else
  {
    std::cerr << "Invalid product array (length = " << product_array.size() << ")" << std::endl;
  }
  return product_json;
}

Json::Value products_array_to_products_json(const Json::Value &products_array)
{
  Json::Value products_json;
  products_json["products"] = {};
  std::cout << products_array << std::endl;
  for (const Json::Value &product_array : products_array)
  {
    std::cout << product_array << std::endl;
    products_json["products"].append(product_array_to_product_json(product_array));
  }
  std::cout << products_json << std::endl;
  return products_json;
}

// post_product(InProductId, InProduct, OutProduct)
PREDICATE(post_product, 3)
{
  ProductController product_controller;
  Json::Value in_product = PlTerm_to_json(PL_A2);
  Json::Value out_product;
  if (in_product.isArray())
  {
    product_controller.post_product(std::string(PL_A1), product_array_to_product_json(in_product), out_product);
  }
  else
  {
    product_controller.post_product(std::string(PL_A1), in_product, out_product);
  }
  if (!out_product.isNull())
  {
    PL_A3 = out_product.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_product(InProductId, InMaterialGroupId, InProduct, OutProduct)
PREDICATE(post_product, 4)
{
  ProductController product_controller;
  Json::Value in_product = PlTerm_to_json(PL_A3);
  Json::Value out_product;
  if (in_product.isArray())
  {
    product_controller.post_product(std::string(PL_A1), std::string(PL_A2), product_array_to_product_json(in_product), out_product);
  }
  else
  {
    product_controller.post_product(std::string(PL_A1), std::string(PL_A2), in_product, out_product);
  }
  if (!out_product.isNull())
  {
    PL_A4 = out_product.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_product(ProductId, InProduct, OutProduct)
PREDICATE(put_product, 3)
{
  ProductController product_controller;
  Json::Value in_product = PlTerm_to_json(PL_A2);
  Json::Value out_product;
  if (in_product.isArray())
  {
    product_controller.put_product(std::string(PL_A1), product_array_to_product_json(in_product), out_product);
  }
  else
  {
    product_controller.put_product(std::string(PL_A1), in_product, out_product);
  }
  if (!out_product.isNull())
  {
    PL_A3 = out_product.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_product(ProductId, InMaterialGroupId, InProduct, OutProduct)
PREDICATE(put_product, 4)
{
  ProductController product_controller;
  Json::Value in_product = PlTerm_to_json(PL_A3);
  Json::Value out_product;
  if (in_product.isArray())
  {
    product_controller.put_product(std::string(PL_A1), std::string(PL_A2), product_array_to_product_json(in_product), out_product);
  }
  else
  {
    product_controller.put_product(std::string(PL_A1), std::string(PL_A2), in_product, out_product);
  }
  if (!out_product.isNull())
  {
    PL_A4 = out_product.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_products(InProducts, OutProducts)
PREDICATE(post_products, 2)
{
  ProductController product_controller;
  Json::Value in_products = PlTerm_to_json(PL_A1);
  Json::Value out_products;
  if (in_products.isArray())
  {
    product_controller.post_products(products_array_to_products_json(in_products), out_products);
  }
  else
  {
    product_controller.post_products(in_products, out_products);
  }
  if (!out_products.isNull())
  {
    PL_A2 = out_products.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_product(ProductId)
PREDICATE(delete_product, 1)
{
  ProductController product_controller;
  return product_controller.delete_product(std::string(PL_A1));
}

// Characteristic

// get_product_characteristics(ProductCharacteristics)
PREDICATE(get_product_characteristics, 1)
{
  ProductCharacteristicController product_characteristic_controller;

  PlTail product_characteristics(PL_A1);
  for (const Json::Value &product_characteristic : product_characteristic_controller.get_product_characteristics())
  {
    product_characteristics.append(product_characteristic.toStyledString().c_str());
  }
  return product_characteristics.close();
}

// post_product_characteristic(InProductCharacteristicName, OutProductCharacteristic)
PREDICATE(post_product_characteristic, 2)
{
  ProductCharacteristicController product_characteristic_controller;
  Json::Value out_product_characteristic;
  if (product_characteristic_controller.post_product_characteristic(std::string(PL_A1), out_product_characteristic))
  {
    PL_A2 = out_product_characteristic.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_product_characteristic(CharacteristicId)
PREDICATE(delete_product_characteristic, 1)
{
  ProductCharacteristicController product_characteristic_controller;
  return product_characteristic_controller.delete_product_characteristic(std::string(PL_A1));
}

// ProductProductProperty

// get_product_properties(StoreId, ProductId, Properties)
PREDICATE(get_product_properties, 3)
{
  ProductPropertyController product_property_controller;

  PlTail product_properties(PL_A3);
  for (const Json::Value &product_property : product_property_controller.get_product_properties(std::string(PL_A1), std::string(PL_A2)))
  {
    product_properties.append(product_property.toStyledString().c_str());
  }
  return product_properties.close();
}

// post_product_property(InStoreId, InProductId, InProductCharacteristicId, InValue, OutProductProperty)
PREDICATE(post_product_property, 5)
{
  ProductPropertyController product_property_controller;
  Json::Value out_product_property;
  if (product_property_controller.post_product_property(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), std::string(PL_A4), out_product_property))
  {
    PL_A5 = out_product_property.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_product_property(StoreId, ProductId, ProductCharacteristicId)
PREDICATE(delete_product_property, 3)
{
  ProductPropertyController product_property_controller;
  return product_property_controller.delete_product_property(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3));
}

// LogisticalUnit

const Json::Value logistical_unit_array_to_logistical_unit_json(const Json::Value &logistical_unit_array)
{
  Json::Value logistical_unit_json;

  if (logistical_unit_array.size() == 14)
  {
    logistical_unit_json["basicUnit"] = logistical_unit_array[0];
    logistical_unit_json["deliveryExpanseUnit"] = logistical_unit_array[1];
    logistical_unit_json["dimensionUnit"] = logistical_unit_array[2];
    logistical_unit_json["height"] = logistical_unit_array[3];
    logistical_unit_json["length"] = logistical_unit_array[4];
    logistical_unit_json["maxStackSize"] = logistical_unit_array[5];
    logistical_unit_json["netWeight"] = logistical_unit_array[6];
    logistical_unit_json["orderUnit"] = logistical_unit_array[7];
    logistical_unit_json["quantityOfPredecessors"] = logistical_unit_array[8];
    logistical_unit_json["quantityUnit"] = logistical_unit_array[9];
    logistical_unit_json["quantityUnitIso"] = logistical_unit_array[10];
    logistical_unit_json["retailUnit"] = logistical_unit_array[11];
    logistical_unit_json["weightUnit"] = logistical_unit_array[12];
    logistical_unit_json["width"] = logistical_unit_array[13];
  }
  else if (logistical_unit_array.size() == 15)
  {
    logistical_unit_json["basicUnit"] = logistical_unit_array[0];
    logistical_unit_json["deliveryExpanseUnit"] = logistical_unit_array[1];
    logistical_unit_json["dimensionUnit"] = logistical_unit_array[2];
    logistical_unit_json["height"] = logistical_unit_array[3];
    logistical_unit_json["length"] = logistical_unit_array[4];
    logistical_unit_json["maxStackSize"] = logistical_unit_array[5];
    logistical_unit_json["netWeight"] = logistical_unit_array[6];
    logistical_unit_json["orderUnit"] = logistical_unit_array[7];
    logistical_unit_json["predecessorId"] = logistical_unit_array[8];
    logistical_unit_json["quantityOfPredecessors"] = logistical_unit_array[9];
    logistical_unit_json["quantityUnit"] = logistical_unit_array[10];
    logistical_unit_json["quantityUnitIso"] = logistical_unit_array[11];
    logistical_unit_json["retailUnit"] = logistical_unit_array[12];
    logistical_unit_json["weightUnit"] = logistical_unit_array[13];
    logistical_unit_json["width"] = logistical_unit_array[14];
  }
  else if (logistical_unit_array.size() == 16)
  {
    logistical_unit_json["basicUnit"] = logistical_unit_array[0];
    logistical_unit_json["deliveryExpanseUnit"] = logistical_unit_array[1];
    logistical_unit_json["dimensionUnit"] = logistical_unit_array[2];
    logistical_unit_json["height"] = logistical_unit_array[3];
    logistical_unit_json["length"] = logistical_unit_array[4];
    logistical_unit_json["maxStackSize"] = logistical_unit_array[5];
    logistical_unit_json["netWeight"] = logistical_unit_array[6];
    logistical_unit_json["orderUnit"] = logistical_unit_array[7];
    logistical_unit_json["predecessorId"] = logistical_unit_array[8];
    logistical_unit_json["productId"] = logistical_unit_array[9];
    logistical_unit_json["quantityOfPredecessors"] = logistical_unit_array[10];
    logistical_unit_json["quantityUnit"] = logistical_unit_array[11];
    logistical_unit_json["quantityUnitIso"] = logistical_unit_array[12];
    logistical_unit_json["retailUnit"] = logistical_unit_array[13];
    logistical_unit_json["weightUnit"] = logistical_unit_array[14];
    logistical_unit_json["width"] = logistical_unit_array[15];
  }
  else
  {
    std::cerr << "Invalid logistical unit array (length = " << logistical_unit_array.size() << ")" << std::endl;
  }
  return logistical_unit_json;
}

// get_logistical_units(LogisticalUnitList)
PREDICATE(get_logistical_units, 1)
{
  LogisticalUnitController logistical_unit_controller;

  PlTail logistical_units(PL_A1);
  for (const Json::Value &logistical_unit : logistical_unit_controller.get_logistical_units())
  {
    logistical_units.append(logistical_unit.toStyledString().c_str());
  }
  return logistical_units.close();
}

// get_logistical_unit(LogisticalUnitId, LogisticalUnit)
PREDICATE(get_logistical_unit, 2)
{
  LogisticalUnitController logistical_unit_controller;

  Json::Value logistical_unit = logistical_unit_controller.get_logistical_unit(std::string(PL_A1));
  std::string logistical_unit_id = logistical_unit["id"].asString();
  if (std::stoi(std::string(PL_A1)) == std::stoi(logistical_unit_id))
  {
    PL_A2 = logistical_unit.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_logistical_unit(InLogisticalUnit, OutLogisticalUnit)
PREDICATE(post_logistical_unit, 2)
{
  LogisticalUnitController logistical_unit_controller;
  Json::Value in_logistical_unit = PlTerm_to_json(PL_A1);
  Json::Value out_logistical_unit;
  if (in_logistical_unit.isArray())
  {
    logistical_unit_controller.post_logistical_unit(logistical_unit_array_to_logistical_unit_json(in_logistical_unit), out_logistical_unit);
  }
  else
  {
    logistical_unit_controller.post_logistical_unit(in_logistical_unit, out_logistical_unit);
  }
  if (!out_logistical_unit.isNull())
  {
    PL_A2 = out_logistical_unit.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_logistical_unit(InProductId, InLogisticalUnit, OutLogisticalUnit)
PREDICATE(post_logistical_unit, 3)
{
  LogisticalUnitController logistical_unit_controller;
  Json::Value in_logistical_unit = PlTerm_to_json(PL_A2);
  Json::Value out_logistical_unit;
  if (in_logistical_unit.isArray())
  {
    logistical_unit_controller.post_logistical_unit(std::string(PL_A1), logistical_unit_array_to_logistical_unit_json(in_logistical_unit), out_logistical_unit);
  }
  else
  {
    logistical_unit_controller.post_logistical_unit(std::string(PL_A1), in_logistical_unit, out_logistical_unit);
  }
  if (!out_logistical_unit.isNull())
  {
    PL_A3 = out_logistical_unit.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_logistical_unit(InPredecessorId, InProductId, InLogisticalUnit, OutLogisticalUnit)
PREDICATE(post_logistical_unit, 4)
{
  LogisticalUnitController logistical_unit_controller;
  Json::Value in_logistical_unit = PlTerm_to_json(PL_A3);
  Json::Value out_logistical_unit;
  if (in_logistical_unit.isArray())
  {
    logistical_unit_controller.post_logistical_unit(std::string(PL_A1), std::string(PL_A2), logistical_unit_array_to_logistical_unit_json(in_logistical_unit), out_logistical_unit);
  }
  else
  {
    logistical_unit_controller.post_logistical_unit(std::string(PL_A1), std::string(PL_A2), in_logistical_unit, out_logistical_unit);
  }
  if (!out_logistical_unit.isNull())
  {
    PL_A4 = out_logistical_unit.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_logistical_unit(InLogisticalUnitId, InLogisticalUnit, OutLogisticalUnit)
PREDICATE(put_logistical_unit, 3)
{
  LogisticalUnitController logistical_unit_controller;
  Json::Value in_logistical_unit = PlTerm_to_json(PL_A2);
  Json::Value out_logistical_unit;
  if (in_logistical_unit.isArray())
  {
    logistical_unit_controller.put_logistical_unit(std::string(PL_A1), logistical_unit_array_to_logistical_unit_json(in_logistical_unit), out_logistical_unit);
  }
  else
  {
    logistical_unit_controller.put_logistical_unit(std::string(PL_A1), in_logistical_unit, out_logistical_unit);
  }
  if (!out_logistical_unit.isNull())
  {
    PL_A3 = out_logistical_unit.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_logistical_unit(InLogisticalUnitId, InProductId, InLogisticalUnit, OutLogisticalUnit)
PREDICATE(put_logistical_unit, 4)
{
  LogisticalUnitController logistical_unit_controller;
  Json::Value in_logistical_unit = PlTerm_to_json(PL_A3);
  Json::Value out_logistical_unit;
  if (in_logistical_unit.isArray())
  {
    logistical_unit_controller.put_logistical_unit(std::string(PL_A1), std::string(PL_A2), logistical_unit_array_to_logistical_unit_json(in_logistical_unit), out_logistical_unit);
  }
  else
  {
    logistical_unit_controller.put_logistical_unit(std::string(PL_A1), std::string(PL_A2), in_logistical_unit, out_logistical_unit);
  }
  if (!out_logistical_unit.isNull())
  {
    PL_A4 = out_logistical_unit.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_logistical_unit(InLogisticalUnitId, InPredecessorId, InProductId, InLogisticalUnit, OutLogisticalUnit)
PREDICATE(put_logistical_unit, 5)
{
  LogisticalUnitController logistical_unit_controller;
  Json::Value in_logistical_unit = PlTerm_to_json(PL_A4);
  Json::Value out_logistical_unit;
  if (in_logistical_unit.isArray())
  {
    logistical_unit_controller.put_logistical_unit(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), logistical_unit_array_to_logistical_unit_json(in_logistical_unit), out_logistical_unit);
  }
  else
  {
    logistical_unit_controller.put_logistical_unit(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), in_logistical_unit, out_logistical_unit);
  }
  if (!out_logistical_unit.isNull())
  {
    PL_A5 = out_logistical_unit.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_logistical_unit(LogisticalUnitId)
PREDICATE(delete_logistical_unit, 1)
{
  LogisticalUnitController logistical_unit_controller;
  return logistical_unit_controller.delete_logistical_unit(std::string(PL_A1));
}

// ProductGtin

const Json::Value product_gtin_array_to_product_gtin_json(const Json::Value &product_gtin_array)
{
  Json::Value product_gtin_json;
  if (product_gtin_array.size() == 2)
  {
    product_gtin_json["gtinType"] = product_gtin_array[0];
    product_gtin_json["mainGtin"] = product_gtin_array[1];
  }
  else if (product_gtin_array.size() == 5)
  {
    product_gtin_json["id"] = product_gtin_array[0];
    product_gtin_json["gtinType"] = product_gtin_array[1];
    product_gtin_json["logisticalUnitId"] = product_gtin_array[2];
    product_gtin_json["mainGtin"] = product_gtin_array[3];
    product_gtin_json["productId"] = product_gtin_array[4];
  }
  else
  {
    std::cerr << "Invalid store array (length = " << product_gtin_array.size() << ")" << std::endl;
  }
  return product_gtin_json;
}

// get_product_gtins(ProductGtinList)
PREDICATE(get_product_gtins, 1)
{
  ProductGtinController product_gtin_controller;

  PlTail product_gtins(PL_A1);
  for (const Json::Value &product_gtin : product_gtin_controller.get_product_gtins())
  {
    product_gtins.append(product_gtin.toStyledString().c_str());
  }
  return product_gtins.close();
}

// get_product_gtin(ProductGtinId, ProductGtin)
PREDICATE(get_product_gtin, 2)
{
  ProductGtinController product_gtin_controller;

  Json::Value product_gtin = product_gtin_controller.get_product_gtin(std::string(PL_A1));
  std::string product_gtin_id = product_gtin["id"].asString();
  if (std::string(PL_A1) == product_gtin_id)
  {
    PL_A2 = product_gtin.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_product_gtin(InProductGtin, OutProductGtin)
PREDICATE(post_product_gtin, 2)
{
  ProductGtinController product_gtin_controller;
  Json::Value in_product_gtin = PlTerm_to_json(PL_A1);
  Json::Value out_product_gtin;

  if (in_product_gtin.isArray())
  {
    product_gtin_controller.post_product_gtin(product_gtin_array_to_product_gtin_json(in_product_gtin), out_product_gtin);
  }
  else
  {
    product_gtin_controller.post_product_gtin(in_product_gtin, out_product_gtin);
  }
  if (!out_product_gtin.isNull())
  {
    PL_A2 = out_product_gtin.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_product_gtin(InProductGtinId, InLogisticalUnitId, InProductId, InProductGtin, OutProductGtin)
PREDICATE(post_product_gtin, 5)
{
  ProductGtinController product_gtin_controller;
  Json::Value in_product_gtin = PlTerm_to_json(PL_A4);
  Json::Value out_product_gtin;
  if (in_product_gtin.isArray())
  {
    product_gtin_controller.post_product_gtin(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), product_gtin_array_to_product_gtin_json(in_product_gtin), out_product_gtin);
  }
  else
  {
    product_gtin_controller.post_product_gtin(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), in_product_gtin, out_product_gtin);
  }
  if (!out_product_gtin.isNull())
  {
    PL_A5 = out_product_gtin.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_product_gtin(InProductGtinId, InProductGtin, OutProductGtin)
PREDICATE(put_product_gtin, 3)
{
  ProductGtinController product_gtin_controller;
  Json::Value in_product_gtin = PlTerm_to_json(PL_A2);
  Json::Value out_product_gtin;
  if (in_product_gtin.isArray())
  {
    product_gtin_controller.put_product_gtin(std::string(PL_A1), product_gtin_array_to_product_gtin_json(in_product_gtin), out_product_gtin);
  }
  else
  {
    product_gtin_controller.put_product_gtin(std::string(PL_A1), in_product_gtin, out_product_gtin);
  }
  if (!out_product_gtin.isNull())
  {
    PL_A3 = out_product_gtin.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_product_gtin(InProductGtinId, InLogisticalUnitId, InProductId, InProductGtin, OutProductGtin)
PREDICATE(put_product_gtin, 5)
{
  ProductGtinController product_gtin_controller;
  Json::Value in_product_gtin = PlTerm_to_json(PL_A4);
  Json::Value out_product_gtin;
  if (in_product_gtin.isArray())
  {
    product_gtin_controller.put_product_gtin(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), product_gtin_array_to_product_gtin_json(in_product_gtin), out_product_gtin);
  }
  else
  {
    product_gtin_controller.put_product_gtin(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), in_product_gtin, out_product_gtin);
  }
  if (!out_product_gtin.isNull())
  {
    PL_A5 = out_product_gtin.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_product_gtin(ProductGtinId)
PREDICATE(delete_product_gtin, 1)
{
  ProductGtinController product_gtin_controller;
  return product_gtin_controller.delete_product_gtin(std::string(PL_A1));
}

// ProductGroup

// get_product_groups(StoreId, ProductGroupList)
PREDICATE(get_product_groups, 2)
{
  ProductGroupController product_group_controller;

  PlTail product_groups(PL_A2);
  for (const Json::Value &product_group : product_group_controller.get_product_groups(std::string(PL_A1)))
  {
    product_groups.append(product_group.toStyledString().c_str());
  }
  return product_groups.close();
}

// get_product_group(ProductGroupId, ProductGroup)
PREDICATE(get_product_group, 2)
{
  ProductGroupController product_group_controller;

  Json::Value product_group = product_group_controller.get_product_group(std::string(PL_A1));
  std::string product_group_id = product_group["id"].asString();
  if (std::stoi(std::string(PL_A1)) == std::stoi(product_group_id))
  {
    PL_A2 = product_group.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_product_to_product_group(InProductGroupId, InProductId, OutProductGroup)
PREDICATE(post_product_to_product_group, 3)
{
  ProductGroupController product_group_controller;
  Json::Value out_product_group;

  if (product_group_controller.post_product_to_product_group(std::string(PL_A1), std::string(PL_A2), out_product_group))
  {

    PL_A3 = out_product_group.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_product_group(InStoreId, InProductGroup, OutProductGroup)
PREDICATE(post_product_group, 3)
{
  ProductGroupController product_group_controller;
  Json::Value in_product_group = PlTerm_to_json(PL_A2);
  Json::Value out_product_group;
  if (product_group_controller.post_product_group(std::string(PL_A1), in_product_group, out_product_group))
  {
    PL_A3 = out_product_group.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_product_from_product_group(ProductGroupId, ProductId)
PREDICATE(delete_product_from_product_group, 2)
{
  ProductGroupController product_group_controller;
  return product_group_controller.delete_product_from_product_group(std::string(PL_A1), std::string(PL_A2));
}

// delete_product_group(ProductGroupId)
PREDICATE(delete_product_group, 1)
{
  ProductGroupController product_group_controller;
  return product_group_controller.delete_product_group(std::string(PL_A1));
}

// Shelf

const Json::Value shelf_array_to_shelf_json(const Json::Value &shelf_array)
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
  else if (shelf_array.size() == 13)
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
    shelf_json["productGroupId"] = shelf_array[11];
    shelf_json["width"] = shelf_array[12];
  }
  else if (shelf_array.size() == 14)
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
    shelf_json["productGroupId"] = shelf_array[11];
    shelf_json["storeId"] = shelf_array[12];
    shelf_json["width"] = shelf_array[13];
  }
  else
  {
    std::cerr << "Invalid shelf array (length = " << shelf_array.size() << ")" << std::endl;
  }
  return shelf_json;
}

// get_shelves(StoreId, Shelves)
PREDICATE(get_shelves, 2)
{
  ShelfController shelf_controller;

  PlTail shelves(PL_A2);
  for (const Json::Value &shelf : shelf_controller.get_shelves(std::string(PL_A1)))
  {
    shelves.append(shelf.toStyledString().c_str());
  }
  return shelves.close();
}

// get_shelf(ShelfId, Shelf)
PREDICATE(get_shelf, 2)
{
  ShelfController shelf_controller;

  Json::Value shelf = shelf_controller.get_shelf(std::string(PL_A1));
  std::string shelf_id = shelf["id"].asString();
  if (std::stoi(std::string(PL_A1)) == std::stoi(shelf_id))
  {
    PL_A2 = shelf.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_shelf(InStoreId, InProductGroupId, InShelf, OutShelf)
PREDICATE(post_shelf, 4)
{
  ShelfController shelf_controller;
  Json::Value in_shelf = PlTerm_to_json(PL_A3);
  Json::Value out_shelf;
  if (in_shelf.isArray())
  {
    shelf_controller.post_shelf(std::string(PL_A1), std::string(PL_A2), shelf_array_to_shelf_json(in_shelf), out_shelf);
  }
  else
  {
    shelf_controller.post_shelf(std::string(PL_A1), std::string(PL_A2), in_shelf, out_shelf);
  }
  if (!out_shelf.isNull())
  {
    PL_A4 = out_shelf.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_shelf(InStoreId, InShelf, OutShelf)
PREDICATE(post_shelf, 3)
{
  ShelfController shelf_controller;
  Json::Value in_shelf = PlTerm_to_json(PL_A2);
  Json::Value out_shelf;
  if (in_shelf.isArray())
  {
    shelf_controller.post_shelf(std::string(PL_A1), shelf_array_to_shelf_json(in_shelf), out_shelf);
  }
  else
  {
    shelf_controller.post_shelf(std::string(PL_A1), in_shelf, out_shelf);
  }
  if (!out_shelf.isNull())
  {
    PL_A3 = out_shelf.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_shelf(InShelfId, InStoreId, InProductGroupId, InShelf, OutShelf)
PREDICATE(put_shelf, 5)
{
  ShelfController shelf_controller;
  Json::Value in_shelf = PlTerm_to_json(PL_A4);
  Json::Value out_shelf;
  if (in_shelf.isArray())
  {
    shelf_controller.put_shelf(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), shelf_array_to_shelf_json(in_shelf), out_shelf);
  }
  else
  {
    shelf_controller.put_shelf(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), in_shelf, out_shelf);
  }
  if (!out_shelf.isNull())
  {
    PL_A5 = out_shelf.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_shelf(InShelfId, InShelf, OutShelf)
PREDICATE(put_shelf, 3)
{
  ShelfController shelf_controller;
  Json::Value in_shelf = PlTerm_to_json(PL_A2);
  Json::Value out_shelf;
  if (in_shelf.isArray())
  {
    shelf_controller.put_shelf(std::string(PL_A1), shelf_array_to_shelf_json(in_shelf), out_shelf);
  }
  else
  {
    shelf_controller.put_shelf(std::string(PL_A1), in_shelf, out_shelf);
  }
  if (!out_shelf.isNull())
  {
    PL_A3 = out_shelf.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_shelf(ShelfId)
PREDICATE(delete_shelf, 1)
{
  ShelfController shelves;
  return shelves.delete_shelf(std::string(PL_A1));
}

//////////////////////////////// Kaviya ////////////////////////////////////////
// // k4r_get_shelf_data(Shelf, ShelfPosition, ShelfOrientation, ShelfDimension)
// PREDICATE(k4r_get_shelf_data, 4)
// {
//   Json::Value shelf = PlTerm_to_json(PL_A1);
//   PlTail shelf_position(PL_A2);
//   shelf_position.append(shelf["positionX"].asDouble());
//   shelf_position.append(shelf["positionY"].asDouble());
//   shelf_position.append(shelf["positionZ"].asDouble());
//   shelf_position.close();
//   PlTail shelf_orientation(PL_A3);
//   shelf_orientation.append(shelf["orientationX"].asDouble());
//   shelf_orientation.append(shelf["orientationY"].asDouble());
//   shelf_orientation.append(shelf["orientationZ"].asDouble());
//   shelf_orientation.append(shelf["orientationW"].asDouble());
//   shelf_orientation.close();
//   PlTail shelf_dimension(PL_A4);
//   shelf_dimension.append(shelf["depth"].asFloat());
//   shelf_dimension.append(shelf["width"].asFloat());
//   shelf_dimension.append(shelf["height"].asFloat());
//   shelf_dimension.close();
//   return true;
// }

// // k4r_get_shelf_by_id(Link, ShelfId, StoreId, ShapeData, Pose)
// PREDICATE(k4r_get_shelf_by_id, 5)
// {
//   ShelfController shelves(PL_A1);
//   PlTerm pose_term;
//   PlTail pose_list(pose_term);

//   Json::Value shelf = shelves.get_shelf(std::string(PL_A2));
//   std::string shelf_id = shelf["id"].asString();
//   remove_new_line(shelf_id);
//   if (std::stoi(std::string(PL_A2)) == std::stoi(shelf_id))
//   {
//     PlTerm shape_term;
//     PlTail shape(shape_term);

//     shape.append(shelf["depth"].asFloat());
//     shape.append(shelf["width"].asFloat());
//     shape.append(shelf["height"].asFloat());

//     PL_A3 = shelf["storeId"].asInt();

//     PlTerm pos_term;

//     PlTail position_list(pos_term);

//     position_list.append(shelf["positionX"].asDouble());
//     position_list.append(shelf["positionY"].asDouble());
//     position_list.append(shelf["positionZ"].asDouble());
//     position_list.close();

//     PlTerm rotation_term;

//     PlTail rotation_list(rotation_term);

//     // rotation_list.append(shelf["orientationX"].asDouble());
//     rotation_list.append(shelf["orientationY"].asDouble());
//     rotation_list.append(shelf["orientationZ"].asDouble());
//     //rotation_list.append(shelf["orientationW"].asDouble());

//     pose_list.append(pos_term);
//     pose_list.append(rotation_term);

//     PL_A4 = shape_term;
//     PL_A5 = pose_term;

//     return true;
//   }
//   else
//   {
//     return false;
//   }
// }

// // k4r_post_shelf(Link, StoreId, [Translation, Rotation], [D,W,H], ProductGroupId, ExtRefId, CadPlanId)
// PREDICATE(k4r_post_shelf, 7)
// {
//   ShelfController shelves(PL_A1, std::string(PL_A2));

//   Json::Value shelf_data;

//   PlTail pose_list(PL_A3);
//   PlTerm temp_term, traversal_term;
//   pose_list.next(temp_term);

//   PlTail translation(temp_term);

//   translation.next(traversal_term);
//   shelf_data["positionX"] = (double)traversal_term;
//   translation.next(traversal_term);
//   shelf_data["positionY"] = (double)traversal_term;
//   translation.next(traversal_term);
//   shelf_data["positionZ"] = (double)traversal_term;
//   pose_list.next(temp_term);

//   PlTail rotation(temp_term);

//   rotation.next(traversal_term);
//   shelf_data["orientationX"] = (double)traversal_term;
//   rotation.next(traversal_term);
//   shelf_data["orientationY"] = (double)traversal_term;
//   rotation.next(traversal_term);
//   shelf_data["orientationZ"] = (double)traversal_term;
//   rotation.next(traversal_term);
//   shelf_data["orientationW"] = (double)traversal_term;
//   PlTail dimension(PL_A4);
//   dimension.next(traversal_term);
//   shelf_data["depth"] = (int)traversal_term;
//   dimension.next(traversal_term);
//   shelf_data["width"] = (int)traversal_term;
//   dimension.next(traversal_term);
//   shelf_data["height"] = (int)traversal_term;
//   shelf_data["productGroupId"] = (int) PL_A5;
//   shelf_data["externalReferenceId"] = (std::string) PL_A6;
//   shelf_data["cadPlanId"] = (std::string) PL_A7;
//   return shelves.post_shelf(shelf_data);
// }
///////////////////////////////////////////////////////////////////

// Shelf layer

//////////////////////////////// Kaviya ////////////////////////////////////////
// // k4r_post_shelf_layer(Link, ShelfId, Z, [D, W, H], Level, ExtRefId, Type)
// PREDICATE(k4r_post_shelf_layer, 7)
// {
//   ShelfLayerController shelf_layer_controller(PL_A1, std::string(PL_A2));

//   Json::Value shelf_layer;
//   shelf_layer["positionZ"] =  double(PL_A3);

//   PlTerm traversal_term;
//   PlTail dimension(PL_A4);

//   dimension.next(traversal_term);
//   double temp = double(traversal_term);
//   shelf_layer["depth"]  = int(temp *1000);
//   dimension.next(traversal_term);
//   temp = double(traversal_term);
//   shelf_layer["width"] = int(temp *1000);
//   dimension.next(traversal_term);
//   temp = double(traversal_term);
//   shelf_layer["height"] = int(temp*1000);
//   shelf_layer["level"] = int (PL_A5);
//   shelf_layer["externalReferenceId"] = (std::string) PL_A6;
//   shelf_layer["type"] = (std::string) PL_A7;
//   std::cout << shelf_layer << std::endl;
//   return shelf_layer_controller.post_shelf_layer(shelf_layer);
// }
/////////////////////////////////////////////////////////////////////////////////

const Json::Value shelf_layer_array_to_shelf_layer_json(const Json::Value &shelf_layer_array)
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
    std::cerr << "Invalid shelf layer array (length = " << shelf_layer_array.size() << ")" << std::endl;
  }
  return shelf_layer_json;
}

// get_shelf_layers(ShelfId, ShelfLayers)
PREDICATE(get_shelf_layers, 2)
{
  ShelfLayerController shelf_layer_controller;

  PlTail shelf_layers(PL_A2);
  for (const Json::Value &shelf_layer : shelf_layer_controller.get_shelf_layers(std::string(PL_A1)))
  {
    shelf_layers.append(shelf_layer.toStyledString().c_str());
  }
  return shelf_layers.close();
}

// get_shelf_layer(ShelfLayerId, ShelfLayer)
PREDICATE(get_shelf_layer, 2)
{
  ShelfLayerController shelf_layer_controller;

  Json::Value shelf_layer = shelf_layer_controller.get_shelf_layer(std::string(PL_A1));
  std::string shelf_layer_id = shelf_layer["id"].asString();
  if (std::stoi(std::string(PL_A1)) == std::stoi(shelf_layer_id))
  {
    PL_A2 = shelf_layer.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_shelf_layer(InShelfId, InShelfLayer, OutShelfLayer)
PREDICATE(post_shelf_layer, 3)
{
  ShelfLayerController shelf_layer_controller;
  Json::Value in_shelf_layer = PlTerm_to_json(PL_A2);
  Json::Value out_shelf_layer;
  if (in_shelf_layer.isArray())
  {
    shelf_layer_controller.post_shelf_layer(std::string(PL_A1), shelf_layer_array_to_shelf_layer_json(in_shelf_layer), out_shelf_layer);
  }
  else
  {
    shelf_layer_controller.post_shelf_layer(std::string(PL_A1), in_shelf_layer, out_shelf_layer);
  }
  if (!out_shelf_layer.isNull())
  {
    PL_A3 = out_shelf_layer.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_shelf_layer(ShelfLayerId)
PREDICATE(delete_shelf_layer, 1)
{
  ShelfLayerController shelf_layer_controller;
  return shelf_layer_controller.delete_shelf_layer(std::string(PL_A1));
}

// Facing

const Json::Value facing_array_to_facing_json(const Json::Value &facing_array)
{
  Json::Value facing_json;
  if (facing_array.size() == 3)
  {
    facing_json["layerRelativePosition"] = facing_array[0];
    facing_json["noOfItemsDepth"] = facing_array[1];
    facing_json["noOfItemsWidth"] = facing_array[2];
  }
  else if (facing_array.size() == 4)
  {
    facing_json["layerRelativePosition"] = facing_array[0];
    facing_json["noOfItemsDepth"] = facing_array[1];
    facing_json["noOfItemsWidth"] = facing_array[2];
    facing_json["shelfLayerId"] = facing_array[3];
  }
  else
  {
    std::cerr << "Invalid shelf layer array (length = " << facing_array.size() << ")" << std::endl;
  }
  return facing_json;
}

// get_facings(ShelfLayerId, FacingList)
PREDICATE(get_facings, 2)
{
  FacingController facing_controller;

  PlTail facings(PL_A2);
  for (const Json::Value &facing : facing_controller.get_facings(std::string(PL_A1)))
  {
    facings.append(facing.toStyledString().c_str());
  }
  return facings.close();
}

// get_facing(FacingId, Facing)
PREDICATE(get_facing, 2)
{
  FacingController facing_controller;

  Json::Value facing = facing_controller.get_facing(std::string(PL_A1));
  std::string facing_id = facing["id"].asString();
  if (std::stoi(std::string(PL_A1)) == std::stoi(facing_id))
  {
    PL_A2 = facing.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_facing(ShelfLayerId, InFacing, OutFacing)
PREDICATE(post_facing, 3)
{
  FacingController facing_controller;
  Json::Value in_facing = PlTerm_to_json(PL_A2);
  Json::Value out_facing;
  if (in_facing.isArray())
  {
    facing_controller.post_facing(std::string(PL_A1), facing_array_to_facing_json(in_facing), out_facing);
  }
  else
  {
    facing_controller.post_facing(std::string(PL_A1), in_facing, out_facing);
  }
  if (!out_facing.isNull())
  {
    PL_A3 = out_facing.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_facing(InFacingId, InFacing, OutFacing)
PREDICATE(put_facing, 3)
{
  FacingController facing_controller;
  Json::Value in_facing = PlTerm_to_json(PL_A2);
  Json::Value out_facing;
  if (in_facing.isArray())
  {
    facing_controller.put_facing(std::string(PL_A1), facing_array_to_facing_json(in_facing), out_facing);
  }
  else
  {
    facing_controller.put_facing(std::string(PL_A1), in_facing, out_facing);
  }
  if (!out_facing.isNull())
  {
    PL_A3 = out_facing.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_facing(InFacingId, InShelfLayerId, InFacing, OutFacing)
PREDICATE(put_facing, 4)
{
  FacingController facing_controller;
  Json::Value in_facing = PlTerm_to_json(PL_A3);
  Json::Value out_facing;
  if (in_facing.isArray())
  {
    facing_controller.put_facing(std::string(PL_A1), std::string(PL_A2), facing_array_to_facing_json(in_facing), out_facing);
  }
  else
  {
    facing_controller.put_facing(std::string(PL_A1), std::string(PL_A2), in_facing, out_facing);
  }
  if (!out_facing.isNull())
  {
    PL_A4 = out_facing.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_facing(FacingId)
PREDICATE(delete_facing, 1)
{
  FacingController facing_controller;
  return facing_controller.delete_facing(std::string(PL_A1));
}

// // Shopping basket

// PREDICATE(k4r_get_shopping_basket_positions, 4)
// {
//   ShoppingBasketPositionController shopping_basket_position_controller(PL_A1, std::string(PL_A2), std::string(PL_A3));

//   PlTail shopping_basket_positions(PL_A4);
//   for (const Json::Value &shopping_basket_position : shopping_basket_position_controller.get_shopping_basket_positions())
//   {
//     shopping_basket_positions.append(shopping_basket_position.toStyledString().c_str());
//   }
//   return shopping_basket_positions.close();
// }

// PREDICATE(k4r_get_shopping_basket_position, 3)
// {
//   ShoppingBasketPositionController shopping_basket_position_controller(PL_A1);

//   Json::Value shopping_basket_position = shopping_basket_position_controller.get_shopping_basket_position(std::string(PL_A2));
//   std::string shopping_basket_position_id = shopping_basket_position["id"].asString();

//   if (std::stoi(std::string(PL_A2)) == std::stoi(shopping_basket_position_id))
//   {
//     PL_A3 = shopping_basket_position.toStyledString().c_str();
//     return true;
//   }
//   else
//   {
//     return false;
//   }
// }

// Json::Value shopping_basket_position_array_to_shopping_basket_position_json(const Json::Value& shopping_basket_position_array)
// {
//   Json::Value shopping_basket_position_json;
//   if (shopping_basket_position_array.size() == 3)
//   {
//     shopping_basket_position_json["currency"] = shopping_basket_position_array[0];
//     shopping_basket_position_json["quantity"] = shopping_basket_position_array[1];
//     shopping_basket_position_json["sellingPrice"] = shopping_basket_position_array[2];
//   }
//   else
//   {
//     std::cout << "Invalid shopping basket position array (length = " << shopping_basket_position_array.size() << ")" << std::endl;
//   }
//   return shopping_basket_position_json;
// }

// PREDICATE(k4r_post_shopping_basket_position, 5)
// {
//   ShoppingBasketPositionController shopping_basket_position_controller(PL_A1, std::string(PL_A2), std::string(PL_A3), std::string(PL_A4));
//   Json::Value shopping_basket_position = PlTerm_to_json(PL_A5);
//   if (shopping_basket_position.isArray())
//   {
//     return shopping_basket_position_controller.post_shopping_basket_position(shopping_basket_position_array_to_shopping_basket_position_json(shopping_basket_position));
//   }
//   else
//   {
//     return shopping_basket_position_controller.post_shopping_basket_position(shopping_basket_position);
//   }
// }

// PREDICATE(k4r_delete_shopping_basket_position, 2)
// {
//   ShoppingBasketPositionController shopping_basket_positions(PL_A1);
//   return shopping_basket_positions.delete_shopping_basket_position(std::string(PL_A2));
// }

// PREDICATE(k4r_delete_shopping_basket_positions, 3)
// {
//   ShoppingBasketPositionController shopping_basket_positions(PL_A1, std::string(PL_A2), std::string(PL_A3));
//   return shopping_basket_positions.delete_shopping_basket_positions();
// }

// // Planogram

// // k4r_get_planograms(Link, Planograms)
// PREDICATE(k4r_get_planograms, 2)
// {
//   PlanogramController planogram_controller(PL_A1);

//   PlTail planograms(PL_A2);
//   for (const Json::Value &planogram : planogram_controller.get_planograms())
//   {
//     planograms.append(planogram.toStyledString().c_str());
//   }
//   return planograms.close();
// }

// Json::Value planogram_array_to_planogram_json(const Json::Value& planogram_array)
// {
//   Json::Value planogram_json;
//   if (planogram_array.size() == 4)
//   {
//     planogram_json["numberOfFacings"] = planogram_array[0];
//     planogram_json["orientationYaw"] = planogram_array[1];
//     planogram_json["positionX"] = planogram_array[2];
//     planogram_json["versionTimestamp"] = planogram_array[3];
//   }
//   else
//   {
//     std::cout << "Invalid planogram array (length = " << planogram_array.size() << ")" << std::endl;
//   }
//   return planogram_json;
// }

// // k4r_post_planogram(Link, ProductId, ShelfLayerId, Planogram)
// PREDICATE(k4r_post_planogram, 4)
// {
//   PlanogramController planogram_controller(PL_A1, std::string(PL_A2), std::string(PL_A3));
//   Json::Value planogram = PlTerm_to_json(PL_A4);
//   if (planogram.isArray())
//   {
//     return planogram_controller.post_planogram(planogram_array_to_planogram_json(planogram));
//   }
//   else
//   {
//     return planogram_controller.post_planogram(planogram);
//   }
// }

// // k4r_put_planogram(Link, ProductId, ShelfLayerId, PlanogramId, Planogram)
// PREDICATE(k4r_put_planogram, 5)
// {
//   PlanogramController planogram_controller(PL_A1, std::string(PL_A2), std::string(PL_A3));
//   Json::Value planogram = PlTerm_to_json(PL_A5);
//   if (planogram.isArray())
//   {
//     return planogram_controller.put_planogram(std::string(PL_A4), planogram_array_to_planogram_json(planogram));
//   }
//   else
//   {
//     return planogram_controller.put_planogram(std::string(PL_A4), planogram);
//   }
// }

// // k4r_delete_planogram(Link, PlanogramId)
// PREDICATE(k4r_delete_planogram, 2)
// {
//   PlanogramController planogram_controller(PL_A1);
//   return planogram_controller.delete_planogram(std::string(PL_A2));
// }