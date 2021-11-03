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
#include "Entities/DeviceController.cpp"
#include "Entities/FacingController.cpp"
#include "Entities/ItemController.cpp"
#include "Entities/ItemGroupController.cpp"
#include "Entities/MaterialGroupController.cpp"
#include "Entities/PlanogramController.cpp"
#include "Entities/ProductCharacteristicController.cpp"
#include "Entities/ProductController.cpp"
#include "Entities/ProductDescriptionController.cpp"
#include "Entities/ProductGroupController.cpp"
#include "Entities/ProductGtinController.cpp"
#include "Entities/ProductPropertyController.cpp"
#include "Entities/ProductUnitController.cpp"
#include "Entities/ShelfController.cpp"
#include "Entities/ShelfLayerController.cpp"
#include "Entities/ShoppingBasketPositionController.cpp"
#include "Entities/StoreCharacteristicController.cpp"
#include "Entities/StoreController.cpp"
#include "Entities/StoreObjectController.cpp"
#include "Entities/StorePropertyController.cpp"
#include "GraphQl/GraphQlController.cpp"
#include <boost/lexical_cast.hpp>

PREDICATE(double_m_to_int_mm, 2)
{
  PlTail tail_1(PL_A1);
  PlTerm term_1;
  PlTail tail_2(PL_A2);
  PlTerm term_2;
  while (tail_1.next(term_1))
  {
    if (tail_2.next(term_2))
    {
      term_2 = int(boost::lexical_cast<double>(std::string(term_1)) * 1000);
    }
    else
    {
      return false;
    }
  }
  return true;
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
      while (tail.next(term))
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
  if (!customer["id"].isNull())
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
  if (!store["id"].isNull())
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

// StoreCharacteristic

// get_store_characteristics(StoreCharacteristics)
PREDICATE(get_store_characteristics, 1)
{
  StoreCharacteristicController store_characteristic_controller;

  PlTail store_characteristics(PL_A1);
  for (const Json::Value &store_characteristic : store_characteristic_controller.get_store_characteristics())
  {
    store_characteristics.append(store_characteristic.toStyledString().c_str());
  }
  return store_characteristics.close();
}

// post_store_characteristic(InStoreCharacteristicName, OutStoreCharacteristic)
PREDICATE(post_store_characteristic, 2)
{
  StoreCharacteristicController store_characteristic_controller;
  Json::Value out_store_characteristic;
  if (store_characteristic_controller.post_store_characteristic(std::string(PL_A1), out_store_characteristic))
  {
    PL_A2 = out_store_characteristic.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_store_characteristic(CharacteristicId)
PREDICATE(delete_store_characteristic, 1)
{
  StoreCharacteristicController store_characteristic_controller;
  return store_characteristic_controller.delete_store_characteristic(std::string(PL_A1));
}

// StoreObject

const Json::Value store_object_array_to_store_object_json(const Json::Value &store_object_array)
{
  Json::Value store_object_json;
  if (store_object_array.size() == 13)
  {
    store_object_json["depth"] = store_object_array[0];
    store_object_json["description"] = store_object_array[1];
    store_object_json["height"] = store_object_array[2];
    store_object_json["locationTimestamp"] = store_object_array[3];
    store_object_json["orientationW"] = store_object_array[4];
    store_object_json["orientationX"] = store_object_array[5];
    store_object_json["orientationY"] = store_object_array[6];
    store_object_json["orientationZ"] = store_object_array[7];
    store_object_json["positionX"] = store_object_array[8];
    store_object_json["positionY"] = store_object_array[9];
    store_object_json["positionZ"] = store_object_array[10];
    store_object_json["type"] = store_object_array[11];
    store_object_json["width"] = store_object_array[12];
  }
  else
  {
    std::cerr << "Invalid store object array (length = " << store_object_array.size() << ")" << std::endl;
  }
  return store_object_json;
}

// get_store_objects(StoreId, StoreObjects)
PREDICATE(get_store_objects, 2)
{
  StoreObjectController store_object_controller;

  PlTail store_objects(PL_A2);
  for (const Json::Value &store_object : store_object_controller.get_store_objects(std::string(PL_A1)))
  {
    store_objects.append(store_object.toStyledString().c_str());
  }
  return store_objects.close();
}

// post_store_object(StoreId, InStoreObjectObject, OutStoreObject)
PREDICATE(post_store_object, 3)
{
  StoreObjectController store_object_controller;
  Json::Value in_store_object = PlTerm_to_json(PL_A2);
  Json::Value out_store_object;
  if (in_store_object.isArray() ? store_object_controller.post_store_object(std::string(PL_A1), store_object_array_to_store_object_json(in_store_object), out_store_object) : store_object_controller.post_store_object(std::string(PL_A1), in_store_object, out_store_object))
  {
    PL_A3 = out_store_object.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_store_object(ObjectId)
PREDICATE(delete_store_object, 1)
{
  StoreObjectController store_object_controller;
  return store_object_controller.delete_store_object(std::string(PL_A1));
}

// StoreProperty

// get_store_properties(StoreId, Properties)
PREDICATE(get_store_properties, 2)
{
  StorePropertyController store_property_controller;

  PlTail store_properties(PL_A2);
  for (const Json::Value &store_property : store_property_controller.get_store_properties(std::string(PL_A1)))
  {
    store_properties.append(store_property.toStyledString().c_str());
  }
  return store_properties.close();
}

// post_store_property(InStoreId, InStoreCharacteristicId, InValueLow, InValueHigh, OutStoreProperty)
PREDICATE(post_store_property, 5)
{
  StorePropertyController store_property_controller;
  Json::Value out_store_property;
  if (store_property_controller.post_store_property(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), std::string(PL_A4), out_store_property))
  {
    PL_A5 = out_store_property.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_store_property(StoreId, StoreCharacteristicId)
PREDICATE(delete_store_property, 2)
{
  StorePropertyController store_property_controller;
  return store_property_controller.delete_store_property(std::string(PL_A1), std::string(PL_A2));
}

// MaterialGroup

const Json::Value material_group_array_to_material_group_json(const Json::Value &material_group_array)
{
  Json::Value material_group_json;
  if (material_group_array.size() == 3)
  {
    material_group_json["description"] = material_group_array[0];
    material_group_json["hierarchyLevel"] = material_group_array[1];
    material_group_json["name"] = material_group_array[2];
  }
  else if (material_group_array.size() == 4)
  {
    material_group_json["description"] = material_group_array[0];
    material_group_json["hierarchyLevel"] = material_group_array[1];
    material_group_json["name"] = material_group_array[2];
    material_group_json["parentId"] = material_group_array[3];
  }
  else
  {
    std::cerr << "Invalid material_group array (length = " << material_group_array.size() << ")" << std::endl;
  }
  return material_group_json;
}

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
  if (!material_group["id"].isNull())
  {
    PL_A2 = material_group.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_material_group(InParentId, InMaterialGroup, OutMaterialGroup)
PREDICATE(post_material_group, 3)
{
  MaterialGroupController material_group_controller;
  Json::Value in_material_group = PlTerm_to_json(PL_A2);
  Json::Value out_material_group;
  if (in_material_group.isArray())
  {
    material_group_controller.post_material_group(std::string(PL_A1), material_group_array_to_material_group_json(in_material_group), out_material_group);
  }
  else
  {
    material_group_controller.post_material_group(std::string(PL_A1), in_material_group, out_material_group);
  }
  if (!out_material_group.isNull())
  {
    PL_A3 = out_material_group.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_material_group(InMaterialGroupId, InParentId, InMaterialGroup, OutMaterialGroup)
PREDICATE(put_material_group, 4)
{
  MaterialGroupController material_group_controller;
  Json::Value in_material_group = PlTerm_to_json(PL_A3);
  Json::Value out_material_group;
  if (in_material_group.isArray())
  {
    material_group_controller.put_material_group(std::string(PL_A1), std::string(PL_A2), material_group_array_to_material_group_json(in_material_group), out_material_group);
  }
  else
  {
    material_group_controller.put_material_group(std::string(PL_A1), std::string(PL_A2), in_material_group, out_material_group);
  }
  if (!out_material_group.isNull())
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
  for (const Json::Value &product_array : products_array)
  {
    products_json["products"].append(product_array_to_product_json(product_array));
  }
  return products_json;
}

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

// ProductCharacteristic

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

// post_product_characteristic(InProductCharacteristicCode, InProductCharacteristicName, OutProductCharacteristic)
PREDICATE(post_product_characteristic, 3)
{
  ProductCharacteristicController product_characteristic_controller;
  Json::Value out_product_characteristic;
  if (product_characteristic_controller.post_product_characteristic(std::string(PL_A1), std::string(PL_A2), out_product_characteristic))
  {
    PL_A3 = out_product_characteristic.toStyledString().c_str();
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

// ProductProperty

// get_product_properties(ProductId, StoreId, Properties)
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

// post_product_property(InProductId, InProductCharacteristicId, InStoreId, InValueLow, InValueHigh, OutProductProperty)
PREDICATE(post_product_property, 6)
{
  ProductPropertyController product_property_controller;
  Json::Value out_product_property;
  if (product_property_controller.post_product_property(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), std::string(PL_A4), std::string(PL_A5), out_product_property))
  {
    PL_A6 = out_product_property.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_product_property(ProductId, ProductCharacteristicId, StoreId)
PREDICATE(delete_product_property, 3)
{
  ProductPropertyController product_property_controller;
  return product_property_controller.delete_product_property(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3));
}

// ProductUnit

const Json::Value product_unit_array_to_product_unit_json(const Json::Value &product_unit_array)
{
  Json::Value product_unit_json;

  if (product_unit_array.size() == 13)
  {
    product_unit_json["denominatorBaseUnit"] = product_unit_array[0];
    product_unit_json["dimensionUnit"] = product_unit_array[1];
    product_unit_json["grossWeight"] = product_unit_array[2];
    product_unit_json["height"] = product_unit_array[3];
    product_unit_json["length"] = product_unit_array[4];
    product_unit_json["maxStackSize"] = product_unit_array[5];
    product_unit_json["netWeight"] = product_unit_array[6];
    product_unit_json["numeratorBaseUnit"] = product_unit_array[7];
    product_unit_json["unitCode"] = product_unit_array[8];
    product_unit_json["volume"] = product_unit_array[9];
    product_unit_json["volumeUnit"] = product_unit_array[10];
    product_unit_json["weightUnit"] = product_unit_array[11];
    product_unit_json["width"] = product_unit_array[12];
  }
  else
  {
    std::cerr << "Invalid product unit array (length = " << product_unit_array.size() << ")" << std::endl;
  }
  return product_unit_json;
}

// get_product_units(ProductUnitList)
PREDICATE(get_product_units, 1)
{
  ProductUnitController product_unit_controller;

  PlTail product_units(PL_A1);
  for (const Json::Value &product_unit : product_unit_controller.get_product_units())
  {
    product_units.append(product_unit.toStyledString().c_str());
  }
  return product_units.close();
}

// get_product_unit(ProductUnitId, ProductUnit)
PREDICATE(get_product_unit, 2)
{
  ProductUnitController product_unit_controller;

  Json::Value product_unit = product_unit_controller.get_product_unit(std::string(PL_A1));
  if (!product_unit["id"].isNull())
  {
    PL_A2 = product_unit.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_product_unit(InProductId, InProductUnit, OutProductUnit)
PREDICATE(post_product_unit, 3)
{
  ProductUnitController product_unit_controller;
  Json::Value in_product_unit = PlTerm_to_json(PL_A2);
  Json::Value out_product_unit;
  if (in_product_unit.isArray() ? product_unit_controller.post_product_unit(std::string(PL_A1), product_unit_array_to_product_unit_json(in_product_unit), out_product_unit) : product_unit_controller.post_product_unit(std::string(PL_A1), in_product_unit, out_product_unit))
  {
    PL_A3 = out_product_unit.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_product_unit(InProductUnitId, InProductId, InProductUnit, OutProductUnit)
PREDICATE(put_product_unit, 4)
{
  ProductUnitController product_unit_controller;
  Json::Value in_product_unit = PlTerm_to_json(PL_A3);
  Json::Value out_product_unit;
  if (in_product_unit.isArray() ? product_unit_controller.put_product_unit(std::string(PL_A1), std::string(PL_A2), product_unit_array_to_product_unit_json(in_product_unit), out_product_unit) : product_unit_controller.put_product_unit(std::string(PL_A1), std::string(PL_A2), in_product_unit, out_product_unit))
  {
    PL_A4 = out_product_unit.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_product_unit(ProductUnitId)
PREDICATE(delete_product_unit, 1)
{
  ProductUnitController product_unit_controller;
  return product_unit_controller.delete_product_unit(std::string(PL_A1));
}

// ProductDescription

const Json::Value product_description_array_to_product_description_json(const Json::Value &product_description_array)
{
  Json::Value product_description_json;
  if (product_description_array.size() == 2)
  {
    product_description_json["description"] = product_description_array[0];
    product_description_json["isoLanguageCode"] = product_description_array[1];
  }
  else
  {
    std::cerr << "Invalid product description array (length = " << product_description_array.size() << ")" << std::endl;
  }
  return product_description_json;
}

// get_product_descriptions(ProductDescriptionList)
PREDICATE(get_product_descriptions, 1)
{
  ProductDescriptionController product_description_controller;

  PlTail product_descriptions(PL_A1);
  for (const Json::Value &product_description : product_description_controller.get_product_descriptions())
  {
    product_descriptions.append(product_description.toStyledString().c_str());
  }
  return product_descriptions.close();
}

// get_product_description(ProductDescriptionId, ProductDescription)
PREDICATE(get_product_description, 2)
{
  ProductDescriptionController product_description_controller;

  Json::Value product_description = product_description_controller.get_product_description(std::string(PL_A1));
  std::string product_description_id = product_description["id"].asString();
  if (std::string(PL_A1) == product_description_id)
  {
    PL_A2 = product_description.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_product_description(InProductId, InProductDescription, OutProductDescription)
PREDICATE(post_product_description, 3)
{
  ProductDescriptionController product_description_controller;
  Json::Value in_product_description = PlTerm_to_json(PL_A2);
  Json::Value out_product_description;
  if (in_product_description.isArray() ? product_description_controller.post_product_description(std::string(PL_A1), product_description_array_to_product_description_json(in_product_description), out_product_description) : product_description_controller.post_product_description(std::string(PL_A1), in_product_description, out_product_description))
  {
    PL_A3 = out_product_description.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_product_description(InProductId, InProductId, InProductDescription, OutProductDescription)
PREDICATE(put_product_description, 4)
{
  ProductDescriptionController product_description_controller;
  Json::Value in_product_description = PlTerm_to_json(PL_A3);
  Json::Value out_product_description;
  if (in_product_description.isArray() ? product_description_controller.put_product_description(std::string(PL_A1), std::string(PL_A2), product_description_array_to_product_description_json(in_product_description), out_product_description) : product_description_controller.put_product_description(std::string(PL_A1), std::string(PL_A2), in_product_description, out_product_description))
  {
    PL_A4 = out_product_description.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_product_description(ProductDescriptionId)
PREDICATE(delete_product_description, 1)
{
  ProductDescriptionController product_description_controller;
  return product_description_controller.delete_product_description(std::string(PL_A1));
}

// ProductGtin

const Json::Value product_gtin_array_to_product_gtin_json(const Json::Value &product_gtin_array)
{
  Json::Value product_gtin_json;
  if (product_gtin_array.size() == 3)
  {
    product_gtin_json["gtin"] = product_gtin_array[0];
    product_gtin_json["gtinType"] = product_gtin_array[1];
    product_gtin_json["mainGtin"] = product_gtin_array[2];
  }
  else
  {
    std::cerr << "Invalid product gtin array (length = " << product_gtin_array.size() << ")" << std::endl;
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

// post_product_gtin(InProductUnitId, InProductGtin, OutProductGtin)
PREDICATE(post_product_gtin, 3)
{
  ProductGtinController product_gtin_controller;
  Json::Value in_product_gtin = PlTerm_to_json(PL_A2);
  Json::Value out_product_gtin;
  if (in_product_gtin.isArray() ? product_gtin_controller.post_product_gtin(std::string(PL_A1), product_gtin_array_to_product_gtin_json(in_product_gtin), out_product_gtin) : product_gtin_controller.post_product_gtin(std::string(PL_A1), in_product_gtin, out_product_gtin))
  {
    PL_A3 = out_product_gtin.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_product_gtin(InProductUnitId, InProductId, InProductGtin, OutProductGtin)
PREDICATE(put_product_gtin, 4)
{
  ProductGtinController product_gtin_controller;
  Json::Value in_product_gtin = PlTerm_to_json(PL_A3);
  Json::Value out_product_gtin;
  if (in_product_gtin.isArray() ? product_gtin_controller.put_product_gtin(std::string(PL_A1), std::string(PL_A2), product_gtin_array_to_product_gtin_json(in_product_gtin), out_product_gtin) : product_gtin_controller.put_product_gtin(std::string(PL_A1), std::string(PL_A2), in_product_gtin, out_product_gtin))
  {
    PL_A4 = out_product_gtin.toStyledString().c_str();
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
  if (!product_group["id"].isNull())
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

// post_product_group(InName, InStoreId, InProductGroup, OutProductGroup)
PREDICATE(post_product_group, 4)
{
  ProductGroupController product_group_controller;
  Json::Value in_product_group = PlTerm_to_json(PL_A3);
  Json::Value out_product_group;
  if (product_group_controller.post_product_group(std::string(PL_A1), std::string(PL_A2), in_product_group, out_product_group))
  {
    PL_A4 = out_product_group.toStyledString().c_str();
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
  if (shelf_array.size() == 13)
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
    shelf_json["lengthUnitId"] = shelf_array[12];
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
  if (!shelf["id"].isNull())
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
  if (in_shelf.isArray() ? shelf_controller.post_shelf(std::string(PL_A1), std::string(PL_A2), shelf_array_to_shelf_json(in_shelf), out_shelf) : shelf_controller.post_shelf(std::string(PL_A1), std::string(PL_A2), in_shelf, out_shelf))
  {
    PL_A4 = out_shelf.toStyledString().c_str();
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
  if (in_shelf.isArray() ? shelf_controller.put_shelf(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), shelf_array_to_shelf_json(in_shelf), out_shelf) : shelf_controller.put_shelf(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), in_shelf, out_shelf))
  {
    PL_A5 = out_shelf.toStyledString().c_str();
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
//
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

// Shelf layer

const Json::Value shelf_layer_array_to_shelf_layer_json(const Json::Value &shelf_layer_array)
{
  Json::Value shelf_layer_json;
  if (shelf_layer_array.size() == 8)
  {
    shelf_layer_json["depth"] = shelf_layer_array[0];
    shelf_layer_json["externalReferenceId"] = shelf_layer_array[1];
    shelf_layer_json["height"] = shelf_layer_array[2];
    shelf_layer_json["level"] = shelf_layer_array[3];
    shelf_layer_json["positionZ"] = shelf_layer_array[4];
    shelf_layer_json["type"] = shelf_layer_array[5];
    shelf_layer_json["width"] = shelf_layer_array[6];
    shelf_layer_json["lengthUnitId"] = shelf_layer_array[7];
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
  if (!shelf_layer["id"].isNull())
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
  if (in_shelf_layer.isArray() ? shelf_layer_controller.post_shelf_layer(std::string(PL_A1), shelf_layer_array_to_shelf_layer_json(in_shelf_layer), out_shelf_layer) : shelf_layer_controller.post_shelf_layer(std::string(PL_A1), in_shelf_layer, out_shelf_layer))
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
  if (!facing["id"].isNull())
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
  if (in_facing.isArray() ? facing_controller.post_facing(std::string(PL_A1), facing_array_to_facing_json(in_facing), out_facing) : facing_controller.post_facing(std::string(PL_A1), in_facing, out_facing))
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
  if (in_facing.isArray() ? facing_controller.put_facing(std::string(PL_A1), std::string(PL_A2), facing_array_to_facing_json(in_facing), out_facing) : facing_controller.put_facing(std::string(PL_A1), std::string(PL_A2), in_facing, out_facing))
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

// ItemGroup

// get_item_groups(ItemGroupList)
PREDICATE(get_item_groups, 1)
{
  ItemGroupController item_group_controller;

  PlTail item_groups(PL_A1);
  for (const Json::Value &item_group : item_group_controller.get_item_groups())
  {
    item_groups.append(item_group.toStyledString().c_str());
  }
  return item_groups.close();
}

// get_item_group(ItemGroupId, ItemGroup)
PREDICATE(get_item_group, 2)
{
  ItemGroupController item_group_controller;

  Json::Value item_group = item_group_controller.get_item_group(std::string(PL_A1));
  if (!item_group["id"].isNull())
  {
    PL_A2 = item_group.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_item_group(InFacingId, InProductUnitId, InStock, OutItemGroup)
PREDICATE(post_item_group, 4)
{
  ItemGroupController item_group_controller;
  Json::Value out_item_group;
  if (item_group_controller.post_item_group(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), out_item_group))
  {
    PL_A4 = out_item_group.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_item_group(InItemGroupId, InFacingId, InProductUnitId, InStock, OutItemGroup)
PREDICATE(put_item_group, 5)
{
  ItemGroupController item_group_controller;
  Json::Value out_item_group;
  if (item_group_controller.put_item_group(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), std::string(PL_A4), out_item_group))
  {
    PL_A5 = out_item_group.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_item_group(ItemGroupId)
PREDICATE(delete_item_group, 1)
{
  ItemGroupController item_group_controller;
  return item_group_controller.delete_item_group(std::string(PL_A1));
}

// Item

const Json::Value item_array_to_item_json(const Json::Value &item_array)
{
  Json::Value item_json;
  if (item_array.size() == 3)
  {
    item_json["positionInFacingX"] = item_array[0];
    item_json["positionInFacingY"] = item_array[1];
    item_json["positionInFacingZ"] = item_array[2];
  }
  else
  {
    std::cerr << "Invalid item array (length = " << item_array.size() << ")" << std::endl;
  }
  return item_json;
}

// get_items(ItemList)
PREDICATE(get_items, 1)
{
  ItemController item_controller;

  PlTail items(PL_A1);
  for (const Json::Value &item : item_controller.get_items())
  {
    items.append(item.toStyledString().c_str());
  }
  return items.close();
}

// get_item(ItemId, Item)
PREDICATE(get_item, 2)
{
  ItemController item_controller;

  Json::Value item = item_controller.get_item(std::string(PL_A1));
  if (!item["id"].isNull())
  {
    PL_A2 = item.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// post_item(InItemGroupId, InItem, OutItem)
PREDICATE(post_item, 3)
{
  ItemController item_controller;
  Json::Value in_item = PlTerm_to_json(PL_A2);
  Json::Value out_item;
  if (in_item.isArray() ? item_controller.post_item(std::string(PL_A1), item_array_to_item_json(in_item), out_item) : item_controller.post_item(std::string(PL_A1), in_item, out_item))
  {
    PL_A3 = out_item.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_item(InItemId, InItemGroupId, InItem, OutItem)
PREDICATE(put_item, 4)
{
  ItemController item_controller;
  Json::Value in_item = PlTerm_to_json(PL_A3);
  Json::Value out_item;
  if (in_item.isArray() ? item_controller.put_item(std::string(PL_A1), std::string(PL_A2), item_array_to_item_json(in_item), out_item) : item_controller.put_item(std::string(PL_A1), std::string(PL_A2), in_item, out_item))
  {
    PL_A4 = out_item.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_item(ItemId)
PREDICATE(delete_item, 1)
{
  ItemController item_controller;
  return item_controller.delete_item(std::string(PL_A1));
}

// Planogram

const Json::Value planogram_array_to_planogram_json(const Json::Value &planogram_array)
{
  Json::Value planogram_json;
  if (planogram_array.size() == 4)
  {
    planogram_json["numberOfFacings"] = planogram_array[0];
    planogram_json["orientationYaw"] = planogram_array[1];
    planogram_json["positionX"] = planogram_array[2];
    planogram_json["versionTimestamp"] = planogram_array[3];
  }
  else if (planogram_array.size() == 6)
  {
    planogram_json["logisticalUnitId"] = planogram_array[0];
    planogram_json["numberOfFacings"] = planogram_array[1];
    planogram_json["orientationYaw"] = planogram_array[2];
    planogram_json["positionX"] = planogram_array[3];
    planogram_json["shelfLayerId"] = planogram_array[4];
    planogram_json["versionTimestamp"] = planogram_array[5];
  }

  else
  {
    std::cerr << "Invalid planogram array (length = " << planogram_array.size() << ")" << std::endl;
  }
  return planogram_json;
}

// get_planograms(PlanogramList)
PREDICATE(get_planograms, 1)
{
  PlanogramController planogram_controller;

  PlTail planograms(PL_A1);
  for (const Json::Value &planogram : planogram_controller.get_planograms())
  {
    planograms.append(planogram.toStyledString().c_str());
  }
  return planograms.close();
}

// post_planogram(InProductUnitId, InShelfLayerId, InPlanogram, OutPlanogram)
PREDICATE(post_planogram, 4)
{
  PlanogramController planogram_controller;
  Json::Value in_planogram = PlTerm_to_json(PL_A3);
  Json::Value out_planogram;
  if (in_planogram.isArray() ? planogram_controller.post_planogram(std::string(PL_A1), std::string(PL_A2), planogram_array_to_planogram_json(in_planogram), out_planogram) : planogram_controller.post_planogram(std::string(PL_A1), std::string(PL_A2), in_planogram, out_planogram))
  {
    PL_A4 = out_planogram.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_planogram(InPlanogramId, InProductUnitId, InShelfLayerId, InPlanogram, OutPlanogram)
PREDICATE(put_planogram, 5)
{
  PlanogramController planogram_controller;
  Json::Value in_planogram = PlTerm_to_json(PL_A4);
  Json::Value out_planogram;
  if (in_planogram.isArray() ? planogram_controller.put_planogram(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), planogram_array_to_planogram_json(in_planogram), out_planogram) : planogram_controller.put_planogram(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), in_planogram, out_planogram))
  {
    PL_A5 = out_planogram.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_planogram(PlanogramId)
PREDICATE(delete_planogram, 1)
{
  PlanogramController planogram_controller;
  return planogram_controller.delete_planogram(std::string(PL_A1));
}

// ShoppingBasketPosition

const Json::Value shopping_basket_position_array_to_shopping_basket_position_json(const Json::Value &shopping_basket_position_array)
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
    std::cerr << "Invalid shopping_basket_position array (length = " << shopping_basket_position_array.size() << ")" << std::endl;
  }
  return shopping_basket_position_json;
}

// get_shopping_basket_positions(StoreId, CustomerId, ShoppingBasketPositionList)
PREDICATE(get_shopping_basket_positions, 3)
{
  ShoppingBasketPositionController shopping_basket_position_controller;

  PlTail shopping_basket_positions(PL_A3);
  for (const Json::Value &shopping_basket_position : shopping_basket_position_controller.get_shopping_basket_positions(std::string(PL_A1), std::string(PL_A2)))
  {
    shopping_basket_positions.append(shopping_basket_position.toStyledString().c_str());
  }
  return shopping_basket_positions.close();
}

// post_shopping_basket_position(InStoreId, InCustomerId, InProductId, InShoppingBasketPosition, OutShoppingBasketPosition)
PREDICATE(post_shopping_basket_position, 5)
{
  ShoppingBasketPositionController shopping_basket_position_controller;
  Json::Value in_shopping_basket_position = PlTerm_to_json(PL_A4);
  Json::Value out_shopping_basket_position;
  if (in_shopping_basket_position.isArray() ? shopping_basket_position_controller.post_shopping_basket_position(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), shopping_basket_position_array_to_shopping_basket_position_json(in_shopping_basket_position), out_shopping_basket_position) : shopping_basket_position_controller.post_shopping_basket_position(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), in_shopping_basket_position, out_shopping_basket_position))
  {
    PL_A5 = out_shopping_basket_position.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_shopping_basket_position(ShoppingBasketPositionId)
PREDICATE(delete_shopping_basket_position, 1)
{
  ShoppingBasketPositionController shopping_basket_position_controller;
  return shopping_basket_position_controller.delete_shopping_basket_position(std::string(PL_A1));
}

// delete_shopping_basket_positions(StoreId, CustomerId)
PREDICATE(delete_shopping_basket_positions, 2)
{
  ShoppingBasketPositionController shopping_basket_position_controller;
  return shopping_basket_position_controller.delete_shopping_basket_positions(std::string(PL_A1), std::string(PL_A2));
}

// Device

const Json::Value device_array_to_device_json(const Json::Value &device_array)
{
  Json::Value device_json;
  if (device_array.size() == 2)
  {
    device_json["description"] = device_array[0];
    device_json["deviceType"] = device_array[1];
  }
  else
  {
    std::cerr << "Invalid delivery array (length = " << device_array.size() << ")" << std::endl;
  }
  return device_json;
}

// get_devices(Devices)
PREDICATE(get_devices, 1)
{
  DeviceController device_controller;

  PlTail devices(PL_A1);
  for (const Json::Value &device : device_controller.get_devices())
  {
    devices.append(device.toStyledString().c_str());
  }
  return devices.close();
}

// post_device(InDeviceId, InStoreId, InDelivery, OutDelivery)
PREDICATE(post_device, 4)
{
  DeviceController device_controller;
  Json::Value in_device = PlTerm_to_json(PL_A3);
  Json::Value out_device;
  if (in_device.isArray() ? device_controller.post_device(std::string(PL_A1), std::string(PL_A2), device_array_to_device_json(in_device), out_device) : device_controller.post_device(std::string(PL_A1), std::string(PL_A2), in_device, out_device))
  {
    PL_A4 = out_device.toStyledString().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// delete_device(CharacteristicId)
PREDICATE(delete_device, 1)
{
  DeviceController device_controller;
  return device_controller.delete_device(std::string(PL_A1));
}

// post_graphql(GraphQLQuery, GraphQLResponse)
PREDICATE(post_graphql, 2)
{
  GraphQlController graphql_controller;
  
  std::stringstream out_data;
  if (graphql_controller.post_query(std::string(PL_A1), out_data))
  {
    PL_A2 = out_data.str().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// make_filter(FilterField, Operator, Value, Type, Filter)
PREDICATE(make_filter, 5)
{
  std::string filter = std::string(PL_A1) + ":{" + "operator:\"" + std::string(PL_A2) + "\",value:\"" + std::string(PL_A3) + "\",type:\"" + std::string(PL_A4) + "\"}";
  PL_A5 = filter.c_str();
  return true;
}

// make_key_with_filter(Key, Filter, KeyWithFilter)
PREDICATE(make_key_with_filter, 3)
{
  std::string key_with_filter = std::string(PL_A1) + "(filter:{" + std::string(PL_A2) + "})";
  PL_A3 = key_with_filter.c_str();
  return true;
}