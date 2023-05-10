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
#include "Entities/DataController.cpp"
#include "GraphQl/GraphQlController.cpp"
#include <boost/lexical_cast.hpp>

// PREDICATE(double_m_to_int_mm, 2)
// {
//   PlTail tail_1(PL_A1);
//   PlTerm term_1;
//   PlTail tail_2(PL_A2);
//   PlTerm term_2;
//   while (tail_1.next(term_1))
//   {
//     if (tail_2.next(term_2))
//     {
//       std::string team_2_str = std::string(term_1);
//       if (team_2_str.at(0) != '_')
//       {
//         term_2 = int(boost::lexical_cast<double>(std::string(term_1)) * 1000);
//       }
//     }
//     else
//     {
//       return false;
//     }
//   }
//   return true;
// }

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

PREDICATE(get_entity_from_id, 3)
{
  std::string entity = std::string(PL_A1) + "/";
  DataController data_controller(entity.c_str());

  std::stringstream out_data;
  if (data_controller.get_data(out_data, std::string(PL_A2)))
  {
    PL_A3 = out_data.str().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

PREDICATE(post_entity, 3)
{
  std::string entity = std::string(PL_A1) + "/";
  DataController data_controller(entity.c_str());

  std::stringstream out_data;
  if (data_controller.post_data(std::string(PL_A2), out_data))
  {
    PL_A3 = out_data.str().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

PREDICATE(delete_entity_from_id, 2)
{
  std::string entity = std::string(PL_A1) + "/";
  DataController data_controller(entity.c_str());
  return data_controller.delete_data(std::string(PL_A2));
}

// Customer

const Json::Value customer_array_to_customer_json(const Json::Value &customer_array)
{
  Json::Value customer_json;
  if (customer_array.size() == 1)
  {
    customer_json["anonymisedName"] = customer_array[0];
  }
  else
  {
    std::cerr << "Invalid customer array (length = " << customer_array.size() << ")" << std::endl;
  }
  return customer_json;
}

PREDICATE(post_customer, 2)
{
  DataController customer_controller("customers/");

  std::stringstream out_data;
  Json::Value customer = customer_array_to_customer_json(PlTerm_to_json(PL_A1));
  if (customer_controller.post_data(customer.toStyledString().c_str(), out_data))
  {
    PL_A2 = out_data.str().c_str();
    return true;
  }
  else
  {
    return false;
  }
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

PREDICATE(post_store, 2)
{
  DataController store_controller("stores/");

  std::stringstream out_data;
  Json::Value store = store_array_to_store_json(PlTerm_to_json(PL_A1));
  if (store_controller.post_data(store.toStyledString().c_str(), out_data))
  {
    PL_A2 = out_data.str().c_str();
    return true;
  }
  else
  {
    return false;
  }
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

PREDICATE(post_store_object, 3)
{
  std::string link_tail = "stores/" + std::string(PL_A1) + "/storeobjects";
  DataController store_object_controller(link_tail.c_str());

  std::stringstream out_data;
  Json::Value store = store_array_to_store_json(PlTerm_to_json(PL_A1));
  if (store_object_controller.post_data(store.toStyledString().c_str(), out_data))
  {
    PL_A2 = out_data.str().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// StoreProperty

const Json::Value store_property_array_to_store_property_json(const Json::Value &store_property_array)
{
  Json::Value store_property_json;
  if (store_property_array.size() == 2)
  {
    store_property_json["valueHigh"] = store_property_array[0];
    store_property_json["valueLow"] = store_property_array[1];
  }
  else
  {
    std::cerr << "Invalid store object array (length = " << store_property_array.size() << ")" << std::endl;
  }
  return store_property_json;
}

PREDICATE(post_store_property, 4)
{
  std::string link_tail = "stores/" + std::string(PL_A2) + "/storeproperties/" + std::string(PL_A3);
  DataController store_property_controller(link_tail.c_str());

  std::stringstream out_data;
  Json::Value store_property = store_property_array_to_store_property_json(PlTerm_to_json(PL_A1));
  store_property["storeId"] = std::string(PL_A2);
  store_property["characteristicId"] = std::string(PL_A3);
  if (store_property_controller.post_data(store_property.toStyledString().c_str(), out_data))
  {
    PL_A4 = out_data.str().c_str();
    return true;
  }
  else
  {
    return false;
  }
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

PREDICATE(post_shelf, 4)
{
  std::string link_tail = "stores/" + std::string(PL_A1) + "/shelves/";
  DataController shelf_controller(link_tail.c_str());

  std::stringstream out_data;
  Json::Value shelf = shelf_array_to_shelf_json(PlTerm_to_json(PL_A3));
  shelf["storeId"] = std::string(PL_A1);
  shelf["productGroupId"] = std::string(PL_A2);
  if (shelf_controller.post_data(shelf.toStyledString().c_str(), out_data))
  {
    PL_A4 = out_data.str().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

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

PREDICATE(post_shelf_layer, 3)
{
  std::string link_tail = "shelves/" + std::string(PL_A1) + "/shelflayers/";
  DataController shelf_layer_controller(link_tail.c_str());

  std::stringstream out_data;
  Json::Value shelf_layer = shelf_layer_array_to_shelf_layer_json(PlTerm_to_json(PL_A2));
  shelf_layer["shelfId"] = std::string(PL_A1);
  if (shelf_layer_controller.post_data(shelf_layer.toStyledString().c_str(), out_data))
  {
    PL_A3 = out_data.str().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// Facing

const Json::Value facing_array_to_facing_json(const Json::Value &facing_array)
{
  Json::Value facing_json;
  if (facing_array.size() == 9)
  {
    facing_json["layerRelativePosition"] = facing_array[0];
    facing_json["noOfItemsDepth"] = facing_array[1];
    facing_json["noOfItemsWidth"] = facing_array[2];
    facing_json["noOfItemsHeight"] = facing_array[3];
    facing_json["minStock"] = facing_array[4];
    facing_json["stock"] = facing_array[5];
    facing_json["misplacedStock"] = facing_array[6];
    facing_json["productUnitId"] = facing_array[7];
    facing_json["externalReferenceId"] = facing_array[8];
  }
  else
  {
    std::cerr << "Invalid shelf layer array (length = " << facing_array.size() << ")" << std::endl;
  }
  return facing_json;
}

PREDICATE(post_facing, 3)
{
  std::string link_tail = "shelflayers/" + std::string(PL_A1) + "/facings/";
  DataController facing_controller(link_tail.c_str());

  std::stringstream out_data;
  Json::Value facing = facing_array_to_facing_json(PlTerm_to_json(PL_A2));
  facing["shelfLayerId"] = std::string(PL_A1);
  if (facing_controller.post_data(facing.toStyledString().c_str(), out_data))
  {
    PL_A3 = out_data.str().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_facing(ShelfLayerId, [layerRelativePosition,noOfItemsDepth, noOfItemsWidth, noOfItemsHeight, 
// minStock, stock,  ProductUnitId, ExtRefId], FacingId)
PREDICATE(put_facing, 4)
{
  std::string link_tail = "facings/";
  DataController facing_controller(link_tail.c_str());

  std::stringstream out_data;
  Json::Value facing = facing_array_to_facing_json(PlTerm_to_json(PL_A2));
  facing["shelfLayerId"] = std::string(PL_A1);
  if (facing_controller.put_data(facing.toStyledString().c_str(), out_data, std::string(PL_A4)))
  {
    PL_A3 = out_data.str().c_str();
    return true;
  }
  else
  {
    return false;
  }

}

// ItemGroup

const Json::Value item_group_array_to_item_group_json(const Json::Value &item_group_array)
{
  Json::Value item_group_json;
  if (item_group_array.size() == 3)
  {
    item_group_json["facingId"] = item_group_array[0];
    item_group_json["productUnitId"] = item_group_array[1];
    item_group_json["stock"] = item_group_array[2];
  }
  else
  {
    std::cerr << "Invalid shelf layer array (length = " << item_group_array.size() << ")" << std::endl;
  }
  return item_group_json;
}

// post_item_group([FacingId, ProductUnitId, Stock], ItemGroup)
PREDICATE(post_item_group, 2)
{
  std::string link_tail = "itemgroups/";
  DataController item_group_controller(link_tail.c_str());

  std::stringstream out_data;
  Json::Value item_group = item_group_array_to_item_group_json(PlTerm_to_json(PL_A1));
  if (item_group_controller.post_data(item_group.toStyledString().c_str(), out_data))
  {
    PL_A2 = out_data.str().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// put_item_group([FacingId, ProductUnitId, Stock], ItemGroup, ItemGroupId)
PREDICATE(put_item_group, 3)
{
  std::string link_tail = "itemgroups/";
  DataController item_group_controller(link_tail.c_str());

  std::stringstream out_data;
  Json::Value item_group = item_group_array_to_item_group_json(PlTerm_to_json(PL_A1));
  if (item_group_controller.put_data(item_group.toStyledString().c_str(), out_data, std::string(PL_A3)))
  {
    PL_A2 = out_data.str().c_str();
    return true;
  }
  else
  {
    return false;
  }
}



// Item

const Json::Value item_array_to_item_json(const Json::Value &item_array)
{
  Json::Value item_json;
  if (item_array.size() == 6)
  {
    item_json["facingId"] = item_array[0];
    item_json["positionInFacingX"] = item_array[1];
    item_json["positionInFacingY"] = item_array[2];
    item_json["positionInFacingZ"] = item_array[3];
    item_json["externalReferenceId"] = item_array[4];
    item_json["productUnitId"] = item_array[5];
  }
  else
  {
    std::cerr << "Invalid item array (length = " << item_array.size() << ")" << std::endl;
  }
  return item_json;
}

PREDICATE(post_item, 2)
{
  std::string link_tail = "items/";
  DataController item_controller(link_tail.c_str());

  std::stringstream out_data;
  Json::Value item = item_array_to_item_json(PlTerm_to_json(PL_A1));
  if (item_controller.post_data(item.toStyledString().c_str(), out_data))
  {
    PL_A2 = out_data.str().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// Data, Item, ItemId
PREDICATE(put_item, 3)
{
  std::string link_tail = "items/";
  DataController item_controller(link_tail.c_str());

  std::stringstream out_data;
  Json::Value item = item_array_to_item_json(PlTerm_to_json(PL_A1));
  if (item_controller.put_data(item.toStyledString().c_str(), out_data, std::string(PL_A3)))
  {
    PL_A2 = out_data.str().c_str();
    return true;
  }
  else
  {
    return false;
  }
}

// // Planogram

// const Json::Value planogram_array_to_planogram_json(const Json::Value &planogram_array)
// {
//   Json::Value planogram_json;
//   if (planogram_array.size() == 4)
//   {
//     planogram_json["numberOfFacings"] = planogram_array[0];
//     planogram_json["orientationYaw"] = planogram_array[1];
//     planogram_json["positionX"] = planogram_array[2];
//     planogram_json["versionTimestamp"] = planogram_array[3];
//   }
//   else if (planogram_array.size() == 6)
//   {
//     planogram_json["logisticalUnitId"] = planogram_array[0];
//     planogram_json["numberOfFacings"] = planogram_array[1];
//     planogram_json["orientationYaw"] = planogram_array[2];
//     planogram_json["positionX"] = planogram_array[3];
//     planogram_json["shelfLayerId"] = planogram_array[4];
//     planogram_json["versionTimestamp"] = planogram_array[5];
//   }

//   else
//   {
//     std::cerr << "Invalid planogram array (length = " << planogram_array.size() << ")" << std::endl;
//   }
//   return planogram_json;
// }

// // get_planograms(PlanogramList)
// PREDICATE(get_planograms, 1)
// {
//   PlanogramController planogram_controller;

//   PlTail planograms(PL_A1);
//   for (const Json::Value &planogram : planogram_controller.get_planograms())
//   {
//     planograms.append(planogram.toStyledString().c_str());
//   }
//   return planograms.close();
// }

// // post_planogram(InProductUnitId, InShelfLayerId, InPlanogram, OutPlanogram)
// PREDICATE(post_planogram, 4)
// {
//   PlanogramController planogram_controller;
//   Json::Value in_planogram = PlTerm_to_json(PL_A3);
//   Json::Value out_planogram;
//   if (in_planogram.isArray() ? planogram_controller.post_planogram(std::string(PL_A1), std::string(PL_A2), planogram_array_to_planogram_json(in_planogram), out_planogram) : planogram_controller.post_planogram(std::string(PL_A1), std::string(PL_A2), in_planogram, out_planogram))
//   {
//     PL_A4 = out_planogram.toStyledString().c_str();
//     return true;
//   }
//   else
//   {
//     return false;
//   }
// }

// // put_planogram(InPlanogramId, InProductUnitId, InShelfLayerId, InPlanogram, OutPlanogram)
// PREDICATE(put_planogram, 5)
// {
//   PlanogramController planogram_controller;
//   Json::Value in_planogram = PlTerm_to_json(PL_A4);
//   Json::Value out_planogram;
//   if (in_planogram.isArray() ? planogram_controller.put_planogram(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), planogram_array_to_planogram_json(in_planogram), out_planogram) : planogram_controller.put_planogram(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), in_planogram, out_planogram))
//   {
//     PL_A5 = out_planogram.toStyledString().c_str();
//     return true;
//   }
//   else
//   {
//     return false;
//   }
// }

// // delete_planogram(PlanogramId)
// PREDICATE(delete_planogram, 1)
// {
//   PlanogramController planogram_controller;
//   return planogram_controller.delete_planogram(std::string(PL_A1));
// }

// // ShoppingBasketPosition

// const Json::Value shopping_basket_position_array_to_shopping_basket_position_json(const Json::Value &shopping_basket_position_array)
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
//     std::cerr << "Invalid shopping_basket_position array (length = " << shopping_basket_position_array.size() << ")" << std::endl;
//   }
//   return shopping_basket_position_json;
// }

// // get_shopping_basket_positions(StoreId, CustomerId, ShoppingBasketPositionList)
// PREDICATE(get_shopping_basket_positions, 3)
// {
//   ShoppingBasketPositionController shopping_basket_position_controller;

//   PlTail shopping_basket_positions(PL_A3);
//   for (const Json::Value &shopping_basket_position : shopping_basket_position_controller.get_shopping_basket_positions(std::string(PL_A1), std::string(PL_A2)))
//   {
//     shopping_basket_positions.append(shopping_basket_position.toStyledString().c_str());
//   }
//   return shopping_basket_positions.close();
// }

// // post_shopping_basket_position(InStoreId, InCustomerId, InProductId, InShoppingBasketPosition, OutShoppingBasketPosition)
// PREDICATE(post_shopping_basket_position, 5)
// {
//   ShoppingBasketPositionController shopping_basket_position_controller;
//   Json::Value in_shopping_basket_position = PlTerm_to_json(PL_A4);
//   Json::Value out_shopping_basket_position;
//   if (in_shopping_basket_position.isArray() ? shopping_basket_position_controller.post_shopping_basket_position(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), shopping_basket_position_array_to_shopping_basket_position_json(in_shopping_basket_position), out_shopping_basket_position) : shopping_basket_position_controller.post_shopping_basket_position(std::string(PL_A1), std::string(PL_A2), std::string(PL_A3), in_shopping_basket_position, out_shopping_basket_position))
//   {
//     PL_A5 = out_shopping_basket_position.toStyledString().c_str();
//     return true;
//   }
//   else
//   {
//     return false;
//   }
// }

// // delete_shopping_basket_position(ShoppingBasketPositionId)
// PREDICATE(delete_shopping_basket_position, 1)
// {
//   ShoppingBasketPositionController shopping_basket_position_controller;
//   return shopping_basket_position_controller.delete_shopping_basket_position(std::string(PL_A1));
// }

// // delete_shopping_basket_positions(StoreId, CustomerId)
// PREDICATE(delete_shopping_basket_positions, 2)
// {
//   ShoppingBasketPositionController shopping_basket_position_controller;
//   return shopping_basket_position_controller.delete_shopping_basket_positions(std::string(PL_A1), std::string(PL_A2));
// }

// // Device

// const Json::Value device_array_to_device_json(const Json::Value &device_array)
// {
//   Json::Value device_json;
//   if (device_array.size() == 2)
//   {
//     device_json["description"] = device_array[0];
//     device_json["deviceType"] = device_array[1];
//   }
//   else
//   {
//     std::cerr << "Invalid delivery array (length = " << device_array.size() << ")" << std::endl;
//   }
//   return device_json;
// }

// // get_devices(Devices)
// PREDICATE(get_devices, 1)
// {
//   DeviceController device_controller;

//   PlTail devices(PL_A1);
//   for (const Json::Value &device : device_controller.get_devices())
//   {
//     devices.append(device.toStyledString().c_str());
//   }
//   return devices.close();
// }

// // post_device(InDeviceId, InStoreId, InDelivery, OutDelivery)
// PREDICATE(post_device, 4)
// {
//   DeviceController device_controller;
//   Json::Value in_device = PlTerm_to_json(PL_A3);
//   Json::Value out_device;
//   if (in_device.isArray() ? device_controller.post_device(std::string(PL_A1), std::string(PL_A2), device_array_to_device_json(in_device), out_device) : device_controller.post_device(std::string(PL_A1), std::string(PL_A2), in_device, out_device))
//   {
//     PL_A4 = out_device.toStyledString().c_str();
//     return true;
//   }
//   else
//   {
//     return false;
//   }
// }

// // delete_device(CharacteristicId)
// PREDICATE(delete_device, 1)
// {
//   DeviceController device_controller;
//   return device_controller.delete_device(std::string(PL_A1));
// }

// Graphql

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