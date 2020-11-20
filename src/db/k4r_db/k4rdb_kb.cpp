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

PREDICATE(k4r_get_stores, 2) {
  StoreController stores(PL_A1);

  PlTail values(PL_A2);
  values.append(stores.get_stores().toStyledString().c_str());
  return values.close();
}

PREDICATE(k4r_get_customers, 2)
{
  CustomerController customers(PL_A1);

  PlTail values(PL_A2);
  values.append(customers.get_customers().toStyledString().c_str());
  return values.close();
}

PREDICATE(k4r_post_customer, 2)
{
  CustomerController customers(PL_A1);

  return customers.post_customer(std::string(PL_A2));
}

PREDICATE(k4r_delete_customer, 2)
{
  CustomerController customers(PL_A1);
  std::string customer_id;
  for (const auto customer : customers.get_customers())
  {
    if (customer["anonymisedName"].compare(std::string(PL_A2)) == 0)
    {
      customer_id = customer["id"].asString();
      break; // In case we find more...
    }
  }
  return customers.delete_customer(customer_id);
}