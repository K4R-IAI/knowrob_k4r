// To compile: g++ StoreTutorial.cpp -o Test -lcurl -lcurlpp -ljsoncpp 

// #include "../Entities/CustomerController.cpp"
// #include "../Entities/CharacteristicController.cpp"
// #include "../Entities/ProductController.cpp"
// #include "../Entities/PropertyController.cpp"
// #include "../Entities/ShelfController.cpp"
// #include "../Entities/ShelfLayerController.cpp"
#include "../Entities/StoreController.cpp"

#define A_1
#define A_2
#define A_3

int main(int, char**)
{
  // Example 5: Store
  // 0. Make store_controller 
  StoreController store_controller("http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/"); // stores is ready, but it has nothing in it

#ifdef A_1
  // 1. Get all stores (in Json, string and const char*)
  std::cout << "------------------------------" << std::endl;
  std::cout << "\nStores in json:" << std::endl;
  std::cout << store_controller.get_stores() << std::endl;                                // Return all Stores in json

  std::cout << "\nStores in string:" << std::endl;
  std::cout << store_controller.get_stores().toStyledString() << std::endl;               // Return all stores in string

  std::cout << "\nStores in const char*:" << std::endl;
  std::cout << store_controller.get_stores().toStyledString().c_str() << std::endl;       // Return all stores in const char* (for Predicate variable)
#endif

#ifdef A_2
  // 2. Send a store
  std::cout << "------------------------------" << std::endl;
  Json::Value store;
  store["addressAdditional"] = "abcdef";
  store["addressCity"] = "abcdef";
  store["addressCountry"] = "abcdef";
  store["addressPostcode"] = "abcdef";
  store["addressState"] = "abcdef";
  store["addressStreet"] = "abcdef";
  store["addressStreetNumber"] = "abcdef";
  store["cadPlanId"] = "abcdef";
  store["latitude"] = 15;
  store["longitude"] = 91;
  store["storeName"] = "abcdef";
  store["storeNumber"] = "abcdef";
  
  store_controller.post_store(store);                                     // store is sent
  std::cout << store_controller.get_stores() << std::endl;                // You see?
#endif

#ifdef A_3
  // 3. Delete a store with id
  // First we search for storeName "abcdef"
  std::string store_id;
  for (const auto store : store_controller.get_stores())
  {
    if (store["storeName"].asString() == "abcdef")
    {
      store_id = store["id"].asString();
      std::cout << "\nFound store id: " << store_id << std::endl;
      break; // In case we find more...
    }
  }
  // Then we can delete store with this id
  store_controller.delete_store(store_id);
  std::cout << "\nStores without abcdef" << std::endl;
  std::cout << store_controller.get_stores() << std::endl;
#endif  
}