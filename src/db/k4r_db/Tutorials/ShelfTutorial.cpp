// To compile: g++ ShelfTutorial.cpp -o Test -lcurl -lcurlpp -ljsoncpp 

// #include "../Entities/CustomerController.cpp"
// #include "../Entities/CharacteristicController.cpp"
// #include "../Entities/ShelfController.cpp"
// #include "../Entities/PropertyController.cpp"
#include "../Entities/ShelfController.cpp"
// #include "../Entities/ShelfLayerController.cpp"
// #include "../Entities/StoreController.cpp"

#define A_1
#define A_2
#define A_3
#define A_4

int main(int, char**)
{
  // Example 6: Shelf
  // 0. Make shelf_controller 
  ShelfController shelf_controller("http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/"); // shelf_controller is ready, but it has nothing in it

#ifdef A_1
  // 1.1. Give storeId so shelf_controller can find
  std::cout << "------------------------------" << std::endl;
  std::cout << shelf_controller.get_shelves() << std::endl; // It wont work because store_id is unknown
  std::cout << "------------------------------" << std::endl;
  shelf_controller.set_store("21");                               // store id 21
  std::cout << shelf_controller.get_shelves() << std::endl;       // Now it works

  // 1. Get all shelves (in Json, string and const char*)
  std::cout << "------------------------------" << std::endl;
  std::cout << "\nShelves in json:" << std::endl;
  std::cout << shelf_controller.get_shelves() << std::endl;                                    // Return all shelves in json

  std::cout << "\nShelves in string:" << std::endl;
  std::cout << shelf_controller.get_shelves().toStyledString() << std::endl;                   // Return all shelves in string

  std::cout << "\nShelves in const char*:" << std::endl;
  std::cout << shelf_controller.get_shelves().toStyledString().c_str() << std::endl;           // Return shelves in const char* (for Predicate variable)
#endif

#ifdef A_2
  // 2. Get a shelf with id (in Json, string and const char*)
  std::cout << "------------------------------" << std::endl;
  std::cout << "\nShelf with id 74 in json:" << std::endl;
  std::cout << shelf_controller.get_shelf("74") << std::endl;
#endif

#ifdef A_3
  // 3. Send a shelf at store id 21
  std::cout << "------------------------------" << std::endl;
  Json::Value shelf;                                            // Create shelf, do not give store id here because it will be ignored
  shelf["cadPlanId"] = "abcdef";
  shelf["depth"] = 12;
  shelf["externalReferenceId"]= "xyz";
  shelf["height"] = 23;
  shelf["orientationY"] = 34;
  shelf["orientationYaw"] = 45;
  shelf["orientationZ"] = 56;
  shelf["orientationx"] = 67;
  shelf["positionX"] = 78;
  shelf["positionY"] = 89;
  shelf["positionZ"] = 90;
  shelf["productGroupId"] = 21;
  shelf["width"] = 41;
  shelf_controller.post_shelf(shelf);                    // Shelve is sent
  std::cout << shelf_controller.get_shelves() << std::endl;    // You see?
#endif

#ifdef A_4
  // 4. Delete a shelf with a shelf id
  std::cout << "------------------------------" << std::endl;
  // First we search for externalReferenceId "xyz"
  std::string shelf_id;
  for (const auto shelf : shelf_controller.get_shelves())
  {
    if (shelf["externalReferenceId"].asString() == "xyz")
    {
      shelf_id = shelf["id"].asString();
      std::cout << "\nFound shelf id: " << shelf_id << std::endl;
      break; // In case we find more...
    }
  }
  // We delete that shelf with id
  shelf_controller.delete_shelf(shelf_id);
  std::cout << "\nShelves without shelf externalReferenceId xyz" << std::endl;
  std::cout << shelf_controller.get_shelves() << std::endl;
#endif
}