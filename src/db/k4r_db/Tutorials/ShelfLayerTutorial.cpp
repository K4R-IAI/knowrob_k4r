// To compile: g++ ShelfLayerTutorial.cpp -o Test -lcurl -lcurlpp -ljsoncpp 

// #include "../Entities/CustomerController.cpp"
// #include "../Entities/CharacteristicController.cpp"
// #include "../Entities/ShelfController.cpp"
// #include "../Entities/PropertyController.cpp"
// #include "../Entities/ShelfController.cpp"
#include "../Entities/ShelfLayerController.cpp"
// #include "../Entities/StoreController.cpp"

#define A_1
#define A_2
#define A_3
#define A_4

int main(int, char**)
{
  // Example 7: ShelfLayer
  // 0. Make shelf_layer_controller 
  ShelfLayerController shelf_layer_controller("http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/"); // shelf_layer_controller is ready, but it has nothing in it

#ifdef A_1
  // 1.1. Give shelfId so shelf_layer_controller can find
  std::cout << "------------------------------" << std::endl;
  std::cout << shelf_layer_controller.get_shelf_layers() << std::endl;        // It wont work because shelf_id is unknown
  std::cout << "------------------------------" << std::endl;
  shelf_layer_controller.set_shelf("74");                                     // shelf id 0
  std::cout << shelf_layer_controller.get_shelf_layers() << std::endl;        // Now it works

  // 1. Get all shelf_layers (in Json, string and const char*)
  std::cout << "------------------------------" << std::endl;
  std::cout << "\nShelves in json:" << std::endl;
  std::cout << shelf_layer_controller.get_shelf_layers() << std::endl;                                    // Return all shelf_layers in json

  std::cout << "\nShelves in string:" << std::endl;
  std::cout << shelf_layer_controller.get_shelf_layers().toStyledString() << std::endl;                   // Return all shelf_layers in string

  std::cout << "\nShelves in const char*:" << std::endl;
  std::cout << shelf_layer_controller.get_shelf_layers().toStyledString().c_str() << std::endl;           // Return shelf_layers in const char* (for Predicate variable)
#endif

#ifdef A_2
  // 2. Get a shelf_layer with id (in Json, string and const char*)
  std::cout << "------------------------------" << std::endl;
  std::cout << "\nShelf layer with id 44 in json:" << std::endl;
  std::cout << shelf_layer_controller.get_shelf_layer("74") << std::endl;
#endif

#ifdef A_3
  // 3. Send a shelf_layer at shelf id 21
  std::cout << "------------------------------" << std::endl;
  Json::Value shelf_layer;                                                  // Create shelf_layer, do not give shelf id here because it will be ignored
  shelf_layer["depth"] = 12;
  shelf_layer["externalReferenceId"] = "abc123";
  shelf_layer["height"] = 23;
  shelf_layer["level"] = 32;
  shelf_layer["positionZ"] = 34;
  shelf_layer["type"] = "1234abcd";
  shelf_layer["width"] = 44;
  shelf_layer_controller.post_shelf_layer(shelf_layer);                     // Shelf layer is sent
  std::cout << shelf_layer_controller.get_shelf_layers() << std::endl;      // You see?
#endif

#ifdef A_4
  // 4. Delete a shelf_layer with externalReferenceId 
  std::cout << "------------------------------" << std::endl;
  // First we search for externalReferenceId "abc123"
  std::string shelf_layer_id;
  for (const auto shelf_layer : shelf_layer_controller.get_shelf_layers())
  {
    if (shelf_layer["externalReferenceId"].asString() == "abc123")
    {
      shelf_layer_id = shelf_layer["id"].asString();
      std::cout << "\nFound shelf_layer id: " << shelf_layer_id << std::endl;
      break; // In case we find more...
    }
  }
  // We delete that shelf_layer with that id
  shelf_layer_controller.delete_shelf_layer(shelf_layer_id);
  std::cout << "\nShelves without shelf_layer externalReferenceId abc123" << std::endl;
  std::cout << shelf_layer_controller.get_shelf_layers() << std::endl;
#endif
}