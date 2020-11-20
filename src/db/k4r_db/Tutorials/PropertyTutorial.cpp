// To compile: g++ PropertyTutorial.cpp -o Test -lcurl -lcurlpp -ljsoncpp

// #include "../Entities/CustomerController.cpp"
// #include "../Entities/CharacteristicController.cpp"
// #include "../Entities/ProductController.cpp"
#include "../Entities/PropertyController.cpp"
// #include "../Entities/ShelfController.cpp"
// #include "../Entities/ShelfLayerController.cpp"
// #include "../Entities/StoreController.cpp"

#define A_1
#define A_2
#define A_3

int main(int, char**)
{
  // Example 4: Property
  // 0. Make property_controller 
  PropertyController property_controller("http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/"); // property_controller is ready, but it has nothing in it

#ifdef A_1
  // 1.1 Give storeId, productId so property_controller can find
  std::cout << "------------------------------" << std::endl;
  std::cout << property_controller.get_properties() << std::endl; // It wont work because store_id and product_id is unknown
  std::cout << "------------------------------" << std::endl;
  property_controller.set_store("21");    // store id 21
  property_controller.set_product("0");   // product id 0
  std::cout << property_controller.get_properties() << std::endl; // Now it works

  // 1. Get all properties (in Json, string and const char*)
  std::cout << "------------------------------" << std::endl;
  std::cout << "\nProducts in json:" << std::endl;
  std::cout << property_controller.get_properties() << std::endl;                                    // Return all properties in json

  std::cout << "\nProducts in string:" << std::endl;
  std::cout << property_controller.get_properties().toStyledString() << std::endl;                   // Return all properties in string

  std::cout << "\nProducts in const char*:" << std::endl;
  std::cout << property_controller.get_properties().toStyledString().c_str() << std::endl;           // Return properties in const char* (for Predicate variable)
#endif

#ifdef A_2
  // 3. Send a property at storeId 21, productId 0, characteristicId 211 with value "1000 Euro"
  std::cout << "------------------------------" << std::endl;
  property_controller.post_property("21","0","211","1000 Euro");                  // Property is sent
  std::cout << property_controller.get_properties() << std::endl;                 // You see?
#endif

#ifdef A_3
  // 4. Delete a property at storeId 21, productId 0, characteristicId 211
  std::cout << "------------------------------" << std::endl;
  // The storeId, productId and characteristicId are saved so we dont need to put it
  property_controller.delete_property(); // is same as property_controller.delete_property("21", "0", "211");
  std::cout << "\nProducts without property characteristicId 211" << std::endl;
  std::cout << property_controller.get_properties() << std::endl;
#endif
}