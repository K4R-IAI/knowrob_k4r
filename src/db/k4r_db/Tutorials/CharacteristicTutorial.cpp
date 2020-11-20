// To compile: g++ CharacteristicTutorial.cpp -o Test -lcurl -lcurlpp -ljsoncpp 

// #include "../Entities/CustomerController.cpp"
#include "../Entities/CharacteristicController.cpp"
// #include "../Entities/ProductController.cpp"
// #include "../Entities/PropertyController.cpp"
// #include "../Entities/ShelfController.cpp"
// #include "../Entities/ShelfLayerController.cpp"
// #include "../Entities/StoreController.cpp"

#define A_1
#define A_2
#define A_3

int main(int, char**)
{
  // Example 2: Characteristic
  // 0. Make char_controller 
  CharacteristicController char_controller("http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/"); // customers is ready, but it has nothing in it

#ifdef A_1
  // 1. Get all characteristics (in Json, string and const char*)
  std::cout << "------------------------------" << std::endl;
  std::cout << "\nCharacteristics in json:" << std::endl;
  std::cout << char_controller.get_characteristics() << std::endl;                                // Return all Characteristics in json

  std::cout << "\nCharacteristics in string:" << std::endl;
  std::cout << char_controller.get_characteristics().toStyledString() << std::endl;               // Return all characteristics in string

  std::cout << "\nCharacteristics in const char*:" << std::endl;
  std::cout << char_controller.get_characteristics().toStyledString().c_str() << std::endl;       // Return all characteristics in const char* (for Predicate variable)
#endif

#ifdef A_2
  // 2. Send a characteristic (name price for example)
  std::cout << "------------------------------" << std::endl;
  char_controller.post_characteristic("Price");                                   // characteristic price is sent
  std::cout << char_controller.get_characteristics() << std::endl;                // You see?
#endif

#ifdef A_3
  // 3. Delete a customer with id
  // First we search for my id
  std::string price_id;
  for (const auto characteristic : char_controller.get_characteristics())
  {
    if (characteristic["name"].asString() == "Price")
    {
      price_id = characteristic["id"].asString();
      std::cout << "\nFound price id: " << price_id << std::endl;
      break; // In case we find more...
    }
  }
  // Then we can delete characteristic with this id
  std::cout << "\nCharacteristics without Price" << std::endl;
  std::cout << char_controller.get_characteristics() << std::endl;
#endif  
}