// To compile: g++ ShoppingBasketTutorial.cpp -o Test -lcurl -lcurlpp -ljsoncpp

// #include "../Entities/CustomerController.cpp"
// #include "../Entities/CharacteristicController.cpp"
// #include "../Entities/ProductController.cpp"
// #include "../Entities/shopping_basketController.cpp"
// #include "../Entities/ShelfController.cpp"
// #include "../Entities/ShelfLayerController.cpp"
// #include "../Entities/StoreController.cpp"
#include "../Entities/ShoppingBasketController.cpp"

#define A_1
#define A_2
#define A_3

int main(int, char**)
{
  // Example 8: shopping_basket
  // 0. Make shopping_basket_controller
  ShoppingBasketController shopping_basket_controller("http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/"); // shopping_basket_controller is ready, but it has nothing in it

#ifdef A_1
  // 1.1 Give storeId, customerId so shopping_basket_controller can find
  std::cout << "------------------------------" << std::endl;
  std::cout << shopping_basket_controller.get_shopping_baskets() << std::endl; // It wont work because store_id and product_id is unknown
  std::cout << "------------------------------" << std::endl;
  shopping_basket_controller.set_store("21");                                   // store id 21
  shopping_basket_controller.set_customer("23");                               // customer id 211
  std::cout << shopping_basket_controller.get_shopping_baskets() << std::endl;  // Now it works

  // 1. Get all shopping_baskets (in Json, string and const char*)
  std::cout << "------------------------------" << std::endl;
  std::cout << "\nProducts in json:" << std::endl;
  std::cout << shopping_basket_controller.get_shopping_baskets() << std::endl; // Return all shopping_baskets in json

  std::cout << "\nProducts in string:" << std::endl;
  std::cout << shopping_basket_controller.get_shopping_baskets().toStyledString() << std::endl; // Return all shopping_baskets in string

  std::cout << "\nProducts in const char*:" << std::endl;
  std::cout << shopping_basket_controller.get_shopping_baskets().toStyledString().c_str() << std::endl; // Return shopping_baskets in const char* (for Predicate variable)
#endif

#ifdef A_2
  // 3. Send a shopping_basket at storeId 21, customerId 23 with productId 0
  std::cout << "------------------------------" << std::endl;
  Json::Value shopping_basket;
  shopping_basket["productId"] = "0";
  shopping_basket["quantity"] = 10;
  shopping_basket["sellingPrice"] = 12.99;
  shopping_basket_controller.post_shopping_basket("21", "23", shopping_basket); // shopping_basket is sent
  shopping_basket["productId"] = "2";
  shopping_basket["quantity"] = 20;
  shopping_basket["sellingPrice"] = 129.99;
  shopping_basket_controller.post_shopping_basket("21", "23", shopping_basket); // shopping_basket is sent
  std::cout << shopping_basket_controller.get_shopping_baskets() << std::endl;   // You see?
#endif

#ifdef A_3
  // 4. Delete a shopping_basket at storeId 21, customerId 23 and productId 0
  std::cout << "------------------------------" << std::endl;
  // The storeId, customerId and productId are saved so we dont need to put it
  shopping_basket_controller.delete_shopping_basket("0"); // is same as shopping_basket_controller.delete_shopping_basket("21", "23", "0");
  std::cout << "\nProducts without shopping_basket productId 0" << std::endl;
  std::cout << shopping_basket_controller.get_shopping_baskets() << std::endl;
#endif
}