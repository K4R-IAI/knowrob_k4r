// To compile: g++ ProductTutorial.cpp -o Test -lcurl -lcurlpp -ljsoncpp 

// #include "../Entities/CustomerController.cpp"
// #include "../Entities/CharacteristicController.cpp"
#include "../Entities/ProductController.cpp"
// #include "../Entities/PropertyController.cpp"
// #include "../Entities/ShelfController.cpp"
// #include "../Entities/ShelfLayerController.cpp"
// #include "../Entities/StoreController.cpp"

#define A_1
#define A_2
#define A_3
#define A_4
#define A_5

int main(int, char**)
{
  // Example 3: Product
  // 0. Make product_controller 
  ProductController product_controller("http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/"); // product_controller is ready, but it has nothing in it

#ifdef A_1
  // 1. Get all products (in Json, string and const char*)
  std::cout << "------------------------------" << std::endl;
  std::cout << "\nProducts in json:" << std::endl;
  std::cout << product_controller.get_products() << std::endl;                                    // Return all products in json

  std::cout << "\nProducts in string:" << std::endl;
  std::cout << product_controller.get_products().toStyledString() << std::endl;                   // Return all products in string

  std::cout << "\nProducts in const char*:" << std::endl;
  std::cout << product_controller.get_products().toStyledString().c_str() << std::endl;           // Return products in const char* (for Predicate variable)
#endif

#ifdef A_2
  // 2. Get a product with id (in Json, string and const char*)
  std::cout << "------------------------------" << std::endl;
  std::cout << "\nProduct with id 0 in json:" << std::endl;
  std::cout << product_controller.get_product("0") << std::endl;
#endif

#ifdef A_3
  // 3. Send a product at id 10
  std::cout << "------------------------------" << std::endl;
  Json::Value product;                                            // Create product, do not give id here because it will be ignored
  product["name"] = "A good product";
  product["gtin"] = "A good gtin???";
  product["description"] = "A well writen description";
  product["length"] = 100;
  product["depth"] = 200;
  product["height"] = 300;
  product["weight"] = 400;
  product_controller.post_product(product, "10");                 // Product is sent
  std::cout << product_controller.get_products() << std::endl;    // You see?
#endif

#ifdef A_4
  // 4. Delete a product with an id
  std::cout << "------------------------------" << std::endl;
  // We delete that product with id 10
  product_controller.delete_product("10");
  std::cout << "\nProducts without product id 10" << std::endl;
  std::cout << product_controller.get_products() << std::endl;
#endif

#ifdef A_5
  // 5. Send a product list
  std::cout << "------------------------------" << std::endl;
  Json::Value product_list;
  Json::Value product2 = product;
  product["id"] = 101;            // id must be added now
  product2["id"] = 102;           // id must be added now
  product_list["products"].append(product);
  product_list["products"].append(product2);
  product_controller.post_products(product_list);  // Product list is sent
  std::cout << "\nNew products" << std::endl;
  std::cout << product_controller.get_products() << std::endl;    // New products with id 101 and 102
  // Clean up
  product_controller.delete_product("101");       
  product_controller.delete_product("102");
#endif  
}