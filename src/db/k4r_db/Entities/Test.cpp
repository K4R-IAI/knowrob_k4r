#include <curlpp/Easy.hpp>
#include <curlpp/Options.hpp>
#include <curlpp/cURLpp.hpp>
#include <curlpp/Infos.hpp>
#include <jsoncpp/json/json.h>
#include <jsoncpp/json/reader.h>
#include <sstream>

int main(int, char**)
{
  Json::Value x_json;
  x_json["id"] = "52";
  x_json["name"] = "Giang";

  Json::Value y_json;
  x_json["id"] = "53";
  x_json["name"] = "Simon";

  std::string x_string = x_json.toStyledString();
  const char* x_char = x_string.c_str();

  std::cout << x_json << std::endl;
  std::cout << x_char << std::endl;

  std::cout << x_json.compare(y_json) << std::endl;
  std::cout << "-------------------------" << std::endl;
  Json::Value x_json_copy_string(x_string);
  std::cout << x_json.compare(x_json_copy_string) << std::endl;
  std::cout << x_json_copy_string.compare(x_json) << std::endl;
  std::cout << (x_json_copy_string == x_string) << std::endl;
  std::cout << "--------------------------------" << std::endl;
  Json::Value x_json_copy_char(x_char);
  std::cout << x_json.compare(x_json_copy_char) << std::endl;
  std::cout << x_json_copy_char.compare(x_json) << std::endl;
  std::cout << (x_json_copy_char == x_char) << std::endl;
  std::cout << "------------------------------" << std::endl;
  Json::Value x_json_copy_json(x_json);
  std::cout << x_json.compare(x_json_copy_json) << std::endl;
  std::cout << x_json_copy_json.compare(x_json) << std::endl;

  Json::Value x_json_from_char;
  Json::Reader reader;
  reader.parse( x_char, x_json_from_char );
  std::cout << x_json_from_char["id"] << std::endl;
  return 0;
}