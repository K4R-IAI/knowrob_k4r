#pragma once

#include <curlpp/Easy.hpp>
#include <curlpp/Options.hpp>
#include <curlpp/cURLpp.hpp>
#include <jsoncpp/json/json.h>
#include <jsoncpp/json/reader.h>
#include <sstream>

class Entity
{
private:
  std::string link;
  
public:
  virtual Json::Value get_data(std::string link_tail = "")
  {
    curlpp::Easy request;
    request.setOpt<curlpp::options::Url>((this->link + link_tail).c_str());

    std::stringstream ss;
    ss << request;
    request.perform();

    // Convert the string to json
    Json::Reader reader;
    reader.parse(ss.str(), this->data);
    return this->data;
  }

  virtual bool post_data(std::string link_tail = "")
  {
    curlpp::Easy request;
    try
    {
      request.setOpt<curlpp::options::Url>((this->link + link_tail).c_str());

      std::list<std::string> header;
      header.push_back("Content-Type: application/json");
      request.setOpt(new curlpp::options::HttpHeader(header));
      request.setOpt(new curlpp::options::PostFields(this->data.toStyledString()));
      request.perform();
    }

    catch (curlpp::RuntimeError& e)
    {
      std::cout << e.what() << std::endl;
      std::cout << "Is host name correct?" << std::endl;
    }

    catch (curlpp::LogicError& e)
    {
      std::cout << e.what() << std::endl;
    }

    return true;
  }

  virtual bool delete_data(std::string link_tail = "")
  {
    curlpp::Easy request;
    try
    {
      request.setOpt<curlpp::options::Url>((this->link + link_tail).c_str());
      request.setOpt(new curlpp::options::CustomRequest{"DELETE"});
      request.perform();
    }

    catch (curlpp::RuntimeError& e)
    {
      std::cout << e.what() << std::endl;
      std::cout << "Is host name correct?" << std::endl;
    }

    catch (curlpp::LogicError& e)
    {
      std::cout << e.what() << std::endl;
    }

    return true;
  }

  Json::Value data;

  Entity(const char* link) : link(link) {}
  ~Entity() {}
};

class EntityController : public Entity
{
protected:
  virtual Json::Value get_entity(std::string link_tail = "")
  {
    return this->get_data(link_tail);
  }

  virtual bool post_entity(const Json::Value& data, std::string link_tail = "")
  {
    this->data = data;
    return this->post_data(link_tail);
  }

  virtual bool delete_entity(std::string link_tail = "")
  {
    return this->delete_data(link_tail);
  }

  EntityController(const char* link) : Entity::Entity(link) {}
  ~EntityController() {}
};