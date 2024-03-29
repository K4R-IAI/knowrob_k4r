#pragma once

#include <curlpp/Easy.hpp>
#include <curlpp/Infos.hpp>
#include <curlpp/Options.hpp>
#include <curlpp/cURLpp.hpp>
#include <jsoncpp/json/reader.h>
#include <sstream>
#include <ros/package.h>
#include <ros/param.h>
#include "../Utilities/useful_functions.cpp"

class DataController
{
public:
  DataController(const char *link_tail="")
  {
    std::string dt_url;
    ros::param::get("dt_url", dt_url);
    this->link = dt_url + std::string(link_tail);
  }

  ~DataController() {}

public:
  bool get_data(std::stringstream &response, const std::string link_tail = "")
  {
    curlpp::Cleanup cleanup;
    long status_code = 0;
    try
    {
      this->request.setOpt(new curlpp::options::Url(this->link + link_tail));

      this->request.setOpt(new curlpp::options::WriteStream(&response));
      this->request.perform();

      status_code = curlpp::infos::ResponseCode::get(this->request);
      if (status_code != 200)
      {
        std::cerr << "GET failed - " << this->link + link_tail << " : " << status_code << std::endl;
      }
    }

    catch (curlpp::RuntimeError &e)
    {
      std::cerr << e.what() << std::endl;
      std::cerr << "Is host name correct?" << std::endl;
    }

    catch (curlpp::LogicError &e)
    {
      std::cerr << e.what() << std::endl;
    }
    
    return status_code == 200;
  }

  bool post_data(const std::string &data, std::stringstream &response, const std::string link_tail = "")
  {
    cURLpp::Cleanup cleanup;
    long status_code = 0;
    try
    {
      this->request.setOpt(new curlpp::options::Url(this->link + link_tail));
    
      std::list<std::string> header;
      header.push_back("Content-Type: application/json");
      this->request.setOpt(new curlpp::options::HttpHeader(header));
      this->request.setOpt(new curlpp::options::PostFields(data));

      this->request.setOpt(new curlpp::options::WriteStream(&response));

      this->request.perform();

      status_code = curlpp::infos::ResponseCode::get(this->request);
      if (status_code != 200)
      {
        std::cerr << "POST failed - " << this->link + link_tail << " : " << status_code << std::endl;
        std::cerr << "data:\n" << data << std::endl;
      }
    }

    catch (curlpp::RuntimeError &e)
    {
      std::cerr << e.what() << std::endl;
      std::cerr << "Is host name correct?" << std::endl;
    }

    catch (curlpp::LogicError &e)
    {
      std::cerr << e.what() << std::endl;
    }

    return status_code == 200;
  }

  bool put_data(const std::string &data, std::stringstream &response, const std::string link_tail = "")
  {
    cURLpp::Cleanup cleanup;
    long status_code = 0;
    try
    {
      this->request.setOpt(new curlpp::options::Url(this->link + link_tail));

      std::list<std::string> header;
      header.push_back("Content-Type: application/json");
      this->request.setOpt(new curlpp::options::HttpHeader(header));
      this->request.setOpt(new curlpp::options::CustomRequest("PUT"));
      this->request.setOpt(new curlpp::options::PostFields(data));

      this->request.setOpt(new curlpp::options::WriteStream(&response));

      this->request.perform();

      status_code = curlpp::infos::ResponseCode::get(this->request);
      if (status_code != 200)
      {
        std::cerr << "PUT failed - " << this->link + link_tail << " : " << status_code << std::endl;
        std::cerr << "data:\n" << data << std::endl;
      }
    }

    catch (curlpp::RuntimeError &e)
    {
      std::cerr << e.what() << std::endl;
      std::cerr << "Is host name correct?" << std::endl;
    }

    catch (curlpp::LogicError &e)
    {
      std::cerr << e.what() << std::endl;
    }

    return status_code == 200;
  }

  bool delete_data(const std::string link_tail = "")
  {
    cURLpp::Cleanup cleanup;
    long status_code = 0;
    try
    {
      this->request.setOpt(new curlpp::options::Url(this->link + link_tail));

      this->request.setOpt(new curlpp::options::CustomRequest{"DELETE"});

      std::ostringstream response;
      this->request.setOpt(new curlpp::options::WriteStream(&response));

      this->request.perform();
      status_code = curlpp::infos::ResponseCode::get(this->request);
      if (status_code != 200)
      {
        std::cerr << "DELETE failed - " << this->link + link_tail << " : " << status_code << std::endl;
      }
    }

    catch (curlpp::RuntimeError &e)
    {
      std::cerr << e.what() << std::endl;
      std::cerr << "Is host name correct?" << std::endl;
    }

    catch (curlpp::LogicError &e)
    {
      std::cerr << e.what() << std::endl;
    }

    return status_code == 200;
  }

private:
  curlpp::Easy request;

  std::string link;
};