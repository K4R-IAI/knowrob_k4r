#pragma once

#include <curlpp/Easy.hpp>
#include <curlpp/Infos.hpp>
#include <curlpp/Options.hpp>
#include <curlpp/cURLpp.hpp>
#include "ros/param.h"
#include <ros/package.h>
#include "../Utilities/useful_functions.cpp"

class GraphQlController
{
public:
  GraphQlController()
  {
    std::string graphql_url;
    ros::param::get("graphql_url", graphql_url);
    this->link = graphql_url;
  }

  ~GraphQlController() {}

public:
  bool post_query(const std::string &in_data, std::stringstream &response)
  {
    std::cout << in_data << std::endl;
    cURLpp::Cleanup cleanup;
    long status_code = 0;
    try
    {
      this->request.setOpt(new curlpp::options::Url(this->link));
    
      std::list<std::string> header;
      header.push_back("Content-Type: application/graphql");
      this->request.setOpt(new curlpp::options::HttpHeader(header));
      this->request.setOpt(new curlpp::options::PostFields(in_data));

      this->request.setOpt(new curlpp::options::WriteStream(&response));

      this->request.perform();

      status_code = curlpp::infos::ResponseCode::get(this->request);
      if (status_code != 200)
      {
        std::cerr << "POST failed - " << this->link << " : " << status_code << std::endl;
        std::cerr << "data:\n" << in_data << std::endl;
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