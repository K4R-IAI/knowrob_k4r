#pragma once

#include <algorithm>
#include <curlpp/Easy.hpp>
#include <curlpp/Infos.hpp>
#include <curlpp/Options.hpp>
#include <curlpp/cURLpp.hpp>
#include <jsoncpp/json/json.h>
#include <jsoncpp/json/reader.h>
#include <sstream>
#include <string>
#include <unistd.h>
#define CERT_TYPE "P12"
// #define URL "https://dt-api.dev.knowledge4retail.org/k4r/api/v0/"
// #define CERT_PATH "src/knowrob_k4r/src/db/k4r_db/Entities/K4R_DEV_CERTs/k4r-dev-keystore.p12"
// #define CERT_PASSWD "8FdseHr0wCHwfuDmMv7QdKYWvnZg"
// #define VERIFY_PEER true

#define URL "https://dt-api.sandbox.knowledge4retail.org/k4r/api/v0/"
#define CERT_PATH "src/knowrob_k4r/src/db/k4r_db/Entities/K4R_DEV_CERTs/k4r-sandbox-client.p12"
#define CERT_PASSWD "q7WgPL3OnopoyU4abkrw97LD3iqD"
#define VERIFY_PEER false

void remove_new_line(std::string &str)
{
  while (true)
  {
    size_t str_last_index = str.length() - 1;
    if (!str.empty() && str[str_last_index] == '\n')
    {
      str.erase(str_last_index);
    }
    else
    {
      break;
    }
  }
  str.erase(std::remove(str.begin(), str.end(), '"'), str.end());
}

Json::Value string_to_json(const std::string &entity_in)
{
  Json::Value entity_out;
  Json::Reader reader;
  reader.parse(entity_in, entity_out);
  return entity_out;
}

class DataController
{
public:
  DataController(const char *link_tail="") : link(URL + std::string(link_tail))
  {
    this->request.setOpt<curlpp::options::SslVerifyPeer>(VERIFY_PEER);
    this->request.setOpt(new curlpp::options::SslCertType(CERT_TYPE));
    this->request.setOpt(new curlpp::options::SslCert(CERT_PATH));
    this->request.setOpt(new curlpp::options::SslCertPasswd(CERT_PASSWD));
  }

  ~DataController() {}

protected:
  Json::Value get_data(const std::string link_tail = "")
  {
    curlpp::Cleanup cleanup;
    long status_code = 0;
    Json::Value data;
    try
    {
      this->request.setOpt(new curlpp::options::Url(this->link + link_tail));

      std::ostringstream response;
      this->request.setOpt(new curlpp::options::WriteStream(&response));
      this->request.perform();

      status_code = curlpp::infos::ResponseCode::get(this->request);
      if (status_code == 200)
      {
        data = string_to_json(response.str());
      }
      else
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

    return data;
  }

  bool post_data(const Json::Value &in_data, Json::Value &out_data, const std::string link_tail = "")
  {
    cURLpp::Cleanup cleanup;
    long status_code = 0;
    try
    {
      this->request.setOpt(new curlpp::options::Url(this->link + link_tail));
    
      std::list<std::string> header;
      header.push_back("Content-Type: application/json");
      this->request.setOpt(new curlpp::options::HttpHeader(header));
      this->request.setOpt(new curlpp::options::PostFields(in_data.toStyledString()));

      std::ostringstream response;
      this->request.setOpt(new curlpp::options::WriteStream(&response));

      this->request.perform();

      status_code = curlpp::infos::ResponseCode::get(this->request);
      if (status_code == 200)
      {
        out_data = string_to_json(response.str());
      }
      else
      {
        std::cerr << "POST failed - " << this->link + link_tail << " : " << status_code << std::endl;
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

  bool put_data(const Json::Value &in_data, Json::Value &out_data, const std::string link_tail = "")
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
      this->request.setOpt(new curlpp::options::PostFields(in_data.toStyledString()));

      std::ostringstream response;
      this->request.setOpt(new curlpp::options::WriteStream(&response));

      this->request.perform();

      status_code = curlpp::infos::ResponseCode::get(this->request);
      if (status_code == 200)
      {
        out_data = string_to_json(response.str());
      }
      else
      {
        std::cerr << "PUT failed - " << this->link + link_tail << " : " << status_code << std::endl;
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