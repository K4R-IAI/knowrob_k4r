#pragma once

#include "../Entities/DataController.cpp"

#define CERT_TYPE "P12"
// #define SANDBOX
#ifdef SANDBOX
  #define GRAPHQL_URL "https://dt-api.sandbox.knowledge4retail.org/k4r/graphql"
#else
  #define GRAPHQL_URL "https://dt-api.dev.knowledge4retail.org/k4r/graphql"
#endif

class GraphQlController
{
public:
  GraphQlController() : link(GRAPHQL_URL)
  {
    this->request.setOpt<curlpp::options::SslVerifyPeer>(VERIFY_PEER);
    this->request.setOpt(new curlpp::options::SslCertType(CERT_TYPE));
    std::string knowrob_k4r_path = ros::package::getPath("knowrob_k4r") + CERT_PATH;
    this->request.setOpt(new curlpp::options::SslCert(knowrob_k4r_path));
    this->request.setOpt(new curlpp::options::SslCertPasswd(CERT_PASSWD));
  }

  ~GraphQlController() {}

public:
  bool post_query(const std::string &in_data, Json::Value &out_data)
  {
    cURLpp::Cleanup cleanup;
    long status_code = 0;
    try
    {
      this->request.setOpt(new curlpp::options::Url(this->link));
    
      std::list<std::string> header;
      header.push_back("Content-Type: application/graphql");
      this->request.setOpt(new curlpp::options::HttpHeader(header));
      this->request.setOpt(new curlpp::options::PostFields(in_data));

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