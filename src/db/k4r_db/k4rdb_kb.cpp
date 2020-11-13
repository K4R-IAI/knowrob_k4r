/* 
 * Copyright (c) 2020, Sascha Jongebloed
 * All rights reserved.
 * 
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#define PL_SAFE_ARG_MACROS
#include <SWI-cpp.h>
#include <iostream>
#include <list>    

// For REST
#include <curlpp/cURLpp.hpp>
#include <curlpp/Options.hpp>
#include <sstream>
#include <jsoncpp/json/json.h>
#include <jsoncpp/json/reader.h>

PREDICATE(k4rdb_values, 2) {
	// This method gets the path in the variable PL_A1.
	// This is how you assign the value to path
	std::string path((char*)PL_A1);
	// Then get the values from the API

	// That's all that is needed to do cleanup of used resources (RAII style).
  curlpp::Cleanup myCleanup;

  // Read data as a string
  std::stringstream os;
  os << curlpp::options::Url(std::string("http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/stores/4")); // This is PL_A1 I think
  std::string data = os.str();

  // Convert the string to json
  Json::Value data_json;
  Json::Reader reader;
  reader.parse(data, data_json);

	// Then it should return a list of the values and assign it to PL_A2
	// Returning lists to Prolog can be done like this:
	PlTail values(PL_A2);
	values.append(data_json["storeName"].asString().c_str());
	values.append(data_json["storeNumber"].asString().c_str());
	values.append(data_json["addressCountry"].asString().c_str());
	return values.close();
}
