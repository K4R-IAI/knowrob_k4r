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

PREDICATE(k4rdb_values, 2) {
	// This method gets the path in the variable PL_A1.
	// This is how you assign the value to path
	std::string path((char*)PL_A1);
	// Then get the values from the API
	// Then it should return a list of the values and assign it to PL_A2
	// Returning lists to Prolog can be done like this:
	PlTail values(PL_A2);
	values.append("Test1");
	values.append("Test2");
	values.append("Test3");
	return values.close();
}
