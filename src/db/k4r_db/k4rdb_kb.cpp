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

PREDICATE(k4rdb_values, 2) {PL_A1
	// This method gets the path in the variable PL_A1 and should return a list of values and assign it toPL_A2
	// REturning could look a bit like this:
	PlTail values(PL_A2);
	values.append("Test1");
	values.append("Test2");
	values.append("Test3");
	return values.close();
}
