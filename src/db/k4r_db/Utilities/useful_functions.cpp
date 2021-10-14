#pragma once

#include <algorithm>
#include <jsoncpp/json/json.h>
#include <string>

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