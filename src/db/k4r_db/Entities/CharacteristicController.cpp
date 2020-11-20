#pragma once

#include "EntityController.cpp"

class CharacteristicController : public EntityController
{
public:
  CharacteristicController(const char*);

  Json::Value get_characteristic(const std::string &);
  bool post_characteristic(const std::string &);
  bool delete_characteristic(const std::string &);

  Json::Value get_characteristics();
};

CharacteristicController::CharacteristicController(const char* link) : EntityController::EntityController((std::string(link) + "characteristics/").c_str())
{
}

Json::Value CharacteristicController::get_characteristic(const std::string &characteristic_id)
{
  return this->get_entity(characteristic_id);
}

bool CharacteristicController::post_characteristic(const std::string &characteristic_name)
{
  Json::Value characteristic;
  characteristic["name"] = characteristic_name;
  return this->post_entity(characteristic);
}

bool CharacteristicController::delete_characteristic(const std::string &characteristic_id)
{
  return this->delete_entity(characteristic_id);
}

Json::Value CharacteristicController::get_characteristics()
{
  return this->get_entity();
}