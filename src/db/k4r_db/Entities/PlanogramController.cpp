#pragma once

#include "EntityController.cpp"

class PlanogramController : public EntityController
{
public:
  PlanogramController(const char*);

  bool check_planogram(const Json::Value&);

  Json::Value get_planograms();

  bool post_planogram(const Json::Value&);

  bool put_planogram(const std::string&, const Json::Value&);

  bool delete_planogram(const std::string&);
};

PlanogramController::PlanogramController(const char* link) : EntityController::EntityController((std::string(link) + "planograms/").c_str())
{
}

Json::Value PlanogramController::get_planograms()
{
  return this->get_entity();
}

bool PlanogramController::check_planogram(const Json::Value& planogram)
{
  if (planogram["numberOfFacings"].isInt() &&
      planogram["orientationYaw"].isNumeric() &&
      planogram["positionX"].isInt() &&
      planogram["productId"].isString() &&
      planogram["shelfLayerId"].isInt() &&
      planogram["versionTimestamp"].isInt())
  {
    std::cout << "Invalid planogram" << std::endl;
    return true;
  }
  else
  {
    return false;
  }
}

bool PlanogramController::post_planogram(const Json::Value& planogram)
{
  return this->check_planogram(planogram) && this->post_entity(planogram);
}

bool PlanogramController::put_planogram(const std::string& planogram_id, const Json::Value& planogram)
{
  return this->check_planogram(planogram) && this->put_entity(planogram, planogram_id);
}

bool PlanogramController::delete_planogram(const std::string& planogram_id)
{
  return this->delete_entity(planogram_id);
}