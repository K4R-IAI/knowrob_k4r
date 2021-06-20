#pragma once

#include "DataController.cpp"

class ShoppingBasketPositionController : public DataController
{
public:
  ShoppingBasketPositionController();

public:
  const Json::Value get_shopping_basket_position(const std::string&);
  const Json::Value get_shopping_basket_positions(const std::string&, const std::string&);

  const bool post_shopping_basket_position(const std::string&, const std::string&, const Json::Value&, Json::Value&);
  const bool post_shopping_basket_position(const std::string&, const std::string&, const std::string&, const Json::Value&, Json::Value&);

  const bool delete_shopping_basket_position(const std::string&);
  const bool delete_shopping_basket_positions(const std::string&, const std::string&);
};

ShoppingBasketPositionController::ShoppingBasketPositionController() : DataController::DataController()
{
}

const Json::Value ShoppingBasketPositionController::get_shopping_basket_position(const std::string& shopping_basket_position_id)
{
  return this->get_data("shoppingbasketpositions/" + shopping_basket_position_id);
}

const Json::Value ShoppingBasketPositionController::get_shopping_basket_positions(const std::string& store_id, const std::string& customer_id)
{
  return this->get_data("stores/" + store_id + "/customers/" + customer_id + "/shoppingbasketpositions");
}

const bool ShoppingBasketPositionController::post_shopping_basket_position(const std::string& in_store_id, const std::string& in_customer_id, const Json::Value& in_shopping_basket_position, Json::Value& out_shopping_basket_position)
{
  Json::Value in_shopping_basket_position_tmp = in_shopping_basket_position;
  in_shopping_basket_position_tmp["customerId"] = in_customer_id;
  in_shopping_basket_position_tmp["storeId"] = in_store_id;
  return this->post_data(in_shopping_basket_position_tmp, out_shopping_basket_position, "stores/" + in_store_id + "/customers/" + in_customer_id + "/shoppingbasketpositions");
}

const bool ShoppingBasketPositionController::post_shopping_basket_position(const std::string& in_store_id, const std::string& in_customer_id, const std::string& in_product_id, const Json::Value& in_shopping_basket_position, Json::Value& out_shopping_basket_position)
{
  Json::Value in_shopping_basket_position_tmp = in_shopping_basket_position;
  in_shopping_basket_position_tmp["productId"] = in_product_id;
  return this->post_shopping_basket_position(in_store_id, in_customer_id, in_shopping_basket_position_tmp, out_shopping_basket_position);
}

const bool ShoppingBasketPositionController::delete_shopping_basket_position(const std::string& shopping_basket_position_id)
{
  return this->delete_data("shoppingbasketpositions/" + shopping_basket_position_id);
}

const bool ShoppingBasketPositionController::delete_shopping_basket_positions(const std::string& store_id, const std::string& customer_id)
{
  return this->delete_data("stores/" + store_id + "/customers/" + customer_id + "/shoppingbasketpositions");
}