:- use_module('k4r_db_client.pl').
:- use_module(library('rostest')).

:- begin_tests(
        'k4r_db_client').

% post_test_customer(Link, CustomerId) :-
%   k4r_post_customer(Link, "anonymisedName Test"),
%   k4r_get_customers(Link, CustomerList).
%   k4r_get_entity_by_key_value(CustomerList, "anonymisedName", "anonymisedName Test", Customer),
%   k4r_get_entity_id(Customer, CustomerId).

post_test_store(StoreId) :-
  post_store(
    ["addressAdditional test", 
    "addressCity test", 
    "addressCountry test", 
    "addressPostcode test", 
    "addressState test", 
    "addressStreet test", 
    "addressStreetNumber test", 
    "cadPlanId test",
    1.1,
    1.2,
    "storeName test",
    "storeNumber test"], 
    Store),
  get_entity_id(Store, StoreId).

post_test_product("idTest") :-
  post_product("idTest", ["description test", "name test", "productType test", "productUnit test"], _).
post_test_product2("idTest2") :-
  post_product("idTest2", ["description test", "name test2", "productType test2", "productUnit test2"], _).

post_test_logistical_unit1(ProductId1, ProductId2, LogisticalUnitId1, LogisticalUnitId2) :-
  post_logistical_unit(ProductId1, [true, true, "dimensionUnit 1", 1.1, 1.2, 1, 1.3, true, 1.4, "quantityUnit 1", "quantityUnitIso 1", true, 1.5, 1.6], LogisticalUnit1),
  get_entity_id(LogisticalUnit1, LogisticalUnitId1),
  post_logistical_unit(ProductId2, [true, true, "dimensionUnit 2", 2.1, 2.2, 2, 2.3, true, 2.4, "quantityUnit 2", "quantityUnitIso 2", true, 2.5, 2.6], LogisticalUnit2),
  get_entity_id(LogisticalUnit2, LogisticalUnitId2).

post_test_product_characteristics(ProductCharacteristicId1, ProductCharacteristicId2, ProductCharacteristicId3, ProductCharacteristicId4) :-
  post_product_characteristic("name 1", ProductCharacteristic1),
  get_entity_id(ProductCharacteristic1, ProductCharacteristicId1),
  post_product_characteristic("name 2", ProductCharacteristic2),
  get_entity_id(ProductCharacteristic2, ProductCharacteristicId2),
  post_product_characteristic("name 3", ProductCharacteristic3),
  get_entity_id(ProductCharacteristic3, ProductCharacteristicId3),
  post_product_characteristic("name 4", ProductCharacteristic4),
  get_entity_id(ProductCharacteristic4, ProductCharacteristicId4).

% post_test_product_group(Link, StoreId, ProductGroupId) :-
%   post_test_store(Link, StoreId),
%   k4r_post_product_group(Link, StoreId, "name Test"),
%   k4r_get_product_groups(Link, StoreId, ProductGroupList),
%   k4r_get_entity_by_key_value(ProductGroupList, "name", "name Test", ProductGroup),
%   k4r_get_entity_id(ProductGroup, ProductGroupId).

% post_test_shelf(Link, StoreId, ProductGroupId, ShelfId) :-
%   post_test_product_group(Link, StoreId, ProductGroupId),
%   k4r_post_shelf(Link, StoreId, ProductGroupId,
%     ["cadPlanId Test", 10, "externalReferenceId Test", 11, 12.12, 13.13, 14.14, 15.15, 16.16, 17.17, 18.18, 19]),
%   get_shelf_id_by_shelf_ext_id(Link, StoreId, "externalReferenceId Test", ShelfId).

% post_test_shelf_layer(Link, StoreId, ProductGroupId, ShelfId, ShelfLayerId) :-
%   post_test_shelf(Link, StoreId, ProductGroupId, ShelfId),
%   k4r_post_shelf_layer(Link, ShelfId, [10, "externalReferenceId Test", 11, 12, 13.13, "type Test", 14]),
%   get_shelf_layer_id_by_shelf_layer_ext_id(Link, ShelfId, "externalReferenceId Test", ShelfLayerId).

% get_test_customer_id(Link, CustomerId) :-
%   get_customer_id_by_customer_name(Link, "anonymisedName Test", CustomerId).

get_material_group_id_by_name(MaterialGroupList, Name, MaterialGroupId) :-
  get_entity_by_key_value(MaterialGroupList, "name", Name, MaterialGroup),
  get_entity_id(MaterialGroup, MaterialGroupId).

get_customer_id_by_anonymised_name(CustomerList, CustomerName, CustomerId) :-
  get_entity_by_key_value(CustomerList, "anonymisedName", CustomerName, Customer),
  get_entity_id(Customer, CustomerId).

get_store_id_by_store_name(StoreList, StoreName, StoreId) :-
  get_entity_by_key_value(StoreList, "storeName", StoreName, Store),
  get_entity_id(Store, StoreId).

get_product_characteristic_id_by_name(ProductCharacteristicList, Name, ProductCharacteristicId) :-
  get_entity_by_key_value(ProductCharacteristicList, "name", Name, ProductCharacteristic),
  get_entity_id(ProductCharacteristic, ProductCharacteristicId).

% get_product_group_id_by_product_group_name(Link, StoreId, ProductGroupName, ProductGroupId) :-
%   k4r_get_product_groups(Link, StoreId, ProductGroupList),
%   k4r_get_entity_by_key_value(ProductGroupList, "name", ProductGroupName, ProductGroup),
%   k4r_get_entity_id(ProductGroup, ProductGroupId).

% get_test_product_group_id(Link, StoreId, ProductGroupId) :-
%   get_test_store_id(Link, StoreId),
%   k4r_get_product_groups(Link, StoreId, ProductGroupList),
%   k4r_get_entity_by_key_value(ProductGroupList, "name", "name Test", ProductGroup),
%   k4r_get_entity_id(ProductGroup, ProductGroupId).

% get_shelf_id_by_shelf_ext_id(Link, StoreId, ShelfExtId, ShelfId) :-
%   k4r_get_shelves(Link, StoreId, ShelfList),
%   k4r_get_entity_by_key_value(ShelfList, "externalReferenceId", ShelfExtId, Shelf),
%   k4r_get_entity_id(Shelf, ShelfId).

% get_test_shelf_id(Link, ShelfId) :-
%   get_test_store_id(Link, StoreId),
%   get_shelf_id_by_shelf_ext_id(Link, StoreId, "externalReferenceId Test", ShelfId).

% get_shelf_layer_id_by_shelf_layer_ext_id(Link, ShelfId, ShelfLayerExtId, ShelfLayerId) :-
%   k4r_get_shelf_layers(Link, ShelfId, ShelfLayerList),
%   k4r_get_entity_by_key_value(ShelfLayerList, "externalReferenceId", ShelfLayerExtId, ShelfLayer),
%   k4r_get_entity_id(ShelfLayer, ShelfLayerId).

% get_shopping_basket_position_id_by_product_id(Link, StoreId, CustomerId, ProductId, ShoppingBasketId) :-
%   k4r_get_shopping_basket_positions(Link, StoreId, CustomerId, ShoppingBasketList),
%   k4r_get_entity_by_key_value(ShoppingBasketList, "productId", ProductId, ShoppingBasket),
%   k4r_get_entity_id(ShoppingBasket, ShoppingBasketId).

% get_test_shelf_layer_id(Link, ShelfLayerId) :-
%   get_test_shelf_id(Link, ShelfId),
%   get_shelf_layer_id_by_shelf_layer_ext_id(Link, ShelfId, "externalReferenceId Test", ShelfLayerId).

% get_facing_id_by_layer_rel_pos(Link, ShelfLayerId, LayerRelativePosition, FacingId) :-
%   k4r_get_facings(Link, ShelfLayerId, FacingList),
%   k4r_get_entity_by_key_value(FacingList, "layerRelativePosition", LayerRelativePosition, Facing),
%   k4r_get_entity_id(Facing, FacingId).

% get_planogram_id_by_number_of_facing(Link, NumberOfFacings, PlanogramId) :-
%   k4r_get_planograms(Link, PlanogramList),
%   k4r_get_entity_by_key_value(PlanogramList, "numberOfFacings", NumberOfFacings, Planogram),
%   k4r_get_entity_id(Planogram, PlanogramId).

% delete_test_product_group_and_store(Link) :-
%   get_test_product_group_id(Link, StoreId, ProductGroupId),
%   k4r_delete_product_group(Link, ProductGroupId),
%   k4r_delete_store(Link, StoreId).

% delete_test_shelf_layer_and_shelf(Link) :-
%   get_test_shelf_id(Link, ShelfId),
%   get_shelf_layer_id_by_shelf_layer_ext_id(Link, ShelfId, "externalReferenceId Test", ShelfLayerId),
%   k4r_delete_shelf_layer(Link, ShelfLayerId),
%   k4r_delete_shelf(Link, ShelfId).

% delete_test_data(Link) :-
%   delete_test_product(Link),
%   delete_test_shelf_layer_and_shelf(Link),
%   delete_test_product_group_and_store(Link).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %            Customer           %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('customer_test') :-
%   % POST
%   post_customer("HoangGiang", Customer),

%   % GET ONE
%   get_entity_id(Customer, CustomerId),
%   get_customer(CustomerId, CustomerSame),
%   write('Return customer with id '), writeln(CustomerId),
%   writeln(CustomerSame),

%   % PUT
%   put_customer(CustomerId, "Hoang Giang", _),

%   % GET ALL
%   post_customer("Gautam", Customer2),
%   get_customers(CustomerList),
%   writeln('Return customer list:'),
%   writeln(CustomerList),

%   % DELETE
%   delete_customer(CustomerId),
%   get_entity_id(Customer2, CustomerId2),
%   delete_customer(CustomerId2),
%   get_customers(CustomerListNew),
%   writeln('Return empty customer list:'),
%   writeln(CustomerListNew).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %             Store             %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('store_test') :-
%   % POST
%   post_store(
%     ["addressAdditional 1", 
%     "addressCity 1", 
%     "addressCountry 1", 
%     "addressPostcode 1", 
%     "addressState 1", 
%     "addressStreet 1", 
%     "addressStreetNumber 1", 
%     "cadPlanId 1",
%     1.1,
%     1.2,
%     "storeName 1",
%     "storeNumber 1"], 
%     Store1),
%   post_store(
%     ["addressAdditional 2", 
%     "addressCity 2", 
%     "addressCountry 2", 
%     "addressPostcode 2", 
%     "addressState 2", 
%     "addressStreet 2", 
%     "addressStreetNumber 2", 
%     "cadPlanId 2", 
%     2.1, 
%     2.2,
%     "storeName 2",
%     "storeNumber 2"], 
%     Store2),
%   post_store(
%     "{\"addressAdditional\" : \"addressAdditional 3\",
%     \"addressCity\" : \"addressCity 3\",
%     \"addressCountry\" : \"addressCountry 3\",
%     \"addressPostcode\" : \"addressPostcode 3\",
%     \"addressState\" : \"addressState 3\",
%     \"addressStreet\" : \"addressStreet 3\",
%     \"addressStreetNumber\" : \"addressStreetNumber 3\",
%     \"cadPlanId\" : \"cadPlanId 3\",
%     \"latitude\" : 3.1,
%     \"longitude\" : 3.2,
%     \"storeName\" : \"storeName 3\",
%     \"storeNumber\" : \"storeNumber 3\"}",
%     Store3),
%   post_store(
%     "{\"addressAdditional\" : \"addressAdditional 4\",
%     \"addressCity\" : \"addressCity 4\",
%     \"addressCountry\" : \"addressCountry 4\",
%     \"addressPostcode\" : \"addressPostcode 4\",
%     \"addressState\" : \"addressState 4\",
%     \"addressStreet\" : \"addressStreet 4\",
%     \"addressStreetNumber\" : \"addressStreetNumber 4\",
%     \"cadPlanId\" : \"cadPlanId 4\",
%     \"latitude\" : 4.1,
%     \"longitude\" : 4.2,
%     \"storeName\" : \"storeName 4\",
%     \"storeNumber\" : \"storeNumber 4\"}",
%     Store4),

%   % GET ONE
%   get_entity_id(Store1, StoreId1),
%   get_entity_id(Store2, StoreId2),
%   get_entity_id(Store3, StoreId3),
%   get_entity_id(Store4, StoreId4),
%   get_store(StoreId1, StoreSame1),
%   write('Return store with id:'), writeln(StoreId1),
%   writeln(StoreSame1),

%   % PUT
%   put_store(StoreId1, 
%     "{\"addressAdditional\" : \"Changed\",
%       \"addressCity\" : \"REFD\",
%       \"addressCountry\" : \"DE\",
%       \"addressPostcode\" : \"23232\",
%       \"addressState\" : \"EDFG\",
%       \"addressStreet\" : \"ABCD\",
%       \"addressStreetNumber\" : \"19\",
%       \"cadPlanId\" : \"123CAD321\",
%       \"latitude\" : 12.43,
%       \"longitude\" : 32.435,
%       \"storeName\" : \"storeName 4\",
%       \"storeNumber\" : \"ABC123\"}",
%       _),
%   put_store(StoreId2, 
%     ["addressAdditional changed", 
%       "addressCity changed", 
%       "addressCountry changed", 
%       "addressPostcode changed", 
%       "addressState changed", 
%       "addressStreet changed", 
%       "addressStreetNumber changed", 
%       "cadPlanId changed", 
%       4.1, 
%       4.2,
%       "storeName 4",
%       "storeNumber changed"], 
%       _),

%   % GET ALL
%   get_stores(StoreList),
%   writeln('Return store list:'),
%   writeln(StoreList),

%   % DELETE
%   delete_store(StoreId1),
%   delete_store(StoreId2),
%   delete_store(StoreId3),
%   delete_store(StoreId4),
%   get_stores(StoreListNew),
%   writeln('Return empty store list:'),
%   writeln(StoreListNew).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %         MaterialGroup         %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('material_group_test') :-
%   % POST
%   post_material_group("Group 1", MaterialGroup1),
%   post_material_group("Group2", MaterialGroup2),
%   get_entity_id(MaterialGroup1, MaterialGroupId1),
%   get_entity_id(MaterialGroup2, MaterialGroupId2),
%   post_material_group(MaterialGroupId1, "Group3", MaterialGroup3),
%   post_material_group(MaterialGroupId2, "Group 4", MaterialGroup4),

%   % GET ONE
%   get_entity_id(MaterialGroup3, MaterialGroupId3),
%   get_entity_id(MaterialGroup4, MaterialGroupId4),
%   get_material_group(MaterialGroupId1, MaterialGroupSame1),
%   write('Return material group at Id:'), writeln(MaterialGroupId1),
%   writeln(MaterialGroupSame1),

%   % PUT
%   put_material_group(MaterialGroupId3, "Group 3", _),
%   put_material_group(MaterialGroupId2, MaterialGroupId1, "Group 2", _),

%   % GET ALL
%   get_material_groups(MaterialGroupList),
%   writeln('Return full material group list:'),
%   writeln(MaterialGroupList),

%   % DELETE
%   delete_material_group(MaterialGroupId4),
%   delete_material_group(MaterialGroupId3),
%   delete_material_group(MaterialGroupId2),
%   delete_material_group(MaterialGroupId1),
%   get_material_groups(MaterialGroupListNew),
%   writeln('Return empty material list:'),
%   writeln(MaterialGroupListNew).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %            Product            %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('product_test') :-
%   % POST ONE
%   post_material_group("GroupTest1", MaterialGroup1),
%   get_entity_id(MaterialGroup1, MaterialGroupId1),
%   post_material_group(MaterialGroupId1, "GroupTest2", MaterialGroup2),
%   get_entity_id(MaterialGroup2, MaterialGroupId2),
%   ProductId1 = "id1",
%   post_product(ProductId1, ["description 1", "name 1", "productType 1", "productUnit 1"], _),
%   ProductId2 = "id2",
%   post_product(ProductId2, 
%     "{\"description\" : \"description 2\",
%       \"name\" : \"name 2\",
%       \"productType\" : \"productType 2\",
%       \"productUnit\" : \"productUnit 2\"}", 
%       _),
%   ProductId3 = "id3",
%   post_product(ProductId3, MaterialGroupId1, ["description 3", "name 3", "productType 3", "productUnit 3"], _),
%   ProductId4 = "id4",
%   post_product(ProductId4, MaterialGroupId2,
%     "{\"description\" : \"description 4\",
%       \"name\" : \"name 4\",
%       \"productType\" : \"productType 4\",
%       \"productUnit\" : \"productUnit 4\"}", 
%       _),

%   % POST MANY
%   post_products(
%     "{\"products\": 
%       [
%         {
%           \"description\" : \"description 5\",
%           \"id\" : \"id5\",
%           \"name\" : \"name 5\",
%           \"productType\" : \"productType 5\",
%           \"productUnit\" : \"productUnit 5\"
%         }
%         ,{
%           \"description\" : \"description 6\",
%           \"id\" : \"id6\",
%           \"name\" : \"name 6\",
%           \"productType\" : \"productType 6\",
%           \"productUnit\" : \"productUnit 6\"
%         }
%       ]}", 
%     _),
%   post_products([
%     ["description 7", "id7", "name 7", "productType 7", "productUnit 7"],
%     ["description 8", "id8", "name 8", "productType 8", "productUnit 8"]], 
%     _),

%   % GET ONE
%   get_product("id1", ProductSame1),
%   writeln('Return product with id id1:'),
%   writeln(ProductSame1),

%   % PUT
%   put_product(ProductId1, MaterialGroupId1, ["description 1 changed", "name 1 changed", "productType 1 changed", "productUnit 1 changed"], _),
%   put_product(ProductId2, MaterialGroupId1,
%     "{\"description\" : \"description 2 changed\",
%       \"name\" : \"name 2 changed\",
%       \"productType\" : \"productType 2 changed\",
%       \"productUnit\" : \"productUnit 2 changed\"}", 
%       _),
%   put_product(ProductId3,
%     "{\"description\" : \"description 3 changed\",
%       \"name\" : \"name 3 changed\",
%       \"productType\" : \"productType 3 changed\",
%       \"productUnit\" : \"productUnit 3 changed\"}", 
%       _),
%   put_product(ProductId4, ["description 4 changed", "name 4 changed", "productType 4 changed", "productUnit 4 changed"], _),
  
%   % GET ALL
%   get_products(ProductList),
%   writeln('Return product list:'),
%   writeln(ProductList),

%   % DELETE
%   delete_product(ProductId1),
%   delete_product(ProductId2),
%   delete_product(ProductId3),
%   delete_product(ProductId4),
%   delete_product("id5"),
%   delete_product("id6"),
%   delete_product("id7"),
%   delete_product("id8"),
%   delete_material_group(MaterialGroupId2),
%   delete_material_group(MaterialGroupId1),
%   get_products(ProductListNew),
%   writeln('Return empty product list:'),
%   writeln(ProductListNew).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %     ProductCharacteristic     %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('product_characteristic_post_test') :-
%   % POST
%   post_product_characteristic("Characteristic 1", ProductCharacteristic),

%   % GET ALL
%   get_product_characteristics(ProductCharacteristicList),
%   writeln('Return product characteristic list:'),
%   writeln(ProductCharacteristicList),

%   % DELETE
%   get_entity_id(ProductCharacteristic, ProductCharacteristicId),
%   delete_product_characteristic(ProductCharacteristicId),
%   get_product_characteristics(ProductCharacteristicListNew),
%   writeln('Return empty product characteristic list:'),
%   writeln(ProductCharacteristicListNew).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %        ProductProperty        %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('product_property_test') :-
%   % POST
%   post_test_store(StoreId),
%   post_test_product(ProductId),
%   post_test_product_characteristics(ProductCharacteristicId1, ProductCharacteristicId2, ProductCharacteristicId3, ProductCharacteristicId4),
%   post_product_property(StoreId, ProductId, ProductCharacteristicId1, "value 1", _),
%   post_product_property(StoreId, ProductId, ProductCharacteristicId2, "value 2", _),
%   post_product_property(StoreId, ProductId, ProductCharacteristicId3, "value 3", _),
%   post_product_property(StoreId, ProductId, ProductCharacteristicId4, "value 4", _),

%   % GET ALL
%   get_product_properties(StoreId, ProductId, ProductPropertyList),
%   writeln('Return product property list:'),
%   writeln(ProductPropertyList),

%   % DELETE
%   delete_product_property(StoreId, ProductId, ProductCharacteristicId1),
%   delete_product_property(StoreId, ProductId, ProductCharacteristicId2),
%   delete_product_property(StoreId, ProductId, ProductCharacteristicId3),
%   delete_product_property(StoreId, ProductId, ProductCharacteristicId4),
%   get_product_properties(StoreId, ProductId, ProductPropertyListNew),
%   writeln('Return empty product property list:'),
%   writeln(ProductPropertyListNew),
%   delete_product_characteristic(ProductCharacteristicId1),
%   delete_product_characteristic(ProductCharacteristicId2),
%   delete_product_characteristic(ProductCharacteristicId3),
%   delete_product_characteristic(ProductCharacteristicId4),
%   delete_product(ProductId),
%   delete_store(StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %        LogisticalUnit         %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('logistical_unit_test') :-
%   % POST
%   post_test_product(ProductId),
%   post_logistical_unit(ProductId, [true, true, "dimensionUnit 1", 1.1, 1.2, 1, 1.3, true, 1.4, "quantityUnit 1", "quantityUnitIso 1", true, 1.5, 1.6], LogisticalUnit1),
%   get_entity_id(LogisticalUnit1, LogisticalUnitId1),
%   post_logistical_unit(LogisticalUnitId1, ProductId, [false, false, "dimensionUnit 2", 2.1, 2.2, 2, 2.3, false, 2.4, "quantityUnit 2", "quantityUnitIso 2", false, 2.5, 2.6], LogisticalUnit2),
%   get_entity_id(LogisticalUnit2, LogisticalUnitId2),
%   post_logistical_unit(ProductId, 
%     "{\"basicUnit\" : true,
%       \"deliveryExpanseUnit\" : false,
%       \"dimensionUnit\" : \"dimensionUnit 3\",
%       \"height\" : 3.1,
%       \"length\" : 3.2,
%       \"maxStackSize\" : 3,
%       \"netWeight\" : 3.3,
%       \"orderUnit\" : true,
%       \"quantityOfPredecessors\" : 3.4,
%       \"quantityUnit\" : \"quantityUnit 3\",
%       \"quantityUnitIso\" : \"quantityUnitIso 3\",
%       \"retailUnit\" : false,
%       \"weightUnit\" : 3.6,
%       \"width\" : 3.6}", 
%       LogisticalUnit3),
%   get_entity_id(LogisticalUnit3, LogisticalUnitId3),
%   post_logistical_unit(LogisticalUnitId3, ProductId, 
%     "{\"basicUnit\" : false,
%       \"deliveryExpanseUnit\" : true,
%       \"dimensionUnit\" : \"dimensionUnit 4\",
%       \"height\" : 4.1,
%       \"length\" : 4.2,
%       \"maxStackSize\" : 4,
%       \"netWeight\" : 4.3,
%       \"orderUnit\" : false,
%       \"quantityOfPredecessors\" : 4.4,
%       \"quantityUnit\" : \"quantityUnit 4\",
%       \"quantityUnitIso\" : \"quantityUnitIso 4\",
%       \"retailUnit\" : true,
%       \"weightUnit\" : 4.6,
%       \"width\" : 4.6
%       }", 
%       LogisticalUnit4),
%   get_entity_id(LogisticalUnit4, LogisticalUnitId4),

%   % GET ONE
%   get_logistical_unit(LogisticalUnitId1, LogisticalUnitSame1),
%   write('Return logistical unit at Id '), writeln(LogisticalUnitId1),
%   writeln(LogisticalUnitSame1),

%   % PUT
%   put_logistical_unit(LogisticalUnitId4, ProductId, [true, true, "dimensionUnit 4 changed", 4.1, 4.2, 4, 4.3, true, 4.4, "quantityUnit 4 changed", "quantityUnitIso 4 changed", true, 4.5, 4.6], _),
%   put_logistical_unit(LogisticalUnitId3, LogisticalUnitId2, ProductId, 
%     "{\"basicUnit\" : true,
%       \"deliveryExpanseUnit\" : false,
%       \"dimensionUnit\" : \"dimensionUnit 3\",
%       \"height\" : 3.1,
%       \"length\" : 3.2,
%       \"maxStackSize\" : 3,
%       \"netWeight\" : 3.3,
%       \"orderUnit\" : true,
%       \"quantityOfPredecessors\" : 3.4,
%       \"quantityUnit\" : \"quantityUnit 3\",
%       \"quantityUnitIso\" : \"quantityUnitIso 3\",
%       \"retailUnit\" : false,
%       \"weightUnit\" : 3.6,
%       \"width\" : 3.6}",
%       _),
%   put_logistical_unit(LogisticalUnitId2, LogisticalUnitId1, ProductId, [true, true, "dimensionUnit 2 changed", 2.1, 2.2, 2, 2.3, true, 2.4, "quantityUnit 2 changed", "quantityUnitIso 2 changed", true, 2.5, 2.6], _),
%   put_logistical_unit(LogisticalUnitId1,
%     "{\"basicUnit\" : true,
%       \"deliveryExpanseUnit\" : false,
%       \"dimensionUnit\" : \"dimensionUnit 1 changed\",
%       \"height\" : 1.1,
%       \"productId\" : \"idTest\",
%       \"length\" : 1.2,
%       \"maxStackSize\" : 1,
%       \"netWeight\" : 1.3,
%       \"orderUnit\" : true,
%       \"quantityOfPredecessors\" : 1.4,
%       \"quantityUnit\" : \"quantityUnit 1 changed\",
%       \"quantityUnitIso\" : \"quantityUnitIso 1 changed\",
%       \"retailUnit\" : false,
%       \"weightUnit\" : 1.6,
%       \"width\" : 1.6}", 
%       _),

%   % GET ALL
%   get_logistical_units(LogisticalUnitList),
%   writeln('Return logistical unit list:'),
%   writeln(LogisticalUnitList),

%   % DELETE
%   delete_logistical_unit(LogisticalUnitId4),
%   delete_logistical_unit(LogisticalUnitId3),
%   delete_logistical_unit(LogisticalUnitId2),
%   delete_logistical_unit(LogisticalUnitId1),
%   get_logistical_units(LogisticalUnitListNew),
%   writeln('Return empty logistical unit list:'),
%   writeln(LogisticalUnitListNew),
%   delete_product(ProductId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         Product gtin          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('product_gtin_test') :-
  % POST
  % post_test_product(ProductId1),
  % post_test_product2(ProductId2),
  % post_test_logistical_unit1(ProductId1, ProductId2, LogisticalUnitId1, LogisticalUnitId2),

  ProductGtinId1 = "id1",
  ProductGtinId2 = "id2",
  ProductGtinId3 = "id3",
  ProductGtinId4 = "id4",
  % post_product_gtin(ProductGtinId1, LogisticalUnitId1, ProductId1, ["gtinType1", true], _),
  % post_product_gtin(ProductGtinId2, LogisticalUnitId1, ProductId2, ["gtinType2", false], _),
  % post_product_gtin(ProductGtinId3, LogisticalUnitId2, ProductId1, 
  %   "{\"gtinType\" : \"gtinType 3\",
  %     \"mainGtin\" : true}", 
  %   _),
  % post_product_gtin(ProductGtinId4, LogisticalUnitId2, ProductId2, 
  %   "{\"gtinType\" : \"gtinType 4\",
  %     \"mainGtin\" : false}", 
  %   _),

  LogisticalUnitId1 = "167",
  LogisticalUnitId2 = "168",
  ProductId1 = "idTest",
  ProductId2 = "idTest2",
  
  % GET ONE
  get_product_gtin(ProductGtinId1, ProductGtinSame1),
  write('Return product gtin at Id '), writeln(ProductGtinId1),
  writeln(ProductGtinSame1),

  % PUT
  put_product_gtin(ProductGtinId1, LogisticalUnitId2, ProductId2, ["gtinType 1 changed", false], _),
  % put_product_gtin(ProductGtinId2, LogisticalUnitId1, ProductId2, ["gtinType 2 changed", true], _),
  % put_product_gtin(ProductGtinId3, LogisticalUnitId2, ProductId1, 
  %   "{\"gtinType\" : \"gtinType 3 changed\",
  %     \"mainGtin\" : false}", 
  %   _),
  % put_product_gtin(ProductGtinId4, LogisticalUnitId2, ProductId2, 
  %   "{\"gtinType\" : \"gtinType 4 changed\",
  %     \"mainGtin\" : true}", 
  %   _),

  % GET ALL
  get_product_gtins(ProductGtinList),
  writeln('Return product gtin list'),
  writeln(ProductGtinList).

  % % DELETE
  % delete_product_gtin(ProductGtinId1),
  % delete_product_gtin(ProductGtinId2),
  % delete_product_gtin(ProductGtinId3),
  % delete_product_gtin(ProductGtinId4),
  % get_product_gtins(ProductGtinListNew),
  % writeln('Return empty product gtin list'),
  % writeln(ProductGtinListNew),
  % delete_logistical_unit(LogisticalUnitId1),
  % delete_logistical_unit(LogisticalUnitId2),
  % delete_product(ProductId1),
  % delete_product(ProductId2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         Product group         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('product_group_post_test') :-
%   k4r_get_core_link(Link),
%   post_test_store(Link, StoreId),
%   k4r_post_product_group(Link, StoreId, "name 1"),
%   k4r_post_product_group(Link, StoreId, "name 2"),
%   k4r_post_product(Link, [10, "description 1", "gtin 1", 11, 12, "name 1", 13], "id1"),
%   k4r_post_product(Link, [20, "description 2", "gtin 2", 21, 22, "name 2", 23], "id2"),
%   k4r_post_product(Link, [30, "description 3", "gtin 3", 31, 32, "name 3", 33], "id3"),
%   k4r_post_product(Link, [40, "description 4", "gtin 4", 41, 42, "name 4", 43], "id4"),
%   k4r_post_product(Link, [50, "description 5", "gtin 5", 51, 52, "name 5", 53], "id5"),
%   k4r_post_product(Link, [60, "description 6", "gtin 6", 61, 62, "name 6", 63], "id6"),
%   get_product_group_id_by_product_group_name(Link, StoreId, "name 1", ProductGroupId1),
%   get_product_group_id_by_product_group_name(Link, StoreId, "name 2", ProductGroupId2),
%   k4r_post_product_to_product_group(Link, "id1", ProductGroupId1),
%   k4r_post_product_to_product_group(Link, "id2", ProductGroupId1),
%   k4r_post_product_to_product_group(Link, "id3", ProductGroupId2),
%   k4r_post_product_to_product_group(Link, "id4", ProductGroupId2).

% test('product_group_get_test') :-
%   k4r_get_core_link(Link),
%   get_test_store_id(Link, StoreId),
%   get_product_group_id_by_product_group_name(Link, StoreId, "name 1", ProductGroupId),
%   k4r_get_product_group(Link, ProductGroupId, ProductGroup),
%   writeln('Return product group with name 1:'),
%   writeln(ProductGroup).

% test('product_group_post_and_delete_product_test') :-
%   k4r_get_core_link(Link),
%   get_test_store_id(Link, StoreId),
%   get_product_group_id_by_product_group_name(Link, StoreId, "name 1", ProductGroupId),
%   k4r_post_product_to_product_group(Link, "id5", ProductGroupId),
%   k4r_post_product_to_product_group(Link, "id6", ProductGroupId),
%   k4r_get_product_group(Link, ProductGroupId, ProductGroup1),
%   writeln('Return product group 1 with product id5 and id6:'),
%   writeln(ProductGroup1),
%   k4r_delete_product_from_product_group(Link, "id6", ProductGroupId),
%   k4r_get_product_group(Link, ProductGroupId, ProductGroup2),
%   writeln('Return product group 1 without product id6:'),
%   writeln(ProductGroup2).

% test('product_group_delete_test') :-
%   k4r_get_core_link(Link),
%   get_test_store_id(Link, StoreId),
%   get_product_group_id_by_product_group_name(Link, StoreId, "name 1", ProductGroupId1),
%   get_product_group_id_by_product_group_name(Link, StoreId, "name 2", ProductGroupId2),
%   k4r_delete_product_group(Link, ProductGroupId1),
%   k4r_delete_product_group(Link, ProductGroupId2),
%   k4r_delete_product(Link, "id1"),
%   k4r_delete_product(Link, "id2"),
%   k4r_delete_product(Link, "id3"),
%   k4r_delete_product(Link, "id4"),
%   k4r_delete_product(Link, "id5"),
%   k4r_delete_product(Link, "id6"),
%   k4r_delete_store(Link, StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %             Shelf             %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('shelf_post_test') :-
%   k4r_get_core_link(Link),
%   post_test_product_group(Link, StoreId, ProductGroupId),
%   k4r_post_shelf(Link, StoreId, ProductGroupId,
%     ["cadPlanId 1", 10, "externalReferenceId 1", 11, 12.12, 13.13, 14.14, 15.15, 16.16, 17.17, 18.18, 19]),
%   k4r_post_shelf(Link, StoreId, ProductGroupId,
%     ["cadPlanId 2", 20, "externalReferenceId 2", 21, 22.22, 23.23, 24.24, 25.25, 26.26, 27.27, 28.28, 29]),
%   k4r_post_shelf(Link, StoreId, ProductGroupId,
%     ["cadPlanId 3", 30, "externalReferenceId 3", 31, 32.32, 33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39]),
%   k4r_post_shelf(Link, StoreId, ProductGroupId,
%     ["cadPlanId 4", 40, "externalReferenceId 4", 41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48, 49]).

% test('shelf_get_test') :-
%   k4r_get_core_link(Link),
%   get_test_store_id(Link, StoreId),
%   get_shelf_id_by_shelf_ext_id(Link, StoreId, "externalReferenceId 1", ShelfId),
%   k4r_get_shelf(Link, ShelfId, Shelf),
%   writeln('Return Shelf at with externalReferenceId 1:'),
%   writeln(Shelf).

% test('shelf_put_test') :-
%   k4r_get_core_link(Link),
%   get_test_product_group_id(Link, StoreId, ProductGroupId),
%   get_shelf_id_by_shelf_ext_id(Link, StoreId, "externalReferenceId 1", ShelfId),
%   k4r_put_shelf(Link, StoreId, ProductGroupId, ShelfId, 
%     "{
%     \"cadPlanId\" : \"an_new_cadPlanId\",
%     \"depth\" : 32,
%     \"externalReferenceId\" : \"R4\",
%     \"height\" : 12,
%     \"orientationW\" : 23,
%     \"orientationX\" : 32,
%     \"orientationY\" : 312,
%     \"orientationZ\" : 32,
%     \"positionX\" : 432,
%     \"positionY\" : 421,
%     \"positionZ\" : 14,
%     \"width\" : 21
%     }"),
%   k4r_put_shelf(Link, StoreId, ProductGroupId, ShelfId,
%     ["cadPlanId changed", 40, "externalReferenceId 1", 41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48, 49]),
%   k4r_get_shelves(Link, StoreId, ShelfList),
%   writeln('Return Shelves with shelf with cadPlanId changed:'),
%   writeln(ShelfList).

% test('shelf_get_data_test') :-
%   k4r_get_core_link(Link),
%   get_test_store_id(Link, StoreId),
%   get_shelf_id_by_shelf_ext_id(Link, StoreId, "externalReferenceId 1", ShelfId),
%   k4r_get_shelf(Link, ShelfId, Shelf),
%   k4r_get_shelf_data(
%     Shelf,
%     [ShelfPositionX, ShelfPositionY, ShelfPositionZ], 
%     [ShelfOrientationX, ShelfOrientationY, ShelfOrientationZ, ShelfOrientationW], 
%     [ShelfDepth, ShelfWidth, ShelfHeight]),
%   writeln('Position:'),
%   writeln(ShelfPositionX),
%   writeln(ShelfPositionY),
%   writeln(ShelfPositionZ),
%   writeln('Orientation:'),
%   writeln(ShelfOrientationX),
%   writeln(ShelfOrientationY),
%   writeln(ShelfOrientationZ),
%   writeln(ShelfOrientationW),
%   writeln('Dimention:'),
%   writeln(ShelfDepth),
%   writeln(ShelfWidth),
%   writeln(ShelfHeight).

% test('shelf_delete_test') :-
%   k4r_get_core_link(Link),
%   get_test_product_group_id(Link, StoreId, ProductGroupId),
%   get_shelf_id_by_shelf_ext_id(Link, StoreId, "externalReferenceId 1", ShelfId1),
%   k4r_delete_shelf(Link, ShelfId1),
%   get_shelf_id_by_shelf_ext_id(Link, StoreId, "externalReferenceId 2", ShelfId2),
%   k4r_delete_shelf(Link, ShelfId2),
%   get_shelf_id_by_shelf_ext_id(Link, StoreId, "externalReferenceId 3", ShelfId3),
%   k4r_delete_shelf(Link, ShelfId3),
%   get_shelf_id_by_shelf_ext_id(Link, StoreId, "externalReferenceId 4", ShelfId4),
%   k4r_delete_shelf(Link, ShelfId4),
%   k4r_delete_product_group(Link, ProductGroupId),
%   k4r_delete_store(Link, StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %          Shelf layer          %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('shelf_layer_post_test') :-
%   k4r_get_core_link(Link),
%   post_test_shelf(Link, _, _, ShelfId),
%   k4r_post_shelf_layer(Link, ShelfId, [10, "externalReferenceId 1", 11, 12, 13.13, "type 1", 14]),
%   k4r_post_shelf_layer(Link, ShelfId, [20, "externalReferenceId 2", 21, 22, 23.23, "type 2", 24]),
%   k4r_post_shelf_layer(Link, ShelfId, [30, "externalReferenceId 3", 31, 32, 33.33, "type 3", 34]),
%   k4r_post_shelf_layer(Link, ShelfId, [40, "externalReferenceId 4", 41, 42, 43.43, "type 4", 44]).

% test('shelf_layer_get_test') :-
%   k4r_get_core_link(Link),
%   get_test_shelf_id(Link, ShelfId),
%   get_shelf_layer_id_by_shelf_layer_ext_id(Link, ShelfId, "externalReferenceId 1", ShelfLayerId),
%   k4r_get_shelf_layer(Link, ShelfLayerId, ShelfLayer),
%   writeln('Return shelf layer with externalReferenceId 1'),
%   writeln(ShelfLayer).

% test('shelf_layer_delete_test') :-
%   k4r_get_core_link(Link),
%   get_test_shelf_id(Link, ShelfId),
%   get_shelf_layer_id_by_shelf_layer_ext_id(Link, ShelfId, "externalReferenceId 1", ShelfLayerId1),
%   k4r_delete_shelf_layer(Link, ShelfLayerId1),
%   get_shelf_layer_id_by_shelf_layer_ext_id(Link, ShelfId, "externalReferenceId 2", ShelfLayerId2),
%   k4r_delete_shelf_layer(Link, ShelfLayerId2),
%   get_shelf_layer_id_by_shelf_layer_ext_id(Link, ShelfId, "externalReferenceId 3", ShelfLayerId3),
%   k4r_delete_shelf_layer(Link, ShelfLayerId3),
%   get_shelf_layer_id_by_shelf_layer_ext_id(Link, ShelfId, "externalReferenceId 4", ShelfLayerId4),
%   k4r_delete_shelf_layer(Link, ShelfLayerId4),
%   k4r_delete_shelf(Link, ShelfId),
%   delete_test_product_group_and_store(Link).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %        Shopping basket        %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('shopping_basket_post_test') :-
%   k4r_get_core_link(Link),
%   post_test_store(Link, StoreId),
%   post_test_customer(Link, CustomerId), 
%   k4r_post_product(Link, [10, "description 1", "gtin 1", 11, 12, "name 1", 13], "id1"),
%   k4r_post_product(Link, [20, "description 2", "gtin 2", 21, 22, "name 2", 23], "id2"),
%   k4r_post_product(Link, [30, "description 3", "gtin 3", 31, 32, "name 3", 33], "id3"),
%   k4r_post_product(Link, [40, "description 4", "gtin 4", 41, 42, "name 4", 43], "id4"),
%   k4r_post_shopping_basket_position(Link, StoreId, CustomerId, "id1", ["currency 1", 10, 11.11]),
%   k4r_post_shopping_basket_position(Link, StoreId, CustomerId, "id2", ["currency 2", 20, 21.21]),
%   k4r_post_shopping_basket_position(Link, StoreId, CustomerId, "id3", ["currency 3", 30, 31.31]),
%   k4r_post_shopping_basket_position(Link, StoreId, CustomerId, "id4", ["currency 4", 40, 41.41]).

% test('shopping_basket_get_test') :-
%   k4r_get_core_link(Link),
%   get_test_store_id(Link, StoreId),
%   get_test_customer_id(Link, CustomerId),
%   get_shopping_basket_position_id_by_product_id(Link, StoreId, CustomerId, "id1", ShoppingBasketId),
%   k4r_get_shopping_basket_position(Link, ShoppingBasketId, ShoppingBasket),
%   writeln('Return shopping basket with productId id1:'),
%   writeln(ShoppingBasket).

% test('shopping_basket_deletes_test') :-
%   k4r_get_core_link(Link),
%   get_test_store_id(Link, StoreId),
%   k4r_post_customer(Link, "anonymisedName 2"),
%   get_customer_id_by_customer_name(Link, "anonymisedName 2", CustomerId),
%   k4r_post_shopping_basket_position(Link, StoreId, CustomerId, "id1", ["currency 1", 10, 11.11]),
%   k4r_post_shopping_basket_position(Link, StoreId, CustomerId, "id2", ["currency 2", 20, 21.21]),
%   k4r_delete_shopping_basket_positions(Link, StoreId, CustomerId),
%   k4r_delete_customer(Link, CustomerId).

% test('shopping_basket_delete_test') :-
%   k4r_get_core_link(Link),
%   get_test_store_id(Link, StoreId),
%   get_test_customer_id(Link, CustomerId),
%   get_shopping_basket_position_id_by_product_id(Link, StoreId, CustomerId, "id1", ShoppingBasketId1),
%   k4r_delete_shopping_basket_position(Link, ShoppingBasketId1),
%   get_shopping_basket_position_id_by_product_id(Link, StoreId, CustomerId, "id2", ShoppingBasketId2),
%   k4r_delete_shopping_basket_position(Link, ShoppingBasketId2),
%   get_shopping_basket_position_id_by_product_id(Link, StoreId, CustomerId, "id3", ShoppingBasketId3),
%   k4r_delete_shopping_basket_position(Link, ShoppingBasketId3),
%   get_shopping_basket_position_id_by_product_id(Link, StoreId, CustomerId, "id4", ShoppingBasketId4),
%   k4r_delete_shopping_basket_position(Link, ShoppingBasketId4),
%   k4r_delete_product(Link, "id1"),
%   k4r_delete_product(Link, "id2"),
%   k4r_delete_product(Link, "id3"),
%   k4r_delete_product(Link, "id4"),
%   k4r_delete_customer(Link, CustomerId),
%   k4r_delete_store(Link, StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %             Facing            %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('facing_post_test') :-
%   k4r_get_core_link(Link),
%   post_test_shelf_layer(Link, _, _, _, ShelfLayerId),
%   post_test_product(Link, ProductId),
%   k4r_post_facing(Link, ShelfLayerId, ProductId, 10, 11),
%   k4r_post_facing(Link, ShelfLayerId, ProductId, 20, 21),
%   k4r_post_facing(Link, ShelfLayerId, ProductId, 30, 31),
%   k4r_post_facing(Link, ShelfLayerId, ProductId, 40, 41).

% test('facing_get_test') :-
%   k4r_get_core_link(Link),
%   get_test_shelf_layer_id(Link, ShelfLayerId),
%   get_facing_id_by_layer_rel_pos(Link, ShelfLayerId, 10, FacingId),
%   k4r_get_facing(Link, FacingId, Facing),
%   writeln('Return facings with layerRelativePosition 10'),
%   writeln(Facing).

% test('facing_put_test') :-
%   k4r_get_core_link(Link),
%   get_test_shelf_layer_id(Link, ShelfLayerId),
%   get_facing_id_by_layer_rel_pos(Link, ShelfLayerId, 10, FacingId),
%   get_test_product_id(ProductId),
%   k4r_put_facing(Link, ShelfLayerId, ProductId, FacingId, 10, 110),
%   k4r_get_facing(Link, FacingId, Facing),
%   writeln('Return facings with layerRelativePosition 10'),
%   writeln(Facing).

% test('facing_delete_test') :-
%   k4r_get_core_link(Link),
%   get_test_shelf_layer_id(Link, ShelfLayerId),
%   get_facing_id_by_layer_rel_pos(Link, ShelfLayerId, 10, FacingId1),
%   k4r_delete_facing(Link, FacingId1),
%   get_facing_id_by_layer_rel_pos(Link, ShelfLayerId, 20, FacingId2),
%   k4r_delete_facing(Link, FacingId2),
%   get_facing_id_by_layer_rel_pos(Link, ShelfLayerId, 30, FacingId3),
%   k4r_delete_facing(Link, FacingId3),
%   get_facing_id_by_layer_rel_pos(Link, ShelfLayerId, 40, FacingId4),
%   k4r_delete_facing(Link, FacingId4),
%   delete_test_data(Link).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %           Planogram           %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('planogram_post_test') :-
%   k4r_get_core_link(Link),
%   post_test_shelf_layer(Link, _, _, _, ShelfLayerId),
%   post_test_product(Link, ProductId),
%   k4r_post_planogram(Link, ProductId, ShelfLayerId, [10, 11.11, 12, 13]),
%   k4r_post_planogram(Link, ProductId, ShelfLayerId, [20, 21.21, 22, 23]),
%   k4r_post_planogram(Link, ProductId, ShelfLayerId, [30, 31.31, 32, 33]),
%   k4r_post_planogram(Link, ProductId, ShelfLayerId, [40, 41.41, 42, 43]).

% test('planogram_put_test') :-
%   k4r_get_core_link(Link),
%   get_test_shelf_layer_id(Link, ShelfLayerId),
%   get_planogram_id_by_number_of_facing(Link, 10, PlanogramId),
%   k4r_put_planogram(Link, "idTest", ShelfLayerId, PlanogramId, 
%     "{
%       \"numberOfFacings\": 321,
%       \"orientationYaw\": 132.43,
%       \"positionX\": 35,
%       \"versionTimestamp\": 321
%     }"),
%   k4r_put_planogram(Link, "idTest", ShelfLayerId, PlanogramId, [10, 110.11, 120, 130]),
%   k4r_get_planograms(Link, PlanogramListNew),
%   writeln('Return new planogram list:'),
%   writeln(PlanogramListNew).

% test('planogram_delete_test') :-
%   k4r_get_core_link(Link),
%   get_test_shelf_layer_id(Link, ShelfLayerId),
%   get_planogram_id_by_number_of_facing(Link, 10, PlanogramId1),
%   k4r_delete_planogram(Link, PlanogramId1),
%   get_planogram_id_by_number_of_facing(Link, 20, PlanogramId2),
%   k4r_delete_planogram(Link, PlanogramId2),
%   get_planogram_id_by_number_of_facing(Link, 30, PlanogramId3),
%   k4r_delete_planogram(Link, PlanogramId3),
%   get_planogram_id_by_number_of_facing(Link, 40, PlanogramId4),
%   k4r_delete_planogram(Link, PlanogramId4),
%   delete_test_data(Link).

%% ATTENTION: These tests can only show results with mockup data!!!

% test('get_entities_test') :-
%   k4r_get_search_link(Link),
%   k4r_get_entities(Link, 'product', Products),
%   k4r_get_entities(Link, 'shelf', Shelves),
%   k4r_get_entities(Link, 'facing', Facings),
%   writeln('Return product list:'),
%   forall(member(Product, Products), writeln(Product)),
%   writeln('Return shelf list:'),
%   forall(member(Shelf, Shelves), writeln(Shelf)),
%   writeln('Return facing list:'),
%   forall(member(Facing, Facings), writeln(Facing)).

% test('get_entities_by_properties_test') :-
%   k4r_get_search_link(Link),
%   k4r_get_entities_by_properties(Link, 'product', [['length'], [21]], Products1),
%   writeln('Return product list with length 21:'),
%   forall(member(Product, Products1), writeln(Product)),
%   k4r_get_entities_by_properties(Link, 'product', [['length', 'height'], [21, 41]], Products2),
%   writeln('Return product list with length 21 and height 41:'),
%   forall(member(Product, Products2), writeln(Product)),
%   k4r_get_entities_by_properties(Link, 'facing', [['layerRelativePosition', 'quantity'], [40, 11]], Facings),
%   writeln('Return facing list with layerRelativePosition 40 and quantity 11:'),
%   forall(member(Facing, Facings), writeln(Facing)).

% test('get_products_in_store_test') :-
%   k4r_get_search_link(Link),
%   k4r_get_entities_by_properties(Link, 'store', [['storeName'], ['storeName%20Test']], [Store]), % %20 means space, 'storename%20Test' means 'storename Test'
%   k4r_get_value_from_key(Store, 'id', StoreId),
%   k4r_get_entity_property_by_properties(Link, 'product_group', [['storeId'], [StoreId]], 'products', Products), % get products on product_group in store that has store id StoreId
%   writeln('Return product list:'),
%   forall(member(Product, Products), writeln(Product)).


/* test('shelf location') :-
  post_shelf_location(2). */
/* 
test('shelf layer') :-
  gtrace,
  post_shelf_layers(2). */

% test('facing ') :-
%   post_facing(2).

% test('post shelf layer') :-
%   k4r_get_core_link(Link),
%   k4r_post_shelf_layer(Link, 1, 2.5, [0.345, 0.234, 0.01], 1, "Ext_1", "Bottom_Layer"). 

:- end_tests(k4r_db_client).
