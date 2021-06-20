:- use_module('k4r_db_client.pl').
:- use_module(library('rostest')).

:- begin_tests(
        'k4r_db_client').

post_test_customer(CustomerId1, CustomerId2) :-
  post_customer("anonymisedName Test1", Customer1),
  get_entity_id(Customer1, CustomerId1),
  post_customer("anonymisedName Test2", Customer2),
  get_entity_id(Customer2, CustomerId2).

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

post_test_store_characteristics(StoreCharacteristicId1, StoreCharacteristicId2, StoreCharacteristicId3, StoreCharacteristicId4) :-
  post_store_characteristic("name 1", StoreCharacteristic1),
  get_entity_id(StoreCharacteristic1, StoreCharacteristicId1),
  post_store_characteristic("name 2", StoreCharacteristic2),
  get_entity_id(StoreCharacteristic2, StoreCharacteristicId2),
  post_store_characteristic("name 3", StoreCharacteristic3),
  get_entity_id(StoreCharacteristic3, StoreCharacteristicId3),
  post_store_characteristic("name 4", StoreCharacteristic4),
  get_entity_id(StoreCharacteristic4, StoreCharacteristicId4).

post_test_product("idTest") :-
  post_product("idTest", ["description test", "name test", "productType test", "productUnit test"], _).
post_test_product2("idTest2") :-
  post_product("idTest2", ["description test2", "name test2", "productType test2", "productUnit test2"], _).
post_test_products("idTest1", "idTest2", "idTest3", "idTest4", "idTest5", "idTest6") :-
  post_product("idTest1", ["description test1", "name test1", "productType test1", "productUnit test1"], _),
  post_product("idTest2", ["description test2", "name test2", "productType test2", "productUnit test2"], _),
  post_product("idTest3", ["description test3", "name test3", "productType test3", "productUnit test3"], _),
  post_product("idTest4", ["description test4", "name test4", "productType test4", "productUnit test4"], _),
  post_product("idTest5", ["description test5", "name test5", "productType test5", "productUnit test5"], _),
  post_product("idTest6", ["description test6", "name test6", "productType test6", "productUnit test6"], _).

post_test_logistical_unit(ProductId1, ProductId2, LogisticalUnitId1, LogisticalUnitId2) :-
  post_test_product(ProductId1),
  post_test_product2(ProductId2),
  post_logistical_unit("", ProductId1, [true, true, "dimensionUnit 1", 1.1, 1.2, 1, 1.3, true, 1.4, "quantityUnit 1", "quantityUnitIso 1", true, 1.5, 1.6], LogisticalUnit1),
  get_entity_id(LogisticalUnit1, LogisticalUnitId1),
  post_logistical_unit("", ProductId2, [true, true, "dimensionUnit 2", 2.1, 2.2, 2, 2.3, true, 2.4, "quantityUnit 2", "quantityUnitIso 2", true, 2.5, 2.6], LogisticalUnit2),
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

post_test_product_group(StoreId, ProductGroupId) :-
  post_test_store(StoreId),
  post_product_group(StoreId, "{\"name\" : \"name Test\"}", ProductGroup),
  get_entity_id(ProductGroup, ProductGroupId).

post_test_shelf(StoreId, ProductGroupId, ShelfId) :-
  post_test_product_group(StoreId, ProductGroupId),
  post_shelf(StoreId, ProductGroupId,
    ["cadPlanId Test", 10, "externalReferenceId Test", 11, 12.12, 13.13, 14.14, 15.15, 16.16, 17.17, 18.18, 19], Shelf),
  get_entity_id(Shelf, ShelfId).

post_test_shelf_layer(StoreId, ProductGroupId, ShelfId, ShelfLayerId) :-
  post_test_shelf(StoreId, ProductGroupId, ShelfId),
  post_shelf_layer(ShelfId, [10, "externalReferenceId Test", 11, 12, 13.13, "type Test", 14], ShelfLayer),
  get_entity_id(ShelfLayer, ShelfLayerId).

post_test_facings(StoreId, ProductGroupId, ShelfId, ShelfLayerId, FacingId1, FacingId2) :-
  post_test_shelf_layer(StoreId, ProductGroupId, ShelfId, ShelfLayerId),
  post_facing(ShelfLayerId, [10, 11, 12], Facing1),
  get_entity_id(Facing1, FacingId1),
  post_facing(ShelfLayerId, [20, 21, 22], Facing2),
  get_entity_id(Facing2, FacingId2).

post_test_item_groups(StoreId, ProductId1, ProductId2, LogisticalUnitId1, LogisticalUnitId2, ProductGroupId, ShelfId, ShelfLayerId, FacingId1, FacingId2, ItemGroupId1, ItemGroupId2) :-
  post_test_logistical_unit(ProductId1, ProductId2, LogisticalUnitId1, LogisticalUnitId2),
  post_test_facings(StoreId, ProductGroupId, ShelfId, ShelfLayerId, FacingId1, FacingId2),
  post_item_group(FacingId1, LogisticalUnitId1, 10, ItemGroup1),
  get_entity_id(ItemGroup1, ItemGroupId1),
  post_item_group(FacingId2, LogisticalUnitId2, 20, ItemGroup2),
  get_entity_id(ItemGroup2, ItemGroupId2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            Customer           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('customer_test') :-
  % POST
  post_customer("HoangGiang", Customer),

  % GET ONE
  get_entity_id(Customer, CustomerId),
  get_customer(CustomerId, CustomerSame),
  write('Return customer with id '), writeln(CustomerId),
  writeln(CustomerSame),

  % PUT
  put_customer(CustomerId, "Hoang Giang", _),

  % GET ALL
  post_customer("Gautam", Customer2),
  get_customers(CustomerList),
  writeln('Return customer list:'),
  writeln(CustomerList),

  % DELETE
  delete_customer(CustomerId),
  get_entity_id(Customer2, CustomerId2),
  delete_customer(CustomerId2),
  get_customers(CustomerListNew),
  writeln('Return empty customer list:'),
  writeln(CustomerListNew).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             Store             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('store_test') :-
  % POST
  post_store(
    ["addressAdditional 1", 
    "addressCity 1", 
    "addressCountry 1", 
    "addressPostcode 1", 
    "addressState 1", 
    "addressStreet 1", 
    "addressStreetNumber 1", 
    "cadPlanId 1",
    1.1,
    1.2,
    "storeName 1",
    "storeNumber 1"], 
    Store1),
  post_store(
    ["addressAdditional 2", 
    "addressCity 2", 
    "addressCountry 2", 
    "addressPostcode 2", 
    "addressState 2", 
    "addressStreet 2", 
    "addressStreetNumber 2", 
    "cadPlanId 2", 
    2.1, 
    2.2,
    "storeName 2",
    "storeNumber 2"], 
    Store2),
  post_store(
    "{\"addressAdditional\" : \"addressAdditional 3\",
    \"addressCity\" : \"addressCity 3\",
    \"addressCountry\" : \"addressCountry 3\",
    \"addressPostcode\" : \"addressPostcode 3\",
    \"addressState\" : \"addressState 3\",
    \"addressStreet\" : \"addressStreet 3\",
    \"addressStreetNumber\" : \"addressStreetNumber 3\",
    \"cadPlanId\" : \"cadPlanId 3\",
    \"latitude\" : 3.1,
    \"longitude\" : 3.2,
    \"storeName\" : \"storeName 3\",
    \"storeNumber\" : \"storeNumber 3\"}",
    Store3),
  post_store(
    "{\"addressAdditional\" : \"addressAdditional 4\",
    \"addressCity\" : \"addressCity 4\",
    \"addressCountry\" : \"addressCountry 4\",
    \"addressPostcode\" : \"addressPostcode 4\",
    \"addressState\" : \"addressState 4\",
    \"addressStreet\" : \"addressStreet 4\",
    \"addressStreetNumber\" : \"addressStreetNumber 4\",
    \"cadPlanId\" : \"cadPlanId 4\",
    \"latitude\" : 4.1,
    \"longitude\" : 4.2,
    \"storeName\" : \"storeName 4\",
    \"storeNumber\" : \"storeNumber 4\"}",
    Store4),

  % GET ONE
  get_entity_id(Store1, StoreId1),
  get_entity_id(Store2, StoreId2),
  get_entity_id(Store3, StoreId3),
  get_entity_id(Store4, StoreId4),
  get_store(StoreId1, StoreSame1),
  write('Return store with id:'), writeln(StoreId1),
  writeln(StoreSame1),

  % PUT
  put_store(StoreId1, 
    "{\"addressAdditional\" : \"Changed\",
      \"addressCity\" : \"REFD\",
      \"addressCountry\" : \"DE\",
      \"addressPostcode\" : \"23232\",
      \"addressState\" : \"EDFG\",
      \"addressStreet\" : \"ABCD\",
      \"addressStreetNumber\" : \"19\",
      \"cadPlanId\" : \"123CAD321\",
      \"latitude\" : 12.43,
      \"longitude\" : 32.435,
      \"storeName\" : \"storeName 4\",
      \"storeNumber\" : \"ABC123\"}",
      _),
  put_store(StoreId2, 
    ["addressAdditional changed", 
      "addressCity changed", 
      "addressCountry changed", 
      "addressPostcode changed", 
      "addressState changed", 
      "addressStreet changed", 
      "addressStreetNumber changed", 
      "cadPlanId changed", 
      4.1, 
      4.2,
      "storeName 4",
      "storeNumber changed"], 
      _),

  % GET ALL
  get_stores(StoreList),
  writeln('Return store list:'),
  writeln(StoreList),

  % DELETE
  delete_store(StoreId1),
  delete_store(StoreId2),
  delete_store(StoreId3),
  delete_store(StoreId4),
  get_stores(StoreListNew),
  writeln('Return empty store list:'),
  writeln(StoreListNew).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     StoreCharacteristic     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('store_characteristic_test') :-
  % POST
  post_store_characteristic("Characteristic 1", StoreCharacteristic),

  % GET ALL
  get_store_characteristics(StoreCharacteristicList),
  writeln('Return store characteristic list:'),
  writeln(StoreCharacteristicList),

  % DELETE
  get_entity_id(StoreCharacteristic, StoreCharacteristicId),
  delete_store_characteristic(StoreCharacteristicId),
  get_store_characteristics(StoreCharacteristicListNew),
  writeln('Return empty store characteristic list:'),
  writeln(StoreCharacteristicListNew).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       StoreProperty        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('store_property_test') :-
  % Post mockup data
  post_test_store(StoreId),
  post_test_store_characteristics(StoreCharacteristicId1, StoreCharacteristicId2, StoreCharacteristicId3, StoreCharacteristicId4),

  % POST
  post_store_property(StoreId, StoreCharacteristicId1, "value 1", _),
  post_store_property(StoreId, StoreCharacteristicId2, "value 2", _),
  post_store_property(StoreId, StoreCharacteristicId3, "value 3", _),
  post_store_property(StoreId, StoreCharacteristicId4, "value 4", _),

  % GET ALL
  get_store_properties(StoreId, StorePropertyList),
  writeln('Return store property list:'),
  writeln(StorePropertyList),

  % DELETE
  delete_store_property(StoreId, StoreCharacteristicId1),
  delete_store_property(StoreId, StoreCharacteristicId2),
  delete_store_property(StoreId, StoreCharacteristicId3),
  delete_store_property(StoreId, StoreCharacteristicId4),
  get_store_properties(StoreId, StorePropertyListNew),
  writeln('Return empty store property list:'),
  writeln(StorePropertyListNew),

  % Delete mockup data
  delete_store_characteristic(StoreCharacteristicId1),
  delete_store_characteristic(StoreCharacteristicId2),
  delete_store_characteristic(StoreCharacteristicId3),
  delete_store_characteristic(StoreCharacteristicId4),
  delete_store(StoreId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         MaterialGroup         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('material_group_test') :-
  % POST
  post_material_group(["Group 1"], MaterialGroup1),
  post_material_group("{\"name\" : \"name Group 2\"}", MaterialGroup2),
  get_entity_id(MaterialGroup1, MaterialGroupId1),
  get_entity_id(MaterialGroup2, MaterialGroupId2),
  post_material_group(MaterialGroupId1, ["Group3"], MaterialGroup3),
  post_material_group(MaterialGroupId2, "{\"name\" : \"name Group 4\"}", MaterialGroup4),

  % GET ONE
  get_entity_id(MaterialGroup3, MaterialGroupId3),
  get_entity_id(MaterialGroup4, MaterialGroupId4),
  get_material_group(MaterialGroupId1, MaterialGroupSame1),
  write('Return material group at Id: '), writeln(MaterialGroupId1),
  writeln(MaterialGroupSame1),

  % PUT
  put_material_group(MaterialGroupId3, ["Group 3"], _),
  put_material_group(MaterialGroupId2, MaterialGroupId1, "{\"name\" : \"name Group 2\"}", _),

  % GET ALL
  get_material_groups(MaterialGroupList),
  writeln('Return full material group list:'),
  writeln(MaterialGroupList),

  % DELETE
  delete_material_group(MaterialGroupId4),
  delete_material_group(MaterialGroupId3),
  delete_material_group(MaterialGroupId2),
  delete_material_group(MaterialGroupId1),
  get_material_groups(MaterialGroupListNew),
  writeln('Return empty material group list:'),
  writeln(MaterialGroupListNew).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            Product            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('product_test') :-
  % Post mockup data
  post_material_group(["GroupTest1"], MaterialGroup1),

  % POST ONE
  get_entity_id(MaterialGroup1, MaterialGroupId1),
  post_material_group(MaterialGroupId1, ["GroupTest2"], MaterialGroup2),
  get_entity_id(MaterialGroup2, MaterialGroupId2),
  ProductId1 = "id1",
  post_product(ProductId1, ["description 1", "name 1", "productType 1", "productUnit 1"], _),
  ProductId2 = "id2",
  post_product(ProductId2, 
    "{\"description\" : \"description 2\",
      \"name\" : \"name 2\",
      \"productType\" : \"productType 2\",
      \"productUnit\" : \"productUnit 2\"}", 
      _),
  ProductId3 = "id3",
  post_product(ProductId3, MaterialGroupId1, ["description 3", "name 3", "productType 3", "productUnit 3"], _),
  ProductId4 = "id4",
  post_product(ProductId4, MaterialGroupId2,
    "{\"description\" : \"description 4\",
      \"name\" : \"name 4\",
      \"productType\" : \"productType 4\",
      \"productUnit\" : \"productUnit 4\"}", 
      _),
  
  % POST MANY
  post_products(
    "{\"products\": 
      [
        {
          \"description\" : \"description 5\",
          \"id\" : \"id5\",
          \"name\" : \"name 5\",
          \"productType\" : \"productType 5\",
          \"productUnit\" : \"productUnit 5\"
        }
        ,{
          \"description\" : \"description 6\",
          \"id\" : \"id6\",
          \"name\" : \"name 6\",
          \"productType\" : \"productType 6\",
          \"productUnit\" : \"productUnit 6\"
        }
      ]}", 
    _),
  post_products([
    ["description 7", "id7", "name 7", "productType 7", "productUnit 7"],
    ["description 8", "id8", "name 8", "productType 8", "productUnit 8"]], 
    _),

  % GET ONE
  get_product("id1", ProductSame1),
  writeln('Return product with id id1:'),
  writeln(ProductSame1),

  % PUT
  put_product(ProductId1, MaterialGroupId1, ["description 1 changed", "name 1 changed", "productType 1 changed", "productUnit 1 changed"], _),
  put_product(ProductId2, MaterialGroupId1,
    "{\"description\" : \"description 2 changed\",
      \"name\" : \"name 2 changed\",
      \"productType\" : \"productType 2 changed\",
      \"productUnit\" : \"productUnit 2 changed\"}", 
      _),
  put_product(ProductId3,
    "{\"description\" : \"description 3 changed\",
      \"name\" : \"name 3 changed\",
      \"productType\" : \"productType 3 changed\",
      \"productUnit\" : \"productUnit 3 changed\"}", 
      _),
  put_product(ProductId4, ["description 4 changed", "name 4 changed", "productType 4 changed", "productUnit 4 changed"], _),
  
  % GET ALL
  get_products(ProductList),
  writeln('Return product list:'),
  writeln(ProductList),

  % DELETE
  delete_product(ProductId1),
  delete_product(ProductId2),
  delete_product(ProductId3),
  delete_product(ProductId4),
  delete_product("id5"),
  delete_product("id6"),
  delete_product("id7"),
  delete_product("id8"),
  get_products(ProductListNew),
  writeln('Return empty product list:'),
  writeln(ProductListNew),

  % Delete mockup data
  delete_material_group(MaterialGroupId2),
  delete_material_group(MaterialGroupId1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     ProductCharacteristic     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('product_characteristic_post_test') :-
  % POST
  post_product_characteristic("Characteristic 1", ProductCharacteristic),

  % GET ALL
  get_product_characteristics(ProductCharacteristicList),
  writeln('Return product characteristic list:'),
  writeln(ProductCharacteristicList),

  % DELETE
  get_entity_id(ProductCharacteristic, ProductCharacteristicId),
  delete_product_characteristic(ProductCharacteristicId),
  get_product_characteristics(ProductCharacteristicListNew),
  writeln('Return empty product characteristic list:'),
  writeln(ProductCharacteristicListNew).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %       ProductProperty        %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('product_property_test') :-
  % Post mockup data
  post_test_store(StoreId),
  post_test_product(ProductId),
  post_test_product_characteristics(ProductCharacteristicId1, ProductCharacteristicId2, ProductCharacteristicId3, ProductCharacteristicId4),

  % POST
  post_product_property(StoreId, ProductId, ProductCharacteristicId1, "value 1", _),
  post_product_property(StoreId, ProductId, ProductCharacteristicId2, "value 2", _),
  post_product_property(StoreId, ProductId, ProductCharacteristicId3, "value 3", _),
  post_product_property(StoreId, ProductId, ProductCharacteristicId4, "value 4", _),

  % GET ALL
  get_product_properties(StoreId, ProductId, ProductPropertyList),
  writeln('Return product property list:'),
  writeln(ProductPropertyList),

  % DELETE
  delete_product_property(StoreId, ProductId, ProductCharacteristicId1),
  delete_product_property(StoreId, ProductId, ProductCharacteristicId2),
  delete_product_property(StoreId, ProductId, ProductCharacteristicId3),
  delete_product_property(StoreId, ProductId, ProductCharacteristicId4),
  get_product_properties(StoreId, ProductId, ProductPropertyListNew),
  writeln('Return empty product property list:'),
  writeln(ProductPropertyListNew),

  % Delete mockup data
  delete_product_characteristic(ProductCharacteristicId1),
  delete_product_characteristic(ProductCharacteristicId2),
  delete_product_characteristic(ProductCharacteristicId3),
  delete_product_characteristic(ProductCharacteristicId4),
  delete_product(ProductId),
  delete_store(StoreId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        LogisticalUnit         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('logistical_unit_test') :-
  % Post mockup data
  post_test_product(ProductId),

  % POST
  post_logistical_unit("", ProductId, [true, true, "dimensionUnit 1", 1.1, 1.2, 1, 1.3, true, 1.4, "quantityUnit 1", "quantityUnitIso 1", true, 1.5, 1.6], LogisticalUnit1),
  get_entity_id(LogisticalUnit1, LogisticalUnitId1),
  post_logistical_unit(LogisticalUnitId1, ProductId, [false, false, "dimensionUnit 2", 2.1, 2.2, 2, 2.3, false, 2.4, "quantityUnit 2", "quantityUnitIso 2", false, 2.5, 2.6], LogisticalUnit2),
  get_entity_id(LogisticalUnit2, LogisticalUnitId2),
  post_logistical_unit("", ProductId, 
    "{\"basicUnit\" : true,
      \"deliveryExpanseUnit\" : false,
      \"dimensionUnit\" : \"dimensionUnit 3\",
      \"height\" : 3.1,
      \"length\" : 3.2,
      \"maxStackSize\" : 3,
      \"netWeight\" : 3.3,
      \"orderUnit\" : true,
      \"quantityOfPredecessors\" : 3.4,
      \"quantityUnit\" : \"quantityUnit 3\",
      \"quantityUnitIso\" : \"quantityUnitIso 3\",
      \"retailUnit\" : false,
      \"weightUnit\" : 3.6,
      \"width\" : 3.6}", 
      LogisticalUnit3),
  get_entity_id(LogisticalUnit3, LogisticalUnitId3),
  post_logistical_unit(LogisticalUnitId3, ProductId, 
    "{\"basicUnit\" : false,
      \"deliveryExpanseUnit\" : true,
      \"dimensionUnit\" : \"dimensionUnit 4\",
      \"height\" : 4.1,
      \"length\" : 4.2,
      \"maxStackSize\" : 4,
      \"netWeight\" : 4.3,
      \"orderUnit\" : false,
      \"quantityOfPredecessors\" : 4.4,
      \"quantityUnit\" : \"quantityUnit 4\",
      \"quantityUnitIso\" : \"quantityUnitIso 4\",
      \"retailUnit\" : true,
      \"weightUnit\" : 4.6,
      \"width\" : 4.6
      }", 
      LogisticalUnit4),
  get_entity_id(LogisticalUnit4, LogisticalUnitId4),

  % GET ONE
  get_logistical_unit(LogisticalUnitId1, LogisticalUnitSame1),
  write('Return logistical unit at Id '), writeln(LogisticalUnitId1),
  writeln(LogisticalUnitSame1),

  % PUT
  put_logistical_unit(LogisticalUnitId4, "", ProductId, [true, true, "dimensionUnit 4 changed", 4.1, 4.2, 4, 4.3, true, 4.4, "quantityUnit 4 changed", "quantityUnitIso 4 changed", true, 4.5, 4.6], _),
  put_logistical_unit(LogisticalUnitId3, LogisticalUnitId2, ProductId, 
    "{\"basicUnit\" : true,
      \"deliveryExpanseUnit\" : false,
      \"dimensionUnit\" : \"dimensionUnit 3\",
      \"height\" : 3.1,
      \"length\" : 3.2,
      \"maxStackSize\" : 3,
      \"netWeight\" : 3.3,
      \"orderUnit\" : true,
      \"quantityOfPredecessors\" : 3.4,
      \"quantityUnit\" : \"quantityUnit 3\",
      \"quantityUnitIso\" : \"quantityUnitIso 3\",
      \"retailUnit\" : false,
      \"weightUnit\" : 3.6,
      \"width\" : 3.6}",
      _),
  put_logistical_unit(LogisticalUnitId2, LogisticalUnitId1, ProductId, [true, true, "dimensionUnit 2 changed", 2.1, 2.2, 2, 2.3, true, 2.4, "quantityUnit 2 changed", "quantityUnitIso 2 changed", true, 2.5, 2.6], _),
  put_logistical_unit(LogisticalUnitId1,
    "{\"basicUnit\" : true,
      \"deliveryExpanseUnit\" : false,
      \"dimensionUnit\" : \"dimensionUnit 1 changed\",
      \"height\" : 1.1,
      \"productId\" : \"idTest\",
      \"length\" : 1.2,
      \"maxStackSize\" : 1,
      \"netWeight\" : 1.3,
      \"orderUnit\" : true,
      \"quantityOfPredecessors\" : 1.4,
      \"quantityUnit\" : \"quantityUnit 1 changed\",
      \"quantityUnitIso\" : \"quantityUnitIso 1 changed\",
      \"retailUnit\" : false,
      \"weightUnit\" : 1.6,
      \"width\" : 1.6}", 
      _),

  % GET ALL
  get_logistical_units(LogisticalUnitList),
  writeln('Return logistical unit list:'),
  writeln(LogisticalUnitList),

  % DELETE
  delete_logistical_unit(LogisticalUnitId4),
  delete_logistical_unit(LogisticalUnitId3),
  delete_logistical_unit(LogisticalUnitId2),
  delete_logistical_unit(LogisticalUnitId1),
  get_logistical_units(LogisticalUnitListNew),
  writeln('Return empty logistical unit list:'),
  writeln(LogisticalUnitListNew),

  % Delete mockup data
  delete_product(ProductId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         Product gtin          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('product_gtin_test') :-
  % Post mockup data
  post_test_logistical_unit(ProductId1, ProductId2, LogisticalUnitId1, LogisticalUnitId2),

  % POST
  ProductGtinId1 = "id1",
  ProductGtinId2 = "id2",
  ProductGtinId3 = "id3",
  ProductGtinId4 = "id4",
  post_product_gtin(ProductGtinId1, LogisticalUnitId1, ProductId1, ["gtinType1", true], _),
  post_product_gtin(ProductGtinId2, LogisticalUnitId1, ProductId2, ["gtinType2", false], _),
  post_product_gtin(ProductGtinId3, LogisticalUnitId2, ProductId1, 
    "{\"gtinType\" : \"gtinType 3\",
      \"mainGtin\" : true}", 
    _),
  post_product_gtin(ProductGtinId4, LogisticalUnitId2, ProductId2, 
    "{\"gtinType\" : \"gtinType 4\",
      \"mainGtin\" : false}", 
    _),

  % GET ONE
  get_product_gtin(ProductGtinId1, ProductGtinSame1),
  write('Return product gtin at Id '), writeln(ProductGtinId1),
  writeln(ProductGtinSame1),

  % PUT
  put_product_gtin(ProductGtinId1, LogisticalUnitId1, ProductId1, ["gtinType 1 changed", false], _),
  put_product_gtin(ProductGtinId2, LogisticalUnitId1, ProductId2, ["gtinType 2 changed", true], _),
  put_product_gtin(ProductGtinId3, LogisticalUnitId2, ProductId1, 
    "{\"gtinType\" : \"gtinType 3 changed\",
      \"mainGtin\" : false}", 
    _),
  put_product_gtin(ProductGtinId4, LogisticalUnitId2, ProductId2, 
    "{\"gtinType\" : \"gtinType 4 changed\",
      \"mainGtin\" : true}", 
    _),

  % GET ALL
  get_product_gtins(ProductGtinList),
  writeln('Return product gtin list'),
  writeln(ProductGtinList),

  % DELETE
  delete_product_gtin(ProductGtinId1),
  delete_product_gtin(ProductGtinId2),
  delete_product_gtin(ProductGtinId3),
  delete_product_gtin(ProductGtinId4),
  get_product_gtins(ProductGtinListNew),
  writeln('Return empty product gtin list'),
  writeln(ProductGtinListNew),

  % Delete mockup data
  delete_logistical_unit(LogisticalUnitId1),
  delete_logistical_unit(LogisticalUnitId2),
  delete_product(ProductId1),
  delete_product(ProductId2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         Product group         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('product_group_test') :-
  % Post mockup data
  post_test_store(StoreId),
  post_test_products(ProductId1, ProductId2, ProductId3, ProductId4, ProductId5, ProductId6),

  % POST
  post_product_group(StoreId, "{\"name\" : \"name 1\"}", ProductGroup1),
  post_product_group(StoreId, "{\"name\" : \"name 2\"}", ProductGroup2),
  get_entity_id(ProductGroup1, ProductGroupId1),
  get_entity_id(ProductGroup2, ProductGroupId2),
  post_product_to_product_group(ProductGroupId1, ProductId1, _),
  post_product_to_product_group(ProductGroupId1, ProductId2, _),
  post_product_to_product_group(ProductGroupId2, ProductId3, _),
  post_product_to_product_group(ProductGroupId2, ProductId4, _),

  % GET ONE
  get_product_group(ProductGroupId1, ProductGroupSame1),
  writeln('Return product group 1:'),
  writeln(ProductGroupSame1),

  % GET ALL
  get_product_groups(StoreId, ProductGroupList),
  writeln('Return product group list:'),
  writeln(ProductGroupList),

  % DELETE
  delete_product_from_product_group(ProductGroupId1, ProductId1),
  delete_product_from_product_group(ProductGroupId1, ProductId2),
  get_product_group(ProductGroupId1, ProductGroupNew1),
  writeln('Return empty product group 1:'),
  writeln(ProductGroupNew1),
  delete_product_group(ProductGroupId2),
  delete_product_group(ProductGroupId1),
  get_product_groups(StoreId, ProductGroupListNew),
  writeln('Return empty product group list:'),
  writeln(ProductGroupListNew),

  % Delete mockup data
  delete_product(ProductId6),
  delete_product(ProductId5),
  delete_product(ProductId4),
  delete_product(ProductId3),
  delete_product(ProductId2),
  delete_product(ProductId1),
  delete_store(StoreId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             Shelf             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('shelf_test') :-
  % Post mockup data
  post_test_product_group(StoreId, ProductGroupId),

  % POST
  post_shelf(StoreId, ProductGroupId,
    ["cadPlanId 1", 10, "externalReferenceId 1", 11, 12.12, 13.13, 14.14, 15.15, 16.16, 17.17, 18.18, 19], Shelf1),
  post_shelf(StoreId, ProductGroupId,
    ["cadPlanId 2", 20, "externalReferenceId 2", 21, 22.22, 23.23, 24.24, 25.25, 26.26, 27.27, 28.28, 29], Shelf2),
  post_shelf(StoreId, ProductGroupId,
    ["cadPlanId 3", 30, "externalReferenceId 3", 31, 32.32, 33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39], Shelf3),
  post_shelf(StoreId, ProductGroupId,
    ["cadPlanId 4", 40, "externalReferenceId 4", 41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48, 49], Shelf4),
  get_entity_id(Shelf1, ShelfId1),
  get_entity_id(Shelf2, ShelfId2),
  get_entity_id(Shelf3, ShelfId3),
  get_entity_id(Shelf4, ShelfId4),

  % GET ONE
  get_shelf(ShelfId1, ShelfSame1),
  write('Return shelf with id '), writeln(ShelfId1),
  writeln(ShelfSame1),

  % PUT
  put_shelf(ShelfId3, StoreId, ProductGroupId,
    "{\"cadPlanId\" : \"an_new_cadPlanId\",
      \"depth\" : 32,
      \"externalReferenceId\" : \"R4\",
      \"height\" : 12,
      \"orientationW\" : 23,
      \"orientationX\" : 32,
      \"orientationY\" : 312,
      \"orientationZ\" : 32,
      \"positionX\" : 432,
      \"positionY\" : 421,
      \"positionZ\" : 14,
      \"width\" : 21}",
      _),
  put_shelf(ShelfId4, StoreId, ProductGroupId,
    ["cadPlanId changed", 40, "externalReferenceId 1", 41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48, 49], _),

  % GET ALL
  get_shelves(StoreId, ShelfList),
  writeln('Return shelf list:'),
  writeln(ShelfList),

  % DELETE
  delete_shelf(ShelfId1),
  delete_shelf(ShelfId2),
  delete_shelf(ShelfId3),
  delete_shelf(ShelfId4),
  get_shelves(StoreId, ShelfListNew),
  writeln('Return empty shelf list:'),
  writeln(ShelfListNew),

  % Delete mockup data
  delete_product_group(ProductGroupId),
  delete_store(StoreId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Shelf layer          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('shelf_layer_test') :-
  % Post mockup data
  post_test_shelf(StoreId, ProductGroupId, ShelfId),

  % POST
  post_shelf_layer(ShelfId, [10, "externalReferenceId 1", 11, 12, 13.13, "type 1", 14], ShelfLayer1),
  post_shelf_layer(ShelfId, [20, "externalReferenceId 2", 21, 22, 23.23, "type 2", 24], ShelfLayer2),
  post_shelf_layer(ShelfId, [30, "externalReferenceId 3", 31, 32, 33.33, "type 3", 34], ShelfLayer3),
  post_shelf_layer(ShelfId, [40, "externalReferenceId 4", 41, 42, 43.43, "type 4", 44], ShelfLayer4),
  get_entity_id(ShelfLayer1, ShelfLayerId1),
  get_entity_id(ShelfLayer2, ShelfLayerId2),
  get_entity_id(ShelfLayer3, ShelfLayerId3),
  get_entity_id(ShelfLayer4, ShelfLayerId4),

  % GET ONE
  get_shelf_layer(ShelfLayerId1, ShelfLayerSame1),
  write('Return shelf layer with id '), writeln(ShelfLayerId1),
  writeln(ShelfLayerSame1),

  % GET ALL
  get_shelf_layers(ShelfId, ShelfLayerList),
  writeln('Return shelf layer list:'),
  writeln(ShelfLayerList),

  % DELETE
  delete_shelf_layer(ShelfLayerId1),
  delete_shelf_layer(ShelfLayerId2),
  delete_shelf_layer(ShelfLayerId3),
  delete_shelf_layer(ShelfLayerId4),
  get_shelf_layers(ShelfId, ShelfLayerListNew),
  writeln('Return empty shelf layer list:'),
  writeln(ShelfLayerListNew),

  % Delete mockup data
  delete_shelf(ShelfId),
  delete_product_group(ProductGroupId),
  delete_store(StoreId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             Facing            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('facing_test') :-
  % Post mockup data
  post_test_shelf_layer(StoreId, ProductGroupId, ShelfId, ShelfLayerId),

  % POST
  post_facing(ShelfLayerId, [10, 11, 12], Facing1),
  post_facing(ShelfLayerId, [20, 21, 22], Facing2),
  post_facing(ShelfLayerId, [30, 31, 32], Facing3),
  post_facing(ShelfLayerId, [40, 41, 42], Facing4),
  get_entity_id(Facing1, FacingId1),
  get_entity_id(Facing2, FacingId2),
  get_entity_id(Facing3, FacingId3),
  get_entity_id(Facing4, FacingId4),
  
  % GET ONE
  get_facing(FacingId1, FacingSame1),
  write('Return facing with id '), writeln(FacingId1),
  writeln(FacingSame1),

  % PUT
  put_facing(FacingId3, ShelfLayerId,
    "{\"layerRelativePosition\" : 300,
      \"noOfItemsDepth\" : 310,
      \"noOfItemsWidth\" : 320}",
      _),
  put_facing(FacingId4, ShelfLayerId,
    [400, 410, 420], _),

  % GET ALL
  get_facings(ShelfLayerId, FacingList),
  writeln('Return facing list:'),
  writeln(FacingList),

  % DELETE
  delete_facing(FacingId1),
  delete_facing(FacingId2),
  delete_facing(FacingId3),
  delete_facing(FacingId4),
  get_facings(ShelfLayerId, FacingListNew),
  writeln('Return empty facing list:'),
  writeln(FacingListNew),

  % Delete mockup data
  delete_shelf_layer(ShelfLayerId),
  delete_shelf(ShelfId),
  delete_product_group(ProductGroupId),
  delete_store(StoreId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%           ItemGroup           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('item_group_test') :-
  % Post mockup data
  post_test_logistical_unit(ProductId1, ProductId2, LogisticalUnitId1, LogisticalUnitId2),
  post_test_facings(StoreId, ProductGroupId, ShelfId, ShelfLayerId, FacingId1, FacingId2),

  % POST
  post_item_group(FacingId1, LogisticalUnitId1, 10, ItemGroup1),
  post_item_group(FacingId1, LogisticalUnitId2, 20, ItemGroup2),
  post_item_group(FacingId2, LogisticalUnitId1, 30, ItemGroup3),
  post_item_group(FacingId2, LogisticalUnitId2, 40, ItemGroup4),
  get_entity_id(ItemGroup1, ItemGroupId1),
  get_entity_id(ItemGroup2, ItemGroupId2),
  get_entity_id(ItemGroup3, ItemGroupId3),
  get_entity_id(ItemGroup4, ItemGroupId4),
  
  % GET ONE
  get_item_group(ItemGroupId1, ItemGroupSame1),
  write('Return item group with id '), writeln(ItemGroupId1),
  writeln(ItemGroupSame1),

  % PUT
  put_item_group(ItemGroupId3, FacingId1, LogisticalUnitId1, 33, _),
  put_item_group(ItemGroupId4, FacingId1, LogisticalUnitId2, 45, _),

  % GET ALL
  get_item_groups(ItemGroupList),
  writeln('Return item group list:'),
  writeln(ItemGroupList),

  % DELETE
  delete_item_group(ItemGroupId1),
  delete_item_group(ItemGroupId2),
  delete_item_group(ItemGroupId3),
  delete_item_group(ItemGroupId4),
  get_item_groups(ItemGroupListNew),
  writeln('Return empty item group list:'),
  writeln(ItemGroupListNew),

  % Delete mockup data
  delete_facing(FacingId2),
  delete_facing(FacingId1),
  delete_shelf_layer(ShelfLayerId),
  delete_shelf(ShelfId),
  delete_product_group(ProductGroupId),
  delete_logistical_unit(LogisticalUnitId2),
  delete_logistical_unit(LogisticalUnitId1),
  delete_product(ProductId2),
  delete_product(ProductId1),
  delete_store(StoreId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             Item              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('item_test') :-
  % Post mockup data
  post_test_item_groups(StoreId, 
    ProductId1, 
    ProductId2, 
    LogisticalUnitId1, 
    LogisticalUnitId2, 
    ProductGroupId, 
    ShelfId, 
    ShelfLayerId, 
    FacingId1, 
    FacingId2, 
    ItemGroupId1, 
    ItemGroupId2),

  % POST
  post_item(ItemGroupId1, [10, 11, 12], Item1),
  post_item(ItemGroupId1, [20, 21, 22], Item2),
  post_item(ItemGroupId2, 
    "{\"positionInFacingX\" : 30,
      \"positionInFacingY\" : 31,
      \"positionInFacingZ\" : 32}", 
      Item3),
  post_item(ItemGroupId2, 
    "{\"positionInFacingX\" : 40,
      \"positionInFacingY\" : 41,
      \"positionInFacingZ\" : 42}", 
      Item4),
  get_entity_id(Item1, ItemId1),
  get_entity_id(Item2, ItemId2),
  get_entity_id(Item3, ItemId3),
  get_entity_id(Item4, ItemId4),
  
  % GET ONE
  get_item(ItemId1, ItemSame1),
  write('Return item with id '), writeln(ItemId1),
  writeln(ItemSame1),

  % PUT
  put_item(ItemId3, ItemGroupId1, [300, 310, 320], _),
  put_item(ItemId4, ItemGroupId1, 
    "{\"positionInFacingX\" : 400,
      \"positionInFacingY\" : 410,
      \"positionInFacingZ\" : 420}", 
      _),

  % GET ALL
  get_items(ItemList),
  writeln('Return item list:'),
  writeln(ItemList),

  % DELETE
  delete_item(ItemId4),
  delete_item(ItemId3),
  delete_item(ItemId2),
  delete_item(ItemId1),
  get_items(ItemListNew),
  writeln('Return empty item list:'),
  writeln(ItemListNew),

  % Delete mockup data
  delete_item_group(ItemGroupId2),
  delete_item_group(ItemGroupId1),
  delete_facing(FacingId2),
  delete_facing(FacingId1),
  delete_shelf_layer(ShelfLayerId),
  delete_shelf(ShelfId),
  delete_product_group(ProductGroupId),
  delete_logistical_unit(LogisticalUnitId2),
  delete_logistical_unit(LogisticalUnitId1),
  delete_product(ProductId2),
  delete_product(ProductId1),
  delete_store(StoreId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Planogram            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('planogram_test') :-
  % Post mockup data
  post_test_logistical_unit(ProductId1, ProductId2, LogisticalUnitId1, LogisticalUnitId2),
  post_test_shelf_layer(StoreId, ProductGroupId, ShelfId, ShelfLayerId),

  % POST
  post_planogram(LogisticalUnitId1, ShelfLayerId, [10, 11.11, 12, 13], Planogram1),
  post_planogram(LogisticalUnitId2, ShelfLayerId, [20, 21.21, 22, 23], Planogram2),
  post_planogram(LogisticalUnitId1, ShelfLayerId, 
    "{\"numberOfFacings\" : 30,
      \"orientationYaw\" : 31.31,
      \"positionX\" : 32,
      \"versionTimestamp\" : 33}", 
      Planogram3),
  post_planogram(LogisticalUnitId2, ShelfLayerId, 
    "{\"numberOfFacings\" : 40,
      \"orientationYaw\" : 41.41,
      \"positionX\" : 42,
      \"versionTimestamp\" : 43}", 
      Planogram4),
  get_entity_id(Planogram1, PlanogramId1),
  get_entity_id(Planogram2, PlanogramId2),
  get_entity_id(Planogram3, PlanogramId3),
  get_entity_id(Planogram4, PlanogramId4),

  % PUT
  put_planogram(PlanogramId3, LogisticalUnitId2, ShelfLayerId, [300, 310.310, 320, 330], _),
  put_planogram(PlanogramId4, LogisticalUnitId1, ShelfLayerId, 
    "{\"numberOfFacings\" : 400,
      \"orientationYaw\" : 410.410,
      \"positionX\" : 420,
      \"versionTimestamp\" : 430}", 
      _),

  % GET ALL
  get_planograms(PlanogramList),
  writeln('Return planogram list:'),
  writeln(PlanogramList),

  % DELETE
  delete_planogram(PlanogramId4),
  delete_planogram(PlanogramId3),
  delete_planogram(PlanogramId2),
  delete_planogram(PlanogramId1),
  get_planograms(PlanogramListNew),
  writeln('Return empty planogram list:'),
  writeln(PlanogramListNew),

  % Delete mockup data
  delete_shelf_layer(ShelfLayerId),
  delete_shelf(ShelfId),
  delete_product_group(ProductGroupId),
  delete_logistical_unit(LogisticalUnitId2),
  delete_logistical_unit(LogisticalUnitId1),
  delete_product(ProductId2),
  delete_product(ProductId1),
  delete_store(StoreId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    ShoppingBasketPosition     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('shopping_basket_test') :-
  % Post mockup data
  post_test_store(StoreId),
  post_test_customer(CustomerId1, CustomerId2),
  post_test_product(ProductId1),
  post_test_product2(ProductId2),
  
  % POST
  post_shopping_basket_position(StoreId, CustomerId1, ["currency 1", "idTest", 10, 11.11], _),
  post_shopping_basket_position(StoreId, CustomerId1, ProductId2, ["currency 2", 20, 21.21], _),
  post_shopping_basket_position(StoreId, CustomerId2, 
    "{\"currency\" : 30,
      \"productId\" : \"idTest2\",
      \"quantity\" : 32,
      \"sellingPrice\" : 33}", 
      ShoppingBasketPosition3),
  post_shopping_basket_position(StoreId, CustomerId2, ProductId2,
    "{\"currency\" : 40,
      \"quantity\" : 42,
      \"sellingPrice\" : 43}", 
      ShoppingBasketPosition4),
  get_entity_id(ShoppingBasketPosition3, ShoppingBasketPositionId3),
  get_entity_id(ShoppingBasketPosition4, ShoppingBasketPositionId4),

  % GET ALL
  get_shopping_basket_positions(StoreId, CustomerId1, ShoppingBasketPositionList),
  writeln('Return shopping basket position list:'),
  writeln(ShoppingBasketPositionList),

  % DELETE
  delete_shopping_basket_position(ShoppingBasketPositionId4),
  delete_shopping_basket_position(ShoppingBasketPositionId3),
  delete_shopping_basket_positions(StoreId, CustomerId1),

  % Delete mockup data
  delete_product(ProductId1),
  delete_product(ProductId2),
  delete_customer(CustomerId1),
  delete_customer(CustomerId2),
  delete_store(StoreId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%           Delivery            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('delivery_test') :-
  % Post mockup data
  post_test_store(StoreId),
  post_test_logistical_unit(ProductId1, ProductId2, LogisticalUnitId1, LogisticalUnitId2),
  
  % POST
  post_delivery(LogisticalUnitId1, StoreId, [10, "handlingUnit 1", "orderUnit 1", 11, "source 1"], Delivery1),
  post_delivery(LogisticalUnitId1, StoreId, [20, "handlingUnit 2", "orderUnit 1", 21, "source 2"], Delivery2),
  post_delivery(LogisticalUnitId2, StoreId, 
    "{\"amount\" : 30,
      \"handlingUnit\" : \"handlingUnit 3\",
      \"orderUnit\" : \"orderUnit 3\",
      \"plannedDelivery\" : 31,
      \"source\" : \"source 3\"}", 
      Delivery3),
  post_delivery(LogisticalUnitId2, StoreId, 
    "{\"amount\" : 40,
      \"handlingUnit\" : \"handlingUnit 4\",
      \"orderUnit\" : \"orderUnit 4\",
      \"plannedDelivery\" : 41,
      \"source\" : \"source 4\"}", 
      Delivery4),
  get_entity_id(Delivery1, DeliveryId1),
  get_entity_id(Delivery2, DeliveryId2),
  get_entity_id(Delivery3, DeliveryId3),
  get_entity_id(Delivery4, DeliveryId4),

  % GET ALL
  get_deliveries(DeliveryList),
  writeln('Return delivery list:'),
  writeln(DeliveryList),

  % DELETE
  delete_delivery(DeliveryId4),
  delete_delivery(DeliveryId3),
  delete_delivery(DeliveryId2),
  delete_delivery(DeliveryId1),
  get_deliveries(DeliveryListNew),
  writeln('Return empty delivery list:'),
  writeln(DeliveryListNew),

  % Delete mockup data
  delete_logistical_unit(LogisticalUnitId2),
  delete_logistical_unit(LogisticalUnitId1),
  delete_product(ProductId2),
  delete_product(ProductId1),
  delete_store(StoreId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            Device             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('device_test') :-
  % Post mockup data
  post_test_store(StoreId),
  
  % POST
  DeviceId1 = "id 1",
  DeviceId2 = "id 2",
  DeviceId3 = "id 3",
  DeviceId4 = "id 4",
  post_device(DeviceId1, StoreId, ["description 1", "deviceType 1"], _),
  post_device(DeviceId2, StoreId, ["description 2", "deviceType 1"], _),
  post_device(DeviceId3, StoreId, 
    "{\"description\" : \"description 3\",
      \"deviceType\" : \"deviceType 3\"}", 
      _),
  post_device(DeviceId4, StoreId, 
    "{\"description\" : \"description 4\",
      \"deviceType\" : \"deviceType 4\"}", 
      _),

  % GET ALL
  get_devices(DeviceList),
  writeln('Return device list:'),
  writeln(DeviceList),

  % DELETE
  delete_device(DeviceId4),
  delete_device(DeviceId3),
  delete_device(DeviceId2),
  delete_device(DeviceId1),
  get_devices(DeviceListNew),
  writeln('Return empty device list:'),
  writeln(DeviceListNew),

  % Delete mockup data
  delete_store(StoreId).

%%%%%%%%%%%%%% Kaviya %%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
