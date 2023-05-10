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
  post_product("idTest", "", ["description test", "name test", "productType test", "productUnit test"], _).
post_test_product2("idTest2") :-
  post_product("idTest2", "", ["description test2", "name test2", "productType test2", "productUnit test2"], _).
post_test_products("idTest1", "idTest2", "idTest3", "idTest4", "idTest5", "idTest6") :-
  post_product("idTest1", "", ["description test1", "name test1", "productType test1", "productUnit test1"], _),
  post_product("idTest2", "", ["description test2", "name test2", "productType test2", "productUnit test2"], _),
  post_product("idTest3", "", ["description test3", "name test3", "productType test3", "productUnit test3"], _),
  post_product("idTest4", "", ["description test4", "name test4", "productType test4", "productUnit test4"], _),
  post_product("idTest5", "", ["description test5", "name test5", "productType test5", "productUnit test5"], _),
  post_product("idTest6", "", ["description test6", "name test6", "productType test6", "productUnit test6"], _).

post_test_product_unit(ProductId1, ProductId2, ProductUnitId1, ProductUnitId2) :-
  post_test_product(ProductId1),
  post_test_product2(ProductId2),
  post_product_unit(ProductId1, [11, "dimensionUnit 1", 1.1, 1.2, 1.3, 12, 1.4, 13, "unitCode 1", 1.5, "volumeUnit 1", "weightUnit 1", 1.6], ProductUnit1),
  get_entity_id(ProductUnit1, ProductUnitId1),
  post_product_unit(ProductId2, [21, "dimensionUnit 2", 2.1, 2.2, 2.3, 22, 2.4, 23, "unitCode 2", 2.5, "volumeUnit 2", "weightUnit 2", 2.6], ProductUnit2),
  get_entity_id(ProductUnit2, ProductUnitId2).

post_test_product_characteristics(ProductCharacteristicId1, ProductCharacteristicId2, ProductCharacteristicId3, ProductCharacteristicId4) :-
  post_product_characteristic("", "name 1", ProductCharacteristic1),
  get_entity_id(ProductCharacteristic1, ProductCharacteristicId1),
  post_product_characteristic("", "name 2", ProductCharacteristic2),
  get_entity_id(ProductCharacteristic2, ProductCharacteristicId2),
  post_product_characteristic("", "name 3", ProductCharacteristic3),
  get_entity_id(ProductCharacteristic3, ProductCharacteristicId3),
  post_product_characteristic("", "name 4", ProductCharacteristic4),
  get_entity_id(ProductCharacteristic4, ProductCharacteristicId4).

post_test_product_group(StoreId, ProductGroupId) :-
  post_test_store(StoreId),
  post_product_group("name 1", StoreId, "", ProductGroup),
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

post_test_item_groups(StoreId, ProductId1, ProductId2, ProductUnitId1, ProductUnitId2, ProductGroupId, ShelfId, ShelfLayerId, FacingId1, FacingId2, ItemGroupId1, ItemGroupId2) :-
  post_test_product_unit(ProductId1, ProductId2, ProductUnitId1, ProductUnitId2),
  post_test_facings(StoreId, ProductGroupId, ShelfId, ShelfLayerId, FacingId1, FacingId2),
  post_item_group(FacingId1, ProductUnitId1, 10, ItemGroup1),
  get_entity_id(ItemGroup1, ItemGroupId1),
  post_item_group(FacingId2, ProductUnitId2, 20, ItemGroup2),
  get_entity_id(ItemGroup2, ItemGroupId2).

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
% %     StoreObject     %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('store_object_test') :-
%   % Post mockup data
%   post_test_store(StoreId),

%   % POST
%   post_store_object(StoreId, [10, "description 1", 11, "2021-07-14T07:09:31.038Z", 12.12, 13.13, 14.14, 15.15, 16.16, 17.17, 18.18, "type 1", 19], StoreObject1),
%   get_entity_id(StoreObject1, StoreObjectId1),
%   post_store_object(StoreId, [20, "description 2", 21, "2012-08-11T07:12:42.038Z", 22.22, 23.23, 24.24, 25.25, 26.26, 27.27, 28.28, "type 2", 29], StoreObject2),
%   get_entity_id(StoreObject2, StoreObjectId2),
%   post_store_object(StoreId,
%     "{\"depth\" : 30,
%       \"description\" : \"description 3\",
%       \"height\" : 31,
%       \"locationTimestamp\" : \"2002-02-11T07:12:42.038Z\",
%       \"orientationW\" : 32.32,
%       \"orientationX\" : 33.33,
%       \"orientationY\" : 34.34,
%       \"orientationZ\" : 35.35,
%       \"positionX\" : 36.36,
%       \"positionY\" : 37.37,
%       \"positionZ\" : 38.38,
%       \"type\" : \"type 3\",
%       \"width\" : 39",
%     StoreObject3),
%   get_entity_id(StoreObject3, StoreObjectId3),
%   post_store_object(StoreId,
%     "{\"depth\" : 40,
%       \"description\" : \"description 4\",
%       \"height\" : 41,
%       \"locationTimestamp\" : \"2008-05-12T01:12:42.038Z\",
%       \"orientationW\" : 42.42,
%       \"orientationX\" : 43.43,
%       \"orientationY\" : 44.44,
%       \"orientationZ\" : 45.45,
%       \"positionX\" : 46.46,
%       \"positionY\" : 47.47,
%       \"positionZ\" : 48.48,
%       \"type\" : \"type 4\",
%       \"width\" : 49",
%     StoreObject4),
%   get_entity_id(StoreObject4, StoreObjectId4),

%   % GET ALL
%   get_store_objects(StoreId, StoreObjectList),
%   writeln('Return store object list:'),
%   writeln(StoreObjectList),

%   % DELETE
%   delete_store_object(StoreObjectId4),
%   delete_store_object(StoreObjectId3),
%   delete_store_object(StoreObjectId2),
%   delete_store_object(StoreObjectId1),
%   get_store_objects(StoreId, StoreObjectListNew),
%   writeln('Return empty store object list:'),
%   writeln(StoreObjectListNew),

%   % Delete mockup data
%   delete_store(StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %     StoreCharacteristic     %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('store_characteristic_test') :-
%   % POST
%   post_store_characteristic("Characteristic 1", StoreCharacteristic),

%   % GET ALL
%   get_store_characteristics(StoreCharacteristicList),
%   writeln('Return store characteristic list:'),
%   writeln(StoreCharacteristicList),

%   % DELETE
%   get_entity_id(StoreCharacteristic, StoreCharacteristicId),
%   delete_store_characteristic(StoreCharacteristicId),
%   get_store_characteristics(StoreCharacteristicListNew),
%   writeln('Return empty store characteristic list:'),
%   writeln(StoreCharacteristicListNew).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %       StoreProperty        %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('store_property_test') :-
%   % Post mockup data
%   post_test_store(StoreId),
%   post_test_store_characteristics(StoreCharacteristicId1, StoreCharacteristicId2, StoreCharacteristicId3, StoreCharacteristicId4),

%   % POST
%   post_store_property(StoreId, StoreCharacteristicId1, "value 1", "", _),
%   post_store_property(StoreId, StoreCharacteristicId2, "value 2", "", _),
%   post_store_property(StoreId, StoreCharacteristicId3, "value low 3", "value high 3", _),
%   post_store_property(StoreId, StoreCharacteristicId4, "value low 4", "value high 4", _),

%   % GET ALL
%   get_store_properties(StoreId, StorePropertyList),
%   writeln('Return store property list:'),
%   writeln(StorePropertyList),

%   % DELETE
%   delete_store_property(StoreId, StoreCharacteristicId1),
%   delete_store_property(StoreId, StoreCharacteristicId2),
%   delete_store_property(StoreId, StoreCharacteristicId3),
%   delete_store_property(StoreId, StoreCharacteristicId4),
%   get_store_properties(StoreId, StorePropertyListNew),
%   writeln('Return empty store property list:'),
%   writeln(StorePropertyListNew),

%   % Delete mockup data
%   delete_store_characteristic(StoreCharacteristicId1),
%   delete_store_characteristic(StoreCharacteristicId2),
%   delete_store_characteristic(StoreCharacteristicId3),
%   delete_store_characteristic(StoreCharacteristicId4),
%   delete_store(StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %         MaterialGroup         %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('material_group_test') :-
%   % POST
%   post_material_group("", ["description 1", 1, "name 1"], MaterialGroup1),
%   get_entity_id(MaterialGroup1, MaterialGroupId1),
%   post_material_group("", 
%     "{\"description\" : \"description 2\",
%       \"hierarchyLevel\" : 2,
%       \"name\" : \"name 2\"}", 
%     MaterialGroup2),
%   get_entity_id(MaterialGroup2, MaterialGroupId2),
%   post_material_group(MaterialGroupId1, ["", "", "name 3"], MaterialGroup3),
%   post_material_group(MaterialGroupId2, 
%     "{\"description\" : \"description 4\",
%       \"hierarchyLevel\" : 4,
%       \"name\" : \"name 4\"}", 
%     MaterialGroup4),

%   % GET ONE
%   get_entity_id(MaterialGroup3, MaterialGroupId3),
%   get_entity_id(MaterialGroup4, MaterialGroupId4),
%   get_material_group(MaterialGroupId1, MaterialGroupSame1),
%   write('Return material group at Id: '), writeln(MaterialGroupId1),
%   writeln(MaterialGroupSame1),

%   % PUT
%   put_material_group(MaterialGroupId3, "", ["description 3 changed", 30, "name 3 changed"], _),
%   put_material_group(MaterialGroupId2, MaterialGroupId1, 
%     "{\"description\" : \"description 2 changed\",
%       \"hierarchyLevel\" : 20,
%       \"name\" : \"name 2 changed\"}", 
%     _),

%   % GET ALL
%   get_material_groups(MaterialGroupList),
%   writeln('Return material group list:'),
%   writeln(MaterialGroupList),

%   % DELETE
%   delete_material_group(MaterialGroupId4),
%   delete_material_group(MaterialGroupId3),
%   delete_material_group(MaterialGroupId2),
%   delete_material_group(MaterialGroupId1),
%   get_material_groups(MaterialGroupListNew),
%   writeln('Return empty material group list:'),
%   writeln(MaterialGroupListNew).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %            Product            %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('product_test') :-
%   % Post mockup data
%   post_material_group("", ["", "", "GroupTest1"], MaterialGroup1),
%   get_entity_id(MaterialGroup1, MaterialGroupId1),
%   post_material_group(MaterialGroupId1, ["", "", "GroupTest2"], MaterialGroup2),
%   get_entity_id(MaterialGroup2, MaterialGroupId2),

%   % POST ONE
%   ProductId1 = "id1",
%   post_product(ProductId1, "", ["description 1", "name 1", "productType 1", "productUnit 1"], _),
%   ProductId2 = "id2",
%   post_product(ProductId2, "",
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
%   put_product(ProductId3, "",
%     "{\"description\" : \"description 3 changed\",
%       \"name\" : \"name 3 changed\",
%       \"productType\" : \"productType 3 changed\",
%       \"productUnit\" : \"productUnit 3 changed\"}", 
%       _),
%   put_product(ProductId4, "", ["description 4 changed", "name 4 changed", "productType 4 changed", "productUnit 4 changed"], _),
  
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
%   get_products(ProductListNew),
%   writeln('Return empty product list:'),
%   writeln(ProductListNew),

%   % Delete mockup data
%   delete_material_group(MaterialGroupId2),
%   delete_material_group(MaterialGroupId1).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %     ProductCharacteristic     %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('product_characteristic_post_test') :-
%   % POST
%   post_product_characteristic("", "name 1", ProductCharacteristic1),
%   get_entity_id(ProductCharacteristic1, ProductCharacteristicId1),
%   post_product_characteristic("code 2", "name 2", ProductCharacteristic2),
%   get_entity_id(ProductCharacteristic2, ProductCharacteristicId2),

%   % GET ALL
%   get_product_characteristics(ProductCharacteristicList),
%   writeln('Return product characteristic list:'),
%   writeln(ProductCharacteristicList),

%   % DELETE
%   delete_product_characteristic(ProductCharacteristicId2),
%   delete_product_characteristic(ProductCharacteristicId1),
%   get_product_characteristics(ProductCharacteristicListNew),
%   writeln('Return empty product characteristic list:'),
%   writeln(ProductCharacteristicListNew).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %       ProductProperty        %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('product_property_test') :-
%   % Post mockup data
%   post_test_store(StoreId),
%   post_test_product(ProductId),
%   post_test_product_characteristics(ProductCharacteristicId1, ProductCharacteristicId2, ProductCharacteristicId3, ProductCharacteristicId4),

%   % POST
%   post_product_property(ProductId, ProductCharacteristicId1, StoreId, "value 1", "", _),
%   post_product_property(ProductId, ProductCharacteristicId2, "", "value 2", "", _),
%   post_product_property(ProductId, ProductCharacteristicId3, StoreId, "value low 3" , "value high 3", _),
%   post_product_property(ProductId, ProductCharacteristicId4, "", "value low 4", "value high 4", _),

%   % GET ALL
%   get_product_properties(ProductId, "", ProductPropertyList1),
%   writeln('Return product property list at no store:'),
%   writeln(ProductPropertyList1),
%   get_product_properties(ProductId, StoreId, ProductPropertyList2),
%   write('Return product property list at stord id '), writeln(StoreId),
%   writeln(ProductPropertyList2),

%   % DELETE
%   delete_product_property(ProductId, ProductCharacteristicId1, StoreId),
%   delete_product_property(ProductId, ProductCharacteristicId2, ""),
%   delete_product_property(ProductId, ProductCharacteristicId3, StoreId),
%   delete_product_property(ProductId, ProductCharacteristicId4, ""),
%   get_product_properties(ProductId, StoreId, ProductPropertyListNew1),
%   writeln('Return empty product property list at no store:'),
%   writeln(ProductPropertyListNew1),
%   get_product_properties(ProductId, StoreId, ProductPropertyListNew2),
%   write('Return empty product property list at stord id '), writeln(StoreId),
%   writeln(ProductPropertyListNew2),

%   % Delete mockup data
%   delete_product_characteristic(ProductCharacteristicId1),
%   delete_product_characteristic(ProductCharacteristicId2),
%   delete_product_characteristic(ProductCharacteristicId3),
%   delete_product_characteristic(ProductCharacteristicId4),
%   delete_product(ProductId),
%   delete_store(StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %        ProductUnit         %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('product_unit_test') :-
%   % Post mockup data
%   post_test_product(ProductId),

%   % POST
%   post_product_unit(ProductId, [11, "dimensionUnit 1", 1.1, 1.2, 1.3, 12, 1.4, 13, "unitCode 1", 1.5, "volumeUnit 1", "weightUnit 1", 1.6], ProductUnit1),
%   get_entity_id(ProductUnit1, ProductUnitId1),
%   post_product_unit(ProductId, [21, "dimensionUnit 2", 2.1, 2.2, 2.3, 22, 2.4, 23, "unitCode 2", 2.5, "volumeUnit 2", "weightUnit 2", 2.6], ProductUnit2),
%   get_entity_id(ProductUnit2, ProductUnitId2),
%   post_product_unit(ProductId, 
%     "{\"denominatorBaseUnit\" : 31,
%       \"dimensionUnit\" : \"dimensionUnit 3\",
%       \"grossWeight\" : 3.1,
%       \"height\" : 3.2,
%       \"length\" : 3.3,
%       \"maxStackSize\" : 32,
%       \"netWeight\" : 3.4,
%       \"numeratorBaseUnit\" : 33,
%       \"unitCode\" : \"unitCode 3\",
%       \"volume\" : 3.5,
%       \"volumeUnit\" : \"volumeUnit 3\",
%       \"weightUnit\" : \"weightUnit 3\",
%       \"width\" : 3.6}", 
%       ProductUnit3),
%   get_entity_id(ProductUnit3, ProductUnitId3),
%   post_product_unit(ProductId, 
%     "{\"denominatorBaseUnit\" : 41,
%       \"dimensionUnit\" : \"dimensionUnit 4\",
%       \"grossWeight\" : 4.1,
%       \"height\" : 4.2,
%       \"length\" : 4.3,
%       \"maxStackSize\" : 42,
%       \"netWeight\" : 4.4,
%       \"numeratorBaseUnit\" : 43,
%       \"unitCode\" : \"unitCode 4\",
%       \"volume\" : 4.5,
%       \"volumeUnit\" : \"volumeUnit 4\",
%       \"weightUnit\" : \"weightUnit 4\",
%       \"width\" : 4.6}",
%       ProductUnit4),
%   get_entity_id(ProductUnit4, ProductUnitId4),

%   % GET ONE
%   get_product_unit(ProductUnitId1, ProductUnitSame1),
%   write('Return logistical unit at Id '), writeln(ProductUnitId1),
%   writeln(ProductUnitSame1),

%   % PUT
%   put_product_unit(ProductUnitId4, ProductId, [410, "dimensionUnit 4 changed", 40.1, 40.2, 40.3, 420, 40.4, 430, "unitCode 4 changed", 40.5, "volumeUnit 4 changed", "weightUnit 4 changed", 40.6], _),
%   put_product_unit(ProductUnitId3, ProductId, 
%     "{\"denominatorBaseUnit\" : 310,
%       \"dimensionUnit\" : \"dimensionUnit 3 changed\",
%       \"grossWeight\" : 30.1,
%       \"height\" : 30.2,
%       \"length\" : 30.3,
%       \"maxStackSize\" : 320,
%       \"netWeight\" : 30.4,
%       \"numeratorBaseUnit\" : 330,
%       \"unitCode\" : \"unitCode 3 changed\",
%       \"volume\" : 30.5,
%       \"volumeUnit\" : \"volumeUnit 3 changed\",
%       \"weightUnit\" : \"weightUnit 3 changed\",
%       \"width\" : 30.6}",
%       _),

%   % GET ALL
%   get_product_units(ProductUnitList),
%   writeln('Return logistical unit list:'),
%   writeln(ProductUnitList),

%   % DELETE
%   delete_product_unit(ProductUnitId4),
%   delete_product_unit(ProductUnitId3),
%   delete_product_unit(ProductUnitId2),
%   delete_product_unit(ProductUnitId1),
%   get_product_units(ProductUnitListNew),
%   writeln('Return empty logistical unit list:'),
%   writeln(ProductUnitListNew),

%   % Delete mockup data
%   delete_product(ProductId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %      Product description      %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('product_description_test') :-
%   % Post mockup data
%   post_test_products(ProductId1, ProductId2, ProductId3, ProductId4, ProductId5, ProductId6),

%   % POST
%   post_product_description(ProductId1, ["description 1", "isoLanguageCode 1"], ProductDescription1),
%   get_entity_id(ProductDescription1, ProductDescriptionId1),
%   post_product_description(ProductId2, ["description 2", "isoLanguageCode 2"], ProductDescription2),
%   get_entity_id(ProductDescription2, ProductDescriptionId2),
%   post_product_description(ProductId3, 
%     "{\"description\" : \"description 3\",
%       \"isoLanguageCode\" : \"isoLanguageCode 3\"}",
%     ProductDescription3),
%   get_entity_id(ProductDescription3, ProductDescriptionId3),
%   post_product_description(ProductId4,
%     "{\"description\" : \"description 4\",
%       \"isoLanguageCode\" : \"isoLanguageCode 4\"}", 
%     ProductDescription4),
%   get_entity_id(ProductDescription4, ProductDescriptionId4),

%   % GET ONE
%   get_product_description(ProductDescriptionId1, ProductDescriptionSame1),
%   write('Return product description at Id '), writeln(ProductDescriptionId1),
%   writeln(ProductDescriptionSame1),

%   % PUT
%   put_product_description(ProductDescriptionId1, ProductId1, ["description 1 changed", "isoLanguageCode 1 changed"], _),
%   put_product_description(ProductDescriptionId3, ProductId2,
%     "{\"description\" : \"description 3 changed\",
%       \"isoLanguageCode\" : \"isoLanguageCode 3 changed\"}",
%     _),

%   % GET ALL
%   get_product_descriptions(ProductDescriptionList),
%   writeln('Return product description list'),
%   writeln(ProductDescriptionList),

%   % DELETE
%   delete_product_description(ProductDescriptionId1),
%   delete_product_description(ProductDescriptionId2),
%   delete_product_description(ProductDescriptionId3),
%   delete_product_description(ProductDescriptionId4),
%   get_product_descriptions(ProductDescriptionListNew),
%   writeln('Return empty product description list'),
%   writeln(ProductDescriptionListNew),

%   % Delete mockup data
%   delete_product(ProductId6),
%   delete_product(ProductId5),
%   delete_product(ProductId4),
%   delete_product(ProductId3),
%   delete_product(ProductId2),
%   delete_product(ProductId1).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %         Product gtin          %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('product_gtin_test') :-
%   % Post mockup data
%   post_test_product_unit(ProductId1, ProductId2, ProductUnitId1, ProductUnitId2),

%   % POST
%   post_product_gtin(ProductUnitId1, ["gtin 1", "gtinType 1", true], ProductGtin1),
%   get_entity_id(ProductGtin1, ProductGtinId1),
%   post_product_gtin(ProductUnitId1, ["gtin 2", "gtinType 2", false], ProductGtin2),
%   get_entity_id(ProductGtin2, ProductGtinId2),
%   post_product_gtin(ProductUnitId2, 
%     "{\"gtin\" : \"gtin 3\",
%       \"gtinType\" : \"gtinType 3\",
%       \"mainGtin\" : true}",
%     ProductGtin3),
%   get_entity_id(ProductGtin3, ProductGtinId3),
%   post_product_gtin(ProductUnitId2,
%     "{\"gtin\" : \"gtin 4\",
%       \"gtinType\" : \"gtinType 4\",
%       \"mainGtin\" : false}", 
%     ProductGtin4),
%   get_entity_id(ProductGtin4, ProductGtinId4),

%   % GET ONE
%   get_product_gtin(ProductGtinId1, ProductGtinSame1),
%   write('Return product gtin at Id '), writeln(ProductGtinId1),
%   writeln(ProductGtinSame1),

%   % PUT
%   put_product_gtin(ProductGtinId1, ProductUnitId1, ["gtin 1 changed", "gtinType 1 changed", false], _),
%   put_product_gtin(ProductGtinId3, ProductUnitId2,
%     "{\"gtin\" : \"gtin 3 changed\",
%       \"gtinType\" : \"gtinType 3 changed\",
%       \"mainGtin\" : false}",
%     _),

%   % GET ALL
%   get_product_gtins(ProductGtinList),
%   writeln('Return product gtin list'),
%   writeln(ProductGtinList),

%   % DELETE
%   delete_product_gtin(ProductGtinId1),
%   delete_product_gtin(ProductGtinId2),
%   delete_product_gtin(ProductGtinId3),
%   delete_product_gtin(ProductGtinId4),
%   get_product_gtins(ProductGtinListNew),
%   writeln('Return empty product gtin list'),
%   writeln(ProductGtinListNew),

%   % Delete mockup data
%   delete_product_unit(ProductUnitId1),
%   delete_product_unit(ProductUnitId2),
%   delete_product(ProductId1),
%   delete_product(ProductId2).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %         Product group         %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('product_group_test') :-
%   % Post mockup data
%   post_test_store(StoreId),
%   post_test_products(ProductId1, ProductId2, ProductId3, ProductId4, ProductId5, ProductId6),

%   % POST
%   post_product_group("name 1", StoreId, "", ProductGroup1),
%   get_entity_id(ProductGroup1, ProductGroupId1),
%   post_product_group("name 2", StoreId, "", ProductGroup2),
%   get_entity_id(ProductGroup2, ProductGroupId2),
%   post_product_to_product_group(ProductGroupId1, ProductId1, _),
%   post_product_to_product_group(ProductGroupId1, ProductId2, _),
%   post_product_to_product_group(ProductGroupId2, ProductId3, _),
%   post_product_to_product_group(ProductGroupId2, ProductId4, _),

%   % GET ONE
%   get_product_group(ProductGroupId1, ProductGroupSame1),
%   writeln('Return product group 1:'),
%   writeln(ProductGroupSame1),

%   % GET ALL
%   get_product_groups(StoreId, ProductGroupList),
%   writeln('Return product group list:'),
%   writeln(ProductGroupList),

%   % DELETE
%   delete_product_from_product_group(ProductGroupId1, ProductId1),
%   delete_product_from_product_group(ProductGroupId1, ProductId2),
%   get_product_group(ProductGroupId1, ProductGroupNew1),
%   writeln('Return empty product group 1:'),
%   writeln(ProductGroupNew1),
%   delete_product_group(ProductGroupId2),
%   delete_product_group(ProductGroupId1),
%   get_product_groups(StoreId, ProductGroupListNew),
%   writeln('Return empty product group list:'),
%   writeln(ProductGroupListNew),

%   % Delete mockup data
%   delete_product(ProductId6),
%   delete_product(ProductId5),
%   delete_product(ProductId4),
%   delete_product(ProductId3),
%   delete_product(ProductId2),
%   delete_product(ProductId1),
%   delete_store(StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %             Shelf             %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('shelf_test') :-
%   % Post mockup data
%   post_test_product_group(StoreId, ProductGroupId),

%   % POST
%   post_shelf(StoreId, ProductGroupId,
%     ["cadPlanId 1", 10, "externalReferenceId 1", 11, 12.12, 13.13, 14.14, 15.15, 16.16, 17.17, 18.18, 19], Shelf1),
%   get_entity_id(Shelf1, ShelfId1),
%   post_shelf(StoreId, ProductGroupId,
%     ["cadPlanId 2", 20, "externalReferenceId 2", 21, 22.22, 23.23, 24.24, 25.25, 26.26, 27.27, 28.28, 29], Shelf2),
%   get_entity_id(Shelf2, ShelfId2),
%   post_shelf(StoreId, ProductGroupId,
%     ["cadPlanId 3", 30, "externalReferenceId 3", 31, 32.32, 33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39], Shelf3),
%   get_entity_id(Shelf3, ShelfId3),
%   post_shelf(StoreId, ProductGroupId,
%     ["cadPlanId 4", 40, "externalReferenceId 4", 41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48, 49], Shelf4),
%   get_entity_id(Shelf4, ShelfId4),

%   % GET ONE
%   get_shelf(ShelfId1, ShelfSame1),
%   write('Return shelf with id '), writeln(ShelfId1),
%   writeln(ShelfSame1),

%   % PUT
%   put_shelf(ShelfId3, StoreId, ProductGroupId,
%     "{\"cadPlanId\" : \"cadPlanId 1 changed\",
%       \"depth\" : 100,
%       \"externalReferenceId\" : \"externalReferenceId 1 changed\",
%       \"height\" : 110,
%       \"orientationW\" : 120.12,
%       \"orientationX\" : 130.13,
%       \"orientationY\" : 140.14,
%       \"orientationZ\" : 150.15,
%       \"positionX\" : 160.16,
%       \"positionY\" : 170.17,
%       \"positionZ\" : 180.18,
%       \"width\" : 190.19}",
%     _),
%   put_shelf(ShelfId4, StoreId, ProductGroupId,
%     ["cadPlanId 4 changed", 400, "externalReferenceId 4 changed", 410, 420.42, 430.43, 440.44, 450.45, 460.46, 470.47, 480.48, 490], 
%     _),

%   % GET ALL
%   get_shelves(StoreId, ShelfList),
%   writeln('Return shelf list:'),
%   writeln(ShelfList),

%   % DELETE
%   delete_shelf(ShelfId1),
%   delete_shelf(ShelfId2),
%   delete_shelf(ShelfId3),
%   delete_shelf(ShelfId4),
%   get_shelves(StoreId, ShelfListNew),
%   writeln('Return empty shelf list:'),
%   writeln(ShelfListNew),

%   % Delete mockup data
%   delete_product_group(ProductGroupId),
%   delete_store(StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %          Shelf layer          %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('shelf_layer_test') :-
%   % Post mockup data
%   post_test_shelf(StoreId, ProductGroupId, ShelfId),

%   % POST
%   post_shelf_layer(ShelfId, [10, "externalReferenceId 1", 11, 12, 13.13, "type 1", 14], ShelfLayer1),
%   get_entity_id(ShelfLayer1, ShelfLayerId1),
%   post_shelf_layer(ShelfId, [20, "externalReferenceId 2", 21, 22, 23.23, "type 2", 24], ShelfLayer2),
%   get_entity_id(ShelfLayer2, ShelfLayerId2),
%   post_shelf_layer(ShelfId, 
%     "{\"depth\" : 30,
%       \"externalReferenceId\" : \"externalReferenceId 3\",
%       \"height\" : 31,
%       \"level\" : 32,
%       \"positionZ\" : 33.33,
%       \"type\" : \"type 3\",
%       \"width\" : 34}",
%     ShelfLayer3),
%   get_entity_id(ShelfLayer3, ShelfLayerId3),
%   post_shelf_layer(ShelfId, 
%     "{\"depth\" : 40,
%       \"externalReferenceId\" : \"externalReferenceId 4\",
%       \"height\" : 41,
%       \"level\" : 42,
%       \"positionZ\" : 43.43,
%       \"type\" : \"type 4\",
%       \"width\" : 44}",
%     ShelfLayer4),
%   get_entity_id(ShelfLayer4, ShelfLayerId4),

%   % GET ONE
%   get_shelf_layer(ShelfLayerId1, ShelfLayerSame1),
%   write('Return shelf layer with id '), writeln(ShelfLayerId1),
%   writeln(ShelfLayerSame1),

%   % GET ALL
%   get_shelf_layers(ShelfId, ShelfLayerList),
%   writeln('Return shelf layer list:'),
%   writeln(ShelfLayerList),

%   % DELETE
%   delete_shelf_layer(ShelfLayerId1),
%   delete_shelf_layer(ShelfLayerId2),
%   delete_shelf_layer(ShelfLayerId3),
%   delete_shelf_layer(ShelfLayerId4),
%   get_shelf_layers(ShelfId, ShelfLayerListNew),
%   writeln('Return empty shelf layer list:'),
%   writeln(ShelfLayerListNew),

%   % Delete mockup data
%   delete_shelf(ShelfId),
%   delete_product_group(ProductGroupId),
%   delete_store(StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %             Facing            %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('facing_test') :-
%   % Post mockup data
%   post_test_shelf_layer(StoreId, ProductGroupId, ShelfId, ShelfLayerId),

%   % POST
%   post_facing(ShelfLayerId, [10, 11, 12], Facing1),
%   get_entity_id(Facing1, FacingId1),
%   post_facing(ShelfLayerId, [20, 21, 22], Facing2),
%   get_entity_id(Facing2, FacingId2),
%   post_facing(ShelfLayerId, 
%     "{\"layerRelativePosition\" : 30,
%       \"noOfItemsDepth\" : 31,
%       \"noOfItemsWidth\" : 32}", 
%     Facing3),
%   get_entity_id(Facing3, FacingId3),
%   post_facing(ShelfLayerId, 
%     "{\"layerRelativePosition\" : 40,
%       \"noOfItemsDepth\" : 41,
%       \"noOfItemsWidth\" : 42}", 
%     Facing4),
%   get_entity_id(Facing4, FacingId4),
  
%   % GET ONE
%   get_facing(FacingId1, FacingSame1),
%   write('Return facing with id '), writeln(FacingId1),
%   writeln(FacingSame1),

%   % PUT
%   put_facing(FacingId3, ShelfLayerId,
%     "{\"layerRelativePosition\" : 300,
%       \"noOfItemsDepth\" : 310,
%       \"noOfItemsWidth\" : 320}",
%       _),
%   put_facing(FacingId4, ShelfLayerId,
%     [400, 410, 420], _),

%   % GET ALL
%   get_facings(ShelfLayerId, FacingList),
%   writeln('Return facing list:'),
%   writeln(FacingList),

%   % DELETE
%   delete_facing(FacingId1),
%   delete_facing(FacingId2),
%   delete_facing(FacingId3),
%   delete_facing(FacingId4),
%   get_facings(ShelfLayerId, FacingListNew),
%   writeln('Return empty facing list:'),
%   writeln(FacingListNew),

%   % Delete mockup data
%   delete_shelf_layer(ShelfLayerId),
%   delete_shelf(ShelfId),
%   delete_product_group(ProductGroupId),
%   delete_store(StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %           ItemGroup           %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('item_group_test') :-
%   % Post mockup data
%   post_test_product_unit(ProductId1, ProductId2, ProductUnitId1, ProductUnitId2),
%   post_test_facings(StoreId, ProductGroupId, ShelfId, ShelfLayerId, FacingId1, FacingId2),

%   % POST
%   post_item_group(FacingId1, ProductUnitId1, 10, ItemGroup1),
%   get_entity_id(ItemGroup1, ItemGroupId1),
%   post_item_group(FacingId1, ProductUnitId2, 20, ItemGroup2),
%   get_entity_id(ItemGroup2, ItemGroupId2),
%   post_item_group(FacingId2, ProductUnitId1, 30, ItemGroup3),
%   get_entity_id(ItemGroup3, ItemGroupId3),
%   post_item_group(FacingId2, ProductUnitId2, 40, ItemGroup4),
%   get_entity_id(ItemGroup4, ItemGroupId4),
  
%   % GET ONE
%   get_item_group(ItemGroupId1, ItemGroupSame1),
%   write('Return item group with id '), writeln(ItemGroupId1),
%   writeln(ItemGroupSame1),

%   % PUT
%   put_item_group(ItemGroupId3, FacingId1, ProductUnitId1, 33, _),
%   put_item_group(ItemGroupId4, FacingId1, ProductUnitId2, 45, _),

%   % GET ALL
%   get_item_groups(ItemGroupList),
%   writeln('Return item group list:'),
%   writeln(ItemGroupList),

%   % DELETE
%   delete_item_group(ItemGroupId1),
%   delete_item_group(ItemGroupId2),
%   delete_item_group(ItemGroupId3),
%   delete_item_group(ItemGroupId4),
%   get_item_groups(ItemGroupListNew),
%   writeln('Return empty item group list:'),
%   writeln(ItemGroupListNew),

%   % Delete mockup data
%   delete_facing(FacingId2),
%   delete_facing(FacingId1),
%   delete_shelf_layer(ShelfLayerId),
%   delete_shelf(ShelfId),
%   delete_product_group(ProductGroupId),
%   delete_product_unit(ProductUnitId2),
%   delete_product_unit(ProductUnitId1),
%   delete_product(ProductId2),
%   delete_product(ProductId1),
%   delete_store(StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %             Item              %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('item_test') :-
%   % Post mockup data
%   post_test_item_groups(StoreId, 
%     ProductId1, 
%     ProductId2, 
%     ProductUnitId1, 
%     ProductUnitId2, 
%     ProductGroupId, 
%     ShelfId, 
%     ShelfLayerId, 
%     FacingId1, 
%     FacingId2, 
%     ItemGroupId1, 
%     ItemGroupId2),

%   % POST
%   post_item(ItemGroupId1, [10, 11, 12], Item1),
%   get_entity_id(Item1, ItemId1),
%   post_item(ItemGroupId1, [20, 21, 22], Item2),
%   get_entity_id(Item2, ItemId2),
%   post_item(ItemGroupId2, 
%     "{\"positionInFacingX\" : 30,
%       \"positionInFacingY\" : 31,
%       \"positionInFacingZ\" : 32}", 
%       Item3),
%   get_entity_id(Item3, ItemId3),
%   post_item(ItemGroupId2, 
%     "{\"positionInFacingX\" : 40,
%       \"positionInFacingY\" : 41,
%       \"positionInFacingZ\" : 42}", 
%       Item4),
%   get_entity_id(Item4, ItemId4),
  
%   % GET ONE
%   get_item(ItemId1, ItemSame1),
%   write('Return item with id '), writeln(ItemId1),
%   writeln(ItemSame1),

%   % PUT
%   put_item(ItemId3, ItemGroupId1, [300, 310, 320], _),
%   put_item(ItemId4, ItemGroupId1, 
%     "{\"positionInFacingX\" : 400,
%       \"positionInFacingY\" : 410,
%       \"positionInFacingZ\" : 420}", 
%       _),

%   % GET ALL
%   get_items(ItemList),
%   writeln('Return item list:'),
%   writeln(ItemList),

%   % DELETE
%   delete_item(ItemId4),
%   delete_item(ItemId3),
%   delete_item(ItemId2),
%   delete_item(ItemId1),
%   get_items(ItemListNew),
%   writeln('Return empty item list:'),
%   writeln(ItemListNew),

%   % Delete mockup data
%   delete_item_group(ItemGroupId2),
%   delete_item_group(ItemGroupId1),
%   delete_facing(FacingId2),
%   delete_facing(FacingId1),
%   delete_shelf_layer(ShelfLayerId),
%   delete_shelf(ShelfId),
%   delete_product_group(ProductGroupId),
%   delete_product_unit(ProductUnitId2),
%   delete_product_unit(ProductUnitId1),
%   delete_product(ProductId2),
%   delete_product(ProductId1),
%   delete_store(StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %          Planogram            %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('planogram_test') :-
%   % Post mockup data
%   post_test_product_unit(ProductId1, ProductId2, ProductUnitId1, ProductUnitId2),
%   post_test_shelf_layer(StoreId, ProductGroupId, ShelfId, ShelfLayerId),

%   % POST
%   post_planogram(ProductUnitId1, ShelfLayerId, [10, 11.11, 12, 13], Planogram1),
%   get_entity_id(Planogram1, PlanogramId1),
%   post_planogram(ProductUnitId2, ShelfLayerId, [20, 21.21, 22, 23], Planogram2),
%   get_entity_id(Planogram2, PlanogramId2),
%   post_planogram(ProductUnitId1, ShelfLayerId, 
%     "{\"numberOfFacings\" : 30,
%       \"orientationYaw\" : 31.31,
%       \"positionX\" : 32,
%       \"versionTimestamp\" : 33}", 
%       Planogram3),
%       get_entity_id(Planogram3, PlanogramId3),
%   post_planogram(ProductUnitId2, ShelfLayerId, 
%     "{\"numberOfFacings\" : 40,
%       \"orientationYaw\" : 41.41,
%       \"positionX\" : 42,
%       \"versionTimestamp\" : 43}", 
%       Planogram4),
%   get_entity_id(Planogram4, PlanogramId4),

%   % PUT
%   put_planogram(PlanogramId3, ProductUnitId2, ShelfLayerId, [300, 310.310, 320, 330], _),
%   put_planogram(PlanogramId4, ProductUnitId1, ShelfLayerId, 
%     "{\"numberOfFacings\" : 400,
%       \"orientationYaw\" : 410.410,
%       \"positionX\" : 420,
%       \"versionTimestamp\" : 430}", 
%       _),

%   % GET ALL
%   get_planograms(PlanogramList),
%   writeln('Return planogram list:'),
%   writeln(PlanogramList),

%   % DELETE
%   delete_planogram(PlanogramId4),
%   delete_planogram(PlanogramId3),
%   delete_planogram(PlanogramId2),
%   delete_planogram(PlanogramId1),
%   get_planograms(PlanogramListNew),
%   writeln('Return empty planogram list:'),
%   writeln(PlanogramListNew),

%   % Delete mockup data
%   delete_shelf_layer(ShelfLayerId),
%   delete_shelf(ShelfId),
%   delete_product_group(ProductGroupId),
%   delete_product_unit(ProductUnitId2),
%   delete_product_unit(ProductUnitId1),
%   delete_product(ProductId2),
%   delete_product(ProductId1),
%   delete_store(StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %    ShoppingBasketPosition     %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('shopping_basket_test') :-
%   % Post mockup data
%   post_test_store(StoreId),
%   post_test_customer(CustomerId1, CustomerId2),
%   post_test_product(ProductId1),
%   post_test_product2(ProductId2),
  
%   % POST
%   post_shopping_basket_position(StoreId, CustomerId1, ProductId1, ["currency 1", 10, 11.11], _),
%   post_shopping_basket_position(StoreId, CustomerId1, ProductId2, ["currency 2", 20, 21.21], _),
%   post_shopping_basket_position(StoreId, CustomerId2, ProductId1,
%     "{\"currency\" : 30,
%       \"quantity\" : 32,
%       \"sellingPrice\" : 33}", 
%       ShoppingBasketPosition3),
%   get_entity_id(ShoppingBasketPosition3, ShoppingBasketPositionId3),
%   post_shopping_basket_position(StoreId, CustomerId2, ProductId2,
%     "{\"currency\" : 40,
%       \"quantity\" : 42,
%       \"sellingPrice\" : 43}", 
%       ShoppingBasketPosition4),
%   get_entity_id(ShoppingBasketPosition4, ShoppingBasketPositionId4),

%   % GET ALL
%   get_shopping_basket_positions(StoreId, CustomerId1, ShoppingBasketPositionList),
%   writeln('Return shopping basket position list:'),
%   writeln(ShoppingBasketPositionList),

%   % DELETE
%   delete_shopping_basket_position(ShoppingBasketPositionId4),
%   delete_shopping_basket_position(ShoppingBasketPositionId3),
%   delete_shopping_basket_positions(StoreId, CustomerId1),

%   % Delete mockup data
%   delete_product(ProductId1),
%   delete_product(ProductId2),
%   delete_customer(CustomerId1),
%   delete_customer(CustomerId2),
%   delete_store(StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %            Device             %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('device_test') :-
%   % Post mockup data
%   post_test_store(StoreId),
  
%   % POST
%   DeviceId1 = "id 1",
%   DeviceId2 = "id 2",
%   DeviceId3 = "id 3",
%   DeviceId4 = "id 4",
%   post_device(DeviceId1, StoreId, ["description 1", "deviceType 1"], _),
%   post_device(DeviceId2, StoreId, ["description 2", "deviceType 1"], _),
%   post_device(DeviceId3, StoreId, 
%     "{\"description\" : \"description 3\",
%       \"deviceType\" : \"deviceType 3\"}", 
%       _),
%   post_device(DeviceId4, StoreId, 
%     "{\"description\" : \"description 4\",
%       \"deviceType\" : \"deviceType 4\"}", 
%       _),

%   % GET ALL
%   get_devices(DeviceList),
%   writeln('Return device list:'),
%   writeln(DeviceList),

%   % DELETE
%   delete_device(DeviceId4),
%   delete_device(DeviceId3),
%   delete_device(DeviceId2),
%   delete_device(DeviceId1),
%   get_devices(DeviceListNew),
%   writeln('Return empty device list:'),
%   writeln(DeviceListNew),

%   % Delete mockup data
%   delete_store(StoreId).

% test('get_products_by_shelf_test') :-
%   % Post mockup data
%   post_test_item_groups(StoreId, 
%     ProductId1, 
%     ProductId2, 
%     ProductUnitId1, 
%     ProductUnitId2, 
%     ProductGroupId, 
%     ShelfId, 
%     ShelfLayerId, 
%     FacingId1, 
%     FacingId2, 
%     ItemGroupId1, 
%     ItemGroupId2),

%   get_products_by_shelf(ShelfId, ProductList),
%   write('Product list of shelf id '), writeln(ShelfId),
%   writeln(ProductList),

%   % Delete mockup data
%   delete_item_group(ItemGroupId2),
%   delete_item_group(ItemGroupId1),
%   delete_facing(FacingId2),
%   delete_facing(FacingId1),
%   delete_shelf_layer(ShelfLayerId),
%   delete_shelf(ShelfId),
%   delete_product_group(ProductGroupId),
%   delete_product_unit(ProductUnitId2),
%   delete_product_unit(ProductUnitId1),
%   delete_product(ProductId2),
%   delete_product(ProductId1),
%   delete_store(StoreId).

% test('post_shelves') :-
%   % Post mockup data
%   post_test_store(StoreId),

%   % Post shelves from data base
%   post_shelves(StoreId, ShelfList),
%   writeln(ShelfList),
  
%   % Delete all shelves
%   get_shelves(StoreId, Shelves),
%   forall(
%     member(Shelf, Shelves),
%     (
%       get_entity_id(Shelf, ShelfId),
%       delete_shelf(ShelfId)
%     )
%   ),

%   % Delete mockup data
%   delete_store(StoreId).

:- end_tests(k4r_db_client).
