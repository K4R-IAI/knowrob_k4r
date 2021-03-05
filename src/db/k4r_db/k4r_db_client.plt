:- use_module('k4r_db_client.pl').
:- use_module(library('rostest')).

:- begin_tests(
        'k4r_db_client').

% post_test_customer(Link, CustomerId) :-
%   k4r_post_customer(Link, "anonymisedName Test"),
%   k4r_get_customers(Link, CustomerList),
%   k4r_get_entity_by_key_value(CustomerList, "anonymisedName", "anonymisedName Test", Customer),
%   k4r_get_entity_id(Customer, CustomerId).

% post_test_store(Link, StoreId) :-
%   k4r_post_store(Link, 
%     ["addressAdditional test", 
%     "addressCity test", 
%     "addressCountry test", 
%     "addressPostcode test", 
%     "addressState test", 
%     "addressStreet test", 
%     "addressStreetNumber test", 
%     "cadPlanId test",
%     1.1,
%     1.2,
%     "storeName test",
%     "storeNumber test"]),
%   get_store_id_by_store_name(Link, "storeName test", StoreId).

% post_test_product(Link, "idTest") :-
%   k4r_post_product(Link, [10, "description test", "gtin test", 11, 12, "name test", 13], "idTest").

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

% get_customer_id_by_customer_name(Link, CustomerName, CustomerId) :-
%   k4r_get_customers(Link, CustomerList),
%   k4r_get_entity_by_key_value(CustomerList, "anonymisedName", CustomerName, Customer),
%   k4r_get_entity_id(Customer, CustomerId).

% get_store_id_by_store_name(Link, StoreName, StoreId) :-
%   k4r_get_stores(Link, StoreList),
%   k4r_get_entity_by_key_value(StoreList, "storeName", StoreName, Store),
%   k4r_get_entity_id(Store, StoreId).

% get_test_store_id(Link, StoreId) :-
%   get_store_id_by_store_name(Link, "storeName test", StoreId).

% get_characteristic_id_by_characteristic_name(Link, CharacteristicName, CharacteristicId) :-
%   k4r_get_characteristics(Link, CharacteristicList),
%   k4r_get_entity_by_key_value(CharacteristicList, "name", CharacteristicName, Characteristic),
%   k4r_get_entity_id(Characteristic, CharacteristicId).

% get_product_group_id_by_product_group_name(Link, StoreId, ProductGroupName, ProductGroupId) :-
%   k4r_get_product_groups(Link, StoreId, ProductGroupList),
%   k4r_get_entity_by_key_value(ProductGroupList, "name", ProductGroupName, ProductGroup),
%   k4r_get_entity_id(ProductGroup, ProductGroupId).

% get_test_product_id("idTest").

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

% delete_test_product(Link) :-
%   k4r_delete_product(Link, "idTest").

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

% test('customer_post_test') :-
%   k4r_get_core_link(Link),
%   k4r_post_customer(Link, "Giang"),
%   k4r_post_customer(Link, "Kaviya"),
%   k4r_post_customer(Link, "Gautam"),
%   k4r_post_customer(Link, "Nils").

% test('customer_put_test') :-
%   k4r_get_core_link(Link),
%   k4r_post_customer(Link, "TaylorSwift"),
%   get_customer_id_by_customer_name(Link, "TaylorSwift", CustomerId),
%   k4r_get_customer(Link, CustomerId, Customer),
%   write('Return customer with id '), writeln(CustomerId),
%   writeln(Customer),
%   k4r_put_customer(Link, CustomerId, "Taylor Swift"),
%   k4r_get_customers(Link, CustomerList),
%   writeln('Return customer list with name Taylor Swift:'),
%   writeln(CustomerList).

% test('customer_delete_test') :-
%   k4r_get_core_link(Link),
%   get_customer_id_by_customer_name(Link, "Giang", CustomerId1),
%   k4r_delete_customer(Link, CustomerId1),
%   get_customer_id_by_customer_name(Link, "Kaviya", CustomerId2),
%   k4r_delete_customer(Link, CustomerId2),
%   get_customer_id_by_customer_name(Link, "Gautam", CustomerId3),
%   k4r_delete_customer(Link, CustomerId3),
%   get_customer_id_by_customer_name(Link, "Nils", CustomerId4),
%   k4r_delete_customer(Link, CustomerId4).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %             Store             %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('store_post_test') :-
%   k4r_get_core_link(Link),
%   k4r_post_store(Link, 
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
%     "storeNumber 1"]),
%   k4r_post_store(Link,
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
%     "storeNumber 2"]),
%     k4r_post_store(Link,
%     ["addressAdditional 3", 
%     "addressCity 3", 
%     "addressCountry 3", 
%     "addressPostcode 3", 
%     "addressState 3", 
%     "addressStreet 3", 
%     "addressStreetNumber 3", 
%     "cadPlanId 3", 
%     3.1, 
%     3.2,
%     "storeName 3",
%     "storeNumber 3"]),
%   k4r_post_store(Link,
%     ["addressAdditional 4", 
%     "addressCity 4", 
%     "addressCountry 4", 
%     "addressPostcode 4", 
%     "addressState 4", 
%     "addressStreet 4", 
%     "addressStreetNumber 4", 
%     "cadPlanId 4", 
%     4.1, 
%     4.2,
%     "storeName 4",
%     "storeNumber 4"]).

% test('store_put_and_get_test') :-
%   k4r_get_core_link(Link),
%   get_store_id_by_store_name(Link, "storeName 4", StoreId),
%   k4r_put_store(Link, StoreId, "{
%     \"addressAdditional\" : \"Changed\",
%     \"addressCity\" : \"REFD\",
%     \"addressCountry\" : \"DE\",
%     \"addressPostcode\" : \"23232\",
%     \"addressState\" : \"EDFG\",
%     \"addressStreet\" : \"ABCD\",
%     \"addressStreetNumber\" : \"19\",
%     \"cadPlanId\" : \"123CAD321\",
%     \"latitude\" : 12.43,
%     \"longitude\" : 32.435,
%     \"storeName\" : \"storeName 4\",
%     \"storeNumber\" : \"ABC123\"
%   }"),
%   k4r_put_store(Link, StoreId, 
%     ["addressAdditional changed", 
%     "addressCity changed", 
%     "addressCountry changed", 
%     "addressPostcode changed", 
%     "addressState changed", 
%     "addressStreet changed", 
%     "addressStreetNumber changed", 
%     "cadPlanId changed", 
%     4.1, 
%     4.2,
%     "storeName 4",
%     "storeNumber changed"]),
%   get_store_id_by_store_name(Link, "storeName 4", StoreId),
%   k4r_get_store(Link, StoreId, Store),
%   writeln('Return changed store with name storeName 4:'),
%   writeln(Store).

% test('store_delete_test') :-
%   k4r_get_core_link(Link),
%   get_store_id_by_store_name(Link, "storeName 1", StoreId1),
%   k4r_delete_store(Link, StoreId1),
%   get_store_id_by_store_name(Link, "storeName 2", StoreId2),
%   k4r_delete_store(Link, StoreId2),
%   get_store_id_by_store_name(Link, "storeName 3", StoreId3),
%   k4r_delete_store(Link, StoreId3),
%   get_store_id_by_store_name(Link, "storeName 4", StoreId4),
%   k4r_delete_store(Link, StoreId4).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %            Product            %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('product_post_test') :-
%   k4r_get_core_link(Link),
%   k4r_post_product(Link, [10, "description 1", "gtin 1", 11, 12, "name 1", 13], "id1"),
%   k4r_post_product(Link, [20, "description 2", "gtin 2", 21, 22, "name 2", 23], "id2"),
%   k4r_post_product(Link, [30, "description 3", "gtin 3", 31, 32, "name 3", 33], "id3"),
%   k4r_post_product(Link, [40, "description 4", "gtin 4", 41, 42, "name 4", 43], "id4").

% test('product_get_test') :-
%   k4r_get_core_link(Link),
%   k4r_get_product(Link, "id1", Product),
%   writeln('Return product with id id1:'),
%   writeln(Product).

% test('product_put_test') :-
%   k4r_get_core_link(Link),
%   k4r_put_product(Link, "{
%     \"depth\" : 3,
%     \"description\" : \"a new changed product\",
%     \"gtin\" : \"whatisthis\",
%     \"height\" : 2,
%     \"length\" : 12,
%     \"name\" : \"new changed product\",
%     \"weight\" : 111
%   }", "id4"),
%   k4r_put_product(Link, [40, "description changed", "gtin changed", 41, 42, "name changed", 43], "id4"),
%   k4r_get_products(Link, ProductList),
%   writeln('Return products with product new changed product at id id4:'),
%   writeln(ProductList).

% test('product_posts_test') :-
%   k4r_get_core_link(Link),
%   k4r_post_products(Link, "{
%     \"products\": 
%       [
%         {
%           \"depth\" : 50,
%           \"description\" : \"description 5\",
%           \"gtin\" : \"gtin 5\",
%           \"height\" : 51,
%           \"id\" : \"id5\",
%           \"length\" : 52,
%           \"name\" : \"name 5\",
%           \"weight\" : 53
%         }
%         ,{
%           \"depth\" : 60,
%           \"description\" : \"description 6\",
%           \"gtin\" : \"gtin 6\",
%           \"height\" : 61,
%           \"id\" : \"id6\",
%           \"length\" : 62,
%           \"name\" : \"name 6\",
%           \"weight\" : 63
%         }
%       ]
%     }"
%   ),
%   k4r_post_products(Link, [
%     [70, "description 7", "gtin 7", 71, "id7", 72, "name 7", 73],
%     [80, "description 8", "gtin 8", 81, "id8", 82, "name 8", 83]
%   ]),
%   k4r_get_products(Link, ProductList),
%   writeln('Return products with products at id id5 to id8:'),
%   writeln(ProductList).

% test('product_delete_test') :-
%   k4r_get_core_link(Link),
%   k4r_delete_product(Link, "id1"),
%   k4r_delete_product(Link, "id2"),
%   k4r_delete_product(Link, "id3"),
%   k4r_delete_product(Link, "id4"),
%   k4r_delete_product(Link, "id5"),
%   k4r_delete_product(Link, "id6"),
%   k4r_delete_product(Link, "id7"),
%   k4r_delete_product(Link, "id8").

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %         Characteristic        %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test('characteristic_post_and_get_test') :-
%   k4r_get_core_link(Link),
%   k4r_post_characteristic(Link, "New Characteristic"),
%   k4r_get_characteristics(Link, CharacteristicList),
%   writeln('Return characteristics with New Characteristic'),
%   writeln(CharacteristicList).

% test('characteristic_delete_test') :-
%   k4r_get_core_link(Link),
%   k4r_get_characteristics(Link, CharacteristicList),
%   k4r_get_entity_by_key_value(CharacteristicList, "name", "New Characteristic", Characteristic),
%   k4r_get_entity_id(Characteristic, CharacteristicId),
%   k4r_delete_characteristic(Link, CharacteristicId),
%   k4r_get_characteristics(Link, CharacteristicListNew),
%   writeln('Return characteristics without New Characteristic'),
%   writeln(CharacteristicListNew).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %           Property            %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test('property_post_test') :-
%   k4r_get_core_link(Link),
%   post_test_store(Link, StoreId),
%   post_test_product(Link, ProductId),
%   k4r_post_characteristic(Link, "name 1"),
%   k4r_post_characteristic(Link, "name 2"),
%   k4r_post_characteristic(Link, "name 3"),
%   k4r_post_characteristic(Link, "name 4"),
%   get_characteristic_id_by_characteristic_name(Link, "name 1", CharacteristicId1),
%   get_characteristic_id_by_characteristic_name(Link, "name 2", CharacteristicId2),
%   get_characteristic_id_by_characteristic_name(Link, "name 3", CharacteristicId3),
%   get_characteristic_id_by_characteristic_name(Link, "name 4", CharacteristicId4),
%   k4r_post_property(Link, StoreId, ProductId, CharacteristicId1, "value 1"),
%   k4r_post_property(Link, StoreId, ProductId, CharacteristicId2, "value 2"),
%   k4r_post_property(Link, StoreId, ProductId, CharacteristicId3, "value 3"),
%   k4r_post_property(Link, StoreId, ProductId, CharacteristicId4, "value 4").

% test('property_get_test') :-
%   k4r_get_core_link(Link),
%   k4r_get_stores(Link, StoreList),
%   k4r_get_entity_by_key_value(StoreList, "storeName", "storeName test", Store),
%   k4r_get_entity_id(Store, StoreId),
%   k4r_get_properties(Link, StoreId, "idTest", PropertyList),
%   writeln('Return properties of product idTest in store name storeName test:'),
%   writeln(PropertyList).

% test('property_delete_test') :-
%   k4r_get_core_link(Link),
%   get_test_store_id(Link, StoreId),
%   k4r_get_characteristics(Link, CharacteristicList),
%   get_characteristic_id_by_characteristic_name(Link, "name 1", CharacteristicId1),
%   get_characteristic_id_by_characteristic_name(Link, "name 2", CharacteristicId2),
%   get_characteristic_id_by_characteristic_name(Link, "name 3", CharacteristicId3),
%   get_characteristic_id_by_characteristic_name(Link, "name 4", CharacteristicId4),
%   get_test_product_id(ProductId),
%   k4r_delete_property(Link, StoreId, ProductId, CharacteristicId1),
%   k4r_delete_property(Link, StoreId, ProductId, CharacteristicId2),
%   k4r_delete_property(Link, StoreId, ProductId, CharacteristicId3),
%   k4r_delete_property(Link, StoreId, ProductId, CharacteristicId4),
%   k4r_delete_characteristic(Link, CharacteristicId1),
%   k4r_delete_characteristic(Link, CharacteristicId2),
%   k4r_delete_characteristic(Link, CharacteristicId3),
%   k4r_delete_characteristic(Link, CharacteristicId4),
%   k4r_delete_product(Link, ProductId),
%   k4r_delete_store(Link, StoreId).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %         Product group         %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

test('facing ') :-
  post_facing(2).

% test('post shelf layer') :-
%   k4r_get_core_link(Link),
%   k4r_post_shelf_layer(Link, 1, 2.5, [0.345, 0.234, 0.01], 1, "Ext_1", "Bottom_Layer"). 

:- end_tests(k4r_db_client).
