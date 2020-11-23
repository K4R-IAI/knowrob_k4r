:- use_module('k4r_db_client.pl').
:- use_module(library('rostest')).

:- begin_tripledb_tests(
        'k4r_db_client',
        'package://knowrob/owl/test/test_owl.owl'
  ).

% % Customer

% test('k4r_customer_test_1') :-
%   k4r_get_link(Link),
%   k4r_get_customer_by_id(Link, 122, Customer),
%   writeln('Return customer with id 122:'),
%   writeln(Customer).

% test('k4r_customer_test_2') :-
%   k4r_get_link(Link),
%   k4r_get_customers(Link, CustomerList),
%   k4r_get_customer_by_name(CustomerList, "Megan Firefox", Customer),
%   writeln('Return customer with name Megan Firefox:'),
%   writeln(Customer).

test('k4r_customer_test_3') :-
  k4r_get_link(Link),
  % k4r_post_customer(Link, "TaylorSwift"),
  k4r_get_customers(Link, CustomerList),
  k4r_get_customer_by_name(CustomerList, "TaylorSwift", Customer),
  k4r_get_entity_id(Customer, CustomerId),
  % k4r_put_customer(Link, CustomerId, "Taylor Swift").
  % writeln('Return customer with name Taylor Swift:'),
  % writeln(Customer).

% test('k4r_customer_test_4') :-
%   k4r_get_link(Link),
%   k4r_get_customers(Link, CustomerList),
%   k4r_get_customer_by_name(CustomerList, "Taylor Swift", Customer),
%   k4r_get_entity_id(Customer, CustomerId),
%   k4r_delete_customer(Link, CustomerId),
%   k4r_get_customers(Link, CustomerNewList),
%   writeln('Return customer list without customer name Taylor Swift:'),
%   writeln(CustomerNewList).

% % Store

% test('k4r_store_test_1') :-
%   k4r_get_link(Link),
%   k4r_get_stores(Link, StoreList),
%   k4r_get_store_by_name(StoreList, "Refills Lab", Store),
%   writeln('Return store with name Refills Lab:'),
%   writeln(Store).

% test('k4r_store_test_2') :-
%   k4r_get_link(Link),
%   k4r_post_store(Link, "{
%     \"addressAdditional\" : \"Nothing\",
%     \"addressCity\" : \"Bremen\",
%     \"addressCountry\" : \"DE\",
%     \"addressPostcode\" : \"28211\",
%     \"addressState\" : \"Land Bremen\",
%     \"addressStreet\" : \"Heinrich-Hertz-Strasse\",
%     \"addressStreetNumber\" : \"19\",
%     \"cadPlanId\" : \"123CAD321\",
%     \"latitude\" : 12.3455,
%     \"longitude\" : 46.321,
%     \"storeName\" : \"Giang Lab\",
%     \"storeNumber\" : \"123ABC\"
%   }"),
%   k4r_get_stores(Link, StoreList),
%   k4r_get_store_by_name(StoreList, "Giang Lab", Store),
%   writeln('Return store with name Giang Lab:'),
%   writeln(Store).

% test('k4r_store_test_3') :-
%   k4r_get_link(Link),
%   k4r_get_stores(Link, StoreList),
%   k4r_get_store_by_name(StoreList, "Giang Lab", Store),
%   k4r_get_entity_id(Store, StoreId),
%   k4r_delete_store(Link, StoreId),
%   k4r_get_stores(Link, StoreListNew),
%   writeln('Return stores without store name Giang Lab:'),
%   writeln(StoreListNew).

% % Characteristic

% test('k4r_characteristic_test_1') :-
%   k4r_get_link(Link),
%   k4r_get_characteristics(Link, CharacteristicList),
%   writeln('Return characteristics:'),
%   writeln(CharacteristicList).

% test('k4r_characteristic_test_2') :-
%   k4r_get_link(Link),
%   k4r_post_characteristic(Link, "New Characteristic"),
%   k4r_get_characteristics(Link, CharacteristicList),
%   writeln('Return characteristics with New Characteristic'),
%   writeln(CharacteristicList).

% test('k4r_characteristic_test_3') :-
%   k4r_get_link(Link),
%   k4r_get_characteristics(Link, CharacteristicList),
%   k4r_get_characteristic_by_name(CharacteristicList, "New Characteristic", Characteristic),
%   k4r_get_entity_id(Characteristic, CharacteristicId),
%   k4r_delete_characteristic(Link, CharacteristicId),
%   k4r_get_characteristics(Link, CharacteristicListNew),
%   writeln('Return characteristics without New Characteristic'),
%   writeln(CharacteristicListNew).

% % Property

% test('k4r_property_test_1') :-
%   k4r_get_link(Link),
%   k4r_get_properties(Link, "21", "0", PropertyList),
%   writeln('Return properties of product 0 in store 21:'),
%   writeln(PropertyList).

% test('k4r_property_test_2') :-
%   k4r_get_link(Link),
%   k4r_get_characteristics(Link, CharacteristicList),
%   k4r_get_characteristic_by_name(CharacteristicList, "price", Characteristic),
%   k4r_get_entity_id(Characteristic, CharacteristicId),
%   k4r_post_property(Link, "21", "0", CharacteristicId, "1000 Euro"),
%   k4r_get_properties(Link, "21", "0", PropertyList),
%   writeln('Return properties of product 0 in store 21 with price 1000 Euro:'),
%   writeln(PropertyList).

% test('k4r_property_test_3') :-
%   k4r_get_link(Link),
%   k4r_get_characteristics(Link, CharacteristicList),
%   k4r_get_characteristic_by_name(CharacteristicList, "price", Characteristic),
%   k4r_get_entity_id(Characteristic, CharacteristicId),
%   k4r_delete_property(Link, "21", "0", CharacteristicId),
%   k4r_get_properties(Link, "21", "0", PropertyList),
%   writeln('Return properties of product 0 in store 21 without characteristic Price:'),
%   writeln(PropertyList).

% % % Product

% test('k4r_product_test_1') :-
%   k4r_get_link(Link),
%   k4r_get_product_by_id(Link, 0, Product),
%   writeln('Return product with id 0:'),
%   writeln(Product).

% test('k4r_product_test_2') :-
%   k4r_get_link(Link),
%   k4r_get_products(Link, ProductList),
%   k4r_get_product_by_name(ProductList, "shampoo", Product),
%   writeln('Return product with name shampoo:'),
%   writeln(Product).

% test('k4r_product_test_3') :-
%   k4r_get_link(Link),
%   k4r_post_product(Link, "{
%     \"depth\" : 3,
%     \"description\" : \"a new product\",
%     \"gtin\" : \"whatisthis\",
%     \"height\" : 2,
%     \"length\" : 12,
%     \"name\" : \"new product\",
%     \"weight\" : 100
%   }", 10),
%   k4r_get_products(Link, ProductList),
%   writeln('Return products with product new product at id 10:'),
%   writeln(ProductList).

% test('k4r_product_test_4') :-
%   k4r_get_link(Link),
%   k4r_post_products(Link, "{
%     \"products\": 
%       [
%         {
%           \"depth\" : 5,
%           \"description\" : \"a new product 2\",
%           \"gtin\" : \"whatisthis\",
%           \"height\" : null,
%           \"id\" : 101,
%           \"length\" : 1,
%           \"name\" : \"shampoo\",
%           \"weight\" : 2
%         }
%         ,{
%           \"depth\" : 3,
%           \"description\" : \"a new product 3\",
%           \"gtin\" : \"whatisthis\",
%           \"height\" : null,
%           \"id\" : 102,
%           \"length\" : 1,
%           \"name\" : \"paper\",
%           \"weight\" : 4
%         }
%       ]
%     }"
%   ),
%   k4r_get_products(Link, ProductList),
%   writeln('Return products with products at id 101 and 102:'),
%   writeln(ProductList).

% test('k4r_product_test_5') :-
%   k4r_get_link(Link),
%   k4r_delete_product(Link, 10),
%   k4r_delete_product(Link, 101),
%   k4r_delete_product(Link, 102),
%   k4r_get_products(Link, ProductList),
%   writeln('Return products without products at id 10, 101 and 102:'),
%   writeln(ProductList).

% % % Shelf

% test('k4r_shelf_test_1') :-
%   k4r_get_link(Link),
%   k4r_get_shelves(Link, 21, ShelfList),
%   writeln('Return Shelves:'),
%   writeln(ShelfList).

% test('k4r_shelf_test_2') :-
%   k4r_get_link(Link),
%   k4r_get_shelf_by_id(Link, 74, Shelf),
%   writeln('Return Shelf at id 74:'),
%   writeln(Shelf).

% test('k4r_shelf_test_3') :-
%   k4r_get_link(Link),
%   k4r_post_shelf(Link, 21, "{
%     \"cadPlanId\" : \"an_new_cadPlanId\",
%     \"depth\" : 40,
%     \"externalReferenceId\" : \"an_new_extId\",
%     \"height\" : 30,
%     \"orientationY\" : 2,
%     \"orientationYaw\" : 4,
%     \"orientationZ\" : 3,
%     \"orientationx\" : 2,
%     \"positionX\" : 4,
%     \"positionY\" : 5,
%     \"positionZ\" : 6,
%     \"productGroupId\" : 3,
%     \"storeId\" : 21,
%     \"width\" : 20
%  }"),
%  k4r_get_shelves(Link, 21, ShelfList),
%  writeln('Return Shelves with shelf with externalReferenceId an_new_extId:'),
%  writeln(ShelfList).

% test('k4r_shelf_test_4') :-
%   k4r_get_link(Link),
%   k4r_get_shelves(Link, 21, ShelfList),
%   k4r_get_shelf_by_externalReferenceId(ShelfList, "an_new_extId", Shelf),
%   k4r_get_shelf_location(Shelf, ShelfPosX, ShelfPosY, ShelfPosZ, ShelfOrientationX, ShelfOrientationY, ShelfOrientationZ, ShelfOrientationW),
%   writeln('Position and Orientation:'),
%   writeln(ShelfPosX),
%   writeln(ShelfPosY),
%   writeln(ShelfPosZ),
%   writeln(ShelfOrientationX),
%   writeln(ShelfOrientationY),
%   writeln(ShelfOrientationZ),
%   writeln(ShelfOrientationW).

% test('k4r_shelf_test_5') :-
%   k4r_get_link(Link),
%   k4r_get_shelves(Link, 21, ShelfList),
%   k4r_get_shelf_by_externalReferenceId(ShelfList, "an_new_extId", Shelf),
%   k4r_get_entity_id(Shelf, ShelfId),
%   k4r_delete_shelf(Link, ShelfId),
%   k4r_get_shelves(Link, 21, ShelfListNew),
%   writeln('Return Shelves with shelf without externalReferenceId an_new_extId:'),
%   writeln(ShelfListNew).

% % ShelfLayer

% test('k4r_shelf_layer_test_1') :-
%   k4r_get_link(Link),
%   k4r_get_shelf_layers(Link, 74, ShelfLayerList),
%   writeln('Return shelflayers'),
%   writeln(ShelfLayerList).

% test('k4r_shelf_layer_test_2') :-
%   k4r_get_link(Link),
%   k4r_get_shelf_layer_by_id(Link, 57, ShelfLayer),
%   writeln('Return shelflayer at id 57'),
%   writeln(ShelfLayer).

% test('k4r_shelf_layer_test_3') :-
%   k4r_get_link(Link),
%   k4r_post_shelf_layer(Link, 74, "{
%     \"level\": 123,
%     \"type\": \"type123\",
%     \"positionZ\": 34,
%     \"width\": 32,
%     \"height\": 63,
%     \"depth\": 16,
%     \"externalReferenceId\": \"a ref Id\"
%   }").

% test('k4r_shelf_layer_test_4') :-
%   k4r_get_link(Link),
%   k4r_get_shelf_layers(Link, 74, ShelfLayerList),
%   k4r_get_shelf_layer_by_externalReferenceId(ShelfLayerList, "a ref Id", ShelfLayer),
%   k4r_get_entity_id(ShelfLayer, ShelfLayerId),
%   k4r_delete_shelf_layer(Link, ShelfLayerId),
%   writeln('Return ShelfLayers without shelflayer externalReferenceId an_new_extId:'),
%   k4r_get_shelf_layers(Link, 74, ShelfLayerListNew),
%   writeln(ShelfLayerListNew).

% Shopping basket

% test('k4r_shopping_basket_test_1') :-
%   k4r_get_link(Link),
%   k4r_get_shopping_baskets(Link, 21, 23, ShoppingBasketList),
%   writeln('Return shopping baskets'),
%   writeln(ShoppingBasketList).

% test('k4r_shopping_basket_test_2') :-
%   k4r_get_link(Link),
%   k4r_get_shopping_basket_by_id(Link, 21, 23, 0, ShoppingBasket),
%   writeln('Return shopping basket with product id 0'),
%   writeln(ShoppingBasket).

% test('k4r_shopping_basket_test_3') :-
%   k4r_get_link(Link),
%   k4r_post_shopping_basket(Link, 21, 23, "{
%     \"productId\": \"2\",
%     \"sellingPrice\": 5.99,
%     \"quantity\": 50
%   }").

% test('k4r_shopping_basket_test_4') :-
%   k4r_get_link(Link),
%   k4r_delete_shopping_basket(Link, 21, 23, 2),
%   k4r_get_shopping_baskets(Link, 21, 23, ShoppingBasketList),
%   writeln('Return shopping baskets without product id 2'),
%   writeln(ShoppingBasketList).

:- end_tests(k4r_db_client).
