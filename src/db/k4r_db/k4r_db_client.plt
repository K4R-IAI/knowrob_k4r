:- use_module('k4r_db_client.pl').
:- use_module(library('rostest')).

:- begin_tripledb_tests(
        'k4r_db_client',
        'package://knowrob/owl/test/test_owl.owl'
  ).

% Customer

test('k4r_customer_test_1') :-
  k4r_get_link(Link),
  k4r_get_customer_by_id(Link, 10, Customer),
  writeln('Return customer with id 10:'),
  writeln(Customer).

test('k4r_customer_test_2') :-
  k4r_get_link(Link),
  k4r_get_customers(Link, CustomerList),
  k4r_get_entity_by_key_value(CustomerList, "anonymisedName", "Megan Firefox", Customer),
  writeln('Return customer with name Megan Firefox:'),
  writeln(Customer).

test('k4r_customer_test_3') :-
  k4r_get_link(Link),
  k4r_post_customer(Link, "TaylorSwift"),
  k4r_get_customers(Link, CustomerList),
  k4r_get_entity_by_key_value(CustomerList, "anonymisedName", "TaylorSwift", Customer),
  k4r_get_entity_id(Customer, CustomerId),
  k4r_put_customer(Link, CustomerId, "Taylor Swift"),
  writeln('Return customer with name Taylor Swift:'),
  writeln(Customer).

test('k4r_customer_test_4') :-
  k4r_get_link(Link),
  k4r_get_customers(Link, CustomerList),
  k4r_get_entity_by_key_value(CustomerList, "anonymisedName", "Taylor Swift", Customer),
  k4r_get_entity_id(Customer, CustomerId),
  k4r_delete_customer(Link, CustomerId),
  k4r_get_customers(Link, CustomerNewList),
  writeln('Return customer list without customer name Taylor Swift:'),
  writeln(CustomerNewList).

% Store

test('k4r_store_test_1') :-
  k4r_get_link(Link),
  k4r_get_stores(Link, StoreList),
  k4r_get_entity_by_key_value(StoreList, "storeName", "Refills Lab", Store),
  writeln('Return store with name Refills Lab:'),
  writeln(Store).

test('k4r_store_test_2') :-
  k4r_get_link(Link),
  k4r_post_store(Link, "{
    \"addressAdditional\" : \"Nothing\",
    \"addressCity\" : \"Bremen\",
    \"addressCountry\" : \"DE\",
    \"addressPostcode\" : \"28211\",
    \"addressState\" : \"Land Bremen\",
    \"addressStreet\" : \"Heinrich-Hertz-Strasse\",
    \"addressStreetNumber\" : \"19\",
    \"cadPlanId\" : \"123CAD321\",
    \"latitude\" : 12.3455,
    \"longitude\" : 46.321,
    \"storeName\" : \"Giang Lab\",
    \"storeNumber\" : \"123ABC\"
  }"),
  k4r_get_stores(Link, StoreList),
  k4r_get_entity_by_key_value(StoreList, "storeName", "Giang Lab", Store),
  writeln('Return store with name Giang Lab:'),
  writeln(Store).

test('k4r_store_test_3') :-
  k4r_get_link(Link),
  k4r_get_link(Link),
  k4r_get_stores(Link, StoreList),
  k4r_get_entity_by_key_value(StoreList, "storeName", "Giang Lab", Store),
  k4r_get_entity_id(Store, StoreId),
  k4r_put_store(Link, StoreId, "{
    \"addressAdditional\" : \"Changed\",
    \"addressCity\" : \"REFD\",
    \"addressCountry\" : \"DE\",
    \"addressPostcode\" : \"23232\",
    \"addressState\" : \"EDFG\",
    \"addressStreet\" : \"ABCD\",
    \"addressStreetNumber\" : \"19\",
    \"cadPlanId\" : \"123CAD321\",
    \"latitude\" : 12.43,
    \"longitude\" : 32.435,
    \"storeName\" : \"Giang Lab\",
    \"storeNumber\" : \"ABC123\"
  }"),
  k4r_get_stores(Link, StoreList),
  k4r_get_entity_by_key_value(StoreList, "storeName", "Giang Lab", Store),
  writeln('Return changed store with name Giang Lab:'),
  writeln(Store).

test('k4r_store_test_4') :-
  k4r_get_link(Link),
  k4r_get_stores(Link, StoreList),
  k4r_get_entity_by_key_value(StoreList, "storeName", "Giang Lab", Store),
  k4r_get_entity_id(Store, StoreId),
  k4r_delete_store(Link, StoreId),
  k4r_get_stores(Link, StoreListNew),
  writeln('Return stores without store name Giang Lab:'),
  writeln(StoreListNew).

% Characteristic

test('k4r_characteristic_test_1') :-
  k4r_get_link(Link),
  k4r_get_characteristics(Link, CharacteristicList),
  writeln('Return characteristics:'),
  writeln(CharacteristicList).

test('k4r_characteristic_test_2') :-
  k4r_get_link(Link),
  k4r_post_characteristic(Link, "New Characteristic"),
  k4r_get_characteristics(Link, CharacteristicList),
  writeln('Return characteristics with New Characteristic'),
  writeln(CharacteristicList).

test('k4r_characteristic_test_3') :-
  k4r_get_link(Link),
  k4r_get_characteristics(Link, CharacteristicList),
  k4r_get_entity_by_key_value(CharacteristicList, "name", "New Characteristic", Characteristic),
  k4r_get_entity_id(Characteristic, CharacteristicId),
  k4r_delete_characteristic(Link, CharacteristicId),
  k4r_get_characteristics(Link, CharacteristicListNew),
  writeln('Return characteristics without New Characteristic'),
  writeln(CharacteristicListNew).

% Property

test('k4r_property_test_1') :-
  k4r_get_link(Link),
  k4r_get_properties(Link, "2", "A1", PropertyList),
  writeln('Return properties of product A1 in store 2:'),
  writeln(PropertyList).

test('k4r_property_test_2') :-
  k4r_get_link(Link),
  k4r_get_characteristics(Link, CharacteristicList),
  k4r_get_entity_by_key_value(CharacteristicList, "name", "Review", Characteristic),
  k4r_get_entity_id(Characteristic, CharacteristicId),
  k4r_post_property(Link, "2", "A1", CharacteristicId, "Good Review"),
  k4r_get_properties(Link, "2", "A1", PropertyList),
  writeln('Return properties of product A1 in store 2 with Good Review:'),
  writeln(PropertyList).

test('k4r_property_test_3') :-
  k4r_get_link(Link),
  k4r_get_characteristics(Link, CharacteristicList),
  k4r_get_entity_by_key_value(CharacteristicList, "name", "Review", Characteristic),
  k4r_get_entity_id(Characteristic, CharacteristicId),
  k4r_delete_property(Link, "2", "A1", CharacteristicId),
  k4r_get_properties(Link, "2", "A1", PropertyList),
  writeln('Return properties of product A1 in store 2 without Good Review:'),
  writeln(PropertyList).

% Product

test('k4r_product_test_1') :-
  k4r_get_link(Link),
  k4r_get_product_by_id(Link, "A1", Product),
  writeln('Return product with id A1:'),
  writeln(Product).

test('k4r_product_test_2') :-
  k4r_get_link(Link),
  k4r_get_products(Link, ProductList),
  k4r_get_entity_by_key_value(ProductList, "name", "Shampoo", Product),
  writeln('Return product with name Shampoo:'),
  writeln(Product).

test('k4r_product_test_3') :-
  k4r_get_link(Link),
  k4r_post_product(Link, "{
    \"depth\" : 3,
    \"description\" : \"a new product\",
    \"gtin\" : \"whatisthis\",
    \"height\" : 2,
    \"length\" : 12,
    \"name\" : \"new product\",
    \"weight\" : 100
  }", "A10"),
  k4r_get_products(Link, ProductList),
  writeln('Return products with product new product at id A10:'),
  writeln(ProductList).

test('k4r_product_test_4') :-
  k4r_get_link(Link),
  k4r_put_product(Link, "{
    \"depth\" : 3,
    \"description\" : \"a new changed product\",
    \"gtin\" : \"whatisthis\",
    \"height\" : 2,
    \"length\" : 12,
    \"name\" : \"new changed product\",
    \"weight\" : 100
  }", "A10"),
  k4r_get_products(Link, ProductList),
  writeln('Return products with product new changed product at id A10:'),
  writeln(ProductList).

test('k4r_product_test_5') :-
  k4r_get_link(Link),
  k4r_post_products(Link, "{
    \"products\": 
      [
        {
          \"depth\" : 5,
          \"description\" : \"a new product 2\",
          \"gtin\" : \"whatisthis\",
          \"height\" : null,
          \"id\" : \"A101\",
          \"length\" : 1,
          \"name\" : \"shampoo\",
          \"weight\" : 2
        }
        ,{
          \"depth\" : 3,
          \"description\" : \"a new product 3\",
          \"gtin\" : \"whatisthis\",
          \"height\" : null,
          \"id\" : \"A102\",
          \"length\" : 1,
          \"name\" : \"paper\",
          \"weight\" : 4
        }
      ]
    }"
  ),
  k4r_get_products(Link, ProductList),
  writeln('Return products with products at id A101 and A102:'),
  writeln(ProductList).

test('k4r_product_test_6') :-
  k4r_get_link(Link),
  k4r_delete_product(Link, "A10"),
  k4r_delete_product(Link, "A101"),
  k4r_delete_product(Link, "A102"),
  k4r_get_products(Link, ProductList),
  writeln('Return products without products at id A10, A101 and A102:'),
  writeln(ProductList).

% Shelf

test('k4r_shelf_test_1') :-
  k4r_get_link(Link),
  k4r_get_shelves(Link, 2, ShelfList),
  writeln('Return Shelves:'),
  writeln(ShelfList).

test('k4r_shelf_test_2') :-
  k4r_get_link(Link),
  k4r_get_shelf_by_id(Link, 1, Shelf),
  writeln('Return Shelf at id 1:'),
  writeln(Shelf).

test('k4r_shelf_test_3') :-
  k4r_get_link(Link),
  k4r_post_shelf(Link, 2, "{
    \"cadPlanId\" : \"an_new_cadPlanId\",
    \"depth\" : 40,
    \"externalReferenceId\" : \"R4\",
    \"height\" : 30,
    \"orientationY\" : 2,
    \"orientationYaw\" : 4,
    \"orientationZ\" : 3,
    \"orientationX\" : 2,
    \"positionX\" : 4,
    \"positionY\" : 5,
    \"positionZ\" : 6,
    \"productGroupId\" : 3,
    \"width\" : 20
 }"),
 k4r_get_shelves(Link, 2, ShelfList),
 writeln('Return Shelves with shelf with externalReferenceId R4:'),
 writeln(ShelfList).

test('k4r_shelf_test_4') :-
  k4r_get_link(Link),
  k4r_get_shelves(Link, 2, ShelfList),
  k4r_get_entity_by_key_value(ShelfList, "externalReferenceId", "R4", Shelf),
  k4r_get_shelf_location(Shelf, ShelfPosX, ShelfPosY, ShelfPosZ, ShelfOrientationX, ShelfOrientationY, ShelfOrientationZ, ShelfOrientationW),
  writeln('Position and Orientation:'),
  writeln(ShelfPosX),
  writeln(ShelfPosY),
  writeln(ShelfPosZ),
  writeln(ShelfOrientationX),
  writeln(ShelfOrientationY),
  writeln(ShelfOrientationZ),
  writeln(ShelfOrientationW).

test('k4r_shelf_test_5') :-
  k4r_get_link(Link),
  k4r_get_shelves(Link, 2, ShelfList),
  k4r_get_entity_by_key_value(ShelfList, "externalReferenceId", "R4", Shelf),
  k4r_get_entity_id(Shelf, ShelfId),
  k4r_put_shelf(Link, ShelfId, "{
    \"cadPlanId\" : \"an_new_cadPlanId\",
    \"depth\" : 32,
    \"externalReferenceId\" : \"R4\",
    \"height\" : 12,
    \"orientationY\" : 23,
    \"orientationYaw\" : 32,
    \"orientationZ\" : 312,
    \"orientationX\" : 32,
    \"positionX\" : 432,
    \"positionY\" : 421,
    \"positionZ\" : 14,
    \"productGroupId\" : 3,
    \"storeId\" : 2,
    \"width\" : 20
 }"),
 k4r_get_shelves(Link, 2, ShelfList),
 writeln('Return Shelves with shelf with externalReferenceId R4:'),
 writeln(ShelfList).

test('k4r_shelf_test_6') :-
  k4r_get_link(Link),
  k4r_get_shelves(Link, 2, ShelfList),
  k4r_get_entity_by_key_value(ShelfList, "externalReferenceId", "R4", Shelf),
  k4r_get_entity_id(Shelf, ShelfId),
  k4r_delete_shelf(Link, ShelfId),
  k4r_get_shelves(Link, 2, ShelfListNew),
  writeln('Return Shelves with shelf without externalReferenceId R4:'),
  writeln(ShelfListNew).

% ShelfLayer

test('k4r_shelf_layer_test_1') :-
  k4r_get_link(Link),
  k4r_get_shelf_layers(Link, 1, ShelfLayerList),
  writeln('Return shelflayers'),
  writeln(ShelfLayerList).

test('k4r_shelf_layer_test_2') :-
  k4r_get_link(Link),
  k4r_get_shelf_layer_by_id(Link, 2, ShelfLayer),
  writeln('Return shelflayer at id 2'),
  writeln(ShelfLayer).

test('k4r_shelf_layer_test_3') :-
  k4r_get_link(Link),
  k4r_post_shelf_layer(Link, 1, "{
    \"level\": 123,
    \"type\": \"type123\",
    \"positionZ\": 34,
    \"width\": 32,
    \"height\": 63,
    \"depth\": 16,
    \"externalReferenceId\": \"E4\"
  }").

test('k4r_shelf_layer_test_4') :-
  k4r_get_link(Link),
  k4r_get_shelf_layers(Link, 1, ShelfLayerList),
  k4r_get_entity_by_key_value(ShelfLayerList, "externalReferenceId", "E4", ShelfLayer),
  k4r_get_entity_id(ShelfLayer, ShelfLayerId),
  k4r_delete_shelf_layer(Link, ShelfLayerId),
  writeln('Return ShelfLayers without shelflayer externalReferenceId E4:'),
  k4r_get_shelf_layers(Link, 1, ShelfLayerListNew),
  writeln(ShelfLayerListNew).

% Shopping basket

test('k4r_shopping_basket_test_1') :-
  k4r_get_link(Link),
  k4r_get_shopping_basket_positions(Link, 2, 1, ShoppingBasketList),
  writeln('Return shopping baskets of store id 2, customer id 1'),
  writeln(ShoppingBasketList).

test('k4r_shopping_basket_test_2') :-
  k4r_get_link(Link),
  k4r_get_shopping_basket_position_by_id(Link, 5, ShoppingBasket),
  writeln('Return shopping basket with id 5'),
  writeln(ShoppingBasket).

test('k4r_shopping_basket_test_3') :-
  k4r_get_link(Link),
  k4r_post_shopping_basket_position(Link, 2, 1, "{
    \"productId\": \"A3\",
    \"sellingPrice\": 3.99,
    \"quantity\": 6,
    \"currency\": \"yen\"
  }").

test('k4r_shopping_basket_test_4') :-
  k4r_get_link(Link),
  k4r_get_shopping_basket_positions(Link, 2, 1, ShoppingBasketList),
  k4r_get_entity_by_key_value(ShoppingBasketList, "productId", "A3", ShoppingBasket),
  k4r_get_entity_id(ShoppingBasket, ShoppingBasketId),
  k4r_delete_shopping_basket_position(Link, ShoppingBasketId),
  k4r_get_shopping_basket_positions(Link, 2, 1, ShoppingBasketListNew),
  writeln('Return shopping baskets without product id A3'),
  writeln(ShoppingBasketListNew).

% Facing

test('k4r_facing_test_1') :-
  k4r_get_link(Link),
  k4r_get_facings(Link, 1, FacingList),
  writeln('Return facings on shelf layer 1'),
  writeln(FacingList).

test('k4r_facing_test_2') :-
  k4r_get_link(Link),
  k4r_get_facing_by_id(Link, 2, Facing),
  writeln('Return facings at id 2'),
  writeln(Facing).

test('k4r_facing_test_3') :-
  k4r_get_link(Link),
  k4r_post_facing(Link, 1, "{
    \"shelfLayerId\": 1,
    \"productId\": \"A3\",
    \"layerRelativePosition\": 4,
    \"quantity\": 2
  }").

test('k4r_facing_test_4') :-
  k4r_get_link(Link),
  k4r_get_facings(Link, 1, FacingList),
  k4r_get_entity_by_key_value(FacingList, "layerRelativePosition", 4, Facing),
  k4r_get_entity_id(Facing, FacingId),
  k4r_put_facing(Link, FacingId, "{
    \"shelfLayerId\": 1,
    \"productId\": \"A3\",
    \"layerRelativePosition\": 3,
    \"quantity\": 1
  }"),
  writeln('Return facings with facing layerRelativePosition 3:'),
  k4r_get_facings(Link, 1, FacingListNew),
  writeln(FacingListNew).

test('k4r_facing_test_5') :-
  k4r_get_link(Link),
  k4r_get_facings(Link, 1, FacingList),
  k4r_get_entity_by_key_value(FacingList, "layerRelativePosition", 3, Facing),
  k4r_get_entity_id(Facing, FacingId),
  k4r_delete_facing(Link, FacingId),
  writeln('Return facings without facing layerRelativePosition 3:'),
  k4r_get_facings(Link, 1, FacingListNew),
  writeln(FacingListNew).

test('k4r_get_product_by_shelf_test') :-
  k4r_get_link(Link),
  k4r_get_shelf_by_id(Link, 1, Shelf),
  forall(k4r_get_product_by_shelf(Link, Shelf, Product), writeln(Product)).

% Planogram

test('k4r_planogram_test_1') :-
  k4r_get_link(Link),
  k4r_get_planograms(Link, PlanogramList),
  writeln('Return planograms:'),
  writeln(PlanogramList).

test('k4r_customer_test_2') :-
  k4r_get_link(Link),
  k4r_post_planogram(Link, "{
        \"numberOfFacings\": 2,
        \"orientationYaw\": 2,
        \"positionX\": 33,
        \"productId\": \"A2\",
        \"shelfLayerId\": 2,
        \"versionTimestamp\": 112
      }"),
  k4r_get_planograms(Link, PlanogramList),
  k4r_get_entity_by_key_value(PlanogramList, "positionX", 33, Planogram),
  k4r_get_entity_id(Planogram, PlanogramId),
  k4r_put_planogram(Link, PlanogramId, "{
        \"numberOfFacings\": 3,
        \"orientationYaw\": 1,
        \"positionX\": 35,
        \"productId\": \"A1\",
        \"shelfLayerId\": 3,
        \"versionTimestamp\": 321
      }"),
  k4r_get_planograms(Link, PlanogramListNew),
  writeln('Return planograms with new planogram:'),
  writeln(PlanogramListNew).

test('k4r_customer_test_3') :-
  k4r_get_link(Link),
  k4r_get_planograms(Link, PlanogramList),
  k4r_get_entity_by_key_value(PlanogramList, "positionX", 35, Planogram),
  k4r_get_entity_id(Planogram, PlanogramId),
  k4r_delete_planogram(Link, PlanogramId),
  k4r_get_planograms(Link, PlanogramListNew),
  writeln('Return planograms without new planogram with positionX 35:'),
  writeln(PlanogramListNew).

% Product group

test('k4r_product_group_test_1') :-
  k4r_get_link(Link),
  k4r_get_product_groups(Link, 2, ProductGroupList),
  writeln('Return product groups:'),
  writeln(ProductGroupList).

test('k4r_product_group_test_2') :-
  k4r_get_link(Link),
  k4r_get_product_group_by_id(Link, 1, ProductGroup),
  writeln('Return product group at id 1:'),
  writeln(ProductGroup).

test('k4r_product_group_test_3') :-
  k4r_get_link(Link),
  k4r_post_product_to_product_group(Link, 1, "A2"),
  k4r_get_product_group_by_id(Link, 1, ProductGroup),
  writeln('Return product group 1 with product A2:'),
  writeln(ProductGroup).

test('k4r_product_group_test_4') :-
  k4r_get_link(Link),
  k4r_post_product_group(Link, 2, "group new"),
  k4r_get_product_groups(Link, 2, ProductGroupList),
  writeln('Return product groups with new group:'),
  writeln(ProductGroupList).

test('k4r_product_group_test_5') :-
  k4r_get_link(Link),
  k4r_get_products(Link, ProductList),
  k4r_get_entity_by_key_value(ProductList, "id", "A2", Product),
  k4r_get_entity_id(Product, ProductId),
  k4r_delete_product_from_product_group(Link, 1, ProductId),
  k4r_get_product_group_by_id(Link, 1, ProductGroup),
  writeln('Return product group 1 without product A2:'),
  writeln(ProductGroup).

test('k4r_product_group_test_6') :-
  k4r_get_link(Link),
  k4r_get_product_groups(Link, 2, ProductGroupList),
  k4r_get_entity_by_key_value(ProductGroupList, "name", "group new", ProductGroup),
  k4r_get_entity_id(ProductGroup, ProductGroupId),
  k4r_delete_product_group(Link, ProductGroupId),
  k4r_get_product_groups(Link, 2, ProductGroupListNew),
  writeln('Return product groups without new group:'),
  writeln(ProductGroupListNew).

:- end_tests(k4r_db_client).
