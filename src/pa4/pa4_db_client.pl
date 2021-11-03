:- module(pa4_db_client,
          [
            post_fridge_store/1,
            post_fridge_shelf/1,
            get_store_id/2
          ]).

:- use_foreign_library('libk4r_db_client.so').
:- use_module(library('k4r_db/k4r_db_client')).


%%% Fridge store number is 'fridge_1', name is "FridgePA4"
post_fridge_store(StoreNumberInternal):-
    post_store(
        ["null", 
        "addressCity test", 
        "addressCountry test", 
        "addressPostcode test", 
        "addressState test", 
        "addressStreet test", 
        "addressStreetNumber test", 
        "cadPlanId test",
        21.1,
        21.2,
        "fridgepa4",
        StoreNumberInternal], 
        Store).

post_fridge_shelf(StoreNumber) :-
    triple(Store, shop:hasShopId, StoreNumber), 
    has_location(Fridge, Store),
    triple(Fridge, soma:hasPhysicalComponent, Frame),
    has_type(Frame, shop:'ShelfFrame'),
    triple(Frame, shop:erpShelfId, ShelfId),
    % writeln(['shlefid', Frame, ShelfId]),
    % ShelfBase
    triple(Frame, soma:hasPhysicalComponent, ShelfBase),
    has_type(ShelfBase, shop:'ShelfBase'),
    % writeln(['base', ShelfBase]),
    object_dimensions(ShelfBase, D, W, H),
    % writeln(['dim',D, W, H]),
    is_at(ShelfBase, ['base_link', T, R]),
    % writeln(['pose',T, R]),
    get_store_id(StoreNumber, StoreId),
    % writeln([StoreId]),
    [X, Y, Z] = T,
    % writeln([ X, Y, Z]),
    [X1,Y1,Z1,W1] = R, 
    % writeln([ X1, Y1, Z1, W]),
    get_unit_id('meter', UnitId), % UnitId id 1 for meter
    ProductGroupId is 416, % manually added
    post_shelf(StoreId, ProductGroupId, ["null", D,ShelfId,H ,W1, X1,Y1,Z1,X, Y,Z, W, UnitId], Shelf).


post_fridge_shelf_layers(StoreNumber) :-
    get_store_id(StoreNumber, StoreId),
    get_shelf_ids(StoreId, ShelfIds),
    writeln(ShelfIds).


get_store_id(StoreNum, StoreId) :-
    writeln(StoreNum),
    get_graphql("{stores", ["storeNumber", ["eq", StoreNum, "String"]], "{id}}", GraphQLResponse),
    member(StoreDict, GraphQLResponse.stores),
    StoreId = StoreDict.id,
    writeln(['id', StoreId]).


    