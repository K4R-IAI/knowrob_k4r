:- module(pa4_db_client,
          [
            post_fridge_store/1,
            post_fridge_shelf/1,
            get_store_id/2,
            post_fridge_shelf_layers/1
          ]).

:- use_foreign_library('libk4r_db_client.so').
:- use_module(library('k4r_db/k4r_db_client')).
:- use_module(library('shop_reasoner')).


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
    get_shelves_data(StoreId, [id, externalReferenceId], Data),
    writeln(Data),
    get_unit_id('meter', UnitId),
    writeln(['Id', UnitId]),
    forall(member([ShelfId, ExtRefId], Data),
        (atom_number(ExtRefId, NumId),
        get_shelf_with_external_id(NumId, Shelf),
        get_layers_in_shelf(Shelf, Layers),
        post_shelf_layers_of_shelf(ShelfId, UnitId, Layers))
    ).

post_shelf_layers_of_shelf(ShelfId, UnitId, [Layer | Rest]) :-
    % get associated shelf ids
    % 
    % triple(Shelf, soma:hasPhysicalComponent, Layer),
    % has_type(Layer, shop:'ShelfLayer'),
    writeln(['layer', Layer]),
    object_dimensions(Layer, D, W, H),
    writeln(['dim',D, W, H]),
    is_at(Layer, [_, T, _]),
    writeln(['pose',T, R]),
    [_,_,Z] = T,
    triple(Layer, shop:erpShelfLayerId, LayerId),
    post_shelf_layer(ShelfId, [D, LayerId, H, LayerId, Z, "null", W, UnitId], _),
    post_shelf_layers_of_shelf(ShelfId, UnitId, Rest).


post_shelf_layers_of_shelf(_, _, []).

post_fridge_facings(StoreNumber) :-
    get_store_id(StoreNumber, StoreId),
    get_shelf_layers_data(StoreId, [id, externalReferenceId], Value),
    writeln(["shelf", Value]),
    post_fridge_facings_of_layer(Value).

post_fridge_facings_of_layer([[LayerId, ExtId] | Rest]) :-
    atom_number(ExtId, NumId),
    triple(Layer, shop:erpShelfLayerId, NumId),
    % forcing the loop to continue even if there is a failure in posting
    (triple(Facing, shop:layerOfFacing, Layer),
    writeln(Facing),
    k4r_db_client:post_facings(Layer, LayerId)); true,
    post_fridge_facings_of_layer(Rest).

post_fridge_facings_of_layer([]).

get_store_id(StoreNum, StoreId) :-
    k4r_db_client:make_filter("storeNumber", "eq", "fridge1", "string", Filter),
    string_concat("{stores", Filter, StoreFilter),
    atomics_to_string([StoreFilter, "{", id, "}}"], GraphQLQuery),
    get_graphql(GraphQLQuery, GraphQLResponse),
    member(StoreDict, GraphQLResponse.stores),
    StoreId = StoreDict.id,
    writeln(['id', StoreId]).