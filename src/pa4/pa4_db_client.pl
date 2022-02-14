:- module(pa4_db_client,
          [
            post_fridge_store/1,
            post_fridge_shelf/1,
            get_store_id/2,
            get_store/3,
            post_fridge_shelf_layers/1,
            post_items_in_store/1
          ]).

:- use_foreign_library('libk4r_db_client.so').
:- use_module(library('k4r_db/k4r_db_client')).
:- use_module(library('shop_reasoner')).
:- use_module(library('semweb/rdf_db'),
    [ rdf_split_url/3 ]).

:- rdf_db:rdf_register_ns(soma,
    'http://www.ease-crc.org/ont/SOMA.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(shop, 'http://knowrob.org/kb/shop.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(urdf, 'http://knowrob.org/kb/urdf.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(fridge, 'http://knowrob.org/kb/fridge.owl#', [keep(true)]).


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
    triple(Store, shop:hasShopNumber, StoreNumber), 
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
    % This works only because fridge has only one shelf 
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

post_items_in_store(StoreNumber) :-
    get_store_id(StoreNumber, StoreId),
    % Check if the shelf is in the store
    get_shelves_data(StoreId, [id, externalReferenceId], Value),
    writeln(["ShelfData",Value]),
    forall(member([ShelfPlId, ShelfExtRefId], Value),
        ( writeln(["PId", ShelfPlId]),
        atom_number(ShelfExtRefId, NumId),
        %get_shelf_with_external_id(NumId, Shelf),
        triple(Shelf, shop:erpShelfId, NumId),
        post_items_in_shelf(Shelf, ShelfPlId))
    ).

post_items_in_shelf(Shelf, _) :-
    \+ triple(Shelf, soma:hasPhysicalComponent, _),
    print_message(info, 'Shelf has no layers').

post_items_in_shelf(Shelf, PlatformShelfId) :-
    writeln("post_items_in_shelf"),
    get_layer_and_facing_data(PlatformShelfId, Key{shelfLayers: [externalReferenceId], facings: [id, layerRelativePosition]}, LayersDict),
    forall(member(LayerDict, LayersDict.shelfLayers),
        (atom_number(LayerDict.externalReferenceId, NumId),
        triple(Layer, shop:erpShelfLayerId, NumId),
        triple(Shelf, soma:hasPhysicalComponent, Layer),
        writeln([Layer,LayerDict.facings]),
        post_items_in_layer(LayerDict.facings, Layer)
    )).

post_items_in_layer([], _).

post_items_in_layer([FacingDict | Rest], Layer) :-
    writeln("post_items_in_layer"),
    %integer(FacingDict.layerRelativePosition),
    writeln([FacingDict.layerRelativePosition, FacingDict.id]),
    %atom_number(FacingDict.layerRelativePosition, FacingRelPos),
    triple(Facing, shop:erpFacingId, FacingDict.layerRelativePosition),
    triple(Facing, shop:layerOfFacing, Layer),
    writeln(Facing),
    rdf_split_url(_,FacingFrame,Facing),
    shopping:get_all_items_in_facing(Facing, Items),
    post_items_in_facing(Items, FacingFrame, FacingDict.id),
    post_items_in_layer(Rest, Layer).
    %).

post_items_in_facing([], _, _).

post_items_in_facing([Item | Rest], ParentName, FacingPlatformId) :-
    writeln(["Items", Item]),
    has_type(Item, Product),
    get_product_gtin(Product, Gtin),
    k4r_db_client:make_gtin_filter(Gtin, GtinFilter),
    k4r_db_client:get_product_unit_data_with_filter(GtinFilter, [productUnitId], [ProductUnitId]),
    writeln(["PUnitId", ProductUnitId]),
    % TODO : Enable the count
    %shop_reasoner:get_number_of_items_in_facing(Facing, NoOfItems),

    is_at(Item, [ParentName, [X, Y, Z], _]),
    k4r_db_client:double_m_to_int_mm([X, Y, Z], [X_mm, Y_mm, Z_mm]),

    post_item_group([FacingPlatformId, ProductUnitId, 1], ItemGroup),
    k4r_db_client:get_entity_id(ItemGroup, ItemGroupId),
    post_item([ItemGroupId, X_mm, Y_mm, Z_mm], _),
    post_items_in_facing(Rest, ParentName, FacingPlatformId).


get_layer_and_facing_data(PlatformShelfId, KeyList, LayersDict) :- % test{shelfLayers: [externalReferenceId], facings: [id, layerRelativePosition]}
    k4r_db_client:make_store_id_filter(PlatformShelfId, Filter),
    list_to_string_(KeyList.shelfLayers, LayerKeys),
    list_to_string_(KeyList.facings, FacingKeys),
    string_concat("{shelves", Filter, ShelfFilter),
    string_concat("{shelfLayers{", LayerKeys, LayerFilter),
    string_concat(" facings{", FacingKeys, FacingFilter),
    writeln([LayerFilter, FacingFilter]),
    atomics_to_string([ShelfFilter, LayerFilter, FacingFilter,"}}}}"], GraphQLQuery),
    get_graphql(GraphQLQuery, GraphQLResponse),
    writeln(GraphQLResponse.shelves),
    member(LayersDict, GraphQLResponse.shelves).
    % forall(member(Layer, LayersDict.shelfLayers),
    % (writeln(Layer.externalReferenceId),
    % writeln(Layer.facings))
    % ).

list_to_string_(KeyList, KeyString) :-
    atomic_list_concat(KeyList, ',', KeyListConcat),
    atom_string(KeyListConcat, KeyString).

get_store_id(StoreNum, StoreId) :-
    get_store_filter_("storeNumber", "eq", StoreNum, "string", StoreFilter),
    atomics_to_string([StoreFilter, "{", id, "}}"], GraphQLQuery),
    get_graphql(GraphQLQuery, GraphQLResponse),
    member(StoreDict, GraphQLResponse.stores),
    StoreId = StoreDict.id,
    writeln(['id', StoreId]).

get_store(StoreNum, StoreParam, Store) :-
    get_store_filter_("storeNumber", "eq", StoreNum, "string", StoreFilter),
    list_to_string_(StoreParam, KeyParamStr),
    atomics_to_string([StoreFilter, "{", KeyParamStr,  "}}"], Query),
    get_graphql(GraphQLQuery, GraphQLResponse),
    writeln(GraphQLResponse).

get_store_filter_(Param, Op, Value, Type, StoreFilter) :-
    k4r_db_client:make_filter(Param, Op, Value, Type, Filter),
    string_concat("{stores", Filter, StoreFilter).