:- module(pa4_db_client,
          [
            get_facing_id/2,
            get_layer_id/2,
            get_store_id/2,
            get_store/3,
            get_product_unit_id/2,
            get_item_group_id/3,
            get_all_shelf_data/3,
            get_all_items/2,
            get_item_data/2,
            get_facing_data/2,
            get_all_item_groups/2,
            get_all_facings_platform/2,
            get_all_layers_platform/2,
            get_gtin/2,
            get_product_dimenion_platform/4,
            get_store_param/1,
            get_shelf_param/1,
            get_layer_param/1,
            get_item_param/1,
            post_fridge_store/2,
            post_fridge_shelf/1,
            post_fridge_shelf/6,
            post_fridge_shelf_layer/5,
            post_fridge_shelf_layers/1,
            post_fridge_facing/6,
            post_fridge_facings/1,
            post_items_in_store/1,
            update_item_position_platform/2,
            update_stock/2,
            delete_item_and_update_facing/1
          ]).

%:- use_foreign_library('libk4r_db_client.so').
:- use_module(library('k4r_db/k4r_db_client')).
:- use_module(library('shop_reasoner')).
:- use_module(library('semweb/rdf_db'),
    [ rdf_split_url/3 ]).

:- rdf_db:rdf_register_ns(soma,
    'http://www.ease-crc.org/ont/SOMA.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(shop, 'http://knowrob.org/kb/shop.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(urdf, 'http://knowrob.org/kb/urdf.owl#', [keep(true)]).
% :- rdf_db:rdf_register_ns(fridge, 'http://knowrob.org/kb/fridge.owl#', [keep(true)]).


%%% Fridge store number is 'fridge_1', name is "FridgePA4"
post_fridge_store(StoreData, Store):-
    post_store(StoreData, Store).

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
    post_shelf(StoreId, ProductGroupId, ["null", D,ShelfId,H ,W1, X1,Y1,Z1,X, Y,Z, W, UnitId], _).

post_fridge_shelf(StorePlatformId, [H, W1, D], [X1, Y1, Z1], [X, Y, Z, W], ExtRefId, ShelfPosted) :-
    get_unit_id('meter', UnitId), % UnitId id 1 for meter
    ProductGroupId is 416, % manually added
    post_shelf(StorePlatformId, ProductGroupId, ["null", D,ExtRefId,H ,W, X, Y, Z, X1, Y1, Z1, W1, UnitId], ShelfPosted).


post_fridge_shelf_layers(StoreNumber) :-
    get_store_id(StoreNumber, StoreId),
    get_shelves_data(StoreId, [id, externalReferenceId], Data),
    %writeln(Data),
    get_unit_id('meter', UnitId),
    %writeln(['Id', UnitId]),
    forall(member([ShelfId, ExtRefId], Data),
        (atom_number(ExtRefId, NumId),
        triple(Shelf, shop:erpShelfId, NumId),
        get_layers_in_shelf(Shelf, Layers),
        post_shelf_layers_of_shelf_(Shelf, ShelfId, UnitId, Layers))
    ).

post_shelf_layers_of_shelf_(Shelf, ShelfId, UnitId, [Layer | Rest]) :-
    % get associated shelf ids
    % 
    % triple(Shelf, soma:hasPhysicalComponent, Layer),
    % has_type(Layer, shop:'ShelfLayer'),
    %writeln(['layer', Layer]),
    object_dimensions(Layer, D, W, H),
    %writeln(['dim',D, W, H]),
    rdf_split_url(_, Frame, Shelf),
    is_at(Layer, [Frame, T, _]),
    % writeln(['pose',T, R]),
    [_,_,Z] = T,
    triple(Layer, shop:erpShelfLayerId, LayerId),
    post_shelf_layer(ShelfId, [D, LayerId, H, LayerId, Z, "null", W, UnitId], _),
    post_shelf_layers_of_shelf_(Shelf, ShelfId, UnitId, Rest).


post_shelf_layers_of_shelf_(_, _, _, []).

post_fridge_shelf_layer(ShelfPlId, [D, W, H], ExtRefId, Z, LayerPosted) :-
    get_unit_id('meter', UnitId),
    post_shelf_layer(ShelfPlId, [D, ExtRefId, H, ExtRefId, Z, "null", W, UnitId], LayerPosted).

post_fridge_facings(StoreNumber) :-
    get_store_id(StoreNumber, StoreId),
    % This works only because fridge has only one shelf 
    get_shelf_layers_data(StoreId, [id, externalReferenceId], Value),
    %writeln(["shelf", Value]),
    post_fridge_facings_of_layer(Value).

post_fridge_facings_of_layer([[LayerId, ExtId] | Rest]) :-
    atom_number(ExtId, NumId),
    triple(Layer, shop:erpShelfLayerId, NumId),
    rdf_split_url(_, Frame, Layer),
    % forcing the loop to continue even if there is a failure in posting
    forall(triple(Facing, shop:layerOfFacing, Layer),
    %writeln(Facing),
        (is_at(Facing, [Frame, [X, _, _], _]),
        triple(Facing, shop:erpFacingId, ExtRefId),
        post_fridge_facing(LayerId, X, ExtRefId, 1))),
    post_fridge_facings_of_layer(Rest).

post_fridge_facings_of_layer([]).

post_fridge_facing(LayerPlId, LayerRelPos, ExtRefId, Count, PdtUnitId, FacingId) :-
    %NoOfItemDepth is 1,
    LayerRelPosMM is round(LayerRelPos*1000),
    NoOfItemWidth is 1,
    NoOfItemDepth is 1,
    NoOfItemHeight is 1,
    MinStock is 0,
    k4r_db_client:post_facing(LayerPlId, [LayerRelPosMM, NoOfItemWidth , NoOfItemDepth, NoOfItemHeight, 
        MinStock, Count, PdtUnitId, ExtRefId], Facing),
    k4r_db_client:get_entity_id(Facing, FacingId).

post_items_in_store(StoreNumber) :-
    get_store_id(StoreNumber, StoreId),
    % Check if the shelf is in the store
    get_shelves_data(StoreId, [id, externalReferenceId], Value),
    %writeln(["ShelfData",Value]),
    forall(member([ShelfPlId, ShelfExtRefId], Value),
        ( %writeln(["PId", ShelfPlId]),
        atom_number(ShelfExtRefId, NumId),
        %get_shelf_with_external_id(NumId, Shelf),
        triple(Shelf, shop:erpShelfId, NumId),
        post_items_in_shelf(Shelf, ShelfPlId))
    ).

post_items_in_shelf(Shelf, _) :-
    \+ triple(Shelf, soma:hasPhysicalComponent, _),
    print_message(info, 'Shelf has no layers').

post_items_in_shelf(Shelf, PlatformShelfId) :-
    %writeln("post_items_in_shelf"),
    get_layer_and_facing_data(PlatformShelfId, _{shelfLayers: [externalReferenceId], facings: [id, layerRelativePosition]}, LayersDict),
    forall(member(LayerDict, LayersDict.shelfLayers),
        (atom_number(LayerDict.externalReferenceId, NumId),
        triple(Layer, shop:erpShelfLayerId, NumId),
        triple(Shelf, soma:hasPhysicalComponent, Layer),
        %writeln([Layer,LayerDict.facings]),
        post_items_in_layer(LayerDict.facings, Layer)
    )).

post_items_in_layer([], _).

post_items_in_layer([FacingDict | Rest], Layer) :-
    %writeln("post_items_in_layer"),
    %integer(FacingDict.layerRelativePosition),
    %writeln([FacingDict.layerRelativePosition, FacingDict.id]),
    %atom_number(FacingDict.layerRelativePosition, FacingRelPos),
    triple(Facing, shop:erpFacingId, FacingDict.layerRelativePosition),
    triple(Facing, shop:layerOfFacing, Layer),
    %writeln(Facing),
    rdf_split_url(_,FacingFrame,Facing),
    shopping:get_all_items_in_facing(Facing, Items),
    post_items_in_facing(Items, FacingFrame, FacingDict.id),
    post_items_in_layer(Rest, Layer).
    %).

post_items_in_facing([], _, _).

post_items_in_facing([Item | Rest], ParentName, FacingPlatformId) :-
    %writeln(["Items", Item]),
    % has_type(Item, Product),
    % get_product_gtin(Product, Gtin),
    % get_product_unit_id(Gtin, ProductUnitId),
    %writeln(["PUnitId", ProductUnitId]),
    % TODO : Enable the count

    %shop_reasoner:get_number_of_items_in_facing(Facing, NoOfItems),

    is_at(Item, [ParentName, [X, Y, Z], _]),
    k4r_db_client:double_m_to_int_mm([X, Y, Z], [X_mm, Y_mm, Z_mm]),

    triple(Item, shop:hasItemId, ExtItemId),
    post_item([FacingPlatformId, X_mm, Y_mm, Z_mm, ExtItemId], _),
    post_items_in_facing(Rest, ParentName, FacingPlatformId).


update_stock(FacingId, Count) :-
    get_facing_data(FacingId, Data),
    Stock is Data.stock + Count,
    k4r_db_client:put_facing(Data.shelfLayerId, [Data.layerRelativePosition, Data.noOfItemsDepth, Data.noOfItemsWidth,
        Data.noOfItemsHeight, Data.minStock, Stock, Data.productUnitId, Data.externalReferenceId], _, Data.id).

update_item_position_platform(ExtItemId, [X, Y, Z]) :-
    get_filter_("{items","externalReferenceId", "eq", ExtItemId, "string", ItemFilter),
    get_item_param(Fields),
    list_to_string(Fields, FieldsStr),
    atomics_to_string([ItemFilter, "{", FieldsStr, "}}"], GraphQLQuery),
    %writeln(GraphQLQuery),
    get_graphql(GraphQLQuery, Response),
    member(ItemData, Response.items),
    put_item([ItemData.facingId, X, Y, Z, ExtItemId], _, ItemData.id).

delete_item_and_update_facing(ExtItemId) :-
    get_filter_("{items","externalReferenceId", "eq", ExtItemId, "string", ItemFilter),
    atomics_to_string([ItemFilter, "{", id, ",", facingId, "}}"], GraphQLQuery),
    get_graphql(GraphQLQuery, Response), 
    member(ItemData, Response.items),
    update_stock(ItemData.facingId, -1),
    delete_entity_from_id("items", ItemData.id).
   
    %writeln(Data),

get_item_group_data(ItemGroupId, Data) :-
    get_filter_("{itemGroups","id", "eq", ItemGroupId, "string", ItemGroupFilter),
    atomics_to_string([ItemGroupFilter, "{", productUnitId, ",", facingId, ",", stock, "}}"], GraphQLQuery),
    %writeln(["Query", GraphQLQuery]),
    get_graphql(GraphQLQuery, GraphQLResponse),
    %writeln(["Response", GraphQLResponse.itemGroups]),
    member(Data, GraphQLResponse.itemGroups).

get_layer_and_facing_data(PlatformShelfId, KeyList, LayersDict) :- % test{shelfLayers: [externalReferenceId], facings: [id, layerRelativePosition]}
    k4r_db_client:make_store_id_filter(PlatformShelfId, Filter),
    list_to_string(KeyList.shelfLayers, LayerKeys),
    list_to_string(KeyList.facings, FacingKeys),
    string_concat("{shelves", Filter, ShelfFilter),
    string_concat("{shelfLayers{", LayerKeys, LayerFilter),
    string_concat(" facings{", FacingKeys, FacingFilter),
    %writeln([LayerFilter, FacingFilter]),
    atomics_to_string([ShelfFilter, LayerFilter, FacingFilter,"}}}}"], GraphQLQuery),
    get_graphql(GraphQLQuery, GraphQLResponse),
    %writeln(GraphQLResponse.shelves),
    member(LayersDict, GraphQLResponse.shelves).
    % forall(member(Layer, LayersDict.shelfLayers),
    % (writeln(Layer.externalReferenceId),
    % writeln(Layer.facings))
    % ).

get_item_group_id(FacingId, ProductUnitId, ItemGrpId) :-
    get_filter_("{facings","id", "eq", FacingId, "string", FacingFilter),
    get_filter_("{itemGroups","productUnitId", "eq", ProductUnitId, "string", ItemGroupFilter),
    atomics_to_string([FacingFilter, ItemGroupFilter, "{", id, "}}}"], IdQuery),
    get_graphql(IdQuery, IdResponse),
    %writeln(IdResponse.facings),
    member(ItemGrpList, IdResponse.facings),
    member(ItemGrp, ItemGrpList.itemGroups),
    ItemGrpId = ItemGrp.id.

get_product_unit_id(Gtin, ProductUnitId) :-
    k4r_db_client:make_gtin_filter(Gtin, GtinFilter),
    k4r_db_client:get_product_unit_data_with_filter(GtinFilter, [productUnitId], [ProductUnitId]).

get_facing_id([StoreNum, ShelfExt, LayerExt, FacingExt], FacingId):-
    get_facing_data_with_ext_ids(StoreNum, ShelfExt, LayerExt, FacingExt, id, Facings),
    FacingId = Facings.id.

get_layer_id([StoreNum, ShelfExt, LayerExt], LayerId) :-
    get_shelf_layer_data_with_ext_ids(StoreNum, ShelfExt, LayerExt, id, Layer),
    LayerId = Layer.id.

get_facing_data_with_ext_ids(StoreNum, ShelfExt, LayerExt, FacingExt, Key, Facings) :-
    get_filter_("{stores","storeNumber", "eq", StoreNum, "string", StoreFilter),
    get_filter_("{shelves","externalReferenceId", "eq", ShelfExt, "string", ShelfFilter),
    get_filter_("{shelfLayers","externalReferenceId", "eq", LayerExt, "string", LayerFilter),
    get_filter_("{facings","externalReferenceId", "eq", FacingExt, "string", FacingFilter),
    atomics_to_string([StoreFilter, ShelfFilter, LayerFilter, FacingFilter, "{", Key, "}}}}}"], IdQuery),
    get_graphql(IdQuery, IdResponse),
    _{stores:[_{shelves:[_{shelfLayers:[_{facings:[Facings]}]}]}]} = IdResponse.

get_shelf_layer_data_with_ext_ids(StoreNum, ShelfExt, LayerExt, Key, ShelfLayer) :-
    get_filter_("{stores","storeNumber", "eq", StoreNum, "string", StoreFilter),
    get_filter_("{shelves","externalReferenceId", "eq", ShelfExt, "string", ShelfFilter),
    get_filter_("{shelfLayers","externalReferenceId", "eq", LayerExt, "string", LayerFilter),
    atomics_to_string([StoreFilter, ShelfFilter, LayerFilter, "{", Key, "}}}}"], LayerQuery),
    get_graphql(LayerQuery, LayerResponse),
    _{stores:[_{shelves:[_{shelfLayers:[ShelfLayer]}]}]} = LayerResponse.

get_store_id(StoreNum, StoreId) :-
    get_filter_("{stores","storeNumber", "eq", StoreNum, "string", StoreFilter),
    atomics_to_string([StoreFilter, "{", id, "}}"], GraphQLQuery),
    get_graphql(GraphQLQuery, GraphQLResponse),
    member(StoreDict, GraphQLResponse.stores),
    StoreId = StoreDict.id.
    %writeln(['id', StoreId]).

get_store(StoreNum, StoreParam, Store) :-
    get_filter_("{stores", "storeNumber", "eq", StoreNum, "string", StoreFilter),
    list_to_string(StoreParam, KeyParamStr),
    atomics_to_string([StoreFilter, "{", KeyParamStr,  "}}"], GraphQLQuery),
    get_graphql(GraphQLQuery, GraphQLResponse),
    %get_dict(stores,GraphQLResponse,Temp),
    member(Store, GraphQLResponse.stores).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_all_shelf_data(StorePlatformId, ShelfParam, ShelfData) :-
    get_filter_("{shelves", "storeId", "eq", StorePlatformId, "string", ShelfFilter),
    list_to_string(ShelfParam, KeyParamStr),
    atomics_to_string([ShelfFilter, "{", KeyParamStr,  "}}"], GraphQLQuery),
    get_graphql(GraphQLQuery, GraphQLResponse),
    % writeln("pa4"),
    % writeln(GraphQLResponse),
    get_dict(shelves, GraphQLResponse, Temp),
    %writeln(Temp),
    findall(ValueList, 
        (member(T, Temp), 
        get_value_list(T, ShelfParam, ValueList)),
    ShelfData).
    %writeln(ShelfData).

get_all_items(FacingId, ItemData) :-
    get_filter_("{items","facingId", "eq", FacingId, "string", ItemFilter),
    get_item_param(Fields),
    list_to_string(Fields, FieldsStr),
    atomics_to_string([ItemFilter, "{", FieldsStr, "}}"], GraphQLQuery),
    get_graphql(GraphQLQuery, GraphQLResponse),
    get_dict(items, GraphQLResponse, Temp),
    %writeln(Temp),
    findall(ValueList, 
        (member(T, Temp), 
        get_value_list(T, Fields, ValueList)),
    ItemData).
    %writeln(ItemData).

get_all_item_groups(FacingId, ItemGrps) :-
    get_filter_("{itemGroups","facingId", "eq", FacingId, "string", ItemGroupFilter),
    get_item_grp_param(Fields),
    list_to_string(Fields, FieldsStr),
    atomics_to_string([ItemGroupFilter, "{", FieldsStr, "}}"], GraphQLQuery),
    get_graphql(GraphQLQuery, GraphQLResponse),
    %writeln(GraphQLResponse),
    get_dict(itemGroups, GraphQLResponse, Temp),
    %writeln(Temp),
    findall(ValueList, 
        (member(T, Temp), 
        get_value_list(T, Fields, ValueList)),
    ItemGrps).
    %writeln(ItemGrps).

get_all_facings_platform(LayerId, Data) :-
    get_filter_("{facings","shelfLayerId", "eq", LayerId, "string", FacingFilter),
    get_facing_param(Fields),
    list_to_string(Fields, FieldsStr),
    atomics_to_string([FacingFilter, "{", FieldsStr, "}}"], GraphQLQuery),
    get_graphql(GraphQLQuery, GraphQLResponse),
    get_dict(facings, GraphQLResponse, Temp),
    %writeln(Temp),
    findall(ValueList, 
        (member(T, Temp), 
        get_value_list(T, Fields, ValueList)),
    Data).
    %writeln(Data).

get_all_layers_platform(ShelfId, ValueLists) :-
    get_filter_("{shelfLayers","shelfId", "eq", ShelfId, "string", LayerFilter),
    get_layer_param(Fields),
    list_to_string(Fields, FieldsStr),
    atomics_to_string([LayerFilter, "{", FieldsStr, "}}"], GraphQLQuery),
    get_graphql(GraphQLQuery, GraphQLResponse),
    %writeln(GraphQLResponse),
    get_dict(shelfLayers, GraphQLResponse, Temp),
    findall(ValueList, 
        (member(T, Temp), 
        get_value_list(T, Fields, ValueList)),
    ValueLists).
    %writeln(ValueLists).

get_value_list(TempDict, Keys, ValList) :-
    findall(Value,
        (member(Key, Keys),
        string_to_atom(TempDict.Key, Value)),
        %put_dict([Key = Value], InitDict, Init)),
    ValList).

get_gtin(ProductUnitId, Gtin) :-
    get_filter_("{productGtins","productUnitId", "eq", ProductUnitId, "string", GtinFilter),
    atomics_to_string([GtinFilter, "{", gtin, "}}"], GraphQLQuery),
    get_graphql(GraphQLQuery, GraphQLResponse),
    member(Data, GraphQLResponse.productGtins),
    Gtin = Data.gtin.

get_product_dimenion_platform(ProductUnitId, D, W, H) :-
    get_filter_("{productUnits","id", "eq", ProductUnitId, "string", UnitIdFilter),
    get_product_unit_param(Fields),
    list_to_string(Fields, FieldsStr),
    atomics_to_string([UnitIdFilter, "{", FieldsStr, "}}"], GraphQLQuery),
    get_graphql(GraphQLQuery, GraphQLResponse),
    member(Data, GraphQLResponse.productUnits),
    convert_to_m(Data.dimensionUnit, Data.length, D),
    convert_to_m(Data.dimensionUnit, Data.width, W),
    convert_to_m(Data.dimensionUnit, Data.height, H).

get_item_data(ExtItemId, ItemData) :-
    get_filter_("{items","externalReferenceId", "eq", ExtItemId, "string", ItemFilter),
    get_item_param(Fields),
    list_to_string(Fields, FieldsStr),
    atomics_to_string([ItemFilter, "{", FieldsStr, "}}"], GraphQLQuery),
    %writeln(GraphQLQuery),
    get_graphql(GraphQLQuery, Response),
    member(ItemData, Response.items).

get_facing_data(Id, FacingData) :-
    get_filter_("{facings","id", "eq", Id, "string", FacingFilter),
    get_facing_param(Fields),
    list_to_string(Fields, FieldsStr),
    atomics_to_string([FacingFilter, "{", FieldsStr, "}}"], GraphQLQuery),
    %writeln(GraphQLQuery),
    get_graphql(GraphQLQuery, Response),
    member(FacingData, Response.facings).

get_filter_(FieldName, Param, Op, Value, Type, CompleteFilter) :-
    k4r_db_client:make_filter(Param, Op, Value, Type, Filter),
    string_concat(FieldName, Filter, CompleteFilter).

get_item_param(ItemFields) :-
    ItemFields = [
        id,
        positionInFacingX,
        positionInFacingY,
        positionInFacingZ,
        facingId,
        externalReferenceId
    ].

get_item_grp_param(Fields) :-
    Fields = [
        id,
        productUnitId,
        facingId,
        stock
    ].

get_facing_param(Fields) :-
    Fields = [
        id,
        shelfLayerId,
        layerRelativePosition,
        noOfItemsWidth,
        noOfItemsDepth,
        noOfItemsHeight,
        minStock,
        stock,
        productUnitId,
        externalReferenceId
    ].

get_product_unit_param(Fields) :-
    Fields = [
        id,
        productId,
        unitCode,
        numeratorBaseUnit,
        denominatorBaseUnit,
        length,
        width,
        height,
        dimensionUnit,
        volume,
        volumeUnit,
        netWeight,
        grossWeight,
        weightUnit,
        maxStackSize
    ].

get_layer_param(Fields) :-
    Fields = [
        id,
        shelfId,
        level,
        type,
        positionZ,
        width,
        height,
        depth,
        lengthUnitId,
        externalReferenceId
    ].

get_store_param(StoreParam) :-
    StoreParam = [
    id,
    storeName,
    addressCountry,
    addressState,
    addressCity,
    addressStreet,
    addressStreetNumber,
    addressPostcode,
    addressAdditional,
    latitude,
    longitude].

get_shelf_param(ShelfParam) :-
    ShelfParam = [
        id,
        positionX,
        positionY,
        positionZ,
        orientationX,
        orientationY,
        orientationZ,
        orientationW,
        width,
        height,
        depth,
        lengthUnitId, % 1 for m, 2 for mm, 3 for cm
        cadPlanId, % cad model path?
        externalReferenceId
    ].
