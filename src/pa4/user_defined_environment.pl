/** <module> user defined envoronment
 * This module creates an environment from user 
    defined specifications   
    @author Kaviya Dhanabalachandran
    @license BSD
*/

:- module( user_defined_environment,
    [ create_shelf/9,
    init_store/1,
    assert_facing/4]
    ).

:- use_module(library('semweb/rdf_db')).
:- use_module(shopping).
:- use_module(environment).
:- use_module(library('shop_reasoner')).

%% TODO: PRoblem with Ext ref id of facigns if the facings are created at different times

:- rdf_db:rdf_register_ns(shop, 'http://knowrob.org/kb/shop.owl#', [keep(true)]).
% :- rdf_db:rdf_register_ns(fridge, 'http://knowrob.org/kb/fridge.owl#', [keep(true)]).


create_store(StoreNumber, StorePlId, Fridge, Store) :-
    create_store_from_platfrom(StoreNumber, StorePlId, Store),
    tell([has_type(Fridge, shop:'SmartFridge'),
        triple(Fridge, dul:hasLocation, Store)]).

/*
The origin of the mesh of the shelf system will be on the center of the bounding box, 
but the shelf system will have an additional frame, placed on the lower-left-front corner,
*/
create_shelf(Fridge, Parent, Dimensions, Translation, Rotation, RefId, StoreId, Shelf, ShelfPlatformId) :-
    tell(is_physical_object(Shelf)),
    tell(triple(Fridge, soma:hasPhysicalComponent, Shelf)),
    shop:belief_new_object(Shelf, 'http://knowrob.org/kb/shop.owl#ShelfFrame'),
    tell(triple(Shelf, shop:erpShelfId, RefId)),
    [D, W, H] = Dimensions,
    assert_object_shape_(Shelf, D, W, H,[0.0, 1.0, 0.5], 'package://knowrob_k4r/models/SM_ShelfSystemH160T4L10W.dae'),
    %assert_file_path(Shelf, ),
    tell(is_at(Shelf, [Parent, Translation, Rotation])),
    post_fridge_shelf(StoreId, [H, W, D], Translation, Rotation, RefId, ShelfPosted),
    k4r_db_client:get_entity_id(ShelfPosted, ShelfPlatformId).
    
create_shelf_layer(Parent, Dimensions, Z, ShelfLayer) :-
    tell(is_physical_object(ShelfLayer)),
    tell(triple(Parent, soma:hasPhysicalComponent, ShelfLayer)),
    shop:belief_new_object(ShelfLayer, 'http://knowrob.org/kb/shop.owl#ShelfLayer'),
    [D, W, H] = Dimensions,
    assert_object_shape_(ShelfLayer, D, W, H,[0.0, 1.0, 0.5], 'package://knowrob_k4r/models/ShelfLayer4TilesL10.dae'),
    %assert_file_path(ShelfLayer, 'package://knowrob_k4r/models/ShelfLayer4TilesL10.dae'),
    rdf_split_url(_, Frame, Parent),
    tell(is_at(ShelfLayer, [Frame, [0,0,Z], [0,0,0,1]])).
    % post_fridge_shelf_layer(ShelfPlatformId, [D, W, H], ExtRefId, Z, LayerPosted),
    % k4r_db_client:get_entity_id(LayerPosted, LayerPlId).

create_facing_in_layer(Parent, Dimensions, X, Facing) :-
    tell(is_individual(Facing)),
    tell(triple(Facing, shop:layerOfFacing, Parent)),
    shop:belief_new_object(Facing, 'http://knowrob.org/kb/shop.owl#ProductFacing'),
    [D, W, H] = Dimensions,
    assert_object_shape_(Facing, D, W, H, [0.0, 1.0, 0.5], _),
    rdf_split_url(_, Frame, Parent),
    tell(is_at(Facing, [Frame, [X, 0, 0], [0,0,0,1]])).

assert_file_path(Object, Path) :-
    triple(Object, soma:hasShape,Shape), 
    triple(Shape,dul:hasRegion,ShapeRegion),
    tell(triple(ShapeRegion,soma:hasFilePath, Path)).
  
    
assert_object_shape_(Object, D, W, H, RGBValue, Path):- 
    (object_dimensions(Object, D, W, H) -> 
    (triple(Object,soma:hasShape,Shape),
    triple(Shape,dul:hasRegion,ShapeRegion));
    tell(has_type(Shape, soma:'Shape')),
    tell(holds(Object,soma:hasShape,Shape)),
    tell(object_dimensions(Object, D, W, H)),
    triple(Shape,dul:hasRegion,ShapeRegion)),
    
    (ground(Path) -> tell(triple(ShapeRegion,soma:hasFilePath, Path))
    ; true),
    Pos = [0,0,0],  Rot = [0.0, 0.0, 0, 1],
    tell(is_individual(Origin)),
    %writeln('hereee'),
    tell(triple(Origin, soma:hasPositionVector, term(Pos))),
    tell(triple(Origin, soma:hasOrientationVector, term(Rot))),
    tell(triple(ShapeRegion, soma:hasTransparencyValue, 1)),
    tell(has_type(CQ, soma:'Color')),
    tell(holds(Object,soma:hasColor,CQ)),
    tell(object_color_rgb(Object, RGBValue)), !.

init_store(StoreNumber) :-
    create_store(StoreNumber, StorePlId, Fridge, _),
    get_shelf_param(ShelfParam),
    get_all_shelf_data(StorePlId, ShelfParam, ShelfData),
    (\+ is_list_empty_(ShelfData)->assert_shelf_platform(Fridge, ShelfData, _);
    %create_shelf(Fridge, 'map', [0.516, 1.0, 1.6], [4.3622098211, -0.98765496137, 0.8], [0.0, 0.0, -0.708516162752, 0.705694584873], 1, StorePlId, S, ShelfPlId)),
    create_shelf(Fridge, 'map', [0.45, 0.64, 1.6], [4.3622098211, -0.98765496137, 0.8], [0.0, 0.0, 0, 1], 1, StorePlId, S, _),
    %tell(is_individual(ShelfLFFrame)),
    % X is -(0.616/2),
    % Y is  -(1.0/2),
    % Z is -(1.6/2),
    % rdf_split_url(_, Frame, S),
    % tell(is_at(ShelfLFFrame, [Frame, [-0.45, -0.3, Z], [0.0, 0.0, 0, 1]])),
    create_shelf_layer(S, [0.45, 0.64, 0.2], -0.8, _),
    create_shelf_layer(S, [0.45, 0.64, 0.2], -0.4, _),
    %writeln([L1, L2]),
    marker_plugin:republish,
    shop:assert_layer_id(S),
    post_fridge_shelf_layers(StoreNumber)).
    %post_fridge_facings(StoreNumber)).

assert_facings(StoreNum, LayerExtId) :-
    shopping:get_store(StoreNum, Store),
    triple(Fridge, dul:hasLocation, Store),
    triple(Layer, shop:erpShelfLayerId, LayerExtId),
    triple(Shelf, soma:hasPhysicalComponent, Layer),
    triple(Fridge, soma:hasPhysicalComponent, Shelf),
    create_facing_in_layer(Layer, [0.35, 0.16, 0.1], 0.108, _),
    create_facing_in_layer(Layer, [0.35, 0.16, 0.1], -0.108, _),
    shop:assert_facing_id(Layer).

assert_facing(StoreNum, ShelfExt, LayerExtId, Dimensions_Pos_Gtin) :-
    shopping:get_store(StoreNum, Store),
    triple(Fridge, dul:hasLocation, Store),
    triple(Shelf, shop:erpShelfId, ShelfExt),
    triple(Layer, shop:erpShelfLayerId, LayerExtId),
    triple(Shelf, soma:hasPhysicalComponent, Layer),
    triple(Fridge, soma:hasPhysicalComponent, Shelf),
    findall([Facing, PdtId],
        (member([Dim, X, GtinNum], Dimensions_Pos_Gtin),
        create_facing_in_layer(Layer, Dim, X, Facing),
        get_product_unit_id(GtinNum, PdtId)),
        FacingList),
    shop:assert_facing_id(Layer),
    get_layer_id([StoreNum, ShelfExt, LayerExtId], LayerId),
    forall(member([F, PId], FacingList),
        ( post_facing_individual(LayerId, F, PId, FacingId)
    )).

/* assert_shelf_facings(ShelfNo, LayerNumber_Gtins) :-
    triple(Shelf, shop:erpShelfId, ShelfNo),
    dict_pairs(LayerNumber_Gtins,_,Pairs).
    forall(member(L_G, Pairs),
        (K-V = L_G,
        triple(Layer, shop:erpShelfLayerId, K),
        assert_layer_facings(L, V)
    )).

assert_layer_facings(L, V) :-
    Dimensions = [0.35, 0.16, 0.1],
    forall(member([Gtin, Pos], V),
    ( 
        create_facing_in_layer(L, Dimensions, Pos, Gtin)
        )). */


:- begin_tests(user_defined_environment).

test('create shelf') :-
    StoreNum is 300,
    %create_store(StoreNum, 'fridgepa42', "Ger", "BW" , "Bre", ["Uni", 45, 452343, ""], [40, 40], Store),
    gtrace,
    init_store(StoreNum), !,
    LayerExtId is 1,
    %assert_facings(StoreNum, LayerExtId),
    %writeln('hereee'),
    shopping:get_store(StoreNum, Store),
    %writeln('insertttt'),
    shopping:get_facing_(Store, [1, 1, 1], _),
    Dimensions = [0.35, 0.16, 0.1],
    %assert_facing(StoreNum, 1, 1, [[Dimensions, 0, '4004980506206'], [Dimensions, 0.108, '4011800521226']]).
    shopping:insert_all_fridge_items(StoreNum, [1, 1, 2],'4004980506206',[['I4563', [0.07, -0.05]], ['I4564', [0.07,-0.02]], ['I4567', [0.08, -0.03]]]).
    % label_of_facing(55, Facing, [1, 1, 1], '4010355520036', ProductType, ItemGroupId),
    % forall(member([ItemId,  Coordinates], [['I4563', [ 0.05 , 0.02]], ['I4564', [0.05,-0.08]], ['I4567', [0.05, 0.08]]]),
    %     (insert_item(Facing, ProductType, ItemGroupId, ItemId, Coordinates, _),
    %     writeln(ItemInstance)
    % )).



/* test('insert items') :-
    gtrace,
    shopping:insert_all_fridge_items("585", [1, 1, 1],'4010355520036',[['I4563', [-0.17, 0.02]], ['I4564', [0.0,0.02]], ['I4567', [0.08,0.02]]]). */
%     writeln('hereee'),
%     get_store(55, Store),
%     writeln('insertttt'),
%     shopping:get_facing_(Store, [1, 1, 1], Facing),
%     label_of_facing(StoreNum, Facing, [ShelfExt, ShelfLayerExt, FacingExt], Gtin, ProductType, ItemGroupId).

:- end_tests(user_defined_environment).
