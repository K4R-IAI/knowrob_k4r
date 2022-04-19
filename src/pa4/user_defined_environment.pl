/** <module> user defined envoronment
 * This module creates an environment from user 
    defined specifications   
    @author Kaviya Dhanabalachandran
    @license BSD
*/

:- module( user_defined_environment,
    [ create_shelf/9]
    ).

:- use_module(library('semweb/rdf_db')).
:- use_module(shopping).
:- use_module(environment).
:- use_module(library('shop_reasoner')).

:- rdf_db:rdf_register_ns(shop, 'http://knowrob.org/kb/shop.owl#', [keep(true)]).


create_store(StoreNumber, StorePlId, Fridge, Store) :-
    create_store_from_platfrom(StoreNumber, StorePlId, Store),
    tell([has_type(Fridge, shop:'SmartFridge'),
        has_location(Fridge, Store)]).

/*
The origin of the mesh of the shelf system will be on the center of the bounding box, 
but the shelf system will have an additional frame, placed on the lower-left-front corner,
*/
create_shelf(Fridge, Parent, Dimensions, Translation, Rotation, RefId, StoreId, Shelf, ShelfPlId) :-
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
    tell(is_at(Facing, [Frame, [X, 0,0.07], [0,0,0,1]])).

assert_file_path(Object, Path) :-
    triple(Object, soma:hasShape,Shape), 
    triple(Shape,dul:hasRegion,ShapeRegion),
    tell(triple(ShapeRegion,soma:hasFilePath, Path)).
    % Pos = [0,0,0], Rot = [-0.0, -0.0, -0.70710678, 0.70710678],

    % triple(ShapeRegion,'http://knowrob.org/kb/urdf.owl#hasOrigin',Origin),
    % tripledb_forget(Origin, soma:hasPositionVector, _),
    % tripledb_forget(Origin, soma:hasOrientationVector, _),
    % tell(triple(Origin, soma:hasPositionVector, term(Pos))),
    % tell(triple(Origin, soma:hasOrientationVector, term(Rot))).
    
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
    tell(is_individual(Origin)),writeln('hereee'),
    get_store(StoreNum, Store),
    writeln('insertttt'),
    get_facing_(Store, [ShelfExt, ShelfLayerExt, FacingExt], Facing),
    label_of_facing(StoreNum, Facing, [ShelfExt, ShelfLayerExt, FacingExt], Gtin, ProductType, ItemGroupId),
    tell(triple(Region, soma:hasTransparencyValue, 1)), !.

init_store :-
    create_store(55, StorePlId, Fridge, Store),
    get_shelf_param(ShelfParam),
    get_all_shelf_data(StorePlId, ShelfParam, ShelfData),
    (\+ is_list_empty_(ShelfData)->assert_shelf_platform(Fridge, ShelfData, ShelfPlatformId);
    %create_shelf(Fridge, 'map', [0.516, 1.0, 1.6], [4.3622098211, -0.98765496137, 0.8], [0.0, 0.0, -0.708516162752, 0.705694584873], 1, StorePlId, S, ShelfPlId)),
    create_shelf(Fridge, 'map', [0.516, 1.0, 1.6], [4.3622098211, -0.98765496137, 0.8], [0.0, 0.0, 0, 1], 1, StorePlId, S, ShelfPlId)).
    % tell(is_individual(ShelfLFFrame)),
    % X is -(0.616/2),
    % Y is  -(1.0/2),
    % Z is -(1.6/2),
    % rdf_split_url(_, Frame, S),
    % tell(is_at(ShelfLFFrame, [Frame, [-0.45, -0.3, Z], [0.0, 0.0, 0, 1]])),
    %create_shelf_layer(S, [0.516, 1.0, 0.2], -0.8, L1),
    %create_shelf_layer(S, [0.516, 1.0, 0.2], -0.4, L2),
    %writeln([L1, L2]),
    %create_facing_in_layer(L2, [0.12, 0.35, 0.05], 0.108, Facing),
    %create_facing_in_layer(L2, [0.12, 0.35, 0.05], -0.108, Facing1),
    %marker_plugin:republish.
    %shop:assert_layer_id(S),
    %shop:assert_facing_id(L2).

    label_of_facing(StoreNum, Facing, [ShelfNo, LayerNo, FacingExtNo], Gtin, ProductType, ItemGroupId) :-
        get_product_unit_id(Gtin, ProductUnitId),
        get_facing_id([StoreNum, ShelfNo, LayerNo, FacingExtNo], FacingId),
        (triple(Facing,shop:labelOfFacing,Label),
        has_type(Label, shop:'ShelfLabel'),
        shop_reasoner:get_product_type(Gtin, ProductType),
        get_item_group_id(FacingId, ProductUnitId, ItemGroupId)
        );
        ((article_number_of_dan(Gtin, AN);
        create_article_number(gtin(Gtin), AN),
        get_product_dimenion_platform(ProductUnitId, D, W, H),
        create_article_type(AN,[D,W,H], ProductType)),
        tell([has_type(Label, shop:'ShelfLabel'),
        triple(Facing,shop:labelOfFacing,Label),
        triple(Label,shop:articleNumberOfLabel,AN)]),
        post_item_group([FacingId, ProductUnitId, 0], ItemGroup),
        k4r_db_client:get_entity_id(ItemGroup, ItemGroupId)).

:- begin_tests(user_defined_environment).

test('create shelf') :-
    gtrace,
    init_store,
    writeln('hereee'),
    %shopping:insert_all_items("55", [1, 1, 1],'4010355520036',[[['I4563', [-0.17, 0.02]], ['I4564', [0.0,0.02]], ['I4567', [0.08,0.02]]]).
    shopping:get_store(55, Store),
    writeln('insertttt'),
    shopping:get_facing_(Store, [1, 1, 1], Facing),
    label_of_facing(55, Facing, [1, 1, 1], '4010355520036', ProductType, ItemGroupId),
    forall(member([ItemId,  Coordinates], [['I4563', [ 0.05 , 0.02]], ['I4564', [0.05,-0.08]], ['I4567', [0.05, 0.08]]]),
        (insert_item(Facing, ProductType, ItemGroupId, ItemId, Coordinates, _),
        writeln(ItemInstance)
    )).
    %post_fridge_shelf_layers(55),
    %post_fridge_facings(55),



% test('insert items') :-
%     gtrace,
%     writeln('hereee'),
%     get_store(55, Store),
%     writeln('insertttt'),
%     shopping:get_facing_(Store, [1, 1, 1], Facing),
%     label_of_facing(StoreNum, Facing, [ShelfExt, ShelfLayerExt, FacingExt], Gtin, ProductType, ItemGroupId).
    %shopping:insert_all_items("55", [1, 1, 1],'4010355520036',[['I4563', [-0.17, 0.02]], ['I4564', [0.0,0.02]], ['I4567', [0.08,0.0.02]]]).

:- end_tests(user_defined_environment).