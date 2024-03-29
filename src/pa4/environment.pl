/** <module> environment
 * This module creates an environment
    @author Kaviya Dhanabalachandran
    @license BSD
*/

:- module( environment,
    [ create_store/8,
    create_store_from_platfrom/3,
    assert_shelf_platform/3
    ]
    ).

:- use_module(pa4_db_client).
:- use_module(shopping).
:- use_module('utils').
:- use_module(library('semweb/rdf_db'),
    [ rdf_split_url/3 ]).

:- rdf_db:rdf_register_ns(soma,
    'http://www.ease-crc.org/ont/SOMA.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(shop, 'http://knowrob.org/kb/shop.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(fridge, 'http://knowrob.org/kb/fridge.owl#', [keep(true)]).

% StoreParam = [ storeName,
    %addressCountry,
    %addressCity,
    %addressState,
    %addressStreet,
    %addressStreetNumber,
    %addressPostcode,
    %addressAdditional,
    %latitude,
    %longitude]

create_store_from_platfrom(StoreNumber, StorePlatformId, Store):-
    (var(StoreNumber) -> 
        print_message(warning, 'Provide a suitable store number'));
    (get_store_param(Param),
    get_store(StoreNumber, Param, PlatformStore),
    ((is_list_empty_(PlatformStore) -> 
    print_message(info, 'Store not in platform'));
    (StoreData = PlatformStore,
    AddressValue = [StoreData.addressStreet, StoreData.addressStreetNumber, StoreData.addressPostcode, StoreData.addressAdditional],
    StorePlatformId = StoreData.id,
    GeoCoo  = [StoreData.latitude, StoreData.longitude],
    create_store_(StoreNumber, StoreData.storeName, StoreData.addressCountry, StoreData.addressState, StoreData.addressCity, AddressValue, GeoCoo, Store),
    print_message(info, 'Store exists in the platform')))).

create_store(StoreNumber, StoreName, Country, State, City, [Street, StreetNum, PostCode, Additional], [Latitude, Longitude], Store) :-
    (var(StoreNumber) -> 
        print_message(warning, 'Provide a suitable store number'));
    % Add to create a new store internally
    (create_store_(StoreNumber, StoreName, Country, State, City, [Street, StreetNum, PostCode, Additional], [Latitude, Longitude], Store),
    % post to platform
    post_fridge_store([Additional, City, Country, PostCode, State, Street, StreetNum, "", Latitude, Longitude, StoreName,StoreNumber], _)).


create_store_(StoreNumber, StoreName, Country, State, City, AddressValue, GeoCoordinates, Store) :-
    tell(instance_of(Store, shop:'Shop')),
    tell(triple(Store, shop:hasShopNumber, StoreNumber)),
    %writeln(StoreData.storeName),
    ignore(tell(triple(Store, shop:hasShopName, StoreName))),
    %writeln(StoreData.addressCountry),
    ignore(tell(triple(Store, shop:hasCountry, Country))),
    %writeln(StoreData.addressState),
    ignore(tell(triple(Store, shop:hasState, State))),
    %writeln(StoreData.addressCity),
    ignore(tell(triple(Store, shop:hasCity, City))),   
    tell([instance_of(Address, shop:'Address'),
        instance_of(AddressRegion, shop:'AddressRegion'),
        instance_of(Coordinates, shop:'GeometricCoordinate'),
        instance_of(CoordRegion, shop:'GeometricCoordinateRegion')]),
    tell([triple(Address, dul:hasRegion, AddressRegion),
        triple(Coordinates, dul:hasRegion, CoordRegion),
        triple(Store, shop:hasAddress, Address)
        % instance_of(Fridge, shop:'SmartFridge'),
        % has_location(Fridge, Store)
    ]),
    list_to_string(GeoCoordinates, GeoString),
    list_to_string(AddressValue, AddStr),
    ignore(tell(triple(CoordRegion, shop:hasGeometricCoordinateValue, GeoString))),
    ignore(tell(triple(AddressRegion, shop:hasAddressValue, AddStr))).

assert_shelf_platform(Fridge, ShelfData, Ids) :-
    assert_shelf_platform(Fridge, ShelfData, [], Ids).

assert_shelf_platform(Fridge, [Shelf | Rest], Temp, ShelfIds) :-
    [PlatformShelfId, PosX, PosY, PosZ, PosX1, PosY1, PosZ1, PosW1, Width, Height, Depth, LengthUnitIdAtom, CadPlanId, ExtRefIdAtom] = Shelf,
    atom_number(ExtRefIdAtom, ExtRefId),
    atom_number(LengthUnitIdAtom, LengthUnitId),
    triple(Fridge, dul:hasLocation, Store),
    append(Temp, [PlatformShelfId], Temp1),
    convert_to_m(LengthUnitId, PosX, X),
    convert_to_m(LengthUnitId, PosY, Y),
    convert_to_m(LengthUnitId, PosZ, Z),
    convert_to_m(LengthUnitId, PosX1, X1),
    convert_to_m(LengthUnitId, PosY1, Y1),
    convert_to_m(LengthUnitId, PosZ1, Z1),
    convert_to_m(LengthUnitId, PosW1, W1),
    convert_to_m(LengthUnitId, Depth, D),
    convert_to_m(LengthUnitId, Width, W),
    convert_to_m(LengthUnitId, Height, H),
    tell([ has_type(Frame, shop:'ShelfFrame'),
        triple(Fridge, soma:hasPhysicalComponent, Frame),
        triple(Frame, dul:hasLocation, Store),
        triple(Frame, shop:erpShelfId, ExtRefId),
        has_type(ObjShape, soma:'Shape'),
        triple(Frame, soma:'hasShape', ObjShape),
        has_type(ShapeRegion, soma:'ShapeRegion'),
        holds(ObjShape,dul:hasRegion,ShapeRegion),
        object_dimensions(Frame, D, W, H)]),
    get_time(Now),
    time_scope(=(Now), =<('Infinity'), FScope),
    tf_set_pose(Frame,  ['map', [X, Y, Z],[X1, Y1, Z1, W1]], FScope),
    triple(Frame, soma:hasShape, Shape),
    triple(Shape, dul:hasRegion, ShapeRegion),
    ignore(tell(triple(ShapeRegion, soma:hasFilePath, CadPlanId))),
    ignore(assert_layer_platform(PlatformShelfId, Frame)),
    assert_shelf_platform(Fridge, Rest, Temp1, ShelfIds).

assert_shelf_platform(_, [], Temp, Temp).

assert_layer_platform(ShelfId, Parent) :-
    get_all_layers_platform(ShelfId, LayerData),
    assert_layer_platform_(Parent, LayerData).

assert_layer_platform_(Parent, [Layer | Rest]) :-
    [PlatformLayerId, _, _, _, PosZ, Width, Height, Depth, LengthUnitIdAtom, ExtRefIdAtom] = Layer,
    atom_number(ExtRefIdAtom, ExtRefId),
    atom_number(LengthUnitIdAtom, LengthUnitId),
    convert_to_m(LengthUnitId, PosZ, Z),
    convert_to_m(LengthUnitId, Depth, D),
    convert_to_m(LengthUnitId, Width, W),
    convert_to_m(LengthUnitId, Height, H),
    rdf_split_url(_,ParentFrame, Parent),
    tell([ has_type(ShelfLayer, shop:'ShelfLayer'),
        triple(Parent, soma:hasPhysicalComponent, ShelfLayer),
        triple(ShelfLayer, shop:erpShelfLayerId, ExtRefId),
        has_type(ObjShape, soma:'Shape'),
        triple(ShelfLayer, soma:'hasShape', ObjShape),
        has_type(ShapeRegion, soma:'ShapeRegion'),
        holds(ObjShape,dul:hasRegion,ShapeRegion),
        object_dimensions(ShelfLayer, D, W, H)]),
    get_time(Now),
    time_scope(=(Now), =<('Infinity'), FScope),
    tf_set_pose(ShelfLayer,  [ParentFrame, [0, 0, Z],[0,0,0,1]], FScope),
    assert_facing_platform(PlatformLayerId, ShelfLayer),
    assert_layer_platform_(Parent, Rest).

assert_layer_platform_(_, []).

assert_facing_platform(LayerId, Parent) :-
    get_all_facings_platform(LayerId, FacingData),
    assert_facing_platform_(Parent, FacingData).

% There must be only one or two item group ids (i.e the product types in the facing)
% associated to a facing id
assert_facing_platform_(Parent, [Facing | Rest]) :-
    % No shape is asserted -it can be done based on the dimension of the product
    % and the no of item width so 2*Product
    subclass_of(fridge:'FridgeFacing', R1), has_description(R1, value(knowrob:depthOfObject, D)),
    subclass_of(fridge:'FridgeFacing', R2), has_description(R2, value(knowrob:widthOfObject, W)),
    subclass_of(fridge:'FridgeFacing', R3), has_description(R3, value(knowrob:heightOfObject, H)), 
    [PlatformFacingId, _, LayerRelPositionCM, _, _, _,_, _, Misplaced, PdtUnitId, ExternalRefIdAtom] = Facing,
    atom_number(ExternalRefIdAtom, ExtRefId),
    convert_to_m(2, LayerRelPositionCM, X), % is this X or Y
    rdf_split_url(_,ParentFrame, Parent),
    tell([ has_type(PrFacing, shop:'ProductFacing'),
        triple(PrFacing, shop:layerOfFacing, Parent),
        triple(PrFacing, shop:erpFacingId, ExtRefId),
        is_at(PrFacing, [ParentFrame,[X, 0, 0],[0,0,0,1]]),
        has_type(ObjShape, soma:'Shape'),
        triple(PrFacing, soma:'hasShape', ObjShape),
        has_type(ShapeRegion, soma:'ShapeRegion'),
        holds(ObjShape,dul:hasRegion,ShapeRegion),
        object_dimensions(PrFacing, D, W, H)]),
    (ground(Misplaced), Misplaced = null -> MisplacedUpdated is 0;
                        atom_number(Misplaced, MisplacedUpdated)),
    (MisplacedUpdated > 0 -> tell(has_type(PrFacing, shop:'MisplacedProductFacing'));
                        true),
    get_time(Now),
    time_scope(=(Now), =<('Infinity'), FScope),
    tf_set_pose(PrFacing,  [ParentFrame, [X, 0, 0],[0,0,0,1]], FScope),
    get_gtin(PdtUnitId, Gtin),
    PutFlag is 0,
    number_string(GtinNum, Gtin),
    assert_label_of_facing(PrFacing, PdtUnitId, GtinNum, PdtType, PutFlag, _),
    writeln(PrFacing),
    assert_items_platform(PlatformFacingId, PrFacing, PdtType),
    assert_facing_platform_(Parent, Rest).

assert_facing_platform_(_, []).

assert_items_platform(FacingId, Parent, ProductType) :-
    product_dimensions(ProductType, Dimension),
    get_all_items(FacingId, ItemData),
    assert_items_platform_(Parent, Dimension, ProductType, ItemData).

assert_items_platform_(Facing, [D, W, H], ProductType, [Item | Rest]) :-
    [_, PosX, PosY, PosZ, _, ExtRefId, PdtUnitId] = Item,
    convert_to_m(2, PosX, X_m),
    convert_to_m(2, PosY, Y_m),
    convert_to_m(2, PosZ, Z_m),
    (has_type(Facing, shop:'MisplacedProductFacing') ->
    (get_gtin(PdtUnitId, Gtin),
    number_string(GtinNum, Gtin),
    get_product_class(GtinNum, ItemProductType),
    tell(has_type(ItemInstance, ItemProductType)));
    (tell(has_type(ItemInstance, ProductType)))
    ),
    rdf_split_url(_,ParentFrame, Facing),
    atom_string(ExtRefId, ExtRef),
    tell([has_type(ProductShape, soma:'Shape'),
        triple(ItemInstance, soma:'hasShape', ProductShape),
        has_type(ShapeRegion, soma:'ShapeRegion'),
        triple(ProductShape,dul:hasRegion,ShapeRegion),
        object_dimensions(ItemInstance, D, W, H),
        triple(Facing, shop:productInFacing, ItemInstance),
        triple(ItemInstance, shop:hasItemId, ExtRef)]),
    get_time(Now),
    time_scope(=(Now), =<('Infinity'), FScope),
    tf_set_pose(ItemInstance,  [ParentFrame,  [X_m, Y_m, Z_m],[0,0,0,1]], FScope),
    assert_items_platform_(Facing, [D, W, H], ProductType, Rest).

assert_items_platform_(_, _, _, []).
