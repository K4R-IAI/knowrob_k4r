/** <module> environment
 * This module creates an environment from user 
    defined specifications   
    @author Kaviya Dhanabalachandran
    @license BSD
*/

:- module( environment,
    [ create_store/8,
    create_store_from_platfrom/3
    ]
    ).

:- use_module(pa4_db_client).
:- use_module(shopping).
:- use_module('utils').

:- rdf_db:rdf_register_ns(soma,
    'http://www.ease-crc.org/ont/SOMA.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(shop, 'http://knowrob.org/kb/shop.owl#', [keep(true)]).

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
    (AddressValue = [StoreData.addressStreet, StoreData.addressStreetNumber, StoreData.addressPostcode, StoreData.addressAdditional],
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
    list_to_string_(GeoCoordinates, GeoString),
    list_to_string_(AddressValue, AddStr),
    ignore(tell(triple(CoordRegion, shop:hasGeometricCoordinateValue, GeoString))),
    ignore(tell(triple(AddressRegion, shop:hasAddressValue, AddStr))).

assert_shelf_platform(Fridge, ShelfData, Ids) :-
    assert_shelf_platform(Fridge, ShelfData, [], Ids).

assert_shelf_platform(Fridge, [Shelf | Rest], Temp, ShelfIds) :-
    atom_number(Shelf.externalReferenceId, ExtRefId),
    Temp1 = [Temp, Sheld.id],
    convert_to_m(Shelf.lengthUnitId, Shelf.positionX, X),
    convert_to_m(Shelf.lengthUnitId, Shelf.positionY, Y),
    convert_to_m(Shelf.lengthUnitId, Shelf.positionZ, Z),
    convert_to_m(Shelf.lengthUnitId, Shelf.orientationX, X1),
    convert_to_m(Shelf.lengthUnitId, Shelf.orientationY, Y1),
    convert_to_m(Shelf.lengthUnitId, Shelf.orientationZ, Z1),
    convert_to_m(Shelf.lengthUnitId, Shelf.orientationW, W1),
    convert_to_m(Shelf.lengthUnitId, Shelf.length, D),
    convert_to_m(Shelf.lengthUnitId, Shelf.width, W),
    convert_to_m(Shelf.lengthUnitId, Shelf.height, H),
    tell([ has_type(Frame, shop:'ShelfFrame'),
        triple(Fridge, soma:hasPhysicalComponent, Frame),
        triple(Frame, shop:erpShelfId, ExtRefId),
        is_at(Frame, ['map', [X, Y, Z],[X1, Y1, Z1, W1]]),
        has_type(ObjShape, soma:'Shape'),
        triple(Frame, soma:'hasShape', ObjShape),
        has_type(ShapeRegion, soma:'ShapeRegion'),
        holds(ObjShape,dul:hasRegion,ShapeRegion),
        object_dimensions(Frame, D, W, H)]),
    triple(Frame, soma:hasShape, Shape),
    triple(Shape, dul:hasRegion, ShapeRegion),
    ignore(tell(triple(ShapeRegion, soma:hasFilePath, Shelf.cadPlanId))),
    assert_layer_platform(Sheld.id, Frame),
    assert_shelf_platform(Fridge, Rest, Temp1, ShelfIds).

assert_shelf_platform(_, [], Temp, Temp).

assert_layer_platform(ShelfId, Parent) :-
    get_all_layers_platform(ShelfId, LayerData),
    assert_layer_platform_(Parent, LayerData).

assert_layer_platform_(Parent, [Layer | Rest]) :-
    atom_number(Layer.externalReferenceId, ExtRefId),
    convert_to_m(Layer.lengthUnitId, Layer.positionZ, Z),
    convert_to_m(Layer.lengthUnitId, Layer.length, D),
    convert_to_m(Layer.lengthUnitId, Layer.width, W),
    convert_to_m(Layer.lengthUnitId, Layer.height, H),
    rdf_split_url(_,ParentFrame, Parent),
    tell([ has_type(ShelfLayer, shop:'ShelfLayer'),
        triple(Parent, soma:hasPhysicalComponent, Layer),
        triple(ShelfLayer, shop:erpShelfLayerId, ExtRefId),
        has_type(ObjShape, soma:'Shape'),
        triple(ShelfLayer, soma:'hasShape', ObjShape),
        has_type(ShapeRegion, soma:'ShapeRegion'),
        holds(ObjShape,dul:hasRegion,ShapeRegion),
        is_at(ShelfLayer, [ParentFrame, [0, 0, Z],[0,0,0,1]]),
        object_dimensions(ShelfLayer, D, W, H)]),
    assert_facing_platform(ShelfLayer),
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
    atom_number(Facing.externalReferenceId, ExtRefId),
    convert_to_m(2, Facing.layerRelativePosition, X), % is this X or Y
    rdf_split_url(_,ParentFrame, Parent),
    tell([ has_type(PrFacing, shop:'ProductFacing'),
        triple(PrFacing, shop:layerOfFacing, Parent),
        triple(PrFacing, shop:erpFacingId, ExtRefId),
        is_at(PrFacing, [ParentFrame,[X, 0, 0],[0,0,0,1]])]),
    assert_item_group_ids(Facing.id, PrFacing),
    assert_facing_platform_(Parent, Rest).

assert_item_group_ids(FacingId, Parent) :-
    get_all_item_groups(FacingId, ItemData),
    assert_item_group_platform_(Parent, ItemData).

assert_item_group_platform_(Parent, [ItemGrp | Rest]) :-
    get_gtin(ItemGrp.productUnitId, Gtin),
    (article_number_of_dan(Gtin, AN);
    create_article_number(gtin(Gtin), AN),
    get_product_dimenion_platform(ItemGrp.productUnitId, D, W, H),
    create_article_type(AN,[D,W,H],_)),
    tell([has_type(Label, shop:'ShelfLabel'),
    triple(Parent,shop:labelOfFacing,Label),
    triple(Label,shop:articleNumberOfLabel,AN)]),
    assert_items_platform(ItemGrp.id, Gtin, Parent, [D, W, H]),
    assert_item_group_platform_(Parent, Rest).


assert_items_platform(ItemGrpId, Gtin, Parent, Dimension) :-
    get_all_items(ItemGrpId, ItemData),
    get_product_type(Gtin, ProductType),
    assert_items_platform_(Parent, ProductType, Dimension, ItemData).

assert_items_platform_(Facing, ProductType, Dimension, [Item | Rest]) :-
    tell(has_type(Item, ProductType)),
    rdf_split_url(_,ParentFrame, Facing),
    tell([has_type(ProductShape, soma:'Shape'),
        triple(ItemInstance, soma:'hasShape', ProductShape),
        has_type(ShapeRegion, soma:'ShapeRegion'),
        triple(ProductShape,dul:hasRegion,ShapeRegion),
        object_dimensions(ItemInstance, Dimension),
        triple(Facing, shop:productInFacing, ItemInstance),
        triple(ItemInstance, shop:hasItemId, Item.externalReferenceId),
        is_at(ItemInstance, [ParentFrame, [Item.positionInFacingX, Item.positionInFacingY, 0], _])]),
    assert_items_platform_(Facing, ProductType, Dimension, Rest).

assert_items_platform_(_, _, _, []).
    