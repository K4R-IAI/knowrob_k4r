/** <module> environment
 * This module creates an environment from user 
    defined specifications   
    @author Kaviya Dhanabalachandran
    @license BSD
*/

:- module( environment,
    [ create_store/8,
    create_store_from_platfrom/2,
    get_store_param/1,
    get_shelf_param/1
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

create_store_from_platfrom(StoreNumber, Store):-
    (var(StoreNumber) -> 
        print_message(warning, 'Provide a suitable store number'));
    (get_store_param(Param),
    get_store(StoreNumber, Param, PlatformStore),
    ((is_list_empty_(PlatformStore) -> 
    print_message(info, 'Store not in platform'));
    (AddressValue = [StoreData.addressStreet, StoreData.addressStreetNumber, StoreData.addressPostcode, StoreData.addressAdditional],
    GeoCoo  = [StoreData.latitude, StoreData.longitude],
    create_store_(StoreNumber, StoreData.storeName, StoreData.addressCountry, StoreData.addressState, StoreData.addressCity, AddressValue, GeoCoo, Fridge, Store),
    print_message(info, 'Store exists in the platform')))).

create_store(StoreNumber, StoreName, Country, State, City, [Street, StreetNum, PostCode, Additional], [Latitude, Longitude], Store) :-
    (var(StoreNumber) -> 
        print_message(warning, 'Provide a suitable store number'));
    % Add to create a new store internally
    (create_store_(StoreNumber, StoreName, Country, State, City, [Street, StreetNum, PostCode, Additional], [Latitude, Longitude], Fridge, Store),
    % post to platform
    post_fridge_store([Additional, City, Country, PostCode, State, Street, StreetNum, "", Latitude, Longitude, StoreName,StoreNumber], _)).


create_store_(StoreNumber, StoreName, Country, State, City, AddressValue, GeoCoordinates, Fridge, Store) :-
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
        triple(Store, shop:hasAddress, Address),
        instance_of(Fridge, shop:'SmartFridge'),
        has_location(Fridge, Store)
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
    convert_to_m(Shelf.lengthUnitId, Shelf.depth, D),
    convert_to_m(Shelf.lengthUnitId, Shelf.width, W),
    convert_to_m(Shelf.lengthUnitId, Shelf.height, H),
    tell([ has_type(Frame, shop:'ShelfFrame'),
        triple(Fridge, soma:hasPhysicalComponent, Frame),
        triple(Frame, shop:erpShelfId, ExtRefId),
        is_at(Frame, ['map', [X, Y, Z],[X1, Y1, Z1, W1]]),
        object_dimensions(Frame, D, W, H)]),
    triple(Frame, soma:hasShape, Shape),
    triple(Shape, dul:hasRegion, ShapeRegion),
    ignore(tell(triple(ShapeRegion, soma:hasFilePath, Shelf.cadPlanId))),
    assert_shelf_platform(Fridge, Rest, Temp1, ShelfIds).

assert_shelf_platform(_, [], Temp, Temp).

get_store_param(StoreParam) :-
    StoreParam = [ storeName,
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

get_later_param(LayerParam) :-
    LayerParam = [
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
    