/** <module> environment
 * This module creates an environment from user 
    defined specifications   
    @author Kaviya Dhanabalachandran
    @license BSD
*/

:- module( environment,
    [ create_store/8,
    get_store_param/1
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

create_store(StoreNumber, StoreName, Country, State, City, [Street, StreetNum, PostCode, Additional], [Latitude, Longitude], Store) :-
    (var(StoreNumber) -> 
        print_message(warning, 'Provide a suitable store number'));
    (get_store_param(Param),
    get_store(StoreNumber, Param, PlatformStore),
    ((is_list_empty_(PlatformStore) -> 
    print_message(info, 'Not present'),
    % Add to create a new store internally
    create_store_(StoreNumber, StoreName, Country, State, City, [Street, StreetNum, PostCode, Additional], [Latitude, Longitude], Fridge, Store),
    % post to platform
    post_fridge_store([Additional, City, Country, PostCode, State, Street, StreetNum, "", Latitude, Longitude, StoreName,StoreNumber], StoreEntity)
    );
    (AddressValue = [StoreData.addressStreet, StoreData.addressStreetNumber, StoreData.addressPostcode, StoreData.addressAdditional],
    GeoCoo  = [StoreData.latitude, StoreData.longitude],
    create_store_(StoreNumber, StoreData.storeName, StoreData.addressCountry, StoreData.addressState, StoreData.addressCity, AddressValue, GeoCoo, Fridge, Store),
    print_message(info, 'Store exists in the platform')))).



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


is_list_empty_([]).

is_string_empty_("").

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
    