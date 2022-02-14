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

% StoreParam = [ storeName,
    %addressCountry,
    %addressCity,
    %addressState,
    %addressStreet,
    %addressPostcode,
    %addressAdditional,
    %latitude,
    %longitude]

create_store(StoreNumber, StoreName, Country, State, City, Address, GeoCoordinates, Store) :-
    get_store_param(Param),
    get_store(StoreNumber, Param, PlatformStore),
    print_message(info, 'Store exists in the platform').


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
    