:- module(k4r_db_client,
          [ k4r_get_entity_id/2,
            k4r_get_entity_by_key_value/4,
            k4r_get_entity_by_id/3,
            k4r_get_customer_by_name/3,
            k4r_get_store_by_name/3,
            k4r_get_product_by_name/3,
            k4r_get_characteristic_by_name/3,
            k4r_get_shelf_by_externalReferenceId/3,
            k4r_get_shelf_layer_by_externalReferenceId/3,
            k4r_get_facing_by_layerRelativePosition/3,
            k4r_get_product_by_shelf/3
          ]).
/** <module> A client for the k4r db for Prolog.

@author Sascha Jongebloed
@license BSD
*/

:- use_foreign_library('libk4r_db_client.so').

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% % % % % C++ predicates

%% k4_db_get(+Path,-Values) is det.
%
% Get the values from the db given the path
%
% @param DB The database name
% @param Values A List of Values
%
k4r_get_link(Link) :-
    Link = "http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/".

k4r_get_entity_id(Entity, EntityId) :-
    k4r_get_value_from_key(Entity, "id", EntityId).

k4r_get_entity_by_key_value(EntityList, EntityKey, EntityValue, Entity) :-
    member(Entity, EntityList),
    k4r_check_key_value(Entity, EntityKey, EntityValue).

k4r_get_entity_by_id(EntityList, EntityId, Entity) :-
    k4r_get_entity_by_key_value(EntityList, "id", EntityId, Entity).

% and go on.....

k4r_get_customer_by_name(CustomerList, CustomerName, Customer) :-
    k4r_get_entity_by_key_value(CustomerList, "anonymisedName", CustomerName, Customer).

k4r_get_store_by_name(StoreList, StoreName, Store) :-
    k4r_get_entity_by_key_value(StoreList, "storeName", StoreName, Store).

k4r_get_product_by_name(ProductList, ProductName, Product) :-
    k4r_get_entity_by_key_value(ProductList, "name", ProductName, Product).

k4r_get_characteristic_by_name(CharacteristicList, CharacteristicName, Characteristic) :-
    k4r_get_entity_by_key_value(CharacteristicList, "name", CharacteristicName, Characteristic).

k4r_get_shelf_by_externalReferenceId(ShelfList, ShelfExtRefId, Shelf) :-
    k4r_get_entity_by_key_value(ShelfList, "externalReferenceId", ShelfExtRefId, Shelf).

k4r_get_shelf_layer_by_externalReferenceId(ShelfLayerList, ShelfLayerExtRefId, ShelfLayer) :-
    k4r_get_entity_by_key_value(ShelfLayerList, "externalReferenceId", ShelfLayerExtRefId, ShelfLayer).

k4r_get_facing_by_layerRelativePosition(FacingList, FacingRelPos, Facing) :-
    k4r_get_entity_by_key_value(FacingList, "layerRelativePosition", FacingRelPos, Facing).

k4r_get_product_by_shelf(Link, Shelf, Product) :-
    k4r_get_entity_id(Shelf, ShelfId),
    k4r_get_shelf_layers(Link, ShelfId, ShelfLayerList),
    member(ShelfLayer, ShelfLayerList),
    k4r_get_entity_id(ShelfLayer, ShelfLayerId),
    k4r_get_facings(Link, ShelfLayerId, Facings),
    member(Facing, Facings),
    k4r_get_value_from_key(Facing, "productId", ProductId),
    k4r_get_product_by_id(Link, ProductId, Product).