:- module(k4r_db_client,
          [ k4r_get_core_link/1,
            k4r_get_search_link/1,
            k4r_get_entity_by_key_value/4,
            k4r_get_entity_id/2,
            k4r_get_entity_by_id/3,
            k4r_get_product_by_shelf/3,
            post_shelf_location/1,
            post_shelf_layers/1,
            post_shelf_layers/3,
            post_facing/1
          ]).
/** <module> A client for the k4r db for Prolog.

@author Sascha Jongebloed
@license BSD
*/

:- use_foreign_library('libk4r_db_client.so').
:- use_module(library(http/json)).
:- use_module(library('semweb/rdf_db'),
	[ rdf_split_url/3 ]).
:- use_module(library('shop')).

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

k4r_get_core_link(Link) :-
    Link = "http://localhost:8090/k4r-core/api/v0/".
    %Link = "http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/".

k4r_get_search_link(Link) :-
    Link = "http://localhost:7593/k4r-search/".

k4r_get_entity_id(Entity, EntityId) :-
    k4r_get_value_from_key(Entity, "id", EntityId).

k4r_get_entity_by_key_value(EntityList, EntityKey, EntityValue, Entity) :-
    member(Entity, EntityList),
    k4r_check_key_value(Entity, EntityKey, EntityValue).

k4r_get_entity_by_id(EntityList, EntityId, Entity) :-
    k4r_get_entity_by_key_value(EntityList, "id", EntityId, Entity).

% and go on.....

k4r_get_product_by_shelf(Link, Shelf, Product) :-
    k4r_get_entity_id(Shelf, ShelfId),
    k4r_get_shelf_layers(Link, ShelfId, ShelfLayers),
    member(ShelfLayer, ShelfLayers),
    k4r_get_entity_id(ShelfLayer, ShelfLayerId),
    k4r_get_facings(Link, ShelfLayerId, Facings),
    member(Facing, Facings),
    k4r_get_value_from_key(Facing, "productId", ProductId),
    k4r_get_product(Link, ProductId, Product).


post_shelf_location(StoreId):-
  k4r_get_core_link(Link),
  ProductGroupId = 3,
  CadPlanId = "Cad_7",
  findall([ShelfId, ['map', [X,Y,Z],[X1,Y1,Z1,W]], [D, W1, H]],
      (instance_of(Shelf,dmshop:'DMShelfFrame'),
      shelf_with_erp_id(Shelf, ShelfId),
      is_at(Shelf, ['map', [X,Y,Z],[X1,Y1,Z1,W]]),
      object_dimensions(Shelf, D, W1, H),
      k4r_post_shelf(Link, StoreId, [[X,Y,Z], [X1,Y1,Z1,W]], [2,3,4], ProductGroupId,  ShelfId, CadPlanId)
      ),
      _).

%%%%% TODO: Data must be formatted before assigned to a term
post_shelf_layers(StoreId, Shelf, ShelfId) :- %% shelfId is the primary key of the shelf table
    k4r_get_core_link(Link),
    forall(triple(Shelf, soma:hasPhysicalComponent, Layer),
        (writeln('in post shelf'),
        is_at(Layer, ['map', [_,_,Z], _]),
        instance_of(Layer, LayerType),
        rdf_split_url(_,LayerFrame,LayerType),
        writeln('got layer type, pose'),
        triple(Layer, shop:erpShelfLayerId, LayerLevel),
        object_dimensions(Layer, D, W, H),
        writeln('got layer level and dim'),
        writeln(Link), 
        writeln(ShelfId),
        writeln(Z),writeln([D, W, H]), writeln(LayerLevel), writeln(LayerLevel), writeln(LayerFrame),
        k4r_post_shelf_layer(Link, ShelfId, Z, [D, W, H], LayerLevel, LayerLevel, LayerFrame)
        )).
    

post_shelf_layers(StoreId) :-
    k4r_get_search_link(SearchLink),
    forall(
        (instance_of(Shelf,dmshop:'DMShelfFrame'), 
        shelf_with_erp_id(Shelf, ExtRefId)),
        (number_string(ExtRefId, StringId),
        k4r_get_entity_property_by_properties(SearchLink, 'shelf', [['storeId', 'externalReferenceId'], [2, StringId]], "id", ShelfIdList),
        writeln("id list"),
        writeln(ShelfIdList),
        member(ShelfId, ShelfIdList),writeln(ShelfId),
        convert_string_to_int(ShelfId, ShelfIdInt),
        writeln(ShelfIdInt),
        assert_layer_id(Shelf),
        post_shelf_layers(StoreId, Shelf, ShelfIdInt))
        ).

/* post_facing(StoreId) :-
    k4r_get_search_link(SearchLink),
    ProductId = 'id1',
    k4r_get_core_link(Link),
    forall(
        ( triple(Facing, shop:erpFacingId, FacingId),
          triple(Facing, shop:layerOfFacing, Layer),
          triple(Layer, shop:erpShelfLayerId, ErpLayerId),
          triple(Shelf, soma:hasPhysicalComponent, Layer),
          triple(Shelf, shop:erpShelfId, SId)),
        ( convert_string_to_int(Id, ShelfIdInt),
        number_string(SId, StringId),
        k4r_get_entity_property_by_properties(SearchLink, 'shelf', [['storeId', 'externalReferenceId'], [2, StringId]], "id", ShelfIdList),
        member(ShelfId, ShelfIdList),
        convert_string_to_int(ShelfId, ShelfIdInt),
        k4r_get_entity_property_by_properties(SearchLink, 'shelflayer', [['shelfId'], [ShelfIdInt]], "id", ShelfLayerIdValues)
        )), 
    forall(
        member(Id, ShelfLayerIdValues) , 
        (convert_string_to_int(Id, ShelfLayerIdInt),
        get_number_of_items_in_facing(Facing, Quantity),
        k4r_post_facing(Link, ShelfLayerIdInt, ProductId, FacingId, Quantity))
    ). */

post_shelves_and_parts(StoreId) :-
    k4r_get_core_link(Link), 
    k4r_get_search_link(SearchLink),
    forall((instance_of(Shelf,dmshop:'DMShelfFrame'),
      ( triple(Shelf, shop:erpShelfId, Id),
      post_shelf(StoreId, Shelf, ShelfERPId),
      k4r_get_entity_property_by_properties(SearchLink, 'shelf', [['storeId', 'externalReferenceId'], 
          [2, StringId]], "id", ShelfIdList),
        member(ShelfId, ShelfIdList),
        convert_string_to_int(ShelfId, ShelfIdInt),
        post_shelf_layers(Shelf, ShelfId))
      ).

post_facings(ShelfLayer, ShelfLayerId) :-
    k4r_get_core_link(Link),
    ProductId = 'id1',
    forall(
        (triple(Facing, shop:layerOfFacing, Layer)), 
        (get_number_of_items_in_facing(Facing, Quantity),
        triple(Facing, shop:erpFacingId, FacingId)
        k4r_post_facing(Link, ShelfLayerId, ProductId, FacingId, Quantity))
    ).

post_shelf_layers(Shelf, ShelfId) :-
    k4r_get_core_link(Link),
    k4r_get_search_link(SearchLink),
    forall((triple(Shelf, soma:hasPhysicalComponent, Layer)),
        (is_at(Layer, ['map', [_,_,Z], _]),
        instance_of(Layer, LayerType),
        rdf_split_url(_,LayerFrame,LayerType),
        triple(Layer, shop:erpShelfLayerId, LayerLevel),
        object_dimensions(Layer, D, W, H),
        k4r_post_shelf_layer(Link, ShelfId, Z, [D, W, H], LayerLevel, LayerLevel, LayerFrame),
        k4r_get_entity_property_by_properties(SearchLink, 
            'shelflayer', [['shelfId', 'level'], [ShelfIdInt, LayerLevel]], "id", ShelfLayerIdValues),
        member(LayerId, ShelfLayerIdValues),
        convert_string_to_int(LayerId, LayerIdInt),
        post_facings(Layer, LayerIdInt))
        ).
        


post_shelf(StoreId, Shelf, ShelfERPId) :-
    k4r_get_core_link(Link),
    ProductGroupId = 3,
    CadPlanId = "Cad_7",
    is_at(Shelf, ['map', [X,Y,Z],[X1,Y1,Z1,W]]),
    object_dimensions(Shelf, D, W1, H),
    D1 is D*1000, W2 is W1*1000, H1 is H*1000, 
    k4r_post_shelf(Link, StoreId, [[X,Y,Z], [X1,Y1,Z1,W]],
        [D1,W2,H1], ProductGroupId,  ShelfERPId, CadPlanId).


