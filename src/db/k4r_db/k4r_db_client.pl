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

post_facing(StoreId) :-
    k4r_get_search_link(SearchLink),
    % k4r_get_entity_property_by_properties(SearchLink, 'shelf', [['storeId'], [2]], "id", ShelfIdValues),
    %forall((member(Id, ShelfIdValues)), 
    % convert_string_to_int(Id, ShelfIdInt)),
    k4r_get_entity_property_by_properties(SearchLink, 'shelflayers', [['shelfId'], [7]], "id", ShelfLayerIdValues), 
    forall((member(Id, ShelfLayerIdValues) , convert_string_to_int(Id, ShelfLayerIdInt)), writeln(ShelfLayerIdInt)),
    writeln(ShelfLayerIdValues).


  % get_facings_in_shelflayer(ShelfId, Facings) :-  %% ShelfLayer_Facings -- [ShelfLayerId, Facings]
%     k4r_get_core_link(Link), 
%     k4r_get_shelf_layers(Link, ShelfId, ShelfLayers), 
%     %findall(ShelfLayerId, Facings,
%     member(ShelfLayer, ShelfLayers),
%     k4r_get_entity_id(ShelfLayer, ShelfLayerId),
%         k4r_get_facings(Link, ShelfLayerId, Facing)),
%         Facing.

% get_products_in_shelflayer(ShelfId, FacingAndProduct) :- %% FacingAndProduct -- [FacingId, ProductId]
%     get_facings_in_shelflayer(ShelfId, LayersAndFacings),
%     %findall([FacingId, ProductId], 
%     (member([Layer, Facing], LayersAndFacings),
%     k4r_get_entity_id(Facing, FacingId),
%     k4r_get_value_from_key(Facing, "productId", ProductId)).
%     %FacingAndProduct).

