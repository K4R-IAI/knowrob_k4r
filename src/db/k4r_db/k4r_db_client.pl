:- module(k4r_db_client,
          [ k4r_get_link/1,
            k4r_get_entity_id/2,
            k4r_get_entity_by_key_value/4,
            k4r_get_entity_by_id/3,
            k4r_get_product_by_shelf/3
            % write_shelf_location/0,
            % get_shelf_data/0
            %get_products_in_shelflayer/2
          ]).
/** <module> A client for the k4r db for Prolog.

@author Sascha Jongebloed
@license BSD
*/

:- use_foreign_library('libk4r_db_client.so').
:- use_module(library('http/json')).
:- use_module(library(http/json)).
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

k4r_get_link(Link) :-
    % Link = "http://localhost:8090/k4r-core/api/v0/".
    Link = "http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/".

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

% get_shelf_data :-
%   k4r_get_link(Link),
%   k4r_get_shelf(Link, ShelfId, Shelf), % your ShelfId
%   k4r_get_shelf_data(
%     Shelf,
%     [ShelfPositionX, ShelfPositionY, ShelfPositionZ], 
%     [ShelfOrientationX, ShelfOrientationY, ShelfOrientationZ, ShelfOrientationW], 
%     [ShelfDepth, ShelfWidth, ShelfHeight]),
%   writeln(ShelfHeight).

% write_shelf_location:-
%   k4r_get_link(Link),
%   findall([ShelfId, ['map', [X,Y,Z],[X1,Y1,Z1,W]], [D, W1, H]],
%       (instance_of(Shelf,dmshop:'DMShelfFrame'),
%       shelf_with_erp_id(Shelf, ShelfId),
%       is_at(Shelf, ['map', [X,Y,Z],[X1,Y1,Z1,W]]),
%       object_dimensions(Shelf, D, W1, H)
%       % writeln(ShelfId)
%       ),
%     ShelfInfo),
%     StoreId is 2, % test data from Giang
%     member([ShelfId, [_, Translation, Rotation], Dimensions], ShelfInfo),
%     once(k4r_post_shelf(Link, StoreId, ShelfId, [Translation, Rotation], [2,3,4])).
%     % forall(member([ShelfId, [RefFrame, [X,Y,Z], []], D, W, H], ShelfInfo),
%     %     )


% get_facings_in_shelflayer(ShelfId, Facings) :-  %% ShelfLayer_Facings -- [ShelfLayerId, Facings]
%     k4r_get_link(Link), 
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

