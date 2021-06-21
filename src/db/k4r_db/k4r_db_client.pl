:- module(k4r_db_client,
          [
            get_entity_id/2,
            get_entity_by_key_value/4
            % get_product_by_shelf/3,
            % post_shelf_location/1,
            % post_shelf_layers/1,
            % post_facings/2,
            % post_shelves_and_parts/1,
            % post_facings/2,
            % post_shelf_layers/2,
            % post_shelf/3,
            % get_products/0
          ]).

/** <module> A client for the k4r db for Prolog.
@author Sascha Jongebloed
@author Kaviya Dhanabalachandran
@license BSD
*/

:- use_foreign_library('libk4r_db_client.so').
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library('semweb/rdf_db'),
	[ rdf_split_url/3 ]).
:- use_module(library('shop')).
% :- use_module(library('planogram')).

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

get_entity_id(Entity, EntityId) :-
    get_value_from_key(Entity, "id", EntityId).

get_entity_by_key_value(EntityList, EntityKey, EntityValue, Entity) :-
    member(Entity, EntityList),
    check_key_value(Entity, EntityKey, EntityValue).

% and go on.....

% k4r_get_product_by_shelf(Link, Shelf, Product) :-
%     k4r_get_entity_id(Shelf, ShelfId),
%     k4r_get_shelf_layers(Link, ShelfId, ShelfLayers),
%     member(ShelfLayer, ShelfLayers),
%     k4r_get_entity_id(ShelfLayer, ShelfLayerId),
%     k4r_get_facings(Link, ShelfLayerId, Facings),
%     member(Facing, Facings),
%     k4r_get_value_from_key(Facing, "productId", ProductId),
%     k4r_get_product(Link, ProductId, Product).


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

post_shelf_layers(StoreId) :-
    k4r_get_search_link(SearchLink),
    forall(
        (instance_of(Shelf,dmshop:'DMShelfFrame'), 
        shelf_with_erp_id(Shelf, ExtRefId)),
        (number_string(ExtRefId, StringId),
        k4r_get_entity_property_by_properties(SearchLink, 'shelf', [['storeId', 'externalReferenceId'], [StoreId, StringId]], "id", ShelfIdList),
        member(ShelfId, ShelfIdList),
        convert_string_to_int(ShelfId, ShelfIdInt),
        assert_layer_id(Shelf),
        post_shelf_layers(Shelf, ShelfIdInt))
        ).


post_shelves_and_parts(StoreId) :-
    k4r_get_search_link(SearchLink),
    forall((instance_of(Shelf,dmshop:'DMShelfFrame')),
      ( triple(Shelf, shop:erpShelfId, ShelfERPId),
      post_shelf(StoreId, Shelf, ShelfERPId),
      number_string(ShelfERPId, StringId),
      k4r_get_entity_property_by_properties(SearchLink, 'shelf', [['storeId', 'externalReferenceId'], 
          [StoreId, StringId]], "id", ShelfIdList),
        member(ShelfId, ShelfIdList),
        convert_string_to_int(ShelfId, ShelfIdInt),
        post_shelf_layers(Shelf, ShelfIdInt))
      ).

post_facings(ShelfLayer, ShelfLayerId) :-
    k4r_get_core_link(Link),
    k4r_get_search_link(SearchLink),
    forall(
        (triple(Facing, shop:layerOfFacing, ShelfLayer)), 
        (get_number_of_items_in_facing(Facing, Quantity),
        triple(Facing, shop:erpFacingId, FacingId),
        shelf_facing_product_type(Facing, ProductType),
        subclass_of(ProductType, S),
        has_description(S ,value(shop:articleNumberOfProduct,ArticleNumber)),
        k4r_get_entity_property_by_properties(SearchLink, 'product', [['gtin'],[ArticleNumber]], "id", ProductIdList),
        member(ProductId, ProductIdList),
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
        %k4r_post_shelf_layer(Link, ShelfId, Z, [D, W, H], LayerLevel, LayerLevel, LayerFrame),
        % TODO : Fix by filling InShelfLayer
        post_shelf_layer(ShelfId, InShelfLayer, OutShelfLayer),
        get_entity_id(OutShelfLayer, LayerId),
        convert_string_to_int(LayerId, LayerIdInt),
        post_facings(Layer, LayerId))
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


% % Planogram data
% get_products:-
%     get_products(Products),
%     StoreId is 1,
%     create_planogram(StoreId),
%     forall(member(P, Products),
%             (get_dict_from_json_(P, Dict),
%             get_product_attr_from_dict_(Dict, Name, Gtin, Dimension, Weight, Position, NumberOfFacing),
%             create_product_type(Name, Gtin, Dimension, Weight, Position, NumberOfFacing, StoreId, _))).

% get_dict_from_json_(P, Dict) :-
%     atom_to_chars(P,Chars),
%     open_chars_stream(Chars,Stream),
%     json_read_dict(Stream,Dict).

% get_product_attr_from_dict_(Dict, Name, Gtin, Dimension, Weight, Position, NumberOfFacing) :-
%     get_dict(depth, Dict, D),
%     get_dict(weight, Dict, Weight),
%     get_dict(length, Dict, W),
%     get_dict(height, Dict, H),
%     get_dict(name, Dict, Name),
%     get_dict(gtin, Dict, Gtin),
%     get_dict(description, Dict, Disc),
%     get_dict(shelf, Disc, ShelfNum),
%     get_dict(shelflayer, Disc, LayerNum),
%     get_dict(order, Disc, ProductOrder),
%     get_dict(facings, Disc, NumberOfFacing),
%     D_meter is D/1000,
%     H_meter is H/1000,
%     W_meter is W/1000,
%     Dimension = [D_meter, W_meter, H_meter],
%     Position = [ShelfNum, LayerNum, ProductOrder].


/* 

Example Data: 
{ 
    "id":"000000000100002324", 
    "gtin":"4010355055774", 
    "description":
    {  
        "shelf":17, 
        "shelflayer":5, 
        "order":3, 
        "facings":2 
    }, 
    "length":"45", 
    "depth":"205", 
    "height":"95", 
    "weight":"51", 
    "name":"Denkmit Backofen Grillreiniger 500ml" 
}
    {
        "depth" : 85,
        "shelf" : "17",
        "shelflayer" : 5,
        "facing":7,
        "gtin" : "4010355410016",
        "height" : 210,
        "id" : "000000000100002321",
        "length" : 45,
        "name" : "Balea Bodylotion Urea 400ml",
        "weight" : 452
} */