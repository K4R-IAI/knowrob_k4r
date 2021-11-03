:- module(k4r_db_client,
          [
            get_entity_id/2,
            get_entity_by_key_value/4,
            get_shelf_id_by_ext_id/3,
            get_shelf_layer_id_by_ext_id/3,
            get_products_by_shelf/2,
            post_shelves/1,
            delete_shelves/1,
            post_shelf_layers/1,
            delete_shelf_layers/1,
            post_facings/1,
            delete_facings/1,
            get_shelf_data/3,
            get_shelves_data/3,
            get_shelf_layer_id/2,
            get_shelf_layer_ids/2,
            get_graphql/4,
            get_unit_id/2
            % post_shelves_and_parts/1,
            % post_facings/2,
            % post_shelf_layers/2,
            % post_shelf/3,
            % get_products/0
          ]).

/** <module> A client for the k4r db for Prolog.
@author Sascha Jongebloed
@author Kaviya Dhanabalachandran
@author Giang Nguyen
@license BSD
*/

/*  Use assert_shelves_parts_erp_id predicate before using
    predicates to post data. This predicate asserts external reference id of 
    shelves, layers and facings */

:- use_foreign_library('libk4r_db_client.so').
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library('semweb/rdf_db'),
	[ rdf_split_url/3 ]).
:- use_module(library('shop')).
:- use_module(library('shop_reasoner')).
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

% search queries

get_shelf_id_by_ext_id(ShelfList, ExtRefIdFloat, ShelfId) :-
    ExternalReferenceId is integer(ExtRefIdFloat),
    get_entity_by_key_value(ShelfList, "externalReferenceId", ExternalReferenceId, Shelf),
    get_entity_id(Shelf, ShelfId).

get_shelf_layer_id_by_ext_id(ShelfLayerList, ExtRefIdFloat, ShelfLayerId) :-
    ExternalReferenceId is integer(ExtRefIdFloat),
    get_entity_by_key_value(ShelfLayerList, "externalReferenceId", ExternalReferenceId, ShelfLayer),
    get_entity_id(ShelfLayer, ShelfLayerId).

% and go on.....

get_products_by_shelf(ShelfId, ProductList) :-
    get_shelf_layers(ShelfId, ShelfLayerList),
    get_item_groups(ItemGroupList),
    findall(
        Product,
        (
            member(ShelfLayer, ShelfLayerList),
            get_entity_id(ShelfLayer, ShelfLayerId),
            get_facings(ShelfLayerId, FacingList),
            member(Facing, FacingList),
            get_entity_id(Facing, FacingId),
            get_entity_by_key_value(ItemGroupList, "facingId", FacingId, ItemGroup),
            get_value_from_key(ItemGroup, "productUnitId", ProductUnitId),
            get_product_unit(ProductUnitId, ProductUnit),
            get_value_from_key(ProductUnit, "productId", ProductId),
            get_product(ProductId, Product)
        ),
        ProductList
    ).

post_shelves(StoreId) :-
    get_all_shelves(ShelfList),
    forall(
        member(Shelf, ShelfList),
        post_shelf(StoreId, Shelf)
    ).

post_shelf(StoreId, Shelf) :-
    CadPlanId = "Cad_7",
    ProductGroupId = "",
    shelf_with_erp_id(Shelf, ExternalReferenceId),
    is_at(Shelf, ['map', [PositionX, PositionY, PositionZ], [OrientationX, OrientationY, OrientationZ, OrientationW]]),
    object_dimensions(Shelf, DepthInM, WidthInM, HeightInM),
    double_m_to_int_mm([DepthInM, WidthInM, HeightInM], [DepthInMM, WidthInMM, HeightInMM]),
    post_shelf(StoreId, ProductGroupId,
        [CadPlanId, DepthInMM, ExternalReferenceId, HeightInMM,
        OrientationW, OrientationX, OrientationY, OrientationZ,
        PositionX, PositionY, PositionZ, WidthInMM], 
        _).

delete_shelves(StoreId) :-
    get_shelves(StoreId, ShelfList),
    forall(
        member(Shelf, ShelfList),
        (
            get_entity_id(Shelf, ShelfId),
            delete_shelf(ShelfId)
        )
    ).

post_shelf_layers(StoreId) :-
    get_all_shelves(ShelfList),
    forall(
        member(Shelf, ShelfList),
        post_shelf_layers(StoreId, Shelf)
    ).

post_shelf_layers(StoreId, Shelf) :-
    shop:assert_shelf_erp_id(Shelf),
    triple(Shelf, shop:erpShelfId, FloatShelfExternalReferenceId),
    ShelfExternalReferenceId is integer(FloatShelfExternalReferenceId),
    shop:assert_layer_id(Shelf),
    get_shelves(StoreId, ShelfList),
    get_shelf_id_by_ext_id(ShelfList, ShelfExternalReferenceId, ShelfId),
    forall( 
        triple(Shelf, soma:hasPhysicalComponent, ShelfLayer),
        (   
            instance_of(ShelfLayer, LayerType), 
            ((subclass_of(LayerType, dmshop:'DMShelfFloor'),
            rdf_split_url(_, LayerFrame1, LayerType),
            term_to_atom(LayerFrame1, LayerFrame2),
            atom_string(LayerFrame2, LayerFrame));
            LayerFrame = "None"),
            triple(ShelfLayer, shop:erpShelfLayerId, FloatExternalReferenceId),
            ExternalReferenceId is integer(FloatExternalReferenceId),
            is_at(ShelfLayer, ['map', [_,_, PositionZ], _]),
            object_dimensions(ShelfLayer, DepthInM, WidthInM, HeightInM),
            double_m_to_int_mm([DepthInM, WidthInM, HeightInM], [DepthInMM, WidthInMM, HeightInMM]),
            post_shelf_layer(ShelfId, [DepthInMM, ExternalReferenceId, HeightInMM, ExternalReferenceId, PositionZ, LayerFrame, WidthInMM], _)
        )
    ).

delete_shelf_layers(StoreId) :-
    get_shelves(StoreId, ShelfList),
    forall(
        member(Shelf, ShelfList),
        (
            get_entity_id(Shelf, ShelfId),
            get_shelf_layers(ShelfId, ShelfLayerList),
            forall(
                member(ShelfLayer, ShelfLayerList),
                (
                    get_entity_id(ShelfLayer, ShelfLayerId),
                    delete_shelf_layer(ShelfLayerId)
                )
            )
        )
    ).

post_facings(StoreId) :-
    get_shelves(StoreId, ShelfList),
    post_facings_of_shelves(ShelfList).

post_facings_of_shelves(ShelfList):- 
    has_type(ShelfLayer, shop:'ShelfLayer'),
    triple(Shelf, soma:hasPhysicalComponent, ShelfLayer),
    shelf_with_erp_id(Shelf, ShelfExtRefId),
    shop:assert_layer_id(Shelf),
    get_shelf_id_by_ext_id(ShelfList, ShelfExtRefId, ShelfId),
    triple(ShelfLayer, shop:erpShelfLayerId, LayerExtRefId),
    get_shelf_layers(ShelfId, ShelfLayerList),
    get_shelf_layer_id_by_ext_id(ShelfLayerList, LayerExtRefId, ShelfLayerId),
    post_facings(ShelfLayer, ShelfLayerId),
    fail.
 
post_facings_of_shelves(_).

post_facings(ShelfLayer, ShelfLayerId) :-
    forall(
        triple(Facing, shop:layerOfFacing, ShelfLayer), 
        (
            get_number_of_items_in_facing(Facing, NoOfItemDepthFloat),
            triple(Facing, shop:erpFacingId, LayerRelPos),
            NoOfItemDepth is integer(NoOfItemDepthFloat),
            NoOfItemWidth is 1,
            post_facing(ShelfLayerId, [LayerRelPos, NoOfItemDepth, NoOfItemWidth], _)
        )
    ).

delete_facings(StoreId) :-
    get_shelves(StoreId, ShelfList),
    forall(
        member(Shelf, ShelfList),
        (
            get_entity_id(Shelf, ShelfId),
            get_shelf_layers(ShelfId, ShelfLayerList),
            forall(
                member(ShelfLayer, ShelfLayerList),
                (
                    get_entity_id(ShelfLayer, ShelfLayerId),
                    get_facings(ShelfLayerId, FacingList),
                    forall(
                        member(Facing, FacingList),
                        (
                            get_entity_id(Facing, FacingId),
                            delete_facing(FacingId)
                        )
                    )
                )
            )
        )
    ).

make_id_filter("id", ["eq", _, "int"]).

make_entity(Key, Filter, KeyWithFilter) :-
    append("(filter:{", Filter, FilterWithHead),
    append(FilterWithHead, "}", FilterFull),
    string_concat(Key, FilterFull, KeyWithFilter).
    
get_graphql(GraphQLQuery, GraphQLResponse) :-
    post_graphql(GraphQLQuery, GraphQLResponseString),
    open_string(GraphQLResponseString, GraphQLResponseStream),
    json_read_dict(GraphQLResponseStream, GraphQLResponseDict),
    GraphQLResponse=GraphQLResponseDict.data.

get_graphql(GraphQLKey, [FilterField, [Operator, Value, Type]], GraphQlValue, GraphQLResponse) :-
    make_filter(FilterField, Operator, Value, Type, Filter),
    make_key_with_filter(GraphQLKey, Filter, GraphQLKeyWithFilter),
    string_concat(GraphQLKeyWithFilter, GraphQlValue, GraphQLQuery),
    get_graphql(GraphQLQuery, GraphQLResponse).

get_shelf_data(StoreId, RequiredFields, Data) :- % RequiredFields - List
    atomic_list_concat(RequiredFields, ',', TempField),
    atom_string(TempField, FieldString),
    make_id_filter(FilterField, [Operator, StoreId, Type]),
    string_concat("{shelves{", FieldString, Val1),
    string_concat(Val1, "}}}", RequiredString),
    get_graphql("{stores", [FilterField, [Operator, StoreId, Type]], RequiredString, GraphQLResponse),
    member(StoreDict, GraphQLResponse.stores),
    member(ShelfDict, StoreDict.shelves),
    findall(Value,
        (member(ReqField, RequiredFields),
        string_to_atom(ShelfDict.ReqField, Value)),
    Data).

get_shelves_data(StoreId, RequiredFields, DataLists) :-
    findall(DataList, get_shelf_data(StoreId, RequiredFields, DataList), DataLists).
    
get_shelf_layer_id(StoreId, ShelfLayerId) :-
    make_id_filter(FilterField, [Operator, StoreId, Type]),
    get_graphql("{stores", [FilterField, [Operator, StoreId, Type]], "{shelves{shelfLayers{id}}}}", GraphQLResponse),
    member(StoreDict, GraphQLResponse.stores),
    member(ShelfLayerDict, StoreDict.shelves),
    member(ShelfLayerIdDict, ShelfLayerDict.shelfLayers),
    string_to_atom(ShelfLayerIdDict.id, ShelfLayerId).

get_shelf_layer_ids(StoreId, ShelfLayerIds) :-
    findall(ShelfLayerId, get_shelf_layer_id(StoreId, ShelfLayerId), ShelfLayerIds).

get_unit_id(UnitName, Id) :-
    get_graphql("{units", ["name", ["eq", UnitName, "String"]], "{id}}", GraphQLResponse),
    member(UnitDict, GraphQLResponse.units),
    Id = UnitDict.id.


/* post_facings(StoreId) :-
    get_all_shelves(ShelfList),
    forall(
        member(Shelf, ShelfList),
        post_facings(StoreId, Shelf)
    ).

post_facings(StoreId, Shelf) :-
    shop:assert_shelf_erp_id(Shelf),
    triple(Shelf, shop:erpShelfId, FloatShelfExternalReferenceId),
    ShelfExternalReferenceId is integer(FloatShelfExternalReferenceId),
    shop:assert_layer_id(Shelf),
    get_shelves(StoreId, ShelfList),
    get_shelf_id_by_ext_id(ShelfList, ShelfExternalReferenceId, ShelfId),
    forall( 
        (
            triple(Shelf, soma:hasPhysicalComponent, ShelfLayer),
            triple(ShelfLayer, shop:erpShelfLayerId, FloatShelfLayerExternalReferenceId),
            ShelfLayerExternalReferenceId is integer(FloatShelfLayerExternalReferenceId),
            get_shelf_layers(ShelfId, ShelfLayerList),
            get_shelf_layer_id_by_ext_id(ShelfLayerList, ShelfLayerExternalReferenceId, ShelfLayerId)
        ),
        forall(
            triple(Facing, shop:layerOfFacing, ShelfLayer),
            (
                % Todo: fill these variables from Facing
                LayerRelativePosition = "0",
                NumberOfItemsDepth = "1",
                NumberOfItemsWidth = "2",
                post_facing(ShelfLayerId, [LayerRelativePosition, NumberOfItemsDepth, NumberOfItemsWidth], _)
            )
        )
    ).

delete_facings(StoreId) :-
    get_shelves(StoreId, ShelfList),
    forall(
        member(Shelf, ShelfList),
        (
            get_entity_id(Shelf, ShelfId),
            get_shelf_layers(ShelfId, ShelfLayerList),
            forall(
                member(ShelfLayer, ShelfLayerList),
                (
                    get_entity_id(ShelfLayer, ShelfLayerId),
                    get_facings(ShelfLayerId, FacingList),
                    forall(
                        member(Facing, FacingList),
                        (
                            get_entity_id(Facing, FacingId),
                            delete_facing(FacingId)
                        )
                    )
                )
            )
        )
    ). */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Kaviya %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% post_shelf_location(StoreId):-
%   k4r_get_core_link(Link),
%   ProductGroupId = 3,
%   CadPlanId = "Cad_7",
%   findall([ShelfId, ['map', [X,Y,Z],[X1,Y1,Z1,W]], [D, W1, H]],
%       (instance_of(Shelf,dmshop:'DMShelfFrame'),
%       shelf_with_erp_id(Shelf, ShelfId),
%       is_at(Shelf, ['map', [X,Y,Z],[X1,Y1,Z1,W]]),
%       object_dimensions(Shelf, D, W1, H),
%       k4r_post_shelf(Link, StoreId, [[X,Y,Z], [X1,Y1,Z1,W]], [2,3,4], ProductGroupId,  ShelfId, CadPlanId)
%       ),
%       _).

% post_shelf_layers(StoreId) :-
%     k4r_get_search_link(SearchLink),
%     forall(
%         (instance_of(Shelf,dmshop:'DMShelfFrame'), 
%         shelf_with_erp_id(Shelf, ExtRefId)),
%         (number_string(ExtRefId, StringId),
%         k4r_get_entity_property_by_properties(SearchLink, 'shelf', [['storeId', 'externalReferenceId'], [StoreId, StringId]], "id", ShelfIdList),
%         member(ShelfId, ShelfIdList),
%         convert_string_to_int(ShelfId, ShelfIdInt),
%         assert_layer_id(Shelf),
%         post_shelf_layers(Shelf, ShelfIdInt))
%         ).


% post_shelves_and_parts(StoreId) :-
%     k4r_get_search_link(SearchLink),
%     forall((instance_of(Shelf,dmshop:'DMShelfFrame')),
%       ( triple(Shelf, shop:erpShelfId, ShelfERPId),
%       post_shelf(StoreId, Shelf, ShelfERPId),
%       number_string(ShelfERPId, StringId),
%       k4r_get_entity_property_by_properties(SearchLink, 'shelf', [['storeId', 'externalReferenceId'], 
%           [StoreId, StringId]], "id", ShelfIdList),
%         member(ShelfId, ShelfIdList),
%         convert_string_to_int(ShelfId, ShelfIdInt),
%         post_shelf_layers(Shelf, ShelfIdInt))
%       ).

% post_facings(StoreId) :-
%     get_shelves(StoreId, ShelfList),
%     post_facings_of_layers_(StoreId, ShelfList).

% post_facings_of_layers_(StoreId, ShelfList):- 
%     has_type(Layer, shop:'ShelfLayer'),
%     triple(Shelf, soma:hasPhysicalComponent, Layer),
%     triple(Shelf, shop:erpShelfId, ShelfExtRefId),
%     get_shelf_id_by_ext_id(ShelfList, ShelfExtRefId, ShelfId),
%     triple(Layer, shop:erpShelfLayerId, LayerExtRefId),
%     get_shelf_layers(ShelfId, ShelfLayerList),
%     get_shelf_layer_id_by_ext_id(ShelfLayerList, LayerExtRefId, ShelfLayerId),
%     post_facings(Layer, ShelfLayerId),
%     fail.
 
% post_facings_of_layers_(_, _).

% post_facings(ShelfLayer, ShelfLayerId) :-
%     forall(
%         (triple(Facing, shop:layerOfFacing, ShelfLayer)), 
%         (get_number_of_items_in_facing(Facing, NoOfItemDepthFloat),
%         triple(Facing, shop:erpFacingId, LayerRelPos),
%         NoOfItemDepth is integer(NoOfItemDepthFloat),
%         NoOfItemWidth is 1,
%         post_facing(ShelfLayerId, [LayerRelPos, NoOfItemDepth, NoOfItemWidth], _))
%     ).

% post_shelf_layers(Shelf, ShelfId) :-
%     k4r_get_core_link(Link),
%     k4r_get_search_link(SearchLink),
%     forall((triple(Shelf, soma:hasPhysicalComponent, Layer)),
%         (is_at(Layer, ['map', [_,_,Z], _]),
%         instance_of(Layer, LayerType),
%         rdf_split_url(_,LayerFrame,LayerType),
%         triple(Layer, shop:erpShelfLayerId, LayerLevel),
%         object_dimensions(Layer, D, W, H),
%         %k4r_post_shelf_layer(Link, ShelfId, Z, [D, W, H], LayerLevel, LayerLevel, LayerFrame),
%         % TODO : Fix by filling InShelfLayer
%         post_shelf_layer(ShelfId, InShelfLayer, OutShelfLayer),
%         get_entity_id(OutShelfLayer, LayerId),
%         convert_string_to_int(LayerId, LayerIdInt),
%         post_facings(Layer, LayerId))
%         ).
        
% post_shelf(StoreId, Shelf, ShelfERPId) :-
%     k4r_get_core_link(Link),
%     ProductGroupId = 3,
%     CadPlanId = "Cad_7",
%     is_at(Shelf, ['map', [X,Y,Z],[X1,Y1,Z1,W]]),
%     object_dimensions(Shelf, D, W1, H),
%     D1 is D*1000, W2 is W1*1000, H1 is H*1000, 
%     k4r_post_shelf(Link, StoreId, [[X,Y,Z], [X1,Y1,Z1,W]],
%         [D1,W2,H1], ProductGroupId,  ShelfERPId, CadPlanId).


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