:- use_module('shopping.pl').
:- use_module(library('shop_reasoner')).
:- use_module(library('semweb/sparql_client')).

:- begin_tests('shopping').

% test('post store') :-
%     init_fridge('fridge_1', Store, F).

% test('create store'):-
%     % StoreId, Store, Fridge
%     create_store('fridge_1', Store, Fridge),
%     gtrace,
%     shopping:assert_frame_properties(Fridge),
%     shopping:assert_layer_properties(Fridge).


% insert_facing_(Sep1, Sep2, Facing, Pose, Dim) :-
%     % Add a little distance from the separators on both sides to compute dim and X
%     get_child_link_(Sep1, L1),
%     get_object_pose_from_urdf_(L1, [X1, Y1, Z1], _, Parent1),
%     get_object_dimension_from_urdf_(L1, D, W, H),
%     get_child_link_(Sep2, L2),
%     get_object_pose_from_urdf_(L2, [X2, _, _], _, Parent1),
%     Facing_D is (X2-X1),
%     X is (X1+D/2),
%     T = [X, Y1, Z1],
%     Pose is [Parent1, T, [0,0,0,1]],
%     tell([is_physical_object(Facing),
%     triple(Facing, shop:erpFacingId, 1),
%     is_at(Facing, Pose)]),
%     shop:assert_object_shape_(Facing, Facing_D, W, H, [0.5,0.5,0.5]),
%     Dim = [Facing_D, W, H].

% test('insert facing 1') :-
%     insert_facing_('http://knowrob.org/kb/fridge.owl#ShelfSeparator_10',
%         'http://knowrob.org/kb/fridge.owl#ShelfSeparator_11', Facing, Pose ,D),
%     writeln([Facing, Pose, D]).
   

% test('user login') :-
%     gtrace,
%     create_store(42, _, _),
%     user_login(111111, 1, 1600953691, 42).

% test('get facing') :-
%     init_fridge(42, Store, Fridge),
%     shopping:get_facing_(Store, [1,1,1], Facing), 
%     writeln(Facing).

% test('get product type'):-
%     gtrace,
%     EAN= "4010355410016", 
%     atomic_list_concat([ 'PREFIX product_cat: <http://purl.org/goodrelations/v1#>', 
%         'select ?ProductInstance where {?ProductInstance product_cat:hasEAN_UCC-13 "', 
%         EAN,'"}'], Query), 
%     sparql_query(Query, Row, 
%         [ endpoint('https://api.krr.triply.cc/datasets/mkumpel/NonFoodKG/services/NonFoodKG/sparql/'), 
%         variable_names([ProductInstance])] ), 
%     row(Val) = Row.

% test('get product') :-
%     get_product_gtin(shop:'GTIN_4013162021253', Gtin).
    % shopping:get_product_class(4013162021253, Product).

% test('put') :-
%     get_put_data(A, B, C, D, E, F),
%     writeln([A, B, C, D, E, F]). 


% test('assert pose') :-
%     gtrace, 
%     create_store(1, S, Fridge),
%     shopping:assert_frame_properties(Fridge).


/* test('assert pose') :-
    gtrace, 
    create_store(1, S, Fridge),
    shopping:assert_frame_properties(Fridge),
    shopping:assert_layer_properties(Fridge),
    get_time(Now), 
    Stamp2 is Now+5, 
    time_scope(=(Now), =<(Stamp2), QScope),
    is_at('http://knowrob.org/kb/fridge.owl#ShelfLevel_0', LP0),
    is_at('http://knowrob.org/kb/fridge.owl#ShelfLevel_1', LP1),
    writeln(LP0),
    writeln(LP1). */

/* test('check tf') :-
    create_store(1, S, Fridge),
    tell(is_at(fridge:'ShelfBase_0', ['base_link', [-0.1985, 0.0593,0.01015], [0,0,-0.708,0.7054]])),
    tell(is_at('shelf_1_base', ['base_link', [0, -0.241, 0.085], [0,0, -0.708,0.7054]])),
    tell(is_at('shelf_1_level_0_link', ['shelf_1_base', [0,0,0.10665], [0,0, 0,1]])),
    tell(is_at(fridge:'ShelfLevel_0', ['shelf_1_level_0_link', [-0.24, -0.175, -0.022], [0,0,0,1]])),
    gtrace,
    is_at(fridge:'ShelfLevel_0', ['ShelfBase_0', T, R]),
    writeln([T, R]). */

% test('user login') :-
%     create_store(1, S, Fridge),
%     gtrace,
%     get_time(Now), 
%     user_login(100, 101, Now, 1).

% test('pick object') :-
%     tell(is_physical_object(Object)),
%     pick_object(111111, 1, Object, _, 1600953683.2281373, _).

% test('put back object') :-
%     tell(is_physical_object(Object)),
%     put_back_object(111111, Object, _, 1600953683.2281373, _).

test('insert item') :-
    init_fridge(1, Store, Fridge),
    gtrace,
    insert_item(Store, [1, 1, 1], "I4563",'4062300025318', [0.5,0.5], Item),
    is_at(Item, Pose),
    writeln(Pose),
    insert_item(Store, [1, 1, 1], "I4563",'4062300025318', [0.9,0.2], Item),
    is_at(Item, Pose1),
    writeln(Pose1).

/* test('get product class') :-
    shopping:get_product_class('4062300025318', Product). */

:- end_tests('shopping').