:- use_module('pa4_db_client.pl').
:- use_module('environment.pl').

:- begin_tests('pa4_db_client').

% Store id : 1034, shelf Id 1908, Layer Id : 2699, 2700

% test('get store id') :-
    %pa4_db_client:get_store_id("fridge1", Id), 
    %writeln(Id).

% test('post shelf') :-
    % init_fridge('fridge1', S, F),
    % post_fridge_shelf('fridge1').

get_increment([V | R], Temp, B) :-
    gtrace,
    X is V*2,
    Temp1 = [X | Temp],
    get_increment(R, Temp1, B).

get_increment([], Temp, Temp).

% test('post fridge layer'):-
    % init_fridge('fridge1', S, F),
    % post_fridge_shelf_layers('fridge1').


% test('post facings') :-
%     init_fridge('fridge1', S, F),
%     gtrace,
%     pa4_db_client:post_fridge_facings("fridge1").

% test('loop') :-
%     A = [2,3,4,5],
%     get_increment(A, [], B),
%     writeln(B).

/* test('get store data') :-
    StoreNum is 12034,
    %gtrace,
    get_store_param(Param),
    get_store(StoreNum, Param, Store),
    get_store_id(StoreNum, StoreId),
    delete_entity_from_id('stores', StoreId). */

/* test('get item group id') :-
    get_item_group_id("9795", "1388", Id).

test('update item data') :-
    update_item_position_platform("id1234", [450, 550, 0]). */

test('get shelf data') :-
    get_shelf_param(Param),
    get_all_shelf_data("1034", Param, ShelfData).
/* test('get item data') :-
    delete_item_and_update_itemgroup("id4323"). */

:- end_tests('pa4_db_client').