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

test('get store data') :-
    StoreNum is 12034,
    %gtrace,
    get_store_param(Param),
    get_store(StoreNum, Param, Store).

:- end_tests('pa4_db_client').