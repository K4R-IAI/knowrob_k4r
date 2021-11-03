:- use_module('pa4_db_client.pl').

:- begin_tests('pa4_db_client').

% test('get store id') :-
    %pa4_db_client:get_store_id("fridge1", Id), Store id : 1304, shelf Id 1909
    %writeln(Id).

% test('post shelf') :-
    % init_fridge('fridge1', S, F),
    % post_fridge_shelf('fridge1').

test('post fridge layer'):-
    init_fridge('fridge1', S, F),
    post_fridge_shelf_layers('fridge1').

:- end_tests('pa4_db_client').