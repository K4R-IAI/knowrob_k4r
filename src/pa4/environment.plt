:- use_module('environment.pl').

:- begin_tests('environment').

test('call with a non existing store') :-
    StoreNum is 12034,
    gtrace,
    create_store(StoreNum, 'fridgepa42', "Ger", "BW" , "Bre", ["Uni", 45, 452343, ""], [40, 40], Store).

% test('call with an existing store') :-
%     StoreNum is 1234,
%     %gtrace,
%     create_store(StoreNum, _, _, _ , _, _, _, Store).

:- end_tests('environment').
