:- use_module('shopping.pl').

:- begin_tests('shopping').

test('test') :-
    test.

test('user login') :-
     gtrace,
    create_store(42, _, _),user_login(111111, 1, 1600953691, 42). 

test('put') :-
    get_put_data(A, B, C, D, E, F),
    writeln([A, B, C, D, E, F]). 

% test('pick object') :-
%     tell(is_physical_object(Object)),
%     pick_object(111111, Object, _, 1600953683.2281373, _).

% test('put back object') :-
%     tell(is_physical_object(Object)),
%     put_back_object(111111, Object, _, 1600953683.2281373, _).

:- end_tests('shopping').