:- use_module('shopping.pl').

:- begin_tests('shopping').


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
%     %  gtrace,
%     create_store(42, _, _),user_login(111111, 1, 1600953691, 42). 

% test('put') :-
%     get_put_data(A, B, C, D, E, F),
%     writeln([A, B, C, D, E, F]). 


% test('assert pose') :-
%     gtrace, 
%     create_store(1, S, Fridge),
%     shopping:assert_frame_properties(Fridge).


test('assert pose') :-
    gtrace, 
    create_store(1, S, Fridge),
    shopping:assert_layer_properties(Fridge),
    get_time(Now), 
    Stamp2 is Now+5, 
    time_scope(=(Now), =<(Stamp2), QScope),
    is_at('http://knowrob.org/kb/fridge.owl#ShelfLevel_0', LP0),
    is_at('http://knowrob.org/kb/fridge.owl#ShelfLevel_1', LP1),
    writeln(LP0),
    writeln(LP1).


% test('pick object') :-
%     tell(is_physical_object(Object)),
%     pick_object(111111, Object, _, 1600953683.2281373, _).

% test('put back object') :-
%     tell(is_physical_object(Object)),
%     put_back_object(111111, Object, _, 1600953683.2281373, _).

:- end_tests('shopping').