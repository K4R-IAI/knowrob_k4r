/** <module> init store
 * A test module for shopping-pa4

@author Kaviya Dhanabalachandran
@license BSD
*/

:- module( init_store,
    [   
        create_store_and_init_fridge/0,
        test_log_in/0,
        test_log_out/0,
        test_pick_up/0,
        test_put_back/0
]).

% store Id - 5


create_store_and_init_fridge :-
    % writeln('initttttt'),
    init_fridge(5, Store, Fridge),
    %writeln('insertingg'),
    insert_all_items(5, 
        [
            [[1, 1, 1], 'I4563','4062300025318', [0.5,0.5]], 
            [[1, 1, 1], 'I4564','4062300025318', [0.0,0.2]],
            [[1, 1, 1], 'I4567','4062300025318', [0.6,0.7]],
            [[1, 1, 2], 'I4568','4062300020719', [0.5,0.5]], 
            [[1, 1, 2], 'I4569','4062300020719', [0.0,0.2]],
            [[1, 1, 2], 'I4570','4062300020719', [0.6,0.7]],
            [[1, 1, 3], 'I4571','4062300265998', [0.5,0.5]], 
            [[1, 1, 3], 'I4572','4062300265998', [0.0,0.2]],
            [[1, 1, 3], 'I4573','4062300265998', [0.6,0.7]]
    ]).

test_log_in :-
    get_time(Now), 
    UserId is 100,
    DeviceId is 101,
    user_login(UserId, DeviceId, Now, 5),
    get_user(UserId, User).
    % writeln(User).

test_pick_up :-
    %writeln("in pick up"),
    UserId is 100,
    StoreId is 5,
    ItemId = 'I4563',
    Gtin = '4062300025318',
    get_time(Now),
    Position = [1, 1, 1],
    % Is position necessary? We do know the position given the id of the item
    pick_object(UserId, StoreId, ItemId, Gtin, Now),
    items_bought(UserId, Items).
    % writeln(Items).

test_put_back :-
    UserId is 100,
    StoreId is 5,
    ItemId = 'I4563',
    Gtin = '4062300025318',
    get_time(Now),
    Position = [1, 1, 1],
    Coordinates = [0.5, 0.5],
    put_back_object(UserId, StoreId, ItemId, Gtin, 
        Now, Coordinates, Position),
    get_facing(ItemId, Facing).
    % writeln(Facing).


test_log_out :-
    get_time(Now), 
    UserId is 100,
    DeviceId is 101,
    user_logout(UserId, DeviceId, Now, 5),
    get_user(UserId, User).

:- begin_tests('init_store').

test('store init') :-
    create_store_and_init_fridge,
    StoreId is 5,
    % gtrace,
    get_items_in_fridge(StoreId, Items).
    % writeln(Items).

    
    

test('events') :-
    test_log_in,
    % writeln('log in done'),
    test_pick_up,
    % writeln('pick up done'),
    % gtrace,
    test_put_back,
    % writeln('put back done'),
    test_log_out.
    % writeln('log out done').

:- end_tests('init_store').