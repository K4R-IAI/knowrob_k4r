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
    create_store(15, 'fridgepa42', "Ger", "BW" , "Bre", ["Uni", 45, 452343, ""], [40, 40], _),
    init_fridge("15", _, _). % store Pl Id 4958
    %writeln('insertingg'),
    /* insert_all_items(45, [1, 1, 1],'4010355520036',
        [['I4563', [0.5,0.5]], ['I4564', [0.0,0.2]], ['I4567', [0.6,0.7]]]),
    insert_all_items(45, [1, 1, 2],'4008617009771',
        [['I4568', [0.5,0.5]], ['I4569', [0.0,0.2]], ['I4570', [0.6,0.7]]]),
    insert_all_items(45, [1, 1, 3],'4005800431241',
        [['I4571', [0.5,0.5]], ['I4572', [0.0,0.2]], ['I4573', [0.6,0.7]]]). */

test_log_in :-
    get_time(Now), 
    UserId is 100,
    DeviceId is 101,
    user_login(UserId, DeviceId, Now, 45),
    get_user(UserId, _).
    % writeln(User).

test_pick_up :-
    %writeln("in pick up"),
    UserId is 100,
    StoreId is 45,
    ItemId = 'I4574',
    Gtin = '4010355520036',
    get_time(Now),
    % Is position necessary? We do know the position given the id of the item
    pick_object(UserId, StoreId, ItemId, Gtin, Now),
    items_bought(UserId, _).
    % writeln(Items).

test_put_back :-
    UserId is 100,
    StoreId is 45,
    ItemId = 'I4591',
    Gtin = '4010355520036',
    get_time(Now),
    Position = [1, 1, 1],
    Coordinates = [0.25, 0.5],
    put_back_object(UserId, StoreId, ItemId, Gtin, 
        Now, Coordinates, Position),
    get_facing(ItemId, Facing),
    writeln(Facing).


test_log_out :-
    get_time(Now), 
    UserId is 100,
    DeviceId is 101,
    user_logout(UserId, DeviceId, Now, 45),
    get_user(UserId, _).

:- begin_tests('init_store').

test('store init') :-
    gtrace,
    create_store_and_init_fridge,
    StoreId = 45,
    get_items_in_fridge(StoreId, _).

test('events') :-
    gtrace,
    test_log_in,
    %writeln('log in done'),
    test_pick_up,
    %writeln('pick up done'),
    % gtrace,
    test_put_back,
    %writeln('put back done'),
    test_log_out.
    %writeln('log out done').

/* test('tf structure') :-
    gtrace,
    environment:create_store_(456, 'fridgepa42', "Ger", "BW" , "Bre", ["Uni", 45, 452343, ""], [40, 40], Store),
    init_fridge(456, Store, Fridge). */

:- end_tests('init_store').