/** <module> init store
 * A test module for shopping-pa4

@author Kaviya Dhanabalachandran
@license BSD
*/

:- module( init_store,
    [   
        create_store_and_init_fridge/0,
        test_log_in/1,
        test_log_out/1,
        test_pick_up/2,
        test_put_back/1
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

test_log_in(StoreNum) :-
    get_time(Now), 
    UserId is 100,
    DeviceId is 101,
    user_login(UserId, DeviceId, Now, StoreNum),
    get_user(UserId, _).
    % writeln(User).

test_pick_up(StoreNum, ItemId) :-
    %writeln("in pick up"),
    UserId is 100,
    Gtin = '4010355520036',
    get_time(Now),
    % Is position necessary? We do know the position given the id of the item
    pick_object(UserId, StoreNum, ItemId, Gtin, Now),
    items_bought(UserId,  Items),
    writeln(Items).

test_put_back(StoreNum) :-
    UserId is 100,
    ItemId = 'I4599',
    Gtin = '4010355520036',
    get_time(Now),
    Position = [1, 1, 1],
    Coordinates = [0.025, -0.05],
    put_back_object(UserId, StoreNum, ItemId, Gtin, 
        Now, Coordinates, Position),
    get_facing(ItemId, Facing),
    writeln(Facing).


test_log_out(StoreNum) :-
    get_time(Now), 
    UserId is 100,
    DeviceId is 101,
    user_logout(UserId, DeviceId, Now, StoreNum),
    get_user(UserId, _).

:- begin_tests('init_store').

test('store init') :-
    gtrace,
    StoreNum = 300,
    init_store(StoreNum),
    % shopping:insert_all_fridge_items(StoreNum, [1, 1, 1],'4010355520036',[['I4563', [0.07, -0.05]], 
    %     ['I4564', [0.07,-0.02]], ['I4567', [0.08, -0.03]]]),
    get_items_in_fridge(StoreNum, Items),
    writeln(Items).

test('events') :-
    StoreNum is 300,
    ItemId = 'I4567',
    
    test_log_in(StoreNum),
    writeln('log in done'),
    gtrace,
    %test_pick_up(StoreNum, ItemId),
    %writeln('pick up done'),
    % gtrace,
    test_put_back(StoreNum),
    writeln('put back done'),
    test_log_out(StoreNum),
    writeln('log out done').

/* test('tf structure') :-
    gtrace,
    environment:create_store_(456, 'fridgepa42', "Ger", "BW" , "Bre", ["Uni", 45, 452343, ""], [40, 40], Store),
    init_fridge(456, Store, Fridge). */

:- end_tests('init_store').