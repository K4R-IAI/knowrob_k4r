/** <module> shopping_events
 * A client for the k4r db for Prolog.

@author Kaviya Dhanabalachandran
@license BSD
*/


%%% TODO : Check if you are able to get the user

:- module( shopping_events,
    [   
        create_store(+, -, -),
        user_login(r, r, r, r),
        pick_object(r, r, r, r, r), %% how do we handle probability 
        user_logout(r, r, r),
        put_back_object(r,r,r,r,r),
        items_bought(r, ?)
    ]).

:- use_foreign_library('libkafka_plugin.so').
:- use_module(library('semweb/rdf_db')).
:- use_module(library('model/SOMA/ACT')).
:- use_module(library('lang/terms/is_a')).
:- use_module(library('model/DUL/Event')).
:- use_module(library('model/SOMA/EXT')).

:- rdf_db:rdf_register_ns(shop, 'http://knowrob.org/kb/shop.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(soma,
    'http://www.ease-crc.org/ont/SOMA.owl#', [keep(true)]).

%% Create Store
create_store(StoreId, Store, Fridge) :-
    tell([ instance_of(Store, shop:'Shop'),
        triple(Store, shop:hasShopId, StoreId),
        has_type(Fridge, shop:'SmartFridge'),
        has_location(Fridge, Store)
        ]).

% create_planogram(ProductId, Name, ProductPose, StoreId) :-
%     tell(),

user_login(UserId, DeviceId, TimeStamp, StoreId) :-
   triple(Store, shop:hasShopId, StoreId),
   has_location(Fridge, Store),
   tell([is_action(ParentAct),
        has_participant(ParentAct, Fridge),
        instance_of(User, shop:'Customer'),
        instance_of(Task,shop:'Shopping'),
        has_type(Role, soma:'Location'),
        has_task_role(Task, Role),
        is_performed_by(ParentAct, User),
        
        instance_of(ShoppingBasket, shop:'ShopperBasket'),
        has_participant(ParentAct, ShoppingBasket),
        has_type(Role1, soma:'Patient'),
        has_task_role(Task, Role1),
        has_type(Motion, soma:'Holding'),
        is_classified_by(ParentAct, Motion),
        triple(ParentAct, soma:hasExecutionState, soma:'ExecutionState_Active'),
        executes_task(ParentAct, Task)
   ]),

   tell(
       [ is_action(LoggingInAction),
        has_subevent(ParentAct, LoggingInAction), 
        triple(User, shop:hasUserId, UserId),
        has_type(Device, shop:'MobileDevice'),
        has_participant(LoggingInAction, Device),
        triple(Device, shop:hasDeviceId, DeviceId),
        instance_of(Tsk1,shop:'LoggingIn'),
        executes_task(LoggingInAction, Tsk1),
        is_performed_by(LoggingInAction, User)
        ]),
        time_interval_tell(LoggingInAction, Timestamp, Timestamp),
        publish_log_in(TimeStamp, [UserId, StoreId]).


pick_object(UserId, StoreId, ItemId, ObjectType, Timestamp, Position) :-
    triple(User, shop:hasUserId, UserId),
    is_performed_by(ShoppingAct, User),
    executes_task(ShoppingAct, Tsk), 
    instance_of(Tsk, shop:'Shopping'),
    has_participant(ShoppingAct, Basket),
    instance_of(Basket, shop:'ShopperBasket'),
    tell(
        [  
            is_action(PickAct),
            has_subevent(ShoppingAct, PickAct),
            has_type(Task1, shop:'PickProduct'),
            executes_task(PickAct, Task1),
            has_participant(PickAct, soma:'Hand'),
            is_performed_by(PickAct, User),
            has_type(Motion, soma:'PuttingProductInABasket'),
            is_classified_by(PickAct, Motion),
            %% TODO : create an instance of a Product. find Product type with object id or object type.
            triple(Basket, soma:containsObject, ItemId)
        ]),
        time_interval_tell(PickAct, Timestamp, Timestamp),
        publish_pick_event(TimeStamp, [UserId, ]).

put_back_object(UserId, ItemId, ObjectType, Timestamp, Position) :-
    triple(User, shop:hasUserId, UserId),
    is_performed_by(ShoppingAct, User),
    executes_task(ShoppingAct, Tsk), 
    instance_of(Tsk, shop:'Shopping'),
    has_participant(ShoppingAct, Basket),
    instance_of(Basket, shop:'ShopperBasket'),
    %% TODO : create a shopping basket, assign it to user, user holds basket, basket contains item
    tell(
        [    
            is_action(PutAct),
            has_subevent(ShoppingAct, PutAct),
            has_type(Tsk, soma:'Placing'),
            executes_task(PutAct, Tsk),
            has_participant(PutAct, soma:'Hand'),
            is_performed_by(PutAct, User),
            has_type(Motion, shop:'PuttingProductSomewhere'),
            is_classified_by(PutAct, Motion)
        ]),
            %% TODO : create an instance of a Product. find Product type with object id or object type.
        tripledb_forget(Basket, soma:containsObject, ItemId),
        time_interval_tell(PutAct, Timestamp, Timestamp).

user_logout(UserId, DeviceId, Timestamp, StoreId) :-
    %% Device might be another participant in the login and logout action  
    triple(User, shop:hasUserId, UserId),
    is_performed_by(ShoppingAct, User),
    executes_task(ShoppingAct, Tsk), 
    instance_of(Tsk, shop:'Shopping'),

    has_subevent(ShoppingAct, LoginAct),
    executes_task(LoginAct, LoginTsk),
    instance_of(LoginTsk, shop:'LoggingIn'),
    time_interval_data(LoginAct, Start, _),
    tell(
        [  
            is_action(LogoutAct),
            has_subevent(ShoppingAct, LogoutAct),
            instance_of(Tsk1,shop:'LoggingOut'),
            executes_task(LogoutAct, Tsk1),
            is_performed_by(LogoutAct, User),
            triple(ShoppingAct,  soma:hasExecutionState, soma:'ExecutionState_Succeeded')
        ]),
    time_interval_tell(LogoutAct, Timestamp, Timestamp),
    time_interval_tell(ShoppingAct, Start, Timestamp), 
    publish_log_out(Timestamp, [UserId, StoreId]).

items_bought(UserId, Items) :-
    triple(User, shop:hasUserId, UserId),
    is_performed_by(ShoppingAct, User),
    executes_task(ShoppingAct, Tsk), 
    instance_of(Tsk, shop:'Shopping'),
    has_participant(ShoppingAct, Basket),
    instance_of(Basket, shop:'ShopperBasket'),
    findall(ItemId,
        triple(Basket, soma:containsObject, ItemId),
    Items).

% items_bought(UserId, TimeStamp, Items) :-