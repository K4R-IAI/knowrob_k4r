/** <module> shopping_fridge
 * A client for the k4r db for Prolog.

@author Kaviya Dhanabalachandran
@license BSD
*/

:- module( shopping_fridge,
    [
        user_login(r, r, r),
        pick_object(r, r, r, r, r), %% how do we handle probability 
        user_logout(r, r, r),
        put_back_object(r,r,r,r,r),
        items_bought(r, ?)
    ]).

:- use_module(library('semweb/rdf_db')).
:- use_module(library('model/SOMA/ACT')).
:- use_module(library('lang/terms/is_a')).
:- use_module(library('model/DUL/Event')).
:- use_module(library('model/SOMA/EXT')).

:- rdf_db:rdf_register_ns(shop, 'http://knowrob.org/kb/shop.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(soma,
    'http://www.ease-crc.org/ont/SOMA.owl#', [keep(true)]).


user_login(UserId, DeviceId, TimeStamp) :-
    %% Device might be another participant in the first action
   tell(is_action(ParentAct)),
   tell(
       [ is_action(Action),
        has_subevent(ParentAct, Action),
        instance_of(User, shop:'Customer'),
        triple(User, shop:hasUserId, UserId),
        triple(User, shop:hasDeviceId, DeviceId),
        instance_of(Tsk1,shop:'LoggingIn'),
        executes_task(Action, Tsk1),
        is_performed_by(Action, User),
        % occurs(Action) during [TimeStamp, TimeStamp+1]
        is_action(ParentAct),
        instance_of(Tsk2,shop:'Shopping'),
        instance_of(ShoppingBasket, shop:'ShopperBasket'),
        has_participant(ParentAct, ShoppingBasket),
        has_type(Motion, soma:'Holding'),
        is_performed_by(ParentAct, User),
        is_classified_by(ParentAct, Motion),
        triple(Tsk1, soma:starts, Tsk2),
        executes_task(ParentAct, Tsk2),
        triple(ParentAct, soma:hasExecutionState, soma:'ExecutionState_Active')
        ]),
        time_interval_tell(Action, Timestamp, Timestamp).
       


pick_object(UserId, ItemId, ObjectType, Timestamp, Position) :-
    triple(User, shop:hasUserId, UserId),
    is_performed_by(ShoppingAct, User),
    executes_task(ShoppingAct, Tsk), 
    instance_of(Tsk, shop:'Shopping'),
    has_participant(ShoppingAct, Basket),
    %% TODO : create a shopping basket, assign it to user, user holds basket, basket contains item
    tell(
        [  
            is_action(PickAct),
            has_subevent(ShoppingAct, PickAct),
            has_type(Task1, shop:'PickProduct'),
            executes_task(PickAct, Task1),
            has_participant(PickAct, soma:'Hand'),
            is_performed_by(PickAct, User),
            has_type(Motion, soma:'PuttingProductInABasket'), % Not sure if it is picking up or putting product in a basket
            is_classified_by(PickAct, Motion),
            %% TODO : create an instance of a Product. find Product type with object id or object type.
            triple(Basket, soma:containsObject, ItemId)
        ]),
        time_interval_tell(PickAct, Timestamp, Timestamp).

put_back_object(UserId, ItemId, ObjectType, Timestamp, Position) :-
    triple(User, shop:hasUserId, UserId),
    is_performed_by(ShoppingAct, User),
    executes_task(ShoppingAct, Tsk), 
    instance_of(Tsk, shop:'Shopping'),
    has_participant(ShoppingAct, Basket),
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

user_logout(UserId, DeviceId, Timestamp) :-
    %% Device might be another participant in the login and logout action
    triple(User, shop:hasUserId, UserId),
    is_performed_by(ShoppingAct, User),
    executes_task(ShoppingAct, Tsk), 
    instance_of(Tsk, shop:'Shopping'),
    has_participant(ShoppingAct, Basket),

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
    time_interval_tell(ShoppingAct, Start, Timestamp).

items_bought(UserId, Items) :-
    triple(User, shop:hasUserId, UserId),
    is_performed_by(ShoppingAct, User),
    executes_task(ShoppingAct, Tsk), 
    instance_of(Tsk, shop:'Shopping'),
    has_participant(ShoppingAct, Basket),
    findall(ItemId,
    triple(Basket, soma:containsObject, ItemId),
    Items).

% items_bought(UserId, TimeStamp, Items) :-
