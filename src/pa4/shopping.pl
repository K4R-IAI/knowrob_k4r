/** <module> shopping
 * A client for the k4r db for Prolog.

@author Kaviya Dhanabalachandran
@license BSD
*/


%%% TODO : Check if you are able to get the user

:- module( shopping,
    [   
        init_fridge/3,
        create_store(+, -, -),
        assert_frame_properties/1,
        assert_layer_properties/1,
        user_login(r, r, r, r),
        pick_object(r, r, r, r, r, r), %% how do we handle probability 
        user_logout(r, r, r, r),
        put_back_object/6,
        items_bought(r, ?)
    ]).

:- use_foreign_library('libkafka_plugin.so').
:- use_foreign_library('librest_interface.so').
:- use_module(library('semweb/rdf_db')).
:- use_module(library('model/SOMA/ACT')).
:- use_module(library('lang/terms/is_a')).
:- use_module(library('model/DUL/Event')).
:- use_module(library('model/SOMA/EXT')).
:- use_module(library('ros/urdf/URDF')).

:- use_module(library('ros/tf/tf_plugin'), 
                [tf_get_pose/4, tf_set_pose/3]).

:- rdf_db:rdf_register_ns(soma,
    'http://www.ease-crc.org/ont/SOMA.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(shop, 'http://knowrob.org/kb/shop.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(urdf, 'http://knowrob.org/kb/urdf.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(fridge, 'http://knowrob.org/kb/fridge.owl#', [keep(true)]).


init_fridge(StoreId, Store, Fridge) :-
    % StoreId, Store, Fridge
    create_store(StoreId, Store, Fridge),
    % post_fridge_store(StoreId).
    shopping:assert_frame_properties(Fridge),
    shopping:assert_layer_properties(Fridge).

%% Create Store
create_store(StoreId, Store, Fridge) :-
    has_type(Fridge, shop:'SmartFridge'),
    load_fridge_urdf_,
    tell([ instance_of(Store, shop:'Shop'),
        triple(Store, shop:hasShopId, StoreId),
        has_location(Fridge, Store)
        ]).

%% Create the physical rep with properties of the fridge
assert_frame_properties(Fridge) :-
    triple(Fridge, soma:hasPhysicalComponent, Frame),
    has_type(Frame, shop:'ShelfFrame'),
    % ShelfBase
    triple(Frame, soma:hasPhysicalComponent, ShelfBase),
    has_type(ShelfBase, shop:'ShelfBase'),
    get_child_link_(ShelfBase, ChildLink),
    get_object_dimension_from_urdf_(ChildLink, D, W, H),
    % [D, W, H] = Dim,
    writeln('half'),
    shop:assert_object_shape_(ShelfBase, D, W, H, [0.5,0.5,0.5]),
    writeln('half3'),
    get_object_pose_from_urdf_(ChildLink, T1, R1, Parent1),
    writeln('half2'),
    assert_object_pose_(ShelfBase, ChildLink, [Parent1, T1, R1], D, W, H),
    writeln('half1'),
    % ShelfBack
    triple(Frame, soma:hasPhysicalComponent, ShelfBack),
    has_type(ShelfBack, shop:'ShelfBack'),
    get_child_link_(ShelfBack, ChildLinkBack),
    % urdf_link_visual_shape(fridge, ChildLinkBack, Dim1, _, _, _),
    % [D1, W1, H1] = Dim1,
    get_object_dimension_from_urdf_(ChildLinkBack, D1, W1, H1),
    shop:assert_object_shape_(ShelfBack, D1, W1, H1, [0.5,0.5,0.5]),
    get_object_pose_from_urdf_(ChildLinkBack, Translation1, Rotation1, ParentName1),
    tell(is_at(ShelfBack, [ParentName1, Translation1, Rotation1])).

assert_layer_properties(Fridge) :-
    triple(Fridge, soma:hasPhysicalComponent, Frame),
    has_type(Frame, shop:'ShelfFrame'),
    triple(Frame, soma:hasPhysicalComponent, Layer),
    has_type(Layer, shop:'ShelfLayer'),
    get_child_link_(Layer, ChildLink),
    get_object_dimension_from_urdf_(ChildLink, D, W, H),
    shop:assert_object_shape_(Layer, D, W, H, [0.5,0.5,0.5]),
    get_object_pose_from_urdf_(ChildLink, T1, R1, ParentName),
    assert_object_pose_(Layer, ChildLink, [ParentName, T1, R1], D, W, H),
    assert_separator_properties(Layer),
    % tell(is_at(Layer, [ParentName, Translation, Rotation])),
    fail.

assert_layer_properties(_).

assert_separator_properties(Layer) :-
    triple(Layer, soma:hasPhysicalComponent, Separator),
    has_type(Separator, shop:'ShelfSeparator'),
    get_child_link_(Separator, ChildLink),
    get_object_dimension_from_urdf_(ChildLink, D, W, H),
    shop:assert_object_shape_(Separator, D, W, H, [0.5,0.5,0.5]),
    get_object_pose_from_urdf_(ChildLink, T1, R1, ParentName),
    assert_object_pose_(Separator, ChildLink, [ParentName, T1, R1], D, W, H),
    fail.

assert_separator_properties(_).

%% assert layer, separators and facigns on the layer. Assert their dimensions
% insert_layer_components(LayerNumber, 2) :-
%     % 1. assert the layer dimensions
%     has_type(Layer, shop:'ShelfLayer'),
%     triple(Layer, shop:erpShelfLayerId, LayerNum),
%     is_at(Layer, [Parent, [X,_,_], _]),
%     object_dimensions(Layer, D, W, H),
%     DX is D/2.
    %shop:perceived_pos__()
    
% 2. based on number of facings, attach the separators to the layer
%  X pos of the separators has to be computed based on their order and from the x value of the Layer position

% 3. assert the dimensions of facings 
% a. (H of layer above - layer below) - 0.1
% b. no layer above case - (H of shelf frame - layer of facing)-0.1 ?

%compute_offset_(X, Axis, Offset) :-


get_child_link_(Object, Child) :-
    triple(Object, urdf:hasEndLinkName, Child).

get_object_dimension_from_urdf_(Object, D, W, H) :-
    urdf_link_visual_shape(fridge, Object, Dim, _, _, _),
    box(D, W, H) = Dim.

get_object_pose_from_urdf_(Object, Translation, Rotation, ParentName) :-
    urdf_link_parent_joint(fridge, Object, Joint), 
    urdf_joint_origin(fridge, Joint, P),
    [ParentName, Translation, Rotation] = P.

load_fridge_urdf_:-
    ros_package_path('knowrob_k4r', X), 
    atom_concat(X, '/urdf/fridge.urdf', Filename), 
    urdf_load_file(fridge, Filename).

assert_object_pose_(StaticObject, UrdfObj, UrdfPose, D, W, H) :-
    %WorldFrame = 'base_link',
    get_time(Now),
    Stamp is Now + 10,
    time_scope(=(Now), =<(Stamp), FScope1),
    [P1, [X1, Y1, Z1], Rot] = UrdfPose,
    writeln([UrdfObj, P1, X1, Y1, Z1, Rot]),
    time_scope(=(Now), =<('Infinity'), FScope),
    tf_set_pose(UrdfObj, UrdfPose, FScope),
    X is -(D/2),
    Y is -(W/2),
    Z is -(H/2),
    tf_set_pose(StaticObject,[UrdfObj, [X, Y, Z], [0,0,0,1]], FScope1),
    writeln([StaticObject, UrdfObj, [X, Y, Z], [0,0,0,1]]),
    Stamp2 is Now+5,
    time_scope(=(Now), =<(Stamp2), QScope),
    writeln('success2'),
    ((get_child_link_(ParentName, P1),
    rdf_split_url(_,ParentFrame,ParentName)
    );
    ParentFrame = P1), !,
    % tf_get_pose(StaticObject, [WorldFrame, T1, R1], QScope, _),
    % tf_set_pose(StaticObject, [WorldFrame, T1, R1], FScope),
    % writeln([StaticObject, WorldFrame, T1, R1]),
    % ParentFrame \= WorldFrame -> 
    % tell(is_at(StaticObject, [ParentFrame, T1, R1])).
    tf_get_pose(StaticObject, [ParentFrame, T1, R1], QScope, _),
    tf_set_pose(StaticObject, [ParentFrame, T1, R1], FScope),
    writeln([StaticObject, ParentFrame, T1, R1]).

%%%% Get the pose and the dimension from urdf

/* ros_package_path('knowrob_refills', X), atom_concat(X, '/urdf/fridge.urdf', Filename), urdf_load_file(fridge, Filename), 
urdf_link_visual_shape(fridge, shelf_1_level_1_link, ST, O, MT, SId), urdf_joint_origin(fridge, shelf_1_level_0_joint, P).
P: [u'shelf_1_base', [0.0, -0.06, 0.10665], [0.0, 0.0, 0.0, 1.0]],
O: [u'shelf_1_level_1_link', [0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 1.0]],
ST: {u'term': [u'box', 0.48, 0.35, 0.043]},
MT: {u'term': [u'material', [{u'term': [u'rgba', [0.5, 0.5, 0.5, 0.800000011920929]]}]]},
SId: shelf_1_level_1_link0,
X: /home/kaviya/ros_ws/src/knowrob_refills,
Filename: /home/kaviya/ros_ws/src/knowrob_refills/urdf/fridge.urdf. */






% create_planogram(ProductId, Name, ProductPose, StoreId) :-
%     tell(),

user_login(UserId, DeviceId, Timestamp, StoreId) :-
   triple(Store, shop:hasShopId, StoreId),
   has_location(Fridge, Store),
   tell([is_action(ParentAct),
        has_participant(ParentAct, Fridge),
        has_type(PaInterval, dul:'TimeInterval'),
        has_time_interval(ParentAct, PaInterval),
        triple(PaInterval, soma:hasIntervalBegin, Timestamp),
        instance_of(User, shop:'Customer'),
        triple(User, shop:hasUserId, UserId),
        instance_of(Task,shop:'Shopping'),
        has_type(Role, soma:'Location'),
        has_task_role(Task, Role),
        is_performed_by(ParentAct, User),
        instance_of(ShoppingBasket, shop:'ShopperBasket'),
        has_participant(ParentAct, ShoppingBasket),
        has_type(Motion, soma:'Holding'),
        is_classified_by(ParentAct, Motion),
        triple(ParentAct, soma:hasExecutionState, soma:'ExecutionState_Active'),
        executes_task(ParentAct, Task)]), !,
    tell(
       [ is_action(LoggingInAction),
        has_subevent(ParentAct, LoggingInAction), 
        has_type(Device, shop:'MobileDevice'),
        has_participant(LoggingInAction, Device),
        triple(Device, shop:hasDeviceId, DeviceId),
        instance_of(Tsk1,shop:'LoggingIn'),
        executes_task(LoggingInAction, Tsk1),
        is_performed_by(LoggingInAction, User),
        has_type(Interval, dul:'TimeInterval'),
        has_time_interval(LoggingInAction, Interval)]),
        time_interval_tell(LoggingInAction, Timestamp, Timestamp).
        %publish_log_in(TimeStamp, [UserId, StoreId]).


pick_object(UserId, StoreId, ItemId, ObjectType, Timestamp, Position) :-
    % Pose and object type are not used
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
        time_interval_tell(PickAct, Timestamp, Timestamp).
        %publish_pick_event(TimeStamp, [UserId, StoreId, ObjectType]).

put_back_object(UserId, ItemId, ObjectType, Timestamp, Position, PosCoordinates) :-
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
    time_interval_tell(ShoppingAct, Start, Timestamp).
    %publish_log_out(Timestamp, [UserId, StoreId]).

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