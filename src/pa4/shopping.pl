/** <module> shopping
 * 

@author Kaviya Dhanabalachandran
@license BSD
*/


:- module( shopping,
    [   
        init_fridge/3,
        create_store(+, -, -),
        assert_frame_properties/1,
        assert_layer_properties/1,
        user_login(r, r, r, r),
        pick_object(r, r, r, r, r), %% how do we handle probability 
        user_logout(r, r, r, r),
        put_back_object/7,
        items_bought(r, ?),
        insert_all_items(+,+),
        insert_item(+,+,+,+,+,-),
        get_items_in_fridge/2,
        get_user/2,
        get_facing/2
    ]).

:- use_module(library('semweb/sparql_client')).
:- use_foreign_library('libkafka_plugin.so').
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
    ((triple(Store, shop:hasShopId, StoreId),
    print_message(warning, 'Store already exist'));
    (create_store(StoreId, Store, Fridge),
    % writeln('create fridge'),
    % post_fridge_store(StoreId).
    once(shopping:assert_frame_properties(Fridge)),
    % writeln('create shelves'),
    once(shopping:assert_layer_properties(Fridge)))).
    % writeln('create layers').

insert_all_items(StoreId, ItemList) :-
    get_store(StoreId, Store),
    % writeln('insertttt'),
    forall(member([[ShelfExt, ShelfLayerExt, FacingExt], ItemId, Gtin, Coordinates], ItemList),
        (insert_item(Store, [ShelfExt, ShelfLayerExt, FacingExt], ItemId, Gtin, Coordinates, ItemInstance)
        % writeln(ItemInstance)
    )).

% ToDO -- If the item id already exists then just update the position
insert_item(Store, [ShelfExt, ShelfLayerExt, FacingExt], ExtItemId, Gtin, Coordinates, ItemInstance):- % Coordinates - [x,y] is fine
    % writeln(['ITemsss', Store, [ShelfExt, ShelfLayerExt, FacingExt], ExtItemId, Gtin, Coordinates]),
    % check if item exists
    ( item_exists(ExtItemId, ItemInstance),
    update_item_position(Store, ItemInstance, ExtItemId, [ShelfExt, ShelfLayerExt, FacingExt], Coordinates)
    );
    % get facing
    % create an object of the type of Gtin
    % tell(instance_of)
    % associate gtin with the item
    % insert position
    (
    [X ,Y] = Coordinates,
    get_facing_(Store, [ShelfExt, ShelfLayerExt, FacingExt], Facing),
    once(get_product_class(Gtin, Product)),
    % tell([
    tell(instance_of(ItemInstance, Product)),
    tell(triple(Facing, shop:productInFacing, ItemInstance)),
    % writeln([Facing, ItemInstance]),
    rdf_split_url(_,ParentFrame,Facing),
    tell(is_at(ItemInstance, [ParentFrame, [X,Y,0],[0,0,0,1]])),
    tell(triple(ItemInstance, shop:hasItemId, ExtItemId)),
    % ]),
    % insert_item_platform(ExtItemId, Gtin, FacingExtId),
    shop:belief_new_object(Product, ItemInstance)).


update_item_position(Store, ItemInstance, ExtItemId, [ShelfExt, ShelfLayerExt, FacingExt], [X, Y]) :-
    (   item_exists(ExtItemId, ItemInstance),
        get_facing_(Store, [ShelfExt, ShelfLayerExt, FacingExt], Facing),
        tell(triple(Facing, shop:productInFacing, ItemInstance)),
        tell(is_at(ItemInstance, [Facing, [X,Y,0],[0,0,0,1]]))
    ).
    % update_item_platform(ExtItemId, Gtin, FacingExtId),
    %insert_item(Store, [ShelfExt, ShelfLayerExt, FacingExt], ExtItemId, Gtin, [X, Y], ItemInstance).


item_exists(ExtItemId, Item) :-
    triple(Item, shop:hasItemId, ExtItemId).


get_product_class(Gtin, Product) :-
    triple(ArticleNumber, shop:gtin, Gtin),
    has_type(Desc, owl:'Restriction'), 
    has_description(Desc,value(shop:articleNumberOfProduct,ArticleNumber)),
    subclass_of(Product, Desc).
    % transitive(subclass_of(Product, shop:'Product')).

get_product_class(EAN, Product) :-
    atomic_list_concat([ 'PREFIX product_cat: <http://purl.org/goodrelations/v1#>', 
        'select ?ProductInstance where {?ProductInstance product_cat:hasEAN_UCC-13 "', 
        EAN,'"}'], Query), 
    sparql_query(Query, Row, 
        [ endpoint('https://api.krr.triply.cc/datasets/mkumpel/NonFoodKG/services/NonFoodKG/sparql/'), 
        variable_names([ProductInstance])] ),
    row(Product) = Row.

get_product_class(Gtin, Product) :-
    create_article_number(gtin(Gtin), AN),
    create_article_type(AN, Product).

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
    % writeln('half'),
    assert_object_shape_(ShelfBase, D, W, H, [0.5,0.5,0.5]),
    % writeln('half3'),
    get_object_pose_from_urdf_(ChildLink, T1, R1, Parent1),
    % writeln('half2'),
    assert_object_pose_(ShelfBase, ChildLink, [Parent1, T1, R1], D, W, H),
    % writeln('half1'),
    % ShelfBack
    triple(Frame, soma:hasPhysicalComponent, ShelfBack),
    % error ????!!!
    has_type(ShelfBack, shop:'ShelfBack'),
    get_child_link_(ShelfBack, ChildLinkBack),
    %urdf_link_visual_shape(fridge, ChildLinkBack, Dim1, _, _, _),
    % [D1, W1, H1] = Dim1,
    get_object_dimension_from_urdf_(ChildLinkBack, D1, W1, H1),
    assert_object_shape_(ShelfBack, D1, W1, H1, [0.5,0.5,0.5]),
    get_object_pose_from_urdf_(ChildLinkBack, Translation1, Rotation1, ParentName1),
    assert_object_pose_(ShelfBack, ChildLinkBack, [ParentName1, Translation1,Rotation1], D1, W1, H1).
    % tell(is_at(ShelfBack, [ParentName1, Translation1, Rotation1])).

assert_layer_properties(Fridge) :-
    triple(Fridge, soma:hasPhysicalComponent, Frame),
    has_type(Frame, shop:'ShelfFrame'),
    triple(Frame, soma:hasPhysicalComponent, Layer),
    has_type(Layer, shop:'ShelfLayer'),
    get_child_link_(Layer, ChildLink),
    get_object_dimension_from_urdf_(ChildLink, D, W, H),
    assert_object_shape_(Layer, D, W, H, [0.5,0.5,0.5]),
    get_object_pose_from_urdf_(ChildLink, T1, R1, ParentName),
    assert_object_pose_(Layer, ChildLink, [ParentName, T1, R1], D, W, H),
    assert_separator_properties(Layer),
    assert_facing_properties(Layer),
    % tell(is_at(Layer, [ParentName, Translation, Rotation])),
    fail.

assert_layer_properties(_).

assert_separator_properties(Layer) :-
    triple(Layer, soma:hasPhysicalComponent, Separator),
    has_type(Separator, shop:'ShelfSeparator'),
    get_child_link_(Separator, ChildLink),
    get_object_dimension_from_urdf_(ChildLink, D, W, H),
    assert_object_shape_(Separator, D, W, H, [0.5,0.5,0.5]),
    get_object_pose_from_urdf_(ChildLink, T1, R1, ParentName),
    assert_object_pose_(Separator, ChildLink, [ParentName, T1, R1], D, W, H),
    fail.

assert_separator_properties(_).

assert_facing_properties(Layer) :-
    triple(Facing, shop:layerOfFacing, Layer),
    has_type(Facing, shop:'ProductFacing'),
    get_child_link_(Facing, ChildLink),
    get_object_dimension_from_urdf_(ChildLink, D, W, H),
    assert_object_shape_(Facing, D, W, H, [0.5,0.5,0.5]),
    get_object_pose_from_urdf_(ChildLink, T1, R1, ParentName),
    assert_object_pose_(Facing, ChildLink, [ParentName, T1, R1], D, W, H),
    fail.

assert_facing_properties(_).

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
    [P1, [_, _, _], _] = UrdfPose,
    % writeln([UrdfObj, P1, X1, Y1, Z1, Rot]),
    time_scope(=(Now), =<('Infinity'), FScope),
    tf_set_pose(UrdfObj, UrdfPose, FScope),
    X is -(D/2),
    Y is -(W/2),
    Z is -(H/2),
    tf_set_pose(StaticObject,[UrdfObj, [X, Y, Z], [0,0,0,1]], FScope1),
    % writeln([StaticObject, UrdfObj, [X, Y, Z], [0,0,0,1]]),
    Stamp2 is Now+5,
    time_scope(=(Now), =<(Stamp2), QScope),
    % writeln('success2'),
    ((get_child_link_(ParentName, P1),
    rdf_split_url(_,ParentFrame,ParentName)
    );
    ParentFrame = P1), !,
    tf_get_pose(StaticObject, [ParentFrame, T1, R1], QScope, _),
    tf_set_pose(StaticObject, [ParentFrame, T1, R1], FScope).
    % writeln([StaticObject, ParentFrame, T1, R1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Events %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user_login(UserId, DeviceId, Timestamp, StoreId) :-
    ((triple(User, shop:hasUserId, UserId) -> true,  % when there is no existing error, it throws an instantiation error
    print_message(info, 'User has already registered and has not logged out'));
    (triple(Store, shop:hasShopId, StoreId),
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
        time_interval_tell(LoggingInAction, Timestamp, Timestamp),
        writeln([Timestamp,UserId, StoreId]),
        publish_log_in(Timestamp, [UserId, StoreId]))),
        writeln("i am heree").


pick_object(UserId, StoreId, ItemId, Gtin, Timestamp) :-
    % Pose and object type are not used
    % With the gtin
    get_store(StoreId, Store),
    % writeln(["All good 2"]),
    get_item_position(ItemId, Facing),
    % writeln(["All good 3"]),
    triple(Facing, shop:erpFacingId, FacingExtId),
    % get_facing_(Store, Position, Facing),
    % [_, _, FacingExtId] = Position,
    triple(User, shop:hasUserId, UserId),
    % writeln(["All good 4"]),
    is_performed_by(ShoppingAct, User),
    executes_task(ShoppingAct, Tsk), 
    % writeln(["All good 5"]),
    instance_of(Tsk, shop:'Shopping'),
    has_participant(ShoppingAct, Basket),
    instance_of(Basket, shop:'ShopperBasket'),
    item_exists(ItemId, Item),
    % writeln(["All good 6"]),
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
            triple(Basket, soma:containsObject, Item)
        ]),
        % writeln(["All good 1"]),
        % Initialise the store and associate product types with facing
        tripledb_forget(Facing, shop:productInFacing, Item),
        % TODO: delete item in platform
        % delete_item_platform(ItemId, FacingExtId),
        time_interval_tell(PickAct, Timestamp, Timestamp),
        publish_pick_event(Timestamp, [UserId, StoreId, Gtin]). % Not sure her if object type makes sense

put_back_object(UserId, StoreId, ExtItemId, Gtin, Timestamp, Coordinates, Position) :-
    get_store(StoreId, Store),
    [_, _, FacingExtId] = Position,
    % get_facing_(Store, Position, Facing),
    triple(User, shop:hasUserId, UserId),
    is_performed_by(ShoppingAct, User),
    executes_task(ShoppingAct, Tsk), 
    instance_of(Tsk, shop:'Shopping'),
    has_participant(ShoppingAct, Basket),
    instance_of(Basket, shop:'ShopperBasket'),
    tell(
        [    
            is_action(PutAct),
            has_subevent(ShoppingAct, PutAct),
            has_type(Tsk, shop:'PuttingProductOnAShelf'),
            executes_task(PutAct, Tsk),
            has_participant(PutAct, soma:'Hand'),
            is_performed_by(PutAct, User),
            has_type(Motion, soma:'Placing'),
            is_classified_by(PutAct, Motion),
            %triple(Facing, shop:productInFacing, ExtItemId), 
            % TODO : find Product type with gtin
            % create an instance of a Product. Use the item instance
            % in the above triple. 
            has_type(Interval, dul:'TimeInterval'),
            has_time_interval(PutAct, Interval)
        ]),
        % TODO : Add item to a facing in platform
        % insert_item_platform(ExtItemId, Gtin, FacingExtId),
        insert_item(Store, Position, ExtItemId, Gtin, Coordinates, _),
        triple(Item, shop:hasItemId, ExtItemId),
        tripledb_forget(Basket, soma:containsObject, Item),
        time_interval_tell(PutAct, Timestamp, Timestamp),
        publish_return_event(Timestamp, [UserId, StoreId, Gtin]). %% what needs to be here?? Gtin or object type or ?

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
            triple(ShoppingAct,  soma:hasExecutionState, soma:'ExecutionState_Succeeded'),
            has_type(Interval, dul:'TimeInterval'),
            has_time_interval(LogoutAct, Interval)
        ]),
    time_interval_tell(LogoutAct, Timestamp, Timestamp),
    time_interval_tell(ShoppingAct, Start, Timestamp),
    tripledb_forget(_, shop:hasUserId, UserId),
    publish_log_out(Timestamp, [UserId, StoreId]).
    %tripledb_forget(UserId, _, _).
    % Delete all the data of the user id after publishing log out

items_bought(UserId, Items) :-
    triple(User, shop:hasUserId, UserId),
    is_performed_by(ShoppingAct, User),
    executes_task(ShoppingAct, Tsk), 
    instance_of(Tsk, shop:'Shopping'),
    has_participant(ShoppingAct, Basket),
    instance_of(Basket, shop:'ShopperBasket'),
    findall(Item,
        triple(Basket, soma:containsObject, Item),
    Items).

get_facing_(Store, Position, Facing) :-
    has_location(Fridge, Store),
    [ShelfExt, LayerExt, FacingExt] = Position,
    triple(Fridge, soma:hasPhysicalComponent, Shelf),
    triple(Shelf, shop:erpShelfId, ShelfExt),
    triple(Layer, shop:erpShelfLayerId, LayerExt),
    triple(Shelf, soma:hasPhysicalComponent, Layer),
    triple(Facing, shop:erpFacingId, FacingExt),
    triple(Facing, shop:layerOfFacing, Layer).

get_facing(ItemId, Facing) :-
    item_exists(ItemId, Item),
    triple(Facing, shop:productInFacing, Item).

get_store(StoreId, Store) :-
    triple(Store, shop:hasShopId, StoreId).

get_items_in_fridge(StoreId, Items) :-
    %%%%
    % Get all shelves in the store
    % Get all layers in the store
    % Get all facings in the layer
    % Get all items in the facing
    get_store(StoreId, Store),
    holds(Fridge, dul:hasLocation, Store),
    % writeln(['Store', Store]),
    get_all_shelves_in_fridge(Fridge, Shelves),
    % writeln(['Shelf', Shelves]),
    get_all_layers_in_shelves(Shelves, Layers),
    % writeln(['Layer', Layers]),
    get_all_facings_in_layers(Layers, Facings),
    % writeln(['F', Facings]),
    get_all_items_in_fridge_facing(Facings, Items).
    

get_all_shelves_in_fridge(Fridge, Shelves) :-
    findall(Shelf,
        (triple(Fridge, soma:hasPhysicalComponent, Shelf),
        has_type(Shelf, shop:'ShelfFrame')),
    Shelves).

get_all_layers_in_shelves(Shelves, LayersList) :-
    get_all_layers_in_shelves(Shelves, Temp, LayersList).

get_all_layers_in_shelves([], T1, T1).

get_all_layers_in_shelves([Shelf| Rest], T, LL) :-
    get_layers_in_shelf(Shelf, Layers),
    append(T, Layers, T1),
    get_all_layers_in_shelves(Rest, T1, LL).

get_all_facings_in_layers([], T, T).

get_all_facings_in_layers([L | Rest], Temp, FacingList) :-
    ((get_facings_in_layer(L, Facings),
    append(Temp, Facings, Temp1));
    (Temp1 = Temp)),
    get_all_facings_in_layers(Rest, Temp1, FacingList).

get_all_facings_in_layers(L, F) :- 
    get_all_facings_in_layers(L, T, F).

get_all_items_in_fridge_facing([], Temp, Temp).

get_all_items_in_fridge_facing([Facing | Rest], Temp, Items) :-
    ((get_all_items_in_facing(Facing, Item),
    append(Temp, Item, Temp1));
    (Temp1 = Temp)),
    get_all_items_in_fridge_facing(Rest, Temp1, Items).

get_item_position(ItemId, Facing) :-
    % writeln(['Item id', ItemId]),
    triple(ItemInstance, shop:hasItemId, ItemId),
    % writeln(['item', ItemInstance]),
    triple(Facing, shop:productInFacing, ItemInstance).

get_all_items_in_fridge_facing(Facings, Items) :-
    get_all_items_in_fridge_facing(Facings, Temp, Items).

get_all_items_in_facing(Facing, Items) :-
    findall(Item,
        (   has_type(Facing, shop:'ProductFacing'),
            triple(Facing, shop:productInFacing, Item)),
    Items).

get_user(UserId, User) :-
    triple(User, shop:hasUserId, UserId);
    print_message(warning, 'User has logged out').


assert_object_shape_(Object, D, W, H, RGBValue):- 
  (object_dimensions(Object, D, W, H) -> 
    (triple(Object,soma:hasShape,Shape),
  triple(Shape,dul:hasRegion,ShapeRegion));
  tell(has_type(Shape, soma:'Shape')),
  tell(holds(Object,soma:hasShape,Shape)),
  tell(object_dimensions(Object, D, W, H)),
  triple(Shape,dul:hasRegion,ShapeRegion)),
  D1 is D/2, W1 is W/2, H1 is H/2, 
  Pos = [D1, W1, H1], 
  Rot = [0, 0, 0, 1],
  tell(is_individual(Origin)),
  tell(triple(ShapeRegion,'http://knowrob.org/kb/urdf.owl#hasOrigin',Origin)),
  tell(triple(Origin, soma:hasPositionVector, term(Pos))),
  tell(triple(Origin, soma:hasOrientationVector, term(Rot))),

  tell(has_type(ColorType, soma:'Color')),
  tell(holds(Object,soma:hasColor,ColorType)),
  tell(object_color_rgb(Object, RGBValue)), 
  triple(ColorType,dul:hasRegion,Region),
  tell(triple(Region, soma:hasTransparencyValue, 1)), !.
% items_bought(UserId, TimeStamp, Items) :-