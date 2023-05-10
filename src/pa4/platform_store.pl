:- module(platform_store,
    [create_store/1]
    ).

:- use_module(user_defined_environment).

create_store(StoreNum) :-
    init_store(StoreNum),
    triple(Store, shop:hasShopNumber, StoreNum),
    forall((has_type(Frame, shop:'ShelfFrame'), triple(Frame, dul:hasLocation, Store)),
        (object_dimensions(Frame, D, W, H),
        H1 is H*100, D1 is D*10, W1 is W*10, 
        W2 is truncate(W1), D2 is truncate(D1), H2 is truncate(H1),
        get_valid_shelf(H2, W2, D2, W3, D3, Prefix),
        atomics_to_string(['ShelfSystemH', H2, 'T', W3, 'L', D3, 'W'], Type), 
        (ground(Prefix) -> atomics_to_string(['package://refills_models/shelf_models/', Type, '/', Prefix, Type, '.dae'], Path);
        atomics_to_string(['package://refills_models/shelf_models/', Type, '/', Type, '.dae'], Path)),
        assert_mesh_path(Frame, Path),
        assert_path_for_all_layers(Frame, D3, W3)
    )
    ).

assert_path_for_all_layers(Frame, Depth, Width) :-
    findall(Layer,
        (triple(Frame, soma:hasPhysicalComponent, Layer),
        tripledb_forget(Layer, shop:erpShelfLayerId, _)
    ), _),
    assert_layer_id(Frame),
    forall(triple(Frame, soma:hasPhysicalComponent, Layer),
        (   
            
            ((get_valid_blayer(Depth, Width, D, W),
            triple(Layer, shop:erpShelfLayerId, 1) -> tell(has_type(Layer, dmshop:'DMShelfBFloor')),
            atomics_to_string(['ShelfLayer', D, 'TilesL', W, '_Bottom'], Type)
            );
            (get_valid_layer(Depth, Width, D, W, Prefix),
                atomics_to_string(['ShelfLayer', D, 'TilesL', W], Type)
            )),
            (ground(Prefix) -> atomics_to_string(['package://refills_models/shelf_models/', Type, '/', Prefix, Type, '.dae'], Path);
        atomics_to_string(['package://refills_models/shelf_models/', Type, '/', Type, '.dae'], Path)
            ),
            assert_mesh_path(Layer, Path)
        )).

get_valid_shelf(180, 5, 6, 5, 6, _).
get_valid_shelf(180, 5, 10, 5, 10, 'SM_').
get_valid_shelf(180, _, _, W3, D3, 'SM_') :-
    W3 is 5, 
    D3 is 10.

get_valid_shelf(160, 4, 10, 4, 10,_).
get_valid_shelf(160, 5, 10, 5, 10, _).
get_valid_shelf(160, 6, 10, 6, 10, 'SM_').
get_valid_shelf(160, _, _, 6, 10, 'SM_').

get_valid_shelf(200, 5, 6, 5, 6, _).
get_valid_shelf(200, 5, 10, 5, 10, _).
get_valid_shelf(200, 5, 12, 5, 12, _).
get_valid_shelf(200, 6, 6, 6, 6, _).
get_valid_shelf(200, 6, 7, 6, 7, _).
get_valid_shelf(200, 6, 10, 6, 10, _).
get_valid_shelf(200, 6, 12, 6, 12, _).
get_valid_shelf(200, 7, 10, 7, 10, _).
get_valid_shelf(200, 7, 12, 7, 12, _).
get_valid_shelf(200, _, _, W3, D3, _) :-
    W3 is 7,
    D3 is 12.


get_valid_layer(4, 10, 4, 10, _).
get_valid_layer(4, _, 4, 10, _).
get_valid_layer(5, 6, 5, 6, _).
get_valid_layer(5, 7, 5, 7, _).
get_valid_layer(5, 10, 5, 10, _).
get_valid_layer(5, 12, 5, 12, _).
get_valid_layer(5, _, 5, 12, _).
get_valid_layer(6, 10, 6 ,10, _).
get_valid_layer(6, 12, 6 ,12, TypeName):-
    TypeName = 'SM_'.
get_valid_layer(6, _, 6 ,12, 'SM_').
get_valid_layer(7, _, 6, 12, 'SM_').
get_valid_layer(_, _, 6, 12, 'SM_').

get_valid_blayer(4, 10, 4, 10).
get_valid_blayer(4, _, 4, 10).
get_valid_blayer(5, 10, 5, 10).
get_valid_blayer(5, _, 5, 10).
get_valid_blayer(6, 6, 6, 6).
get_valid_blayer(6, 7, 6 ,7).
get_valid_blayer(6, 10, 6 ,10).
get_valid_blayer(6, 12, 6 ,12).
get_valid_blayer(6, _, 6 ,12).
get_valid_blayer(_, _, 7, 12).
get_valid_blayer(7, 10, 7 ,10).
get_valid_blayer(7, 12, 7 ,12).
get_valid_blayer(7, _, 7, 12).


assert_mesh_path(Object, Path) :-
    triple(Object, soma:hasShape,Shape),
    triple(Shape,dul:hasRegion,ShapeRegion),
    tripledb_forget(ShapeRegion,soma:hasFilePath, _),
    tell(triple(ShapeRegion,soma:hasFilePath, Path)),
    writeln(Path),
    object_dimensions(Object, D, W, H),
    %D1 is D/2, W1 is W/2, 
    (H = 0 -> H1 is 0; H1 is H/2),
    Pos = [0,0,H1],
    Rot = [0.0, 0.0 , 0, 1],
    tell(is_individual(Origin)),  
    tell(triple(ShapeRegion,'http://knowrob.org/kb/urdf.owl#hasOrigin',Origin)),  
    tell(triple(Origin, soma:hasPositionVector, term(Pos))),  
    tell(triple(Origin, soma:hasOrientationVector, term(Rot))).


:- begin_tests(platform_store).

test('create shelf') :-
    gtrace,
    create_store(2920).