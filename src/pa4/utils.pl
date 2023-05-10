/** <module> utils
 *  File with all util predicates   
    @author Kaviya Dhanabalachandran
    @license BSD
*/

:- module( utils,
    [ list_to_string/2,
    string_to_list/2,
    convert_to_m/3,
    is_list_empty_/1,
    is_string_empty_/1,
    copy_list_/2
    ]
    ).

list_to_string(KeyList, KeyString) :-
    atomic_list_concat(KeyList, ',', KeyListConcat),
    atom_string(KeyListConcat, KeyString).

string_to_list(StringVal, List) :-
    atom_chars(StringVal, Stringchar),
    delete(Stringchar, ",", AtomList),
    findall(Anum,
        (member(A, AtomList),
        atom_number(A, Anum)),
    List).

is_list_empty_([]).

is_string_empty_("").

convert_to_m(1, In, Out) :- % 1 is m
    (atom(In) -> atom_number(In, InNum); InNum is In),
    Out is InNum.

convert_to_m(2, In, Out) :- % 2 is mm
    (atom(In) -> atom_number(In, InNum); InNum is In),
    Out is InNum/1000.

convert_to_m(3, In, Out) :- % 3 is cm
    writeln(In),
    (atom(In) -> atom_number(In, InNum); InNum is In),
    Out is InNum/100.

copy_list_([], []).

copy_list_([First | Rest], [First | Copy]) :- copy_list_(Rest, Copy).
