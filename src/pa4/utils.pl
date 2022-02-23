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
    is_string_empty_/1
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
    Out is In.

convert_to_m(2, In, Out) :- % 2 is mm
    Out is In/1000.

convert_to_m(3, In, Out) :- % 3 is cm
    writeln(In),
    Out is In/100.
