/** <module> utils
 *  File with all util predicates   
    @author Kaviya Dhanabalachandran
    @license BSD
*/

:- module( utils,
    [ list_to_string/2
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
