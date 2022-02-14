/** <module> utils
 *  File with all util predicates   
    @author Kaviya Dhanabalachandran
    @license BSD
*/

:- module( utils,
    [ list_to_string_/2
    ]
    ).

list_to_string_(KeyList, KeyString) :-
    atomic_list_concat(KeyList, ',', KeyListConcat),
    atom_string(KeyListConcat, KeyString).