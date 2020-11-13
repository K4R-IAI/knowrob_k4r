:- use_module('./k4r_db_client.pl').
:- use_module(library('rostest')).

:- begin_tripledb_tests(
        'k4r_db_client',
        'package://knowrob/owl/test/test_owl.owl'
  ).

test('k4r_db_values') :-
  k4rdb_values('k4r-core/api/v0/stores',StoreList),
  writeln('Returned Stores:'),
  writeln(StoreList),
  k4rdb_values('k4r-core/api/v0/customers',CustomerList),
  writeln('Returned customers:'),
  writeln(CustomerList).

:- end_tests(k4r_db_client).
