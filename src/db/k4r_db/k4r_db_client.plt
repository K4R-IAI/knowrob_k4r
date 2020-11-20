:- use_module('./k4r_db_client.pl').
:- use_module(library('rostest')).

:- begin_tripledb_tests(
        'k4r_db_client',
        'package://knowrob/owl/test/test_owl.owl'
  ).

test('k4r_db_values') :-
  k4r_get_stores("http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/",StoreList),
  writeln('Returned Stores:'),
  writeln(StoreList),

  k4r_get_customers("http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/",CustomerListBefore),
  writeln('Returned customers before delete:'),
  writeln(CustomerListBefore),

  k4r_post_customer("http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/","Giang"),

  k4r_delete_customer("http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/","Giang"),

  k4r_get_customers("http://ked.informatik.uni-bremen.de:8090/k4r-core/api/v0/",CustomerListAfter),
  writeln('Returned customers after delete:'),
  writeln(CustomerListAfter).

:- end_tests(k4r_db_client).
