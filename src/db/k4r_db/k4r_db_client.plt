:- use_module('k4r_db_client.pl').
:- use_module(library('rostest')).

:- begin_tripledb_tests(
        'k4r_db_client',
        'package://knowrob/owl/test/test_owl.owl'
  ).

test('k4r_customer_test_1') :-
  k4r_get_link(Link),
  k4r_get_customers(Link, CustomerList),
  k4r_get_entity_by_id(CustomerList, "122", Customer),
  writeln('Returned customer with id 122:'),
  writeln(Customer).

test('k4r_customer_test_2') :-
  k4r_get_link(Link),
  k4r_get_customers(Link, CustomerList),
  k4r_get_customer_by_name(CustomerList, "Megan Firefox", Customer),
  writeln('Returned customer with name Megan Firefox:'),
  writeln(Customer).

test('k4r_store_test_1') :-
  k4r_get_link(Link),
  k4r_get_stores(Link, StoreList),
  k4r_get_store_by_name(StoreList, "Refills Lab", Store),
  writeln('Returned store with name Refills Lab:'),
  writeln(Store).

test('k4r_product_test_1') :-
  k4r_get_link(Link),
  k4r_get_products(Link, ProductList),
  k4r_get_product_by_name(ProductList, "shampoo", Product),
  writeln('Returned store with name shampoo:'),
  writeln(Product).

:- end_tests(k4r_db_client).
