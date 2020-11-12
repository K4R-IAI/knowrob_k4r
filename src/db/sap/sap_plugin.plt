:- use_module('sap_plugin.pl').
:- use_module(library('rostest')).
:- use_module(library('lang/query')).
:- use_module(library('lang/scopes/temporal')).
:- use_module(library('db/mongo/client')).

:- begin_tripledb_tests(
                'sap_plugin',
                'package://knowrob/owl/test/swrl.owl',
                [ namespace('http://knowrob.org/kb/swrl_test#'),
                  setup(sap_mng_wipe),
                  cleanup(sap_mng_wipe)
                ]).

%Debugging at its finest
:- use_module(library('db/mongo/client')).

test('sap_mng_get_field_values') :-
	sap_mng_get_field_values('E1WBB03.E1WBB07.E1WBB08.KWERT',X),
	write(X),
	write('\n').

test('sap_mng_get_item_filter') :-
	sap_mng_get_item_filter('MATNR',string('000000000100000022'),Items),
	write(Item),
	write('\n').

test('sap_mng_get_price_of_MATNR') :-
  write(-------------------------------),
  write('\n'),
  sap_mng_get_price_of_matnr('000000000100000022',Price),
  write(Price),
  write('\n').

test(sap_mng_get_shelf_of_matnr) :-
  sap_mng_get_shelf_of_matnr('000000000100000022',Shelf),
  write(Shelf),
  write('\n').

test(sap_mng_get_width_of_matnr):-
  sap_mng_get_width_of_matnr('000000000100000022',Width),
  write(Width),
  write('\n').

test(sap_mng_get_height_of_matnr) :-
  sap_mng_get_height_of_matnr('000000000100000022',Height),
  write(Height),
  write('\n').

test(sap_mng_get_lenght_of_matnr):-
  sap_mng_get_lenght_of_matnr('000000000100000022',Lenght),
  write(Lenght),
  write('\n').

test(sap_mng_get_sortf_of_matnr):-
  sap_mng_get_sortf_of_matnr('000000000100000022',Sortf),
  write(Sortf),
  write('\n').

test(sap_mng_get_ammountfacings_of_matnr):-
  sap_mng_get_ammountfacings_of_matnr('000000000100000022',Facings),
  write(Facings),
  write('\n').

:- end_tests('sap_plugin')
