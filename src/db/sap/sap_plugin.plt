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
  sap_mng_get_price_of_MATNR(string('000000000100000022'),Price),
  write(Price),
  write('\n').
  %DB=roslog,
  %Name=sap,
  %P1=,
  %write(-------------------------------),
  %mng_db_name(DB),
  %mng_cursor_create(DB,Name,Cursor),
  %mng_cursor_filter(Cursor,['MATNR',P1]),
  %mng_cursor_materialize(Cursor,Item),
  %write('Item:\n'),
  %write(Item),
  %write('\n'),
  %mng_get_dict('E1WBB03',Item,ProductInfos),
  %mng_get_dict('E1WBB07',ProductInfos,ProductInfosPricerelated),
  %mng_get_dict('E1WBB08',ProductInfosPricerelated,ProductInfosPriceInfo),
  %mng_get_dict('KWERT',ProductInfosPriceInfo,Price),
  %write(Price).

:- end_tests('sap_plugin')
