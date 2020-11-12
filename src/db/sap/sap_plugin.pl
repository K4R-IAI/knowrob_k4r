:- module(sap_plugin,
	[
	sap_mng_wipe/0,
	sap_mng_get_item_filter/3,
	sap_mng_get_field_values/2,
	sap_mng_get_price_of_MATNR/2
	]).

:- use_module(library('db/mongo/client')).


sap_db(DB, Name) :-
	mng_get_db(DB, Name, 'sap').

%%
sap_mng_wipe :-
	mng_db_name(DB),
	sap_db(DB, _).
%	mng_drop(DB,Name).


sap_mng_init :-
	mng_db_name(DB),
	sap_db(DB, Name),
	mng_index_create(DB,Name,['MATNR']). %is that neccesary?
	%%

:- sap_mng_init.

%sap_mng_get_field_values(+Field,?FieldValues)
sap_mng_get_field_values(Field,FieldValues) :-
	mng_db_name(DB),
	sap_db(DB, Name),
	mng_cursor_create(DB,Name,Cursor),
	atomic_list_concat(Fields, ., Field),
	get_field_values(Fields, _, Cursor, FieldValues),
	mng_cursor_destroy(Cursor).

get_field_values([Field|[]], [], Cursor, FieldValues) :-
	get_field_root_values(Field, Cursor, FieldValues).

get_field_values([FieldParent|FieldChild], [], Cursor, FieldValues) :-
	get_field_root_values(FieldParent, Cursor, FieldParentValues),
	get_field_values(FieldChild, FieldParentValues, Cursor, FieldValues).

get_field_values([FieldParent|FieldChild], FieldParentValues, _, FieldValue) :-
	findall(FieldParentValue,
		(	member(Dict, FieldParentValues),
			mng_get_dict(FieldParent, Dict, FieldParentValue)
		), FieldChildValues),
	get_field_values(FieldChild, FieldChildValues, _, FieldValue).

get_field_values([Field|[]], DictList, _, FieldValues) :-
	findall(FieldValue,
		( member(Dict, DictList),
			mng_get_dict(Field, Dict, FieldValue)
		), FieldValues).

get_field_root_values(Field, Cursor, FieldValues) :-
	findall(FieldValue,
		(	mng_cursor_materialize(Cursor,Items),
			mng_get_dict(Field,Items,FieldValue)
		), FieldValues).


%sap_mng_get_item_filter((+Field,+FieldValues,?Items))
sap_mng_get_item_filter(Field,FieldValue,Items) :-
	mng_db_name(DB),
	sap_db(DB, Name),
	mng_cursor_create(DB,Name,Cursor),
	mng_cursor_filter(Cursor,[Field,FieldValue]),
	findall(Item,
	(    mng_cursor_materialize(Cursor,Item)
	), Items),
	mng_cursor_destroy(Cursor).

sap_mng_get_price_of_MATNR(MATNR,Price) :-
	mng_db_name(DB),
	sap_db(DB, Name),
	mng_cursor_create(DB,Name,Cursor),
	mng_cursor_filter(Cursor,['MATNR',MATNR]),
	mng_cursor_materialize(Cursor,Item),
	mng_get_dict('E1WBB03',Item,ProductInfos),
	mng_get_dict('E1WBB07',ProductInfos,ProductInfosPricerelated),
	mng_get_dict('E1WBB08',ProductInfosPricerelated,ProductInfosPriceInfo),
	mng_get_dict('KWERT',ProductInfosPriceInfo,Price),
	mng_cursor_destroy(Cursor).
