:- module(linkeddata_plugin,
	[
	test_sparql(r)
	]).

:- use_module(library('semweb/sparql_client')).

test_sparql(Row) :-
	sparql_query('select * where { ?x rdfs:label "Amsterdam" }', Row, [ host('dbpedia.org'), path('/sparql/')]).
