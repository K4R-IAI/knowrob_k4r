:- module(k4r_db_client,
    [ k4_db_get/2
    ]).
/** <module> A client for the k4r db for Prolog.

@author Sascha Jongebloed
@license BSD
*/

/*
  triple(L, rdf:first, X),
  triple(L, rdf:rest, Ys)
-->
db.triples.aggregate([
   { $match: { p: rdf:rest } },
   { $graphLookup: {
      from: "triples",
      startWith: "$o", connectFromField: "o", connectToField: "s",
      restrictSearchWithMatch: { p: rdf:rest },
      as: "paths"
   }}
])
-->
{ [ ... ] }
*/

:- use_foreign_library('lib_k4rdb_kb.so').

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% % % % % C++ predicates

%% k4_db_get(+Path,-Values) is det.
%
% Get the values from the db given the path
%
% @param DB The database name
% @param Values A List of Values
%
