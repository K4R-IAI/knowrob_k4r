:- module(k4r_purchase,
	[
	test_sparql(r)
	]).

:- use_module(library('semweb/sparql_client')).

/** <module> Interface to the customer data

@author Sascha
@license BSD
*/

%% customer(?ID) is nondet.
%
% True iff there is a customer with this ID.
%
% @param ID ID of the customer.
%
customer('A235').
customer('A352').
customer('A524').

%% has_shopping_basket(?ID,?Basket) is nondet.
%
% True iff there is a customer with this ID that is assigned this basket.
%
% @param ID ID of the customer.
%
has_in_basket('A235', '10001').
has_in_basket('A352', '10002').