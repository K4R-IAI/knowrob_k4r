:- module(k4r_purchase,
	[
	shopping_basket(r),
	in_basket('r','r')
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
shopping_basket('A235').
shopping_basket('A352').
shopping_basket('A524').

%% in_basket(?ID,?Item) is nondet.
%
% True iff there is a customer with this ID that is assigned this basket.
%
% @param ID ID of the customer.
%
in_basket('A235', 'milk').
in_basket('A352', 'water').