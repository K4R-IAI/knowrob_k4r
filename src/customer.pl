:- module(k4r_customer,
	[  customer(r),
	   has_shopping_basket(r,r)
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
customer('235').
customer('352').
customer('524').

has_shopping_basket('234','A235').
