% make sure library path is expanded
:- register_ros_package(knowrob).
:- register_ros_package(knowrob_refills).
:- register_ros_package(knowrob_k4r).

:- use_directory('db').
:- use_directory('pa4').

:- use_module('./customer.pl').
:- use_module('./products.pl').
:- use_module('./purchase.pl').
% :- use_module('./shop.pl').
