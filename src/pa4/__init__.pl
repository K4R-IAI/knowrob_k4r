:- use_module('shopping').
:- use_module('init_store').
:- use_module('pa4_db_client').

:- tripledb_load(
    'package://knowrob_k4r/owl/fridge.owl',
    [ namespace(fridge, 
      'http://knowrob.org/kb/fridge.owl#')
    ]).