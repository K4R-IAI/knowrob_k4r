:- use_module('shopping').
:- use_module('pa4_db_client').

:- tripledb_load(
    'package://knowrob_refills/owl/fridge.owl',
    [ namespace(fridge, 
      'http://knowrob.org/kb/fridge.owl#')
    ]).