:- ensure_loaded('shopping').

:- tripledb_load(
    'package://knowrob_refills/owl/fridge.owl',
    [ namespace(fridge, 
      'http://knowrob.org/kb/fridge.owl#')
    ]).