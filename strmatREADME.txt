
asynchronous trigger is composed of two parts.

* strmats.sql sets up dictionary tables, types, and a package to implement 
  asynchronous trigger using streams. 

* strmatu.sql is an example that creates two row triggers and a table 
  trigger on table u1.accounts, recording any changes to user Alice's 
  balance.

* archiving must be enabled in the database.

 Unlike traditional triggers, asynchronous triggers are created and dropped 
 by a PL/SQL API. The when clause and trigger body syntax are mostly the same,
 with following exceptions:

 * value of the changed row will be referenced as old, new instead of 
   :old, :new.

 * attribute function inserting, updating, deleting will NOT work in 
   asynchronous triggers. Instead one should use 

   bitand(dml_events, async_trig_pkg.on_insert) = async_trig_pkg.on_insert
   bitand(dml_events, async_trig_pkg.on_update) = async_trig_pkg.on_update
   bitand(dml_events, async_trig_pkg.on_delete) = async_trig_pkg.on_delete

   to identify inserting, updating, deleting events respectively.

 Package async_trig_pkg contains the following procedures:

 * set_up_streams() which sets up capture and apply for asynchronous triggers
 * clean_up_streams() which cleans up streams capture apply, queue, etc.
 * create_trigger() which creates an asynchronous trigger
 * drop_trigger() which drops an asynchronous trigger

 LIMITATIONS:

   This package is just a prototype and has the following limitations.
   
   * Name canonicalization. It is assumed that all names passed to 
     async_trig_pkg APIs are ALREADY canonicalized. 

   * Datatype support. The column type of the table on which asynchoronous 
     triggers are defined are limited to the following data types:

     NUMBER, FLOAT, VARCHAR2, RAW, DATE, TIMESTAMP.

   * ALTERATION. To alter a trigger one needs to drop and recreate it.
     Same is the streams set up.

   * ERROR HANDLING. Because we are not inside the kernel, DDL operations
     (set_up_streams, create_trigger etc.) are not strictly atomic. If an
     operation raised an error for some reason, the asynchronous trigger 
     dictionary data might be left in an inconsistent state. In this case,
     ORA-20001 (streams already exists) or ORA-20009 (trigger already 
     exists) will be raised when you retry the operation.

     drop_trigger operation can clean up the state of a failed create_trigger.
     clean_up_streams can clean up the state of a failed set_up_streams.

   * SUPPLIMENTAL LOGGING. To be able to see unchanged column values in
     an update asynchronous trigger, supplemental logging needed to be
     turned on for that table. The asynchronous trigger script will not
     turn on supplemental logging by default, because it can affect the
     performance of the user DML txn.

   Most of the limitations could be amended given some more time. If any
   of the above is not acceptable, please let me know.

 INSTRUCTIONS:

 -- create streams by calling set_up_streams()
 -- create trigger on table of interest using create_trigger()
 -- check that trigger is fired on DMLs of the table
 -- drop unwanted triggers by drop_trigger()
 -- clean_up_streams() will drop all asynchronous triggers, drop capture, 
    apply, queue, etc.
 -- check async_trigger_usecase.sql for an example
 -- report bugs and problems to wei.wang@oracle.com
