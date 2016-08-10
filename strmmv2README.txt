/
/ $Header: strmmv2README.txt 08-sep-2006.14:04:06 wesmith Exp $
/
/ strmmv2README.txt
/
/ Copyright (c) 2006, Oracle. All Rights Reserved.
/
/   NAME
/     strmmv2README.txt - <one-line expansion of the name>
/
/   DESCRIPTION
/     <short description of component this file declares/defines>
/
/   NOTES
/     <other useful comments, qualifications, etc.>
/
/   MODIFIED   (MM/DD/YY)
/   wesmith     06/23/06 - register_mv: add parameter queue_name
/   wesmith     06/12/06 - corrections
/   wesmith     01/24/06 - Creation
/

Streams-driven MV refresh
-------------------------
A set of packages is provided that allows the user to set up an MV for 
streams-driven refresh. Whenever a user transaction commits that modifies 
the MV's master tables, the MV will be refreshed, either using 
dbms_mview.refresh() or a streams-based refresh algorithm. This provides
on-commit MV behavior asynchronously.

In addition, MV refresh changes may be processed using user-supplied 
plsql code. 

Packages:
streams_mv_refresh_adm
streams_mv_refresh
streams_mv_lcr


Package streams_mv_refresh_adm
==============================
1. register_mv: sets up an MV for streams-driven refresh

procedure register_mv(mv_owner             varchar2, 
                      mv_name              varchar2, 
                      capture_name         varchar2  := 'CAPTURE_MV_MASTER',
                      apply_name           varchar2  := 'APPLY_MV_MASTER',
                      queue_name           varchar2  := NULL,
                      instantiate          boolean   := FALSE, 
                      use_streams_refresh  boolean   := FALSE,
                      mv_dml_handler       varchar2  := NULL);

Parameters
----------
mv_owner, mv_name: MV to register

capture_name: 
  name of the capture process to use. It is recommended to specify a capture
  that does not already exist.

apply_name: 
  name of the apply process to use. It is recommended to specify an apply
  that does not already exist.

queue_name:
  name of the streams queue associated with the capture and apply process.
  If NULL, then the queue_name is derived from the capture process
  metadata, if it exists.

instantiate: 
  if TRUE, then the MV is refreshed (FAST if possible) and
  the refresh scn is used for the streams instantiation scn. If FALSE,
  then the MV is not refreshed and the current scn is used (the MV is 
  assumed to be fresh with respect to its dependent master tables.)

use_streams_refresh: 
  if TRUE, then streams-based refresh is used (only supported for limited
  types of MVs). If FALSE, then the standard MV refresh API is used 
  (dbms_mview.refresh)

mv_dml_handler:
  if NOT NULL, then the MV is set up for capture/apply and mv_dml_handler
  is registered as the dml handler for the MV. This allows MV changes to 
  be processed in a specific way by the user (ex: for notification to
  a middle tier). This parameter is most useful when 
  use_streams_refresh = FALSE.  If use_streams_refresh = TRUE, then there 
  is a more efficient way for the user to process an MV refresh LCR 
  (mentioned below.)


Description
-----------
This procedure does the following:
  - if use_streams_refresh = TRUE, validates that the MV is supported
  - if instantiate = TRUE, refreshes the MV and uses the refresh scn as 
    the instantiation_scn
    else uses the current scn as the instantiation scn
  - sets up all the MV's master tables for capture/apply
    - sets streams_mv_refresh.source_dml_handler() as the dml handler
    - set the instantiation_scn
  - sets up capture to include rowids
  - if mv_dml_handler is NOT NULL:
    - sets up the MV for capture/apply
    - sets mv_dml_handler as the dml handler for the MV

Notes
-----
- WARNING: this api should be used only if a separate apply
  process was created, otherwise it could effect other applications
  (since it may change the dml handler to a table)
- this API assumes that the streams queue used for the capture and 
  apply process exists in the stream admin's schema
- for use_streams_refresh = TRUE, the default behavior is that 
  MV refresh LCRs are applied to the MV. If the user wishes to perform
  other steps in addition to or in lieu of applying the LCR, they may 
  modify the procedure streams_mv_lcr.process_lcr(). For example, the 
  user may send a notification to a middle tier in addition to applying 
  the LCR to the MV (similar to passing a non-NULL mv_dml_handler)
- table stmv_reg is used to store registration information and
  must exist (in the stream admin's schema)

table stmv_reg:

  mv_owner        varchar2(30),                                  /* MV owner */
  mv_name         varchar2(30),                                  /* MV name  */
  inst_scn        number,                            /* MV instantiation scn */
  use_str_refresh number,             /* streams-based refresh ? 1=yes, 0=no */
  mjv             number,            /* materialized join view?  1=yes, 0=no */
  key_cols        stmv_column_list,                        /* MV key columns */
  select_list     varchar2(4000),                          /* MV select list */
  base_tabs       stmv_table_list,                         /* MV base tables */
  where_clause    varchar2(4000)                          /* MV where clause */


2. unregister_mv: removes an MV from streams-driven refresh

procedure unregister_mv(mv_owner      varchar2, 
                        mv_name       varchar2,
                        capture_name  varchar2 := 'CAPTURE_MV_MASTER', 
                        apply_name    varchar2 := 'APPLY_MV_MASTER');

Parameters
----------
mv_owner, mv_name: MV to unregister

capture_name: 
  name of the capture process to used for streams-driven refresh

apply_name: 
  name of the apply process to used for streams-driven refresh

Description
-----------
This procedure does the following:
  - remove all MV dml handlers and table rules, it they exist
  - removes dml handlers and table rules for the MV's master tables
    if they are not used by an existing registered MV.
  - deletes registration metadata for the MV

Notes
-----
- WARNING: this api should be used only if a separate capture and apply
  process was created, otherwise it could effect other applications.
- the apply process must be disabled in order to use this procedure
- table stmv_reg is used to store registration information and
  must exist (in the stream admin's schema)


3. remove_streams_mv_refresh: removes streams configuration for 
   streams-driven MV refresh

procedure remove_streams_mv_refresh(
    capture_name varchar2 := 'CAPTURE_MV_MASTER', 
    apply_name   varchar2 := 'APPLY_MV_MASTER');


Parameters
----------
capture_name: 
  name of the capture process to used for streams-driven refresh

apply_name: 
  name of the apply process to used for streams-driven refresh

Description
-----------
This procedure does the following:
  - stops capture and apply
  - remove all dml handlers associated with the apply
  - drops capture and apply
  - removes the queue and queue table
  - removes table instantiation scns, if possible
  - deletes any MV registration metadata



package streams_mv_refresh
==========================
1. source_dml_handler: 
   dml handler used by streams-driven refresh
   classic refresh: 
     if an LCR belongs to a table referenced by MVs registered for 
     classic refresh, store all such MVs in a package state collection 
     for commit-time processing.
   streams-based refresh:
     if an LCR belongs to a table referenced by MVs registered for 
     streams-based refresh, store the table name, the LCR's dmltype 
     and rowid in a package state collection for commit-time processing.

2. source_commit_hdlr: 
  pre-commit handler used by streams-driven refresh
  classic refresh:
    submits a job to call dbms_mview.refresh() on all MVs that were collected
    in the DML handler
  streams-based refresh:
    - derives delete and insert deltas based on the information collected
      in the DML handler
    - creates LCRs from these deltas and calls streams_mv_lcr.process_lcr()
      for each LCR.
    - for OJ MJVs, special processing for antijoin rows (AJ)


package streams_mv_lcr
======================
1. procedure process_lcr(lcr sys.lcr$_row_record)

- this procedure applies an LCR to an MV during streams-based refresh

NOTE: this procedure may be modified by the user to perform
  other steps in addition to or in lieu of applying the LCR



Instructions:
- create streams administrator
- create streams-driven refresh supporting objects
- load streams-driven refresh packages
- create streams queue 
- call streams_mv_refresh_adm.register_mv() passing a special capture 
  and apply name
- set applicable apply parameters
- call dbms_apply_adm.alter_apply to assign precommit handler to apply process 
- start capture and apply
- call streams_mv_refresh_adm.remove_async_mv_streams() to remove the streams 
  configuration for async MV refresh.
- to register a new MV or unregister an MV, stop the apply process first

See the script for a demonstration of this.


Restrictions
------------
- supports single table PK-based MVs or MJVs
- MJVs:
  - no UNION ALL
  - no inline views
- must be read-only MV
- no on-commit MV
- no objects 
- all registered MVs must be registered w/ same capture and apply !
- no MVs with new column aliasing feature


Usage notes
-----------
- MJVs: 
  - must have a unique index on the MV container table's rowid columns
  - call set_key_columns() on the MV container's rowid columns
- MV will be converted to NEVER REFRESH after calling register_mv
- MV logs are not necessary when registering an MV to use streams-based refresh
- all SQL is parsed as the stream admin, so everything in where clause must 
  be qualified by the owner (at least function calls must be)
