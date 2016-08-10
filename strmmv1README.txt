/
/ $Header: strmmv1README.txt 24-jan-2006.16:01:05 wesmith Exp $
/
/ strmmv1README.txt
/
/ Copyright (c) 2006, Oracle. All Rights Reserved.
/
/   NAME
/     strmmv1README.txt - <one-line expansion of the name>
/
/   DESCRIPTION
/     <short description of component this file declares/defines>
/
/   NOTES
/     <other useful comments, qualifications, etc.>
/
/   MODIFIED   (MM/DD/YY)
/   wesmith     01/24/06 - Creation
/

Asynchronous commit-time MV refresh using Streams

This script creates a package with the following procedures:
- register_tables() which sets up streams for all tables with MV logs
- unregister_tables() which removes streams for all tables that no longer
   have MV logs
- apply DML handler which caches all tables involved in a transaction in 
  a package state variable
- apply pre-commit handler which submits a job to call refresh_dependent
  (in this package) on all tables that were cached.
- refresh_dependent() which calls dbms_mview.refresh_dependent() after
  possibly eliminating tables that no longer have any dependent MVs that 
  need refreshing (due to other concurrent refresh_dependent() jobs).
  Nested MVs are refreshed too, if required.
- remove_async_mv_streams() which removes ASYNC_MV streams configuration
Using this package will allow you to set up local MVs that will refresh 
asynchronously at commit-time using Streams.

NOTES:
- refresh_dependent() references all_refresh_dependencies which gives the 
  oldest refresh scn for a master table, so 
  commit_scn > oldest_refresh_scn implies that at least one dependent MV 
  is stale.   So calling dbms_mview.refresh_dependent() may still 
  result in null refreshes. It may be possible to modify this procedure 
  to consider the last refresh scn of each dependent MV.

- refresh_dependent() does not consider refresh groups. It may be possible
  to modify this to consider additional refresh group views.

Instructions:
- create streams administrator
- create streams queue
- call async_mv_pkg.register_tables() passing a special capture and apply name
- set applicable apply parameters
- call dbms_apply_adm.alter_apply to assign precommit handler to apply process 
- start capture and apply
- you can submit jobs for async_mv_pkg.register_tables(), 
  async_mv_pkg.unregister_tables()
  to keep track of new tables with MV logs, or tables that no longer have
  MV logs, simplifying the administration.
- call async_mv_pkg.remove_async_mv_streams() to remove the streams 
  configuration for async MV refresh.
