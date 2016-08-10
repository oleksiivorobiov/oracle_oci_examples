--------------------------------------------------------------------------------
--
-- $Header: dmshgrants.sql 28-aug-2007.07:35:51 xbarr Exp $
--
-- dmshgrants.sql
--
--  Copyright (c) 2001, 2005, Oracle. All rights reserved.
--
--    NAME
--    dmshgrants.sql
--
--    DESCRIPTION
--      This script grants SELECT on SH tables and SYS privileges 
--      required to run the Oracle Data Mining demo programs
--      
--      The script is to be run in SYS account
--
--    NOTES
--       &&1    Name of the DM user
--
--    MODIFIED   (MM/DD/YY)
--       xbarr       08/28/07 - fix security bug 6367775  
--       pstengar    07/05/06 - add create mining model privilege
--       ktaylor     07/11/05 - Minor edits to comments
--       xbarr       12/20/04 - add privileges required by DM demo 
--       cbhagwat    10/10/03 - creation
--
--------------------------------------------------------------------------------
DEFINE DMUSER = &&1 

GRANT create procedure to &DMUSER
/
grant create session to &DMUSER
/
grant create table to &DMUSER
/
grant create sequence to &DMUSER
/
grant create view to &DMUSER
/
grant create job to &DMUSER
/
grant create type to &DMUSER
/
grant create synonym to &DMUSER
/
grant create mining model to &DMUSER
/
grant execute on ctxsys.ctx_ddl to &DMUSER
/

GRANT SELECT ON sh.customers TO &DMUSER
/
GRANT SELECT ON sh.sales TO &DMUSER
/
GRANT SELECT ON sh.products TO &DMUSER
/
GRANT SELECT ON sh.supplementary_demographics TO &DMUSER
/
GRANT SELECT ON sh.countries TO &DMUSER
/ 
