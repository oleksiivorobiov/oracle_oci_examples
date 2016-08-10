Rem
Rem $Header: rdbms/demo/xrwutl.sql /main/2 2008/10/07 11:18:57 jraitto Exp $
Rem
Rem xrwutl.sql
Rem
Rem Copyright (c) 2005, 2008, Oracle and/or its affiliates. All rights reserved.
Rem
Rem    NAME
Rem      xrwutl.sql - Utility to extract output from EXPLAIN_REWRITE()
Rem
Rem    DESCRIPTION
Rem      EXPLAIN_REWRITE() contains a large number of output fields.
Rem      This utility helps to easily select the output fields from
Rem      EXPLAIN_REWRITE().
Rem
Rem    NOTES
Rem      This utility is merged as a demo script
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    jraitto     10/04/08 - Handle internally thrown errors
Rem                         - Fix bug 4390758
Rem    jraitto     10/03/08 - fix bug 4391670 by re-signraising unexpected
Rem                           errors
Rem    mthiyaga    04/01/05 - mthiyaga_xrw_demo
Rem    mthiyaga    28/03/05 - Created
Rem

SET ECHO ON
SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100


set echo off;

-- DEFINITION for xrw (mv_list, command_list, querytxt):
--        This is a procedure to simplify the output from 
--        explain_rewrite().  
-- INPUT
--   mv_list        :  List of mvs separated by commmas.  
--
--   command_list   :  List of commands separated by commas; you can provide
--                     any of the following commands. 
--      COMMAND LIST
--      
--   querytxt          : Text of the query to be rewritten
--
  
connect /as sysdba;

SET SERVEROUTPUT ON SIZE 32512;

set echo off;

-- The procedure is used to print a big string. Procedure dbms_output.put_line 
-- cannot print a string that is longer than 250 chars.

CREATE OR REPLACE PROCEDURE print_big_buf
  ( 
    buf               IN   VARCHAR2
  )
AS
  len                      PLS_INTEGER;
  nl                       PLS_INTEGER;
  bsize                    CONSTANT PLS_INTEGER := 250;

BEGIN
  dbms_output.enable(1000000);
  len := LENGTHB(buf);
  nl := floor(len/bsize);

  FOR j IN 1..nl LOOP
    dbms_output.put_line(SUBSTRB(buf, (j-1)*bsize + 1, bsize));
  END LOOP;

  dbms_output.put_line(SUBSTRB(buf, nl*bsize + 1, len - nl*bsize));     

END;
/

GRANT EXECUTE ON SYS.print_big_buf TO PUBLIC;



------------------------------------------------------------------------------
-- DESCRIPTION for ckrw_help
--  This procedure can be called by the user to get help on
-- how to use xrw
------------------------------------------------------------------------------ 
  
  CREATE OR REPLACE PROCEDURE xrw_help  AUTHID CURRENT_USER 
  AS
  BEGIN
  
  dbms_output.put_line('HELP for the use of precedure xrw');
  
  dbms_output.put_line('sys.xrw (list_of_mvs (separated by commas), '|| 
    'list of commands (separated by commas), querytxt)');
  
  dbms_output.put_line('**** List of commands:');
  dbms_output.put_line('QUERY_TXT ');
  dbms_output.put_line('REWRITTEN_TXT ');
  dbms_output.put_line('QUERY_BLOCK_NO ');
  dbms_output.put_line('PASS ');
  dbms_output.put_line('COSTS ');
  dbms_output.new_line;
  
  END;
/
show errors
  


    
       
------------------------------------------------------------------------------ 
-- DEFINITION for check_mvs:
--        This procedure takes an array of mv names and checks whether
--        they are really mvs; if there is any name in the list that doesn't
--        correspond to a MV then we raise an error
-- INPUT
--   num_mvs       :  number of mvs given
--   mv_array      :  array of mv names  
------------------------------------------------------------------------------
  
CREATE OR REPLACE PROCEDURE check_mvs
  (      
    num_mvs           IN   PLS_INTEGER,
    mv_array          IN   dbms_utility.uncl_array
  ) AUTHID CURRENT_USER 
  AS 
       i                   PLS_INTEGER;
  BEGIN
    IF (num_mvs > 0) THEN
       DECLARE
          mv_found    PLS_INTEGER := 0;        
          mv_name     VARCHAR2(30);
          is_not_mv   EXCEPTION;
          cursor find_mv (given_mv_name VARCHAR2) IS SELECT 1 from user_mviews 
            where upper(mview_name) = given_mv_name;
          
       BEGIN
          FOR i IN 1..num_mvs LOOP
             mv_name := rtrim(ltrim(upper(mv_array(i))));
             open find_mv(mv_name);
             fetch find_mv INTO mv_found;
             -- The mv wasn't found in user_mviews, so the name given 
             -- doesn't correspond to a MV
             IF (mv_found = 0) THEN
               RAISE is_not_mv;
             END IF;
             
             close find_mv; 
          END LOOP;
       EXCEPTION WHEN NO_DATA_FOUND OR is_not_mv THEN
         dbms_output.put_line('**FAILURE** Materialized View ' || mv_name || 
         ' was not found');
         RAISE NO_DATA_FOUND;
       END;
    END IF;
  END;
/
show errors

       


------------------------------------------------------------------------------
-- DEFINITION for xrw:
--        This procedure is the top level api the user will use to
--        simplify output from explain_rewrite().  
------------------------------------------------------------------------------
  
CREATE OR REPLACE PROCEDURE xrw
  ( 
    mv_list           IN   VARCHAR2,
    command_list      IN   VARCHAR2,
    querytxt          IN   VARCHAR2
  ) AUTHID CURRENT_USER 
  AS
  
  Rewrite_Array SYS.RewriteArrayType := SYS.RewriteArrayType();
  no_of_msgs NUMBER;
  i NUMBER;
 
  
  buf                     VARCHAR2(32512); 
 
  
  -- Variables used for the list of mvs given
  num_mvs                 PLS_INTEGER := 0;
  mv_array                dbms_utility.uncl_array; 
  i                       PLS_INTEGER := 1;
  
  -- Variables used for the different commands
  num_commands            PLS_INTEGER := 0;
  commands_array          dbms_utility.lname_array;
  command                 VARCHAR2(30);
  query_txt               BOOLEAN := false;
  rewritten_txt           BOOLEAN := false;
  query_block_no          BOOLEAN := false;
  pass                    BOOLEAN := false;
  costs                   BOOLEAN := false;

  
  
  empty_line              VARCHAR2(80) := NULL;
  
 
  
  querytxt_err            EXCEPTION;
  commlist_err            EXCEPTION;
 
  rewrite_err             EXCEPTION;
  no_rewrite_err          EXCEPTION;
  command_err             EXCEPTION;
  is_not_mv               EXCEPTION;
  explain_rewrite_err     EXCEPTION;
  failure_displayed       BOOLEAN := false;
  
  BEGIN
     
  IF (mv_list IS NOT NULL) THEN 
     BEGIN
         dbms_utility.comma_to_table (mv_list, num_mvs, mv_array);
     EXCEPTION
         WHEN OTHERS THEN
             dbms_output.put_line ('** FAILURE ** Malformed MV list');
             failure_displayed := true;
             RAISE;
     END;
     -- Check whether the given mvs are actually MVs
     BEGIN
         check_mvs (num_mvs, mv_array);
     EXCEPTION
         WHEN NO_DATA_FOUND THEN
            RAISE is_not_mv;
         WHEN OTHERS THEN
            RAISE;
     END;
  END IF;   
     
  IF querytxt IS NULL THEN
     RAISE querytxt_err;
  END IF;
     
--  IF command_list IS NULL THEN
--     RAISE commlist_err;
--  END IF;
 
  IF command_list IS NOT NULL THEN
    BEGIN
        dbms_utility.comma_to_table (command_list, num_commands,  commands_array);
    EXCEPTION
        WHEN OTHERS THEN
            dbms_output.put_line ('** FAILURE ** Malformed command list');
            failure_displayed := true;
            RAISE;
    END;
  END IF;
  
  -- Read the list of command and set the BOOLEAN variables appropriately
  
  FOR i IN 1..num_commands LOOP
     command := rtrim(ltrim(upper(commands_array(i))));
     IF  command = 'QUERY_TXT' THEN  
        query_txt := true;
     ELSIF command = 'REWRITTEN_TXT' THEN
        rewritten_txt := true;
     ELSIF command = 'QUERY_BLOCK_NO' THEN
        query_block_no := true;
     ELSIF command = 'PASS' THEN
        pass := true;
     ELSIF command = 'COSTS' THEN 
        costs := true;
     ELSE
        dbms_output.put_line('Use execute sys.xrw_help to get the list ' || 
          'of valid commands');
        RAISE command_err;
     END IF;
  END LOOP;
  
BEGIN 
   dbms_output.new_line;
   BEGIN
     IF mv_list IS NULL then
       dbms_mview.Explain_Rewrite(querytxt, '',  Rewrite_Array);
     ELSE
       dbms_mview.Explain_Rewrite(querytxt, mv_list,  Rewrite_Array);
     END IF;
   EXCEPTION
     WHEN OTHERS THEN
       dbms_output.put_line ('** Failure ** DBMS_MVIEW.EXPLAIN_REWRITE has encountered an error');
       failure_displayed := true;
       RAISE;
   END;
  
   no_of_msgs := Rewrite_array.count;
   DBMS_OUTPUT.PUT_LINE('============================================================================'); 
   FOR i IN 1..no_of_msgs
     LOOP
       IF (i = 2) THEN
         DBMS_OUTPUT.PUT_LINE('>> ');
         DBMS_OUTPUT.PUT_LINE('------------------------- ANALYSIS OF QUERY REWRITE -------------------------');
       END IF;
       DBMS_OUTPUT.PUT_LINE('>> ');
       IF (query_block_no) THEN
         IF (i > 1) THEN
	  DBMS_OUTPUT.PUT_LINE('>> QRY BLK #: '|| 
		Rewrite_Array(i).query_block_no);
         END IF;
       END IF;
       DBMS_OUTPUT.PUT_LINE('>> MESSAGE  : ' || Rewrite_Array(i).message);
       IF (query_txt) THEN
          DBMS_OUTPUT.PUT_LINE('>> QUERY    : ' || Rewrite_Array(i).query_text);
       END IF;
       IF (rewritten_txt) THEN
	  DBMS_OUTPUT.PUT_LINE('>> RW QUERY : '|| Rewrite_Array(i).rewritten_text);
       END IF;
       IF (costs) THEN 
          DBMS_OUTPUT.PUT_LINE('>> ORIG COST: '|| Rewrite_Array(i).original_cost || '                  RW COST: '|| Rewrite_Array(i).rewritten_cost);
       END IF;
      
       IF (pass) THEN
         IF (i > 1) THEN
              IF (Rewrite_Array(i).pass = 'YES') THEN
                 DBMS_OUTPUT.PUT_LINE('>> MESSAGE OUTPUT BEFORE VIEW MERGING... ');
              ELSE
                 DBMS_OUTPUT.PUT_LINE('>> MESSAGE OUTPUT AFTER VIEW MERGING... '); 
              END IF;
         END IF;
       END IF; 	
       IF (i = 1) THEN
         DBMS_OUTPUT.PUT_LINE('============================================================================');
	ELSIF (i != no_of_msgs) THEN
         DBMS_OUTPUT.PUT_LINE('----------------------------------------------------------------------------');
       END IF;
    END LOOP;
    DBMS_OUTPUT.PUT_LINE('============================ END OF MESSAGES ===============================');
END;
 


  EXCEPTION
     WHEN querytxt_err THEN
       dbms_output.put_line('**FAILURE** The querytxt cannot be NULL');
     WHEN commlist_err THEN 
       dbms_output.put_line('**FAILURE** The command list cannot be NULL');
   
     WHEN command_err THEN
       dbms_output.put_line('**FAILURE** ' || command || 
       ' is not a valid command');
     WHEN is_not_mv THEN
       -- MV not found, this error has already been displayed
       dbms_output.new_line;
     WHEN OTHERS THEN
       dbms_output.new_line;
       IF (NOT failure_displayed) THEN
         dbms_output.put_line('**FAILURE** An unexpected error has occurred');
         dbms_output.new_line;
       END IF;
       RAISE;
  END;
/
show errors;
  

drop public synonym XRW;
create public synonym XRW for SYS.XRW;
GRANT EXECUTE ON XRW TO PUBLIC; 

drop public synonym xrw_HELP;
create public synonym xrw_HELP for SYS.xrw_HELP;
GRANT EXECUTE ON xrw_HELP TO PUBLIC; 






