#ifdef RCSID
static char *RCSid =
   "$Header: rdbms/demo/readpipe.c /main/9 2008/08/29 10:48:36 yitseng Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1991, 2008, Oracle. All rights reserved.
*/

/*
   NAME
     readpipe.c - read a server named pipe
   NOTES
     described in ORACLE7 Server Application Developer's Guide
   MODIFIED   (MM/DD/YY)
    yitseng    08/29/08  - XbranchMerge yitseng_bug6273715 from st_rdbms_10.2
    yitseng    09/20/07  - bug6273715
    ppallapo   02/28/06  - Bug/5068904
    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
    bnnguyen   04/01/98 -  bug487125
    dchatter   07/15/96 -  hda is a ub4 array to prevent bus error
    emendez    04/07/94 -  merge changes from branch 1.3.710.2
    gdoherty   04/06/94 -  merge changes from branch 1.3.710.1
    emendez    02/02/94 -  Fix for bug 157576
    gdoherty   01/31/94 -  make oci header inclusion for ansi or k+r adaptive
    rkooi      12/15/92 -  add some comments
    tssmith      12/15/92 -  Added break in for(;;) loop
    tssmith      12/12/92 -  Creation
*/

/*
 *  readpipe.c
 *
 *  This OCI program demonstrates how to read messages from
 *  a named pipe.  This program can be run in
 *  a window or terminal session, and be used to print messages
 *  from a PL/SQL source program (an anonymous block or a
 *  stored procedure).
 *
 *  This is very useful in debugging PL/SQL programs.
 *
 *  First, you must create a PL/SQL package and package body that packs
 *  your message(s) and sends them to a named pipe.
 *  In the example below, two 'put' procedures are implemented,
 *  overloaded by type: VARCHAR2 and NUMBER.  This you can extend
 *  as required for additional types.
 *
 *  Store this package in the server in an appropriate schema,
 *  and grant execute privilege on it to the users who require it.
 *
 *  You will need to grant execute privilege on the dbms_pipe package
 *  to the owner of the plsdbg package below.
 *
 *  Note that this example uses only a single named pipe.  It could
 *  be confusing if more than one PL/SQL program were writing to
 *  the same pipe (unless some identifying protocol is established).
 *  It would certainly be confusing if several OCI programs like
 *  the one below were reading the same pipe.
 *
 *  Here is the example package:
 *
 *    create or replace package plsdbg as
 *      -- the procedure put is overloaded for varchar strings
 *      -- and numbers.  extend as needed.
 *        procedure put(info varchar2);
 *        procedure put(n number);
 *    end;
 *    /
 *
 *    create or replace package body plsdbg as
 *
 *        procedure put(info varchar2) is
 *            status integer;  -- ret. val. required for the
 *                             -- send_message function, not used here
 *        begin
 *            dbms_pipe.pack_message(info);
 *            status := dbms_pipe.send_message('ora$plsql_debug');
 *        end;
 *
 *        procedure put(n number) is
 *            chr    varchar2(44);
 *            status integer;
 *        begin
 *            chr := to_char(n);
 *            dbms_pipe.pack_message(chr);
 *            status :=  dbms_pipe.send_message('ora$plsql_debug');
 *        end;
 *    end;
 *    /
 *
 *  In a PL/SQL program, you call plsdbg to write messages to the
 *  pipe 'ora$plsql_debug' like this:
 *  ...
 *  declare
 *    my_var   integer;
 *    ...
 *  begin
 *    my_var := 42;
 *    ...   -- program procedes until debug output is needed
 *    plsdbg.put('Hmm, my_var is--');
 *    plsdbg.put(my_var);
 *          -- and the OCI program reading ora$plsql_debug will
 *          -- print the text message, and the value of my_var
 *    ...
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/*  These header files must be included for the
    type information, and #defined constants. */
#include <oratypes.h>
#include <ocidfn.h>
#ifdef __STDC__
#include <ociapr.h>
#else
#include <ocikpr.h>
#endif

/* demo constants and structs */
#include <ocidem.h>

/* Declare the CDA, LDA, and HDA. */
Cda_Def lda;
size_t  hda[HDA_SIZE/sizeof(size_t)];                           /* host area */
Cda_Def cda;

/* Define a string that holds the anonymous PL/SQL block.
   The block calls the DBMS_PIPE procedures to get messages
   written to the pipe named ora$plsql_debug. */

#define GETNEXTITEM "\
declare\
  s integer;\
  chr varchar2(200);\
begin\
  chr := '';\
  s := dbms_pipe.receive_message('ora$plsql_debug');\
  if s = 0 then\
    dbms_pipe.unpack_message(chr);\
  end if;\
  :status := s;\
  :retval := chr;\
end;"

/* Error-handling function */
void errrpt();


main(argc, argv)
sword argc;
text **argv;
{
    text username[128];
    ub1 retval[132];
    sword  status;

/* Prepare username/password, from command line or
   else use scott/tiger as the default if none given on
   the command line. */

    if (argc > 1)
        strncpy((char *) username, (char *) argv[1],
                (sword) sizeof (username) - 1);
    else
        strcpy((char *) username, "SCOTT/TIGER");

/* Connect to ORACLE. */
    if (orlon(&lda, (ub1 *)hda, username, -1,
              (text *) 0, -1, 0))
    {
        printf("Cannot connect as %s. Exiting...\n", username);
        exit(1);
    }
    else
        printf("connected\n");

/* Open the cursor -- must quit if we cannot. */
    if (oopen(&cda, &lda, (text *) 0, -1, -1, (text *) 0, -1))
    {
        printf("Error opening cursor.  Exiting...\n");
        exit(1);
    }

/* Parse the anonymous PL/SQL block. */
    if (oparse(&cda, (text *) GETNEXTITEM,
               (sb4) -1, 0, (ub4) 2))
    {
        errrpt();
        exit(1);
    }

/* Bind the status program variable. */
    if (obndrv(&cda, (text *) ":status", -1, (ub1 *) &status,
               (sword) sizeof (sword), SQLT_INT,
               -1, (sb2 *) 0, (text *) 0, -1, -1))
    {
        errrpt();
        exit(1);
    }

/* Bind the return string (retval). */
    if (obndrv(&cda, (text *) ":retval", -1, (ub1 *) retval,
               (sword) sizeof (retval), SQLT_STR,
               -1, (sb2 *) 0, (text *) 0, -1, -1))
    {
        errrpt();
        exit(1);
    }

/* Loop to look for print messages on the pipe */
    printf("listening...\n");
    for (;;)
    {
        if (oexec(&cda))
        {
            errrpt();
            break;
        }

        if (status != 0)
            printf("Abnormal pipe status: %d\n\r", status);
        else
            printf("%s\n\r", retval);
    }
}


/* Report errors. */
void errrpt()
{
    sword rv;
    text msg[1024];

/* use oerhms to get error messages longer than 70 characters */
    rv = oerhms(&lda, cda.rc, msg, (sword) sizeof (msg));
    printf("ORACLE ERROR\n");
    printf("%.*s\n", rv, msg);
}



