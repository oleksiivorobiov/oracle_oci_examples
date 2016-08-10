/* Copyright (c) 1995, 2006, Oracle. All rights reserved.  
*/

/*
   NAME
     oci09.c
   DESCRIPTION
     Demo program for A22400 OCI Techniques White Paper
     Tests single row fetch via PL/SQL procedures
   MODIFIED   (MM/DD/YY)
    kmohan     03/28/06  - change hda to size_t
    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
    ehayes     05/22/97 -  Fix VMS porting exceptions
    dchatter   07/15/96 -  hda is a ub4 array to prevent bus error
    vraghuna   03/01/95 -  Creation
*/

/*
 * Name:        oci09.c
 *
 * Description: This program retrieves the column information for a set of
 *              tables called TESTxx where xx is a number.
 *
 *              It illustrates getting data back using SINGLE ROW FETCH
 *              through a PLSQL PROCEDURE
 *              It also illustrates re-executing the same query again without
 *              having to rebind
 *
 * Based on:    oci04.c
 * Changes:     changed odefin to obndra calls
 *              no fetch cycle, execute does the fetch now
 *
 * Setup:       Run corresponding ociXX.sql before running this program.
 *              Link program and run it.
 *              Program requires ONE argument - the number of tables
 *
 *                           oci09 4 <return>
 *
 * OCI Calls used:
 *
 *      Phase         OCI Call           Notes
 *      ------------------------------------------------------------------
 *      Login     -   olog               Use instead of orlon as of 7.2
 *      Open      -   oopen
 *      Parse     -   oparse
 *      Bind      -   obndra
 *      Describe  -   none               Hard-coded query
 *      Define    -   none               Use obndra for plsql parameters
 *      Execute   -   oexec
 *      Fetch     -   none               Execute gets data back
 *      Close     -   oclose
 *      Logoff    -   olof
 *
 * This program is for educational purposes.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include <oratypes.h>
/* LDA and CDA struct declarations */
#include <ocidfn.h>
#ifdef __STDC__
#include <ociapr.h>
#else
#include <ocikpr.h>
#endif
/* demo constants and structs */
#include <ocidem.h>

#define MAX_NAME_LENGTH  30               /* Maximum length of a column name */

/* oparse flags */
#define  DEFER_PARSE        1
#define  NATIVE             1
#define  VERSION_7          2

/* exit flags */
#define OCI_EXIT_FAILURE 1
#define OCI_EXIT_SUCCESS 0

Lda_Def lda;                                                   /* login area */
size_t     hda[HDA_SIZE/(sizeof(size_t))];                      /* host area */
Cda_Def cda;                                                  /* cursor area */

/* Globals needed */
text tabname [MAX_NAME_LENGTH+1];       /* table name - generated on the fly */
uword tabnum = 0;                                            /* table number */
uword row_count = 0;                       /* number of rows returned so far */


/* Function prototypes */
void gentable();
void logon ();
void logoff ();
void setup();
void err_report();
void get_data();
void dump_data();
void do_exit();

/* SQL statement used in this program */

text *sqlstmt = (text *)"\
                 begin \
                   oci09pkg.oci09proc(:tabname, :cname, :collen, :colid);\
                 end;";

/* return values from query - kept as globals for simplicity */
uword colid;
uword collen;
text cname [MAX_NAME_LENGTH+1];

main(argc, argv)
eword argc;
text **argv;
{

  uword numtabs;                               /* number of tables requested */
  uword tabindex;                                          /* for loop index */

  if (argc != 2)
  {
     printf("\n\n Usage: <prog> <numtabs> where numtabs is the number of \
tables with name TESTxx \n\n\n");
     do_exit(OCI_EXIT_FAILURE);
  }

  numtabs = atoi((char *)argv[1]);
  printf("\n Number of tables is %d\n", numtabs);

  logon();                                       /* logon to Oracle database */

  setup();                                          /* prepare sql statement */

  for (tabindex = 0; tabindex < numtabs; tabindex++)
  {
    gentable();
    get_data();                                             /* retrieve data */
  }

  logoff();                                        /* logoff Oracle database */

  do_exit(OCI_EXIT_SUCCESS);

}

/*
 * Function: setup
 *
 * Description: This routine does the necessary setup to execute the SQL
 *              statement. Specifically, it does the open, parse, bind and
 *              define phases as needed.
 *
 */
void setup()
{

  if (oopen(&cda, &lda, (text *) 0, -1, -1, (text *) 0, -1))         /* open */
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }

  if (oparse(&cda, sqlstmt, (sb4) -1, DEFER_PARSE,                  /* parse */
               (ub4) VERSION_7))
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }

  if (obndra(&cda, (text *)":tabname", -1,                           /* bind */
             (ub1 *)tabname, (sword)sizeof(tabname),
             SQLT_STR, -1, (sb2 *) 0, (ub2 *)0, (ub2 *)0,
             (ub4)0, (ub4 *)0, (text *)0, -1, -1))
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }

  if (obndra(&cda, (text *)":cname", -1,                             /* bind */
             (ub1 *)cname, (sword)sizeof(cname),
             SQLT_STR, -1, (sb2 *) 0, (ub2 *)0, (ub2 *)0,
             (ub4)0, (ub4 *)0, (text *)0, -1, -1))
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }

  if (obndra(&cda, (text *)":collen", -1,                            /* bind */
             (ub1 *)&collen, (sword)sizeof(collen),
             SQLT_INT, -1, (sb2 *) 0, (ub2 *)0, (ub2 *)0,
             (ub4)0, (ub4 *)0, (text *)0, -1, -1))
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }

  if (obndra(&cda, (text *)":colid", -1,                             /* bind */
             (ub1 *)&colid, (sword)sizeof(colid),
             SQLT_INT, -1, (sb2 *) 0, (ub2 *)0, (ub2 *)0,
             (ub4)0, (ub4 *)0, (text *)0, -1, -1))
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }

}

/*
 * Function: get_data
 *
 * Description: This routine actually executes the cursor and fetches the
 *              data. It calls dump_data to print the data
 *
 */
void get_data()
{

  printf("\n printing column information for table %s :\n\n", tabname);

  while (oexec(&cda) == 0)
  {
    row_count++;                          /* bump this for printing purposes */
    dump_data();
  }

  if (cda.rc != NULL_VALUE_RETURNED)
    err_report(&cda);

  row_count = 0;                                     /* reset for next table */
}

/*
 * Function: dump_data
 *
 * Description: This routine prints out the row/s from each fetch
 *
 */
void dump_data()
{
    printf(" column %d name is %s\n", row_count, cname);
    printf(" column %d length is %d \n", row_count, collen);
    printf(" column %d id is %d \n", row_count, colid);

}

/*
 * Function: err_report
 *
 * Description: This routine prints out the most recent OCI error
 *
 */
void err_report(cursor)
Cda_Def *cursor;
{
    sword n;
    text msg[512];                      /* message buffer to hold error text */

    if (cursor->fc > 0)
      printf("\n-- ORACLE error when processing OCI function %s \n\n",
            oci_func_tab[cursor->fc]);
    else
      printf("\n-- ORACLE error\n");

    n = (sword)oerhms(&lda, cursor->rc, msg, (sword) sizeof msg);
    fprintf(stderr, "%s\n", msg);

}

/*
 * Function: gentable
 *
 * Description: This routine generates the next table name of the form TESTxx
 *
 */
void gentable()
{

  tabnum++;                                  /* generate the next table name */
  sprintf((char *)tabname, "TEST%d", tabnum);

}

/*
 * Function: do_exit
 *
 * Description: This routine exits with a status
 *
 */
void do_exit(status)
eword status;
{

  if (status == OCI_EXIT_FAILURE)
     printf("\n Exiting with FAILURE status %d\n", status);
  else
     printf("\n Exiting with SUCCESS status %d\n", status);

  exit(status);
}

/*
 * Function: login
 *
 * Description: This routine logs on onto the database as OCITEST/OCITEST
 *
 */
void logon()
{

  if (olog(&lda, (ub1 *)hda, (text *)"OCITEST", -1, (text *)"OCITEST", -1,
           (text *)0, -1, (ub4)OCI_LM_DEF))
  {
    err_report((Cda_Def *)&lda);
    exit(OCI_EXIT_FAILURE);
  }

  printf("\n Connected to ORACLE as ocitest\n");

}

/*
 * Function: logoff
 *
 * Description: This routine closes out any cursors and logs off the database
 *
 */
void logoff()
{

  if (oclose(&cda))                                          /* close cursor */
  {
    fprintf(stderr, "Error closing cursor 1.\n");
    do_exit(OCI_EXIT_FAILURE);
  }

  if (ologof(&lda))                                  /* log off the database */
  {
    fprintf(stderr, "Error on disconnect.\n");
    do_exit(OCI_EXIT_FAILURE);
  }

}
