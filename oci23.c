/* Copyright (c) 1995, 2006, Oracle. All rights reserved.  
*/

/*
   NAME
     oci23.c
   DESCRIPTION
     Demo program for A22400 OCI Techniques White Paper
     Tests describe function
   MODIFIED   (MM/DD/YY)
    kmohan     03/28/06  - change hda to size_t
    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
    ehayes     05/29/97 -  Fix olint errors
    dchatter   07/15/96 -  hda is a ub4 array to prevent bus error
    vraghuna   03/01/95 -  Creation
*/

/*
 * Name:        oci23.c
 *
 * Description: This program illustrates how to do a describe on a query
 *
 * Based on:    <nothing>
 * Changes:     <none - base revision>
 *
 * Setup:       Run corresponding ociXX.sql before running this program.
 *              Link program and run it.
 *              Program requires NO arguments
 *
 *                           oci23 <return>
 *
 * OCI Calls used:
 *
 *      Phase         OCI Call           Notes
 *      ------------------------------------------------------------------
 *      Login     -   olog               Use instead of orlon as of 7.2
 *      Open      -   oopen
 *      Parse     -   oparse
 *      Bind      -   none
 *      Describe  -   odescr
 *      Define    -   none
 *      Execute   -   none
 *      Fetch     -   none
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
size_t     hda[HDA_SIZE/sizeof(size_t)];                        /* host area */
Cda_Def cda;                                                  /* cursor area */

/* Function prototypes */
void logon ();
void logoff ();
void setup();
void err_report();
void dump_data();
void do_exit();
void do_describe();

/* SQL statement used in this program */

text *sqlstmt = (text *)"SELECT one, two, three, four FROM oci23tab";

main(argc, argv)
eword argc;
text **argv;
{

  logon();                                       /* logon to Oracle database */

  setup();                                          /* prepare sql statement */

  do_describe();

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

}

/*
 * Function: do_describe
 *
 * Description: This routine actually describes a sql statement and prints
 *              the information pertaining to it.
 *
 */
void do_describe()
{

#define COL_SIZE 20
#define NUM_SELECT_ITEMS 4

sword items = NUM_SELECT_ITEMS;                    /* number of select items */
sword colindex;
sb4 *dbsize;                                       /* maximum size of column */
sb2 *dbtype;                                     /* internal column datatype */
sb1 *column_name;                            /* buffer to store column names */
sb4 *col_buf_len;                                        /* length of column */
sb4 *display_size;                                   /* maximum display size */
sb2 *precision;                                                 /* precision */
sb2 *scale;                                                         /* scale */
sb2 *nullok;                             /* column can have null values in it *

                                              /* allocate the needed storage */

      dbsize         = (sb4 *)malloc((ub4)(items * sizeof(sb4)));
      dbtype         = (sb2 *)malloc((ub4)(items * sizeof(sb2)));
      column_name    = (sb1 *)malloc((ub4)(items * sizeof(sb1) * COL_SIZE));
      col_buf_len    = (sb4 *)malloc((ub4)(items * sizeof(sb4)));
      display_size   = (sb4 *)malloc((ub4)(items * sizeof(sb4)));
      precision      = (sb2 *)malloc((ub4)(items * sizeof(sb2)));
      scale          = (sb2 *)malloc((ub4)(items * sizeof(sb2)));
      nullok         = (sb2 *)malloc((ub4)(items * sizeof(sb2)));

      for (colindex = 0; colindex < items; colindex++)
      {

                 /* Have to initialize length, otherwise nothing is returned */
        *(col_buf_len + colindex) = COL_SIZE;

        if (odescr(&cda, colindex + 1,
                   dbsize + colindex, dbtype + colindex,
                   column_name + (COL_SIZE * colindex), col_buf_len + colindex,
                   display_size + colindex, precision + colindex,
                   scale + colindex, nullok + colindex))
        {
          err_report(&cda);
          do_exit (OCI_EXIT_FAILURE);
        }

      }

       printf("printing out results of the describe \n");

       for (colindex = 0; colindex < items; colindex++)
       {
         printf("\n\n\nInformation from desc for column %d\n\n", colindex + 1);

         printf("Column Maximum Size is  %d\n", *(dbsize + colindex));
         printf("Column Type is          %d\n", *(dbtype + colindex));

         printf("Column Name Length is   %d\n", *(col_buf_len + colindex));
         *(column_name + (COL_SIZE*colindex) + *(col_buf_len+colindex))='\0';
         printf("Column Name is          %s\n",
                column_name + (COL_SIZE*colindex));

         printf("Column Length is        %d\n", *(col_buf_len + colindex));
         printf("Column Display Size is  %d\n", *(display_size + colindex));
         printf("Column Precision is     %d\n", *(precision + colindex));
         printf("Column Scale is         %d\n", *(scale  + colindex));
         printf("Column Nulls allowed is %d\n", *(nullok  + colindex));

       }

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
