/* Copyright (c) 1995, 2006, Oracle. All rights reserved.  
*/

/*
   NAME
     oci12.c
   DESCRIPTION
     Demo program for A22400 OCI Techniques White Paper
     Tests single row fetch of LONG data with piecewise fetch
   MODIFIED   (MM/DD/YY)
    kmohan     03/28/06  - change hda to size_t
    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
    ehayes     05/29/97 -  Fix olint errors
    dchatter   07/15/96 -  hda is a ub4 array to prevent bus error
    vraghuna   03/01/95 -  Creation
*/
/*
 * Name:        oci12.c
 *
 * Description: This program retrieves the rows contained in oci11tab which
 *              contains a long. Some rows have to be fetched piecewise.
 *
 *              It illustrates getting data back using SINGLE ROW FETCH
 *              via a SELECT statement.
 *              It illustrates how to do a piecewise fetch of a long.
 *              Fetching 5 characters per long fetch
 *
 * Based on:    oci04.c
 * Changes:     added long support
 *              hardcoded oci12tab in query
 *
 * Setup:       Run corresponding ociXX.sql before running this program.
 *              Link program and run it.
 *              Program requires NO arguments
 *
 *                           oci12 <return>
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
 *      Define    -   odefin
 *      Execute   -   oexec
 *      Fetch     -   ofetch, oflng      Long fetch
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

Lda_Def lda;                                                   /* login area */
size_t     hda[HDA_SIZE/sizeof(size_t)];                        /* host area */
Cda_Def cda;                                                  /* cursor area */

/* exit flags */
#define OCI_EXIT_FAILURE 1
#define OCI_EXIT_SUCCESS 0

/* Globals needed */
text tabname [MAX_NAME_LENGTH+1];       /* table name - generated on the fly */
uword tabnum = 0;                                            /* table number */
uword row_count = 0;                       /* number of rows returned so far */


/* Function prototypes */
void logon ();
void logoff ();
void setup();
void err_report();
void get_data();
void dump_data();
void do_exit();
void fetch_long();

/* SQL statement used in this program */

text *sqlstmt = (text *)"SELECT col1, col2 FROM oci12tab ORDER BY col1";

/* return values from query - kept as globals for simplicity */

#define LONG_SIZE 5                   /* getting back 5 chars per long fetch */
sword col1;
text col2 [LONG_SIZE + 1];
sb2 col2_ind;                                 /* indicator variable for col2 */
ub2 col2_rlen;                                     /* return length for col2 */

main(argc, argv)
eword argc;
text **argv;
{

  logon();                                       /* logon to Oracle database */

  setup();                                          /* prepare sql statement */

  get_data();                                               /* retrieve data */

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

  if (odefin(&cda, 1, (ub1 *) &col1, (sword) sizeof(col1),        /* define */
             (sword) SQLT_INT,
             (sword) -1, (sb2 *) 0, (text *) 0, -1, -1,
             (ub2 *) 0, (ub2 *) 0))
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }
                                  /* save one character for null termination */
  if (odefin(&cda, 2, (ub1 *) col2, (sword) (sizeof(col2) - 1),
             (sword) SQLT_LNG,
             (sword) -1, (sb2 *)&col2_ind, (text *) 0, -1, -1,
             (ub2 *)&col2_rlen, (ub2 *) 0))
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

  if (oexec(&cda))
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }

  printf("\n Printing the data for table oci12tab \n");

  while (ofetch(&cda) == 0)
  {
    row_count++;                          /* bump this for printing purposes */

    col2[col2_rlen] = '\0';                        /* have to null terminate */

    dump_data();

    if ((col2_ind == -2) || (col2_rlen < col2_ind))
       fetch_long();

    printf("\n");
  }

  if (cda.rc != NO_DATA_FOUND)
    err_report(&cda);

}

/*
 * Function: fetch_long
 *
 * Description: This routine retrieves the rest of a long row in pieces
 *
 */
void fetch_long()
{

   ub4 offset = col2_rlen;
    /* we have already fetched col2_rlen characters so this is where the offset
                                            starts  for the next fetch cycle */
   ub4 piece = 0;

   printf(" <Printing the rest of col2 for row %d>\n", row_count);

   do
   {

     if (oflng(&cda, 2, (ub1 *)col2, (sb4)(sizeof(col2) - 1), SQLT_LNG,
               &piece, offset))
     {
       err_report(&cda);
       do_exit(OCI_EXIT_FAILURE);
     }

     if (piece)                                      /* did we get anything? */
     {
       printf(" received %d characters ", piece);
       col2[piece] ='\0';
       printf(" data piece is %s\n", col2);
     }

     offset += piece;                      /* bump the offset and keep going */

   } while (piece == LONG_SIZE);

}


/*
 * Function: dump_data
 *
 * Description: This routine prints out the row/s from each fetch
 *
 */
void dump_data()
{

    printf(" row %d col1 is %d\n", row_count, col1);
    printf(" row %d col2 is %s \n", row_count, col2);

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

