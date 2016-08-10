/* Copyright (c) 1995, 2006, Oracle. All rights reserved.  
*/

/*
   NAME
     oci24.c
   DESCRIPTION
     Demo program for A22400 OCI Techniques White Paper
     Tests describe of a PL/SQL package
   MODIFIED   (MM/DD/YY)
    kmohan     03/28/06  - change hda to size_t
    chliang    02/06/01  - merge request for porting exception #1593298.
    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
    ehayes     05/29/97 -  Fix olint errors
    dchatter   07/15/96 -  hda is a ub4 array to prevent bus error
    vraghuna   03/01/95 -  Creation
*/

/*
 * Name:        oci24.c
 *
 * Description: This program illustrates how to do a describe on a procedure
 *              using odessp
 *
 * Based on:    <nothing>
 * Changes:     <none - base revision>
 *
 * Setup:       Run corresponding ociXX.sql before running this program.
 *              Link program and run it.
 *              Program requires NO arguments
 *
 *                           oci24 <return>
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
#define ASIZE 15

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
void setup ();
void err_report();
void do_exit();
void do_describe();

/* odessp uses the object name directly, no SQL statement involved */

text *objname = (text *)"oci24pkg.oci24proc";              /* name of object */

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


ub2 ovrld[ASIZE];                                 /* overloading information */
ub2 pos[ASIZE];                                        /* parameter position */
ub2 level[ASIZE];                          /* level for composite parameters */
text argnm[ASIZE][MAX_NAME_LENGTH];                       /* parameter names */
ub2 arnlen[ASIZE];                              /* length of parameter names */
ub2 dtype[ASIZE];                    /* Oracle datatype codes for parameters */
ub1 defsup[ASIZE];                  /* returns "default supplied" indicators */
ub1 mode[ASIZE];                                 /* mode ie. IN, OUT, IN/OUT */
ub4 dtsize[ASIZE];                                                /* lengths */
sb2 prec[ASIZE];                                   /* precision, if a number */
sb2 scale[ASIZE];                                      /* scale, if a number */
ub1 radix[ASIZE];                                      /* radix, if a number */
ub4 spare[ASIZE];                                /* reserved, must be passed */
ub4 array_size;                                    /* size of the OUT arrays */
ub2 i;                                                            /* counter */

       array_size = ASIZE;      /* gotcha - you need to input the array size */


       if (odessp(&lda, objname, -1, (ub1 *)0, 0, (ub1 *)0, 0,
                  ovrld, pos, level, (text **)argnm, arnlen, dtype, defsup, mode,
                  dtsize, prec, scale, radix, spare, &array_size))
       {
         err_report((Cda_Def *)&lda);
         do_exit(OCI_EXIT_FAILURE);
       }

       printf("array contains information for %d entries \n", array_size);

       printf("Overload\tLevel\tPos\tprocName\tDatatype\n");
       for (i = 0; i < array_size; i++)
       {
         printf("%8d\t%5d\t%3d\t%.*s\t%16d\n",
                ovrld[i], level[i], pos[i], arnlen[i], argnm[i], dtype[i]);
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
