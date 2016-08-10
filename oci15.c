/* Copyright (c) 1995, 2006, Oracle. All rights reserved.  
*/

/*
   NAME
     oci15.c
   DESCRIPTION
     Demo program for A22400 OCI Techniques White Paper
     Tests array insert with user prompt on each pass
   MODIFIED   (MM/DD/YY)
    kmohan     03/28/06  - change hda to size_t
    whe        10/20/99 -  #1039217: add int for static variable pass
    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
    azhao      09/11/97 -  increase response buffer size to 3
    ehayes     05/22/97 -  Fix VMS porting exceptions
    dchatter   07/15/96 -  hda is a ub4 array to prevent bus error
    vraghuna   03/01/95 -  Creation
*/
/*
 * Name:        oci15.c
 *
 * Description: This program performs an array insert one column using
 *              an INSERT statement.
 *              Always inserting 5 rows at a time.
 *
 * Based on:    oci14.c
 * Changes:     user to press enter after each cycle
 *
 * Setup:       Run corresponding ociXX.sql before running this program.
 *              Link program and run it.
 *              Program requires ONE argument - number of rows to insert
 *
 *                           oci15 4 <return>
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
 *      Define    -   none               insert statement
 *      Execute   -   oexec
 *      Fetch     -   none               insert
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

/* Globals needed */
uword row_count = 0;                       /* number of rows inserted so far */
uword numrows;                                            /* get it as input */

/* Function prototypes */
void logon ();
void logoff ();
void setup();
void err_report();
void insert_data();
void do_exit();
void initialize_data();

/* SQL statement used in this program */

text *sqlstmt = (text *)"INSERT INTO oci15tab (col1) values (:col1)";

#define NUMCOLS 1               /* number of columns returned by above query */
#define MAX_ROWS_PER_INSERT 5                      /* 5 rows max per insert */

/* array fetch definitions */
struct  col_struct{
         sword size;                                 /* length of the column */
         sword type;                                   /* type of the column */
       } ;

struct col_struct coltable [] =
       {  {MAX_NAME_LENGTH+1, SQLT_STR}
       };


struct   colp   {                                       /* Buffer descriptor */
         sword    c_type;             /* What type is this column fetched as */
         ub1     *c_buf;                     /* The area to store the column */
         ub2      c_size;                        /* Size of the storage area */
         sb2     *c_indp;              /* Indicator variable for this column */
         ub2     *c_rlen;                    /* Fetched length of the column */
         ub2     *c_rcode;                          /* array of return codes */
         sb4      c_curlen;                                /* current length */
  };/* COLUMN STRUCTURE */
typedef  struct   colp colp;
#define  malloc_col(n) (colp *)malloc((size_t)(n * sizeof(colp)));

colp *cols;                                              /* column structure */

main(argc, argv)
eword argc;
text **argv;
{

  if (argc != 2)
  {
     printf("\n\n Usage: <prog> <numrows> where numrows is the number of \
rows the user wants to insert \n");
     do_exit(OCI_EXIT_FAILURE);
  }

  numrows = atoi((char *)argv[1]);
  if (numrows > 26)
  {
    printf("Please input a number under 26\n");
    do_exit(OCI_EXIT_FAILURE);
  }
  printf("\n Total number of rows to be inserted is %d\n", numrows);

  logon();                                       /* logon to Oracle database */

  setup();                                          /* prepare sql statement */

  insert_data();

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

  colp *colsptr;                                        /* temporary pointer */
  sword colindex;

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

  cols = malloc_col(NUMCOLS);
  colsptr = cols;
  for (colindex = 0; colindex < NUMCOLS; colindex++, colsptr++)
  {

    colsptr->c_size  = coltable[colindex].size;
    colsptr->c_type  = coltable[colindex].type;
    colsptr->c_indp  = (sb2*)malloc((size_t)(MAX_ROWS_PER_INSERT*sizeof(sb2)));
    memset((dvoid *)colsptr->c_indp, 0, MAX_ROWS_PER_INSERT * sizeof(sb2));
    colsptr->c_rlen  = (ub2*)malloc((size_t)(MAX_ROWS_PER_INSERT*sizeof(ub2)));
    memset((dvoid *)colsptr->c_rlen, 0, MAX_ROWS_PER_INSERT * sizeof(ub2));
    colsptr->c_rcode = (ub2*)malloc((size_t)(MAX_ROWS_PER_INSERT*sizeof(ub2)));
    memset((dvoid *)colsptr->c_rcode, 0, MAX_ROWS_PER_INSERT * sizeof(ub2));
    colsptr->c_buf   =
                  (ub1 *)malloc((size_t)(MAX_ROWS_PER_INSERT*colsptr->c_size));
    memset((dvoid *)colsptr->c_buf, 0, MAX_ROWS_PER_INSERT * colsptr->c_size);
    colsptr->c_curlen = 0;

    switch (colindex) {
    case(0) :

/* GOTCHA!!! - need to pass in zeroes for mal and cal parameters when
   running a regular insert. Set it to non-zero when using plsql */
        if (obndra(&cda, (text *)":col1", -1,
                   colsptr->c_buf, colsptr->c_size,
                   (sword)colsptr->c_type, (sword) -1,
                   (sb2 *)colsptr->c_indp, colsptr->c_rlen, colsptr->c_rcode,
                   (ub4)0, (ub4 *)0,
                   (text *)0, -1, -1))
        {
          err_report(&cda);
          do_exit(OCI_EXIT_FAILURE);
        }
        break;
    }

  }

}

/*
 * Function: insert_data
 *
 * Description: This routine inserts the data into the table
 *
 */
void insert_data()
{

  colp *colsptr;
  sword colindex;
  sword numinsert;           /* number of rows to insert on the current pass */
  text response[3];

  row_count = 0;
  while (row_count < numrows)
  {

    colsptr = cols;
    numinsert = numrows - row_count < MAX_ROWS_PER_INSERT ?
                       numrows - row_count : MAX_ROWS_PER_INSERT;

    for (colindex = 0; colindex < NUMCOLS; colsptr++, colindex++)
       initialize_data(colsptr->c_buf, colsptr->c_size, colsptr->c_rlen,
                       numinsert);

                      /* using offset zero - we want all the rows right away */
    if (oexn(&cda, numinsert, 0))
    {
        err_report(&cda);
        do_exit(OCI_EXIT_FAILURE);
    }

    printf("\n Do a select * from the table in another window \n");
    printf(" Then press return to continue -->");
    gets((char *)response);

    row_count += numinsert;
  }

}

/*
 * Function: initialize_data
 *
 * Description: This routine generates data for insertion
 *
 */
void initialize_data(arr, len, flen, numinsert)
ub1 *arr;
sword len;
ub2 *flen;
sword numinsert;
{

      static int pass = 0;
      sword colindex;                                               /* index */
      sword numchars;                     /* number of characters to extract */
      text *str = (text *)"ABCDEFGHIJKLMNOPQRSTUVWXYZ";


      for (colindex = 0; colindex < numinsert; colindex++)
      {
          numchars = (pass * MAX_ROWS_PER_INSERT) + colindex + 1;
          memcpy((dvoid *)(arr + colindex*len), (dvoid *)str, numchars);
          *(arr + colindex*len + numchars ) = '\0';        /* null terminate */
          printf("iteration %d string is %s  length is %d\n",
                 pass, arr + colindex *len,
                 strlen((char *)(arr + colindex *len)));

/* GOTCHA!! - The null must be found within the length specified so add one */
/* or just use the max length allowed for the array element */

          *(flen + colindex) = strlen((char *)(arr + colindex*len)) + 1;
                               /* *(flen + colindex) = 30;  <--- or use this */
      }

      pass++;

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

