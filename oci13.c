/* Copyright (c) 1995, 2006, Oracle. All rights reserved.  
*/

/*
   NAME
     oci13.c
   DESCRIPTION
     Demo program for A22400 OCI Techniques White Paper
     Tests array fetch of LONG data with piecewise fetching
   MODIFIED   (MM/DD/YY)
    kmohan     03/28/06  - change hda to size_t
    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
    ehayes     05/29/97 -  Fix olint errors
    dchatter   07/15/96 -  hda is a ub4 array to prevent bus error
    vraghuna   03/01/95 -  Creation
*/

/*
 * Name:        oci13.c
 *
 * Description: This program retrieves the rows contained in oci13tab which
 *              contains a long. Some rows have to be fetched piecewise.
 *
 *              It illustrates getting data back using ARRAY FETCH
 *              via a SELECT statement.
 *              It illustrates how to do a piecewise fetch of a long.
 *              Fetching 5 characters per long fetch
 *              With an array fetch, piecewise longs will have to be
 *              fetched with a different cursor
 *
 * Based on:    oci07.c
 * Changes:     added long support
 *              hardcoded oci13tab in query
 *
 * Setup:       Run corresponding ociXX.sql before running this program.
 *              Link program and run it.
 *              Program requires ONE argument - number of rows per
 *                fetch cycle
 *
 *                           oci13 4 <return>
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

/* exit flags */
#define OCI_EXIT_FAILURE 1
#define OCI_EXIT_SUCCESS 0

/* Globals needed */
Lda_Def lda;                                                   /* login area */
size_t     hda[HDA_SIZE/sizeof(size_t)];                        /* host area */
Cda_Def cda;                                                  /* cursor area */

/* Globals needed */
uword row_count = 0;                       /* number of rows returned so far */
uword numrows;                                            /* get it as input */

/* Function prototypes */
void logon ();
void logoff ();
void setup();
void err_report();
void get_data();
void dump_data();
void do_exit();
void fetch_long();
void setup_long();

/* SQL statement used in this program */

text *sqlstmt = (text *)"SELECT col1, col2 FROM oci13tab ORDER BY col1";

#define NUMCOLS 2               /* number of columns returned by above query */
#define LONG_SIZE 5        /* getting back 5 characters with each long piece */
text col2[LONG_SIZE +1];                   /* buffer to hold the long pieces */

/* array fetch definitions */
struct  col_struct{
         sword size;                                 /* length of the column */
         sword type;                                   /* type of the column */
       } ;

struct col_struct coltable [] =
        { {sizeof(eword), SQLT_INT},
          {LONG_SIZE, SQLT_LNG}
        };

struct   colp   {                                       /* Buffer descriptor */
         sword    c_type;             /* What type is this column fetched as */
         ub1     *c_buf;                     /* The area to store the column */
         ub2      c_size;                        /* Size of the storage area */
         sb2     *c_indp;              /* Indicator variable for this column */
         ub2     *c_rlen;                    /* Fetched length of the column */
         ub2     *c_rcode;                          /* array of return codes */
  };/* COLUMN STRUCTURE */
typedef  struct   colp colp;
#define  malloc_col(n) (colp *)malloc((size_t)(n * sizeof(colp)));

colp *cols;                                              /* column structure */

Cda_Def cda_long;                /* cursor area to get remaining long pieces */

text *long_sqlstmt = (text *)"SELECT col2 FROM oci13tab\
                              where col1 = :row_number";

sword long_row_number;                     /* row number to fetch pieces for */


main(argc, argv)
eword argc;
text **argv;
{

  if (argc != 2)
  {
     printf("\n\n Usage: <prog> <numrows> \
where numrows is number of rows per fetch cycle\n\n\n");
     do_exit(OCI_EXIT_FAILURE);
  }

  numrows = atoi((char *)argv[1]);
  printf("\n Number of rows to be fetched per cycle is %d\n", numrows);

  logon();                                       /* logon to Oracle database */

  setup();                                          /* prepare sql statement */

  setup_long();                                        /* prepare long query */

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
    colsptr->c_indp  = (sb2*)malloc((ub4)(numrows*sizeof(sb2)));
    colsptr->c_rlen  = (ub2*)malloc((ub4)(numrows*sizeof(ub2)));
    colsptr->c_rcode = (ub2*)malloc((ub4)(numrows*sizeof(ub2)));
    colsptr->c_buf   = (ub1 *)malloc((ub4)(numrows*colsptr->c_size));

    if (odefin(&cda, colindex + 1,
               colsptr->c_buf, colsptr->c_size,
               (sword)colsptr->c_type, (sword) -1,
               (sb2 *)colsptr->c_indp, (text *)0, -1, -1,
               colsptr->c_rlen, colsptr->c_rcode))
    {
      err_report(&cda);
      do_exit(OCI_EXIT_FAILURE);
    }

  }

}

/*
 * Function: setup_long
 *
 * Description: This routine allocates the second cursor needed to fetch
 *              the remaining piece of a long.
 *
 */
void setup_long()
{

  if (oopen(&cda_long, &lda, (text *) 0, -1, -1, (text *) 0, -1))    /* open */
  {
    err_report(&cda_long);
    do_exit(OCI_EXIT_FAILURE);
  }

  if (oparse(&cda_long, long_sqlstmt, (sb4) -1, DEFER_PARSE,        /* parse */
               (ub4) VERSION_7))
  {
    err_report(&cda_long);
    do_exit(OCI_EXIT_FAILURE);
  }

  if (obndrv(&cda_long, (text *)":row_number", -1,
             (ub1 *)&long_row_number, (sword)sizeof(long_row_number),
             SQLT_INT, -1, (sb2 *) 0, (text *) 0, -1, -1))           /* bind */
  {
    err_report(&cda_long);
    do_exit(OCI_EXIT_FAILURE);
  }

  if (odefin(&cda_long, 1, (ub1 *) col2, (sword) (sizeof(col2) - 1),
             (sword) SQLT_LNG,
             (sword) -1, (sb2 *)0, (text *) 0, -1, -1,
             (ub2 *)0, (ub2 *) 0))
  {
    err_report(&cda_long);
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

  printf("\n printing the information for oci13tab: \n");

                    /* don't cancel the cursor after the first execute/fetch */
                                           /* also don't want an exact match */
  if (oexfet(&cda, (ub4)numrows, 0, 0) != 0 && cda.rc != NO_DATA_FOUND)
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }

  dump_data();

  if (cda.rc == NO_DATA_FOUND)
  {
    row_count = 0;
    return;                                         /* no more rows to fetch */
  }

  while(1)
  {
    ofen(&cda, (sword)numrows);

    if ((cda.rc == NO_DATA_FOUND) && ((cda.rpc - row_count) == 0))
      break;

    if ((cda.rc == 0) ||
        (cda.rc == NO_DATA_FOUND) && (cda.rpc - row_count)> 0)
    {
     dump_data();
     if ((cda.rc == NO_DATA_FOUND) && (cda.rpc == row_count))
       break;
    }

    else
    {
      err_report(&cda);
      do_exit(OCI_EXIT_FAILURE);
    }

   }

  row_count = 0;                     /* reset running counter for next table */
}

/*
 * Function: dump_data
 *
 * Description: This routine prints out the row/s from each fetch
 *
 */
void dump_data()
{

  uword index;
  ub4 value;
  text longbuf[LONG_SIZE + 1];

  for (index = 0; index < cda.rpc - row_count; index++)
  {


    memcpy((dvoid *)&value, (dvoid *)(cols[0].c_buf + index * cols[0].c_size),
            *(cols[0].c_rlen + index));
    printf(" row %d col1 is %d \n", row_count + index + 1 , value);

    memcpy((dvoid *)longbuf, (dvoid *)(cols[1].c_buf + index * cols[1].c_size),
           cols[1].c_rlen[index]);
    longbuf[cols[1].c_rlen[index]] = '\0';         /* need to null terminate */

    printf(" row %d col2 is %s\n", row_count + index + 1 , longbuf);

    if ((cols[1].c_rlen[index] < cols[1].c_indp[index]) ||
        (cols[1].c_indp[index] == -2))
    {
      long_row_number = row_count + index + 1;
      fetch_long(cols[1].c_rlen[index]);
    }

    printf("\n");

  }

  row_count += cda.rpc - row_count;            /* we only need the increment */

  printf(" Total rows so far is %d\n", row_count);
}

/*
 * Function: fetch_long
 *
 * Description: This routine retrieves the rest of a long row in pieces
 *
 */
void fetch_long(offset)
uword offset;
{

   ub4 piece = 0;

   printf(" <Printing the rest of col2 for row %d>\n", long_row_number);

                                                  /* we need exactly one row */
   if (oexfet(&cda_long, (ub4)1, 0, 0))
   {
     err_report(&cda_long);
     do_exit(OCI_EXIT_FAILURE);
   }

       /* the above call returns the first piece of the long - we have already
                                              printed it - so just ignore it */

   do
   {

     if (oflng(&cda_long, 1, (ub1 *)col2, (sb4)(sizeof(col2) - 1), SQLT_LNG,
               &piece, (ub4)offset))
     {
       err_report(&cda_long);
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

  if (oclose(&cda_long))                                /* close LONG cursor */
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
