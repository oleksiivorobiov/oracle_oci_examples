/* Copyright (c) 1991, 2006, Oracle. All rights reserved.  
*/

/*
   NAME
     cdemo3.c - C demo program # 3
   MODIFIED   (MM/DD/YY)
    kmohan     03/28/06  - change hda to size_t
    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
    ehayes     05/29/97 -  Fix olint errors
    dchatter   01/05/96 -  Creation of the Solaris version with hda as
                           ub4 array of HDA_SIZE/sizeof(ub4) for alignment
                           reasons.
*/
/*
 *  cdemo3.c
 *
 *  Demonstrates using the oflng function to retrieve
 *  a portion of a LONG column.
 *
 *  This example "plays" a digitized voice message
 *  by repeatedly extracting 64 Kbyte chunks of the message
 *  from the table and sending them to a converter buffer
 *  (for example, a digital-to-analog converter's FIFO buffer).
 *
 *  To better understand this example, the table is created by
 *  the program, and some dummy data is inserted into it.
 *
 *  The converter subroutine is only simulated in this example
 *  program.
 *
 *  The size of the HDA is defined by the HDA_SIZE constant,
 *  which is declared in ocidem.h to be 256 bytes for 32-
 *  bit architectures and 512 bytes for 64-bit architectures.
 *
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MSG_SIZE        200000

#include <oratypes.h>
#include <ocidfn.h>
#ifdef __STDC__
#include <ociapr.h>
#else
#include <ocikpr.h>
#endif
#include <ocidem.h>

Cda_Def cda;
Lda_Def lda;
size_t  hda[HDA_SIZE/sizeof(size_t)];


dvoid do_exit();
dvoid oci_error();
dvoid play_msg();


main()
{
    text sql_statement[256];
    register sword i;
    sb2 indp;
    ub2 retl, rcode;
    sword msg_id;
    sb4 msg_len, len, offset;
    ub1 *ucp;
    register ub1 *ucp1;
    ub4 ret_len;


    /* Connect to ORACLE. */
    if (olog(&lda, (ub1 *)hda, (text *) "scott/tiger", -1,
              (text *) 0, -1, (text *) 0, -1, (ub4)OCI_LM_DEF))
    {
        fputs("Cannot connect with username SCOTT. Exiting...\n", stderr);
        exit(EXIT_FAILURE);
    }
    fputs("Connected to ORACLE as user SCOTT.\n", stdout);


    /* Open a cursor. */
    if (oopen(&cda, &lda, (text *) 0, -1,
              -1, (text *) 0, -1))
    {
        fputs("Cannot open cursor. Exiting...\n", stderr);
        exit(EXIT_FAILURE);
    }

    fputs("Program is about to drop the VOICE_MAIL table.\n", stdout);
    fputs("Is this OK (Y or N)? : ", stdout);
    fflush(stdout);

    gets((char *) sql_statement);
    if (*sql_statement != 'y' && *sql_statement != 'Y')
        do_exit(EXIT_SUCCESS);

    /* Parse, set defflg parameter for non-deferred parse
       (to execute the DDL statement immediately). */
    if (oparse(&cda, (text *) "DROP TABLE voice_mail",
               (sb4)-1, FALSE, (ub4)2))
    {
        if (cda.rc == 942)
            fputs("Table did not exist.\n", stdout);
        else
            oci_error(&cda);
    }
    else
        fputs("Dropped table \"voice_mail\".\n", stdout);

    strcpy((char *) sql_statement, "CREATE TABLE voice_mail\
        (msg_id NUMBER(6), msg_len NUMBER(12), msg LONG RAW)");
    if (oparse(&cda, sql_statement, (sb4)-1, FALSE, (ub4)2))
        oci_error(&cda);
    fputs("Created table \"voice_mail\".\n", stdout);


    /* Create a dummy message. */
    strcpy((char *) sql_statement,
           "INSERT INTO voice_mail (msg_id, msg_len, msg) \
                           VALUES (:1, :2, :3)");
    if (oparse(&cda, sql_statement, (sb4)-1, FALSE, (ub4)2))
        oci_error(&cda);

    if (obndrn(&cda, 1, (ub1 *) &msg_id, 4, 3, -1,
               (sb2 *) 0, (text *) 0, 0, -1))
        oci_error(&cda);

    /* Set buffer address before binding. */
    ucp = (ub1 *) malloc(MSG_SIZE);
    if (ucp == 0)
    {
        fputs("malloc error\n", stderr);
        do_exit(EXIT_FAILURE);
    }

    if (obndrn(&cda, 2, (ub1 *) &msg_len, 4, 3, -1,
               (sb2 *) 0, (text *) 0, 0, -1))
        oci_error(&cda);

    if (obndrn(&cda, 3, ucp, MSG_SIZE, 24, -1,
               (sb2 *) 0, (text *) 0, 0, -1))
        oci_error(&cda);

    /* Set bind vars before oexn. */
    msg_id  = 100;
    msg_len = MSG_SIZE;
    for (i = 0, ucp1 = ucp; i < MSG_SIZE; i++)
        *ucp1++ = (ub1) i % 128;

    if (oexn(&cda, 1, 0))
        oci_error(&cda);
    fputs("Data inserted in table \"voice_mail\".\n", stdout);

/*
 *  After setting up the test data, do the
 *  select and fetch the message chunks
 */
    strcpy((char *) sql_statement, "select msg_id, msg_len, msg\
                  from voice_mail where msg_id = 100");
    if (oparse(&cda, sql_statement, (sb4)-1, 0, (ub4)2))
        oci_error(&cda);
    if (odefin(&cda,
               1,               /* index */
               (ub1 *) &msg_id, /* output variable */
               4,               /* length */
               3,               /* datatype */
               -1,              /* scale */
               (sb2 *) 0,       /* indp */
               (text *) 0,      /* fmt */
               0,               /* fmtl */
               -1,              /* fmtt */
               (ub2 *) 0,       /* retl */
               (ub2 *) 0))      /* rcode */
        oci_error(&cda);
    if (odefin(&cda,
               2,               /* index */
               (ub1 *) &msg_len,/* output variable */
               4,               /* length */
               3,               /* datatype */
               -1,              /* scale */
               (sb2 *) 0,       /* indp */
               (text *) 0,      /* fmt */
               0,               /* fmtl */
               -1,              /* fmtt */
               (ub2 *) 0,       /* retl */
               (ub2 *) 0))      /* rcode */
        oci_error(&cda);
    if (odefin(&cda,
               3,               /* index */
               ucp,             /* output variable */
               100,             /* length */
               24,              /* LONG RAW datatype code */
               -1,              /* scale */
               &indp,           /* indp */
               (text *) 0,      /* fmt */
               0,               /* fmtl */
               -1,              /* fmtt */
               &retl,           /* retl */
               &rcode))         /* rcode */
        oci_error(&cda);


    /* Do the query, getting the msg_id and the first
       100 bytes of the message. */
    if (oexfet(&cda,
               (ub4) 1,         /* nrows */
               0,               /* cancel (FALSE) */
               0))              /* exact (FALSE) */
    {
            oci_error(&cda);
    }
    fprintf(stdout,
      "Message %d is available, length is %d.\n", msg_id, msg_len);
    fprintf(stdout,
      "indp = %d, rcode = %d, retl = %d\n", indp, rcode, retl);

    /* Play the message, looping until there are
       no more data points to output. */

    for (offset = (ub4) 0; ; offset += (ub4) 0x10000)
    {
        len = msg_len < 0x10000 ? msg_len : 0x10000;

        if (oflng(&cda,
                  3,                /* position */
                  ucp,              /* buf */
                  len,              /* bufl */
                  24,               /* datatype */
                  &ret_len,         /* retl */
                  offset))          /* offset */
            oci_error(&cda);

        /* Output the message chunk. */
        play_msg(ucp, len);
        msg_len -= len;
        if (msg_len <= 0)
            break;
    }
    do_exit(EXIT_SUCCESS);
}


dvoid
play_msg(buf, len)
ub1 *buf;
sword len;
{
    fprintf(stdout, "\"playing\" %d bytes.\n", len);
}


dvoid
oci_error(cda)
Cda_Def *cda;
{
    text msg[200];
    sword n;

    fputs("\n-- ORACLE ERROR --\n", stderr);
    n = oerhms(&lda, (sb2) cda->rc, msg, 200);
    fprintf(stderr, "%.*s", n, msg);
    fprintf(stderr, "Processing OCI function %s\n",
            oci_func_tab[(int) cda->fc]);
    do_exit(EXIT_FAILURE);
}


dvoid
do_exit(rv)
sword rv;
{
    fputs("Exiting...\n", stdout);
    if (oclose(&cda))
    {
        fputs("Error closing cursor.\n", stderr);
        rv = EXIT_FAILURE;
    }
    if (ologof(&lda))
    {
        fputs("Error logging off.\n", stderr);
        rv = EXIT_FAILURE;
    }
    exit(rv);
}



