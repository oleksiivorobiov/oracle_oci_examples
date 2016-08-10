#ifdef RCSID
static char *RCSid =
   "$Header: obndra.c 04-apr-2005.17:13:10 lzhao Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1991, 2005, Oracle. All rights reserved.  
*/

/*
   NAME
     obndra.c - <one-line expansion of the name>
   DESCRIPTION
     <short description of component this file declares/defines>
   PUBLIC FUNCTION(S)
     <list of external functions declared/defined - with one-line descriptions>
   PRIVATE FUNCTION(S)
     <list of static functions defined in .c file - with one-line descriptions>
   RETURNS
     <function return values, for .c file with single function>
   NOTES
     <other useful comments, qualifications, etc.>
   MODIFIED   (MM/DD/YY)
    lzhao      04/04/05  - bug4184457
    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
    azhao      03/21/97 -  drop table part_nos at the end
    rkooi2     11/28/92 -  Creation
*/

/*  Example program demonstrating an array insert using the obndra
    example from Chapter 4 of the OCI Programmer's Guide. */

#include <stdio.h>
#include <oratypes.h>
#include <ocidfn.h>
#include <ocidem.h>
#include <ocikpr.h>

Cda_Def cda;
Lda_Def lda;


/*  set up the table */
text *dt = (text *) "DROP TABLE part_nos";
text *ct = (text *) "CREATE TABLE part_nos (partno NUMBER, description\
            VARCHAR2(20))";

text *cp = (text *) "\
    CREATE OR REPLACE PACKAGE update_parts AS\
        TYPE part_number IS TABLE OF part_nos.partno%TYPE\
            INDEX BY BINARY_INTEGER;\
        TYPE part_description IS TABLE OF part_nos.description%TYPE\
            INDEX BY BINARY_INTEGER;\
        PROCEDURE add_parts (n            IN   INTEGER,\
                             descrip      IN   part_description,\
                             partno       IN   part_number);\
    END update_parts;";

text *cb = (text *) "\
    CREATE OR REPLACE PACKAGE BODY update_parts AS\
        PROCEDURE add_parts (n            IN   INTEGER,\
                             descrip      IN   part_description,\
                             partno       IN   part_number) is\
        BEGIN\
            FOR i IN 1..n LOOP\
                INSERT INTO part_nos\
                    VALUES (partno(i), descrip(i));\
            END LOOP;\
        END add_parts;\
    END update_parts;";


#define DESC_LEN             20
#define MAX_TABLE_SIZE     1200

text *pl_sql_block = (text *) "\
    BEGIN\
        update_parts.add_parts(3, :description, :partno);\
    END;";

text descrip[3][20] = {"Frammis", "Widget", "Thingie"};
sword  numbers[] = {12125, 23169, 12126};

ub2  descrip_alen[3] = {DESC_LEN, DESC_LEN, DESC_LEN};
ub2  descrip_rc[3];
ub4  descrip_cs = (ub4) 3;
ub2  descrip_indp[3];

ub2 num_alen[3] = {(ub2) sizeof (sword),
                              (ub2) sizeof (sword),
                              (ub2) sizeof (sword)};
ub2  num_rc[3];
ub4  num_cs = (ub4) 3;
ub2  num_indp[3];

dvoid oci_error(void);

main()
{
    printf("Connecting to ORACLE...");
    if (olon(&lda, (oratext *)"scott/tiger", -1, NULL, -1, -1)) {
        printf("Cannot logon as scott/tiger. Exiting...\n");
        exit(1);
    }

    if (oopen(&cda, &lda, NULL, -1, -1, NULL, -1)) {
        printf("Cannot open cursor, exiting...\n");

        exit(1);
    }

    /*  Drop the table. */
    printf("\nDropping table...");
    if (oparse(&cda, dt, -1, 0, 2))
        if (cda.rc != 942)
            oci_error();
    printf("\nCreating table...");
    if (oparse(&cda, ct, -1, 0, 2))
        oci_error();

    /*  Parse and execute the create
        package statement. */
    printf("\nCreating package...");
    if (oparse(&cda, cp, -1, 0, 2))
        oci_error();
    if (oexec(&cda))
        oci_error();

    /*  Parse and execute the create
        package body statement. */
    printf("\nCreating package body...");
    if (oparse(&cda, cb, -1, 0, 2))
        oci_error();
    if (oexec(&cda))
        oci_error();

    /*  Parse the anonymous PL/SQL block
        that calls the stored procedure. */
    printf("\nParsing PL/SQL block...");
    if (oparse(&cda, pl_sql_block, -1, 0, 2))
        oci_error();

    /*  Bind the C arrays to the PL/SQL tables. */
    printf("\nBinding arrays...");
    if (obndra(&cda,
               (text *) ":description",
               -1,
               (ub1 *) descrip,
               DESC_LEN,
               VARCHAR2_TYPE,
               -1,
               (sb2 *)descrip_indp,
               descrip_alen,
               descrip_rc,
               (ub4) MAX_TABLE_SIZE,
               &descrip_cs,
               (text *) 0,
               -1,
               -1))
            oci_error();

    if (obndra(&cda,
               (text *) ":partno",
               -1,
               (ub1 *) numbers,
               (sword) sizeof (sword),
               INT_TYPE,
               -1,
               (sb2 *)num_indp,
               num_alen,
               num_rc,
               (ub4) MAX_TABLE_SIZE,
               &num_cs,
               (text *) 0,
               -1,
               -1))
            oci_error();

    printf("\nExecuting block...");
    if (oexec(&cda))
        oci_error();

    /* drop table part_nos */
    if (oparse(&cda, dt, -1, 0, 2))
        oci_error();

    printf("\n");
    if (oclose(&cda))
    {
        printf("Error closing cursor!\n");
        return -1;
    }

    if (ologof(&lda))
    {
        printf("Error logging off!\n");
        return -1;
    }
    exit(1);
}


dvoid
oci_error(void)
{
    text msg[600];
    sword rv;

    rv = oerhms(&lda, (sb2)cda.rc, msg, 600);

    printf("\n\n%.*s", rv, msg);
    printf("Processing OCI function %s\n", oci_func_tab[cda.fc]);
    if (oclose(&cda))
        printf("Error closing cursor!\n");
    if (ologof(&lda))
        printf("Error logging off!\n");
    exit(1);
}

