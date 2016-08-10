/* Copyright (c) 1991, 2006, Oracle. All rights reserved.  
*/

/*
   NAME
     cdemo4.c - <one-line expansion of the name>
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
    azhao      10/11/06  - case-senstive password change
    kmohan     03/28/06  - change hda to size_t
    jchai      07/27/04  - add order by in select stmt 
    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
    ehayes     05/29/97 -  Fix olint errors
    dchatter   07/15/96 -  hda is a ub4 array to prevent bus error
    plocke     11/18/95 -  to update for v7.3
    slari      04/25/95 -  merge changes from branch 1.6.720.1
    slari      04/19/95 -  replace orlon with olog
    emendez    04/07/94 -  merge changes from branch 1.4.710.3
    gdoherty   04/06/94 -  merge changes from branch 1.4.710.1
    emendez    02/07/94 -  fix for bug 196094
    emendez    02/02/94 -  Fix for bug 157576
    gdoherty   02/02/94 -  make oci header inclusion for ansi or k+r adaptive
    lfeng      01/13/93 -  fix non-portable fflush
    rkooi2     11/27/92 -  Changing e... datatypes to s...
    kaghevli   11/06/92 -  test
    rkooi2     10/29/92 -  Creation
*/
/* cdemo4.c
 *
 *  Demonstrates doing a FETCH from a cursor
 *  into PL/SQL tables.  The tables are bound to C
 *  arrays using the obndra routine.
 *  The fully-commented script to create the stored procedure
 *  is in the demo program file calldemo.sql.
 *
 *  Execute this script using SQL*DBA or SQL*Plus
 *  to store the package before executing this program.
 *
 * The script is:
 * create or replace package calldemo as
 *
 *   type char_array is table of varchar2(20) index by binary_integer;
 *   type num_array is table of float index by binary_integer;
 *
 *   procedure get_employees(
 *     dept_number in     integer,   -- which department to query
 *     batch_size  in     integer,   -- how many rows at a time
 *     found       in out integer,   -- n of rows actually returned
 *     done_fetch  out    integer,   -- all done flag
 *     emp_name    out    char_array,-- arrays of employee names,
 *     job         out    char_array,--                    jobs,
 *     sal         out    num_array);--                    salaries
 *
 * end;
 * /
 *
 * create or replace package body calldemo as
 *
 *   cursor get_emp(
 *     dept_number in     integer) is
 *       select ename, job, sal from emp
 *       where deptno = dept_number order by ename;
 *
 * -- Procedure get_employees fetches a batch of employee
 * -- rows (batch size is determined by the client/caller
 * -- of this procedure).  Procedure may be called from
 * -- other stored procedures or client application
 * -- programs.  The procedure opens the cursor if it is
 * -- not already open, fetches a batch of rows, and
 * -- returns the number of rows actually retrieved.  At
 * -- end of fetch, the procedure closes the cursor.
 *
 *   procedure get_employees(
 *     dept_number in     integer,
 *     batch_size  in     integer,
 *     found       in out integer,
 *     done_fetch  out    integer,
 *     emp_name    out    char_array,
 *     job         out    char_array,
 *     sal         out    num_array) is
 *
 *    begin
 *      if NOT get_emp%ISOPEN then     -- open the cursor if it is
 *        open get_emp(dept_number);   -- not already open
 *      end if;
 *
 * -- Fetch up to "batch_size" rows into PL/SQL table,
 * -- tallying rows found as they are retrieved.  When end
 * -- of fetch is encountered, close the cursor and exit
 * -- the loop, returning only the last set of rows found.
 *
 *     done_fetch := FALSE;
 *     found := 0;
 *
 *     for i in 1..batch_size loop
 *        fetch get_emp               -- get one emp table row
 *          into emp_name(i), job(i), sal(i);
 *
 *        if get_emp%notfound then    -- if no row was found, then
 *          close get_emp;            -- close the cursor
 *          done_fetch := TRUE;       -- indicate all done
 *          exit;                     -- exit the loop
 *        else
 *          found := found + 1;       -- else count the row and continue
 *          end if;
 *     end loop;
 *    end;
 * end;
 * /
 */

#include <stdio.h>
#include <string.h>

#include <oratypes.h>
#include <ocidfn.h>
#ifdef __STDC__
#include <ociapr.h>
#else
#include <ocikpr.h>
#endif
#include <ocidem.h>

#define MAX_ARRAY_SIZE     5
#define NO_PARSE_DEFER     0
#define V7_LNGFLG          2
#define VC_LENGTH         20

/* Declare the data areas. */
Cda_Def cda;
Lda_Def lda;
size_t  hda[HDA_SIZE/sizeof(size_t)];                           /* host area */

/*  Declare routines in this program */
dvoid do_fetch(/*_ void _*/);
dvoid oci_error(/*_ void _*/);

main(argc, argv)
sword argc;
text **argv;
{
    text username[128];

    if (argc > 1)
        strncpy((char *) username, (char *) argv[1],
                sizeof (username) - 1);
    else
        strcpy((char *) username, "SCOTT/tiger");

    if (olog(&lda, (ub1 *)hda, username, -1, (text *) 0, -1,
             (text *) 0, -1, (ub4)OCI_LM_DEF))
    {
        printf("Cannot connect as %s. Exiting...\n", username);
        exit(-1);
    }
    else
        printf("Connected.\n");

    /* Open the OCI cursor. */
    if (oopen(&cda, &lda, (text *) 0, -1, -1, (text *) 0, -1))
    {
        printf("Cannot open cursor data area, exiting...\n");
        exit(-1);
    }

    /* Fetch and print the data. */
    do_fetch();

    /* Close the OCI cursor. */
    if (oclose(&cda))
    {
        printf("Error closing cursor!\n");
        exit(-1);
    }

    /* Disconnect from ORACLE. */
    if (ologof(&lda))
    {
        printf("Error logging off!\n");
        exit(-1);
    }
    exit(0);
}


/*  Set up an anonymous PL/SQL call to the stored
    procedure that fetches the data. */
dvoid
do_fetch(/*_ void _*/)
{
    text *call_fetch = (text *) "\
      begin\
        calldemo.get_employees(:deptno, :t_size, :num_ret, :all_done,\
                             :e_name, :job, :sal);\
      end;";
    sword table_size = MAX_ARRAY_SIZE;
    sword i, n_ret, done_flag;
    sword dept_num;
    sb2 n_ret_indp;
    ub2 n_ret_len, n_ret_rcode;
    ub4 n_ret_cursiz = 0;

    text emp_name[MAX_ARRAY_SIZE][VC_LENGTH];
    sb2 emp_indp_name[MAX_ARRAY_SIZE];
    ub2 emp_len_name[MAX_ARRAY_SIZE];
    ub2 emp_rcode_name[MAX_ARRAY_SIZE];
    ub4 emp_cursiz_name = (ub4) MAX_ARRAY_SIZE;

    text job[MAX_ARRAY_SIZE][VC_LENGTH];
    sb2 job_indp[MAX_ARRAY_SIZE];
    ub2 job_len[MAX_ARRAY_SIZE];
    ub2 job_rcode[MAX_ARRAY_SIZE];
    ub4 job_cursiz = (ub4) MAX_ARRAY_SIZE;

    float salary[MAX_ARRAY_SIZE];
    sb2 salary_indp[MAX_ARRAY_SIZE];
    ub2 salary_len[MAX_ARRAY_SIZE];
    ub2 salary_rcode[MAX_ARRAY_SIZE];
    ub4 salary_cursiz = (ub4) MAX_ARRAY_SIZE;

    /* parse the anonymous SQL block */
    if (oparse(&cda, call_fetch, (sb4)-1,
               NO_PARSE_DEFER, (ub4)V7_LNGFLG))
    {
        oci_error();
        return;
    }

    /* initialize the bind arrays */
    for (i = 0; i < MAX_ARRAY_SIZE; i++)
    {
        emp_len_name[i] = VC_LENGTH;
        job_len[i] = VC_LENGTH;
        salary_len[i] = sizeof (float);
    }

    n_ret_len = sizeof (sword);

    /* bind the department number IN parameter */
    if (obndrv(&cda, (text *) ":deptno", -1, (ub1 *) &dept_num,
               (sword) sizeof (sword), INT_TYPE, -1,
               (sb2 *) 0, (text *) 0, -1, -1))
    {
        oci_error();
        return;
    }
    /* bind the table size IN parameter */
    if (obndrv(&cda, (text *) ":t_size", -1, (ub1 *) &table_size,
               (sword) sizeof (sword),
               INT_TYPE, -1, (sb2 *) 0, (text *) 0, -1, -1))
    {
        oci_error();
        return;
    }
    /* bind the fetch done OUT parameter */
    if (obndrv(&cda, (text *) ":all_done", -1, (ub1 *) &done_flag,
               (sword) sizeof (sword),
               INT_TYPE, -1, (sb2 *) 0, (text *) 0, -1, -1))
    {
        oci_error();
        return;
    }

    /* Bind the OUT n_ret using obndra. obndrv could
       have been used just as well, since no arrays
       are involved, but it is possible to use obndra
       for scalars as well. */
    if (obndra(&cda,
           (text *) ":num_ret",
           -1,
           (ub1 *) &n_ret,
           (sword) sizeof (sword),
           INT_TYPE,
           -1,
           &n_ret_indp,
           &n_ret_len,
           &n_ret_rcode,
           (ub4) 0,      /* pass as 0, not 1, when binding a scalar */
           (ub4 *) 0,    /* pass as the null pointer when scalar */
           (text *) 0,
           -1,
           -1))
    {
        oci_error();
        return;
    }

    /* bind the employee name array */
    if (obndra(&cda,
           (text *) ":e_name",
           -1,
           (ub1 *) emp_name,
           VC_LENGTH,
           VARCHAR2_TYPE,
           -1,
           emp_indp_name,
           emp_len_name,
           emp_rcode_name,
           (ub4) MAX_ARRAY_SIZE,
           &emp_cursiz_name,
           (text *) 0,
           -1,
           -1))
    {
        oci_error();
        return;
    }

    /* bind the job array */
    if (obndra(&cda,
           (text *) ":job",
           -1,
           (ub1 *) job,
           VC_LENGTH,
           VARCHAR2_TYPE,
           -1,
           job_indp,
           job_len,
           job_rcode,
           (ub4) MAX_ARRAY_SIZE,
           &job_cursiz,
           (text *) 0,
           -1,
           -1))
    {
        oci_error();
        return;
    }

    /* bind the salary array */
    if (obndra(&cda,
           (text *) ":sal",
           -1,
           (ub1 *) salary,
           (sword) sizeof (float),
           FLOAT_TYPE,
           -1,
           salary_indp,
           salary_len,
           salary_rcode,
           (ub4) MAX_ARRAY_SIZE,
           &salary_cursiz,
           (text *) 0,
           -1,
           -1))
    {
        oci_error();
        return;
    }

    printf("\nenter deptno: ");
    scanf("%d", &dept_num);

    for (;;)
    {
        /* execute the fetch */
        if (oexec(&cda))
        {
            oci_error();
            return;
        }

        printf("\n%d row%c returned\n",
               n_ret, n_ret == 1 ? '\0' : 's');

        if (n_ret > 0)
        {
            printf("\n%-*.*s%-*.*s%s\n",
                   VC_LENGTH, VC_LENGTH, "Employee Name",
                   VC_LENGTH, VC_LENGTH, "Job", "   Salary");
            for (i = 0; i < n_ret; i++)
            {
                printf("%.*s", emp_len_name[i], emp_name[i]);
                printf("%*c", VC_LENGTH - emp_len_name[i], ' ');
                printf("%.*s", job_len[i], job[i]);
                printf("%*c", VC_LENGTH - job_len[i], ' ');
                printf("%8.2f\n", salary[i]);
            }
        }
        if (done_flag != 0)
        {
            printf("\n");
            break;
        }
    }
    return;
}


dvoid
oci_error(/*_ void _*/)
{
    text msg[900];
    sword rv;

    rv = oerhms(&lda, cda.rc, msg, (sword) sizeof (msg));

    printf("\n\n%.*s", rv, msg);
    printf("Processing OCI function %s\n",
           oci_func_tab[(int) cda.fc]);
    return;
}


