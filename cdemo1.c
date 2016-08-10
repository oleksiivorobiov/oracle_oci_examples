/* Copyright (c) 1991, 2006, Oracle. All rights reserved.  
*/

/*
   NAME
     cdemo1.c - C Demo Program
   MODIFIED   (MM/DD/YY)
    azhao      10/11/06  - case-senstive password change
    kmohan     03/28/06  - change hda to size_t
    emendez    09/13/00  - fix top 5 olint errors
    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
    ehayes     05/22/97 -  Fix VMS porting exceptions
    dchatter   01/05/96 -  Creation of the Solaris version with hda as
                           ub4 array of HDA_SIZE/sizeof(ub4) for alignment
                           reasons.
*/
/*
 *      -- cdemo1.c --
 *  An example program which adds new employee
 *  records to the personnel data base.  Checking
 *  is done to insure the integrity of the data base.
 *  The employee numbers are automatically selected using
 *  the current maximum employee number as the start.
 *
 *  The program queries the user for data as follows:
 *
 *  Enter employee name:
 *  Enter employee job:
 *  Enter employee salary:
 *  Enter employee dept:
 *
 *  The program terminates if return key (CR) is entered
 *  when the employee name is requested.
 *
 *  If the record is successfully inserted, the following
 *  is printed:
 *
 *  "ename" added to department "dname" as employee # "empno"
 *
 *  The size of the HDA is defined by the HDA_SIZE constant,
 *  which is declared in ocidem.h to be 256 bytes for 32-
 *  bit architectures and 512 bytes for 64-bit architectures.
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

/* oparse flags */
#define  DEFER_PARSE        1
#define  NATIVE             1
#define  VERSION_7          2


text *username = (text *) "SCOTT";
text *password = (text *) "tiger";

/* Define SQL statements to be used in program. */
text *insert = (text *) "INSERT INTO emp(empno, ename, job, sal, deptno)\
    VALUES (:empno, :ename, :job, :sal, :deptno)";
text *seldept = (text *) "SELECT dname FROM dept WHERE deptno = :1";
text *maxemp = (text *) "SELECT NVL(MAX(empno), 0) FROM emp";
text *selemp = (text *) "SELECT ename, job FROM emp";

/* Define an LDA, a HDA,  and two cursors. */
Lda_Def lda;
size_t  hda[HDA_SIZE/sizeof(size_t)];
Cda_Def cda1;
Cda_Def cda2;


void err_report();
void do_exit();
void myfflush();



main()
{
    sb4   empno, sal, deptno;
    sb4   len, len2, rv, dsize, dsize2;
    sb4   enamelen, joblen, deptlen;
    sb2   sal_ind, job_ind;
    sb2   db_type, db2_type;
    sb1   name_buf[20], name2_buf[20];
    text  *cp, *ename, *job, *dept;

/*
 *  Connect to ORACLE and open two cursors.
 *  Exit on any error.
 */
    if (olog(&lda, (ub1 *)hda, username, -1, password, -1,
             (text *) 0, -1, (ub4)OCI_LM_DEF))
    {
        err_report(&lda);
        exit(EXIT_FAILURE);
    }
    printf("Connected to ORACLE as %s\n", username);

    if (oopen(&cda1, &lda, (text *) 0, -1, -1, (text *) 0, -1))
    {
        err_report(&cda1);
        do_exit(EXIT_FAILURE);
    }
    if (oopen(&cda2, &lda, (text *) 0, -1, -1, (text *) 0, -1))
    {
        err_report(&cda2);
        do_exit(EXIT_FAILURE);
    }

    /* Turn off auto-commit. Default is off, however. */
    if (ocof(&lda))
    {
        err_report(&lda);
        do_exit(EXIT_FAILURE);
    }

    /* Retrieve the current maximum employee number. */
    if (oparse(&cda1, maxemp, (sb4) -1, DEFER_PARSE,
               (ub4) VERSION_7))
    {
        err_report(&cda1);
        do_exit(EXIT_FAILURE);
    }
    if (odefin(&cda1, 1, (ub1 *) &empno, (sword) sizeof(sword),
               (sword) INT_TYPE,
               (sword) -1, (sb2 *) 0, (text *) 0, -1, -1,
               (ub2 *) 0, (ub2 *) 0))
    {
        err_report(&cda1);
        do_exit(EXIT_FAILURE);
    }
    if (oexfet(&cda1, (ub4) 1, FALSE, FALSE))
    {
        if (cda1.rc == NO_DATA_FOUND)
            empno = 10;
        else
        {
            err_report(&cda1);
            do_exit(EXIT_FAILURE);
        }
    }

    /*  Describe the columns in the select-list
        of "selemp" to determine the max length of
        the employee name and job title.
     */
    if (oparse(&cda1, selemp, (sb4) -1, FALSE, (ub4)VERSION_7))
    {
        err_report(&cda1);
        do_exit(EXIT_FAILURE);
    }

    len = sizeof(name_buf); len2 = sizeof (name2_buf);
    if (odescr(&cda1, 1, &enamelen,
               (sb2 *) &db_type, name_buf, (sb4 *) &len,
               (sb4 *) &dsize, (sb2 *) 0, (sb2 *) 0, (sb2 *) 0) ||
        odescr(&cda1, 2, &joblen,
               (sb2 *) &db_type, name2_buf, (sb4 *) &len2,
               (sb4 *) &dsize2, (sb2 *) 0, (sb2 *) 0, (sb2 *) 0))
    {
        err_report(&cda1);
        do_exit(EXIT_FAILURE);
    }

    /* Parse the INSERT statement. */
    if (oparse(&cda1, insert, (sb4) -1, FALSE, (ub4) VERSION_7))
    {
        err_report(&cda1);
        do_exit(EXIT_FAILURE);
    }

    /* Parse the SELDEPT statement. */
    if (oparse(&cda2, seldept, (sb4) -1, FALSE, (ub4) VERSION_7))
    {
        err_report(&cda2);
        do_exit(EXIT_FAILURE);
    }

    /*  Allocate output buffers. Allow for \n and '\0'. */
    ename = (text *) malloc((int) enamelen + 2);
    job   = (text *) malloc((int) joblen + 2);

    /*  Bind the placeholders in the INSERT statement. */
    if (obndrv(&cda1, (text *) ":ENAME", -1, (ub1 *) ename,
               enamelen+1, STRING_TYPE, -1, (sb2 *) 0,
               (text *) 0, -1, -1) ||
        obndrv(&cda1, (text *) ":JOB", -1, (ub1 *) job, joblen+1,
               STRING_TYPE, -1, &job_ind, (text *) 0, -1, -1) ||
        obndrv(&cda1, (text *) ":SAL", -1, (ub1 *) &sal, (sword) sizeof (sal),
               INT_TYPE, -1, &sal_ind, (text *) 0, -1, -1) ||
        obndrv(&cda1, (text *) ":DEPTNO",-1, (ub1 *) &deptno,
               (sword) sizeof (deptno), INT_TYPE, -1,
               (sb2 *) 0, (text *) 0, -1, -1) ||
        obndrv(&cda1, (text *) ":EMPNO", -1, (ub1 *) &empno,
               (sword) sizeof (empno), INT_TYPE, -1,
               (sb2 *) 0, (text *) 0, -1, -1))
    {
        err_report(&cda1);
        do_exit(EXIT_FAILURE);
    }

    /*  Bind the placeholder in the "seldept" statement. */
    if (obndrn(&cda2,
               1,
               (ub1 *) &deptno,
               (sword) sizeof(deptno),
               INT_TYPE,
               -1,
               (sb2 *) 0,
               (text *) 0,
               -1,
               -1))
    {
        err_report(&cda2);
        do_exit(EXIT_FAILURE);
    }

    /*  Describe the select-list field "dname". */
    len = sizeof (name_buf);
    if (odescr(&cda2, 1, (sb4 *) &deptlen, &db_type,
               name_buf, (sb4 *) &len, (sb4 *) &dsize, (sb2 *) 0,
               (sb2 *) 0, (sb2 *) 0))
    {
        err_report(&cda2);
        do_exit(EXIT_FAILURE);
    }

/*  Allocate the dept buffer now that you have length. */
    dept = (text *) malloc((int) deptlen + 1);

    /*  Define the output variable for the select-list. */
    if (odefin(&cda2,
               1,
               (ub1 *) dept,
               deptlen+1,
               STRING_TYPE,
               -1,
               (sb2 *) 0,
               (text *) 0,
               -1,
               -1,
               (ub2 *) 0,
               (ub2 *) 0))
    {
        err_report(&cda2);
        do_exit(EXIT_FAILURE);
    }

    for (;;)
    {
        /* Prompt for employee name.  Break on no name. */
        printf("\nEnter employee name (or CR to EXIT): ");
        fgets((char *) ename, (int) enamelen+1, stdin);
        cp = (text *) strchr((char *) ename, '\n');
        if (cp == ename)
        {
            printf("Exiting... ");
            do_exit(EXIT_SUCCESS);
        }
        if (cp)
            *cp = '\0';
        else
          {
            printf("Employee name may be truncated.\n");
            myfflush();
          }
        /*  Prompt for the employee's job and salary. */
        printf("Enter employee job: ");
        job_ind = 0;
        fgets((char *) job, (int) joblen + 1, stdin);
        cp = (text *) strchr((char *) job, '\n');
        if (cp == job)
        {
            job_ind = -1;            /* make it NULL in table */
            printf("Job is NULL.\n");/* using indicator variable */
        }
        else if (cp == 0)
          {
            printf("Job description may be truncated.\n");
            myfflush();
          }
        else
            *cp = '\0';

        printf("Enter employee salary: ");
        scanf("%d", &sal);
        myfflush();
        sal_ind = (sal <= 0) ? -2 : 0;  /* set indicator variable */

        /*
         *  Prompt for the employee's department number, and verify
         *  that the entered department number is valid
         *  by executing and fetching.
         */
        do
        {
            printf("Enter employee dept: ");
            scanf("%d", &deptno);
            myfflush();
            if (oexec(&cda2) ||
                    (ofetch(&cda2) && (cda2.rc != NO_DATA_FOUND)))
            {
                err_report(&cda2);
                do_exit(EXIT_FAILURE);
            }
            if (cda2.rc == NO_DATA_FOUND)
                printf("The dept you entered doesn't exist.\n");
        } while (cda2.rc == NO_DATA_FOUND);

        /*
         *  Increment empno by 10, and execute the INSERT
         *  statement. If the return code is 1 (duplicate
         *  value in index), then generate the next
         *  employee number.
         */
        empno += 10;
        if (oexec(&cda1) && cda1.rc != 1)
        {
            err_report(&cda1);
            do_exit(EXIT_FAILURE);
        }
        while (cda1.rc == 1)
        {
            empno += 10;
            if (oexec(&cda1) && cda1.rc != 1)
            {
                err_report(&cda1);
                do_exit(EXIT_FAILURE);
            }
        }  /* end for (;;) */

/* Commit the change. */
        if (ocom(&lda))
        {
            err_report(&lda);
            do_exit(EXIT_FAILURE);
        }
        printf(
        "\n\n%s added to the %s department as employee number %d\n",
                 ename, dept, empno);
    }
    do_exit(EXIT_SUCCESS);

}


void
err_report(cursor)
    Cda_Def *cursor;
{
    sword n;
    text msg[512];

    printf("\n-- ORACLE error--\n");
    printf("\n");
    n = oerhms(&lda, cursor->rc, msg, (sword) sizeof msg);
    fprintf(stderr, "%s\n", msg);
    if (cursor->fc > 0)
        fprintf(stderr, "Processing OCI function %s",
            oci_func_tab[cursor->fc]);
}


/*
 *  Exit program with an exit code.
 */
void do_exit(exit_code)
    sword exit_code;
{
    sword error = 0;

    if (oclose(&cda1))
    {
        fprintf(stderr, "Error closing cursor 1.\n");
        error++;
    }
    if (oclose(&cda2))
    {
        fprintf(stderr, "Error closing cursor 2.\n");
        error++;
    }
    if (ologof(&lda))
    {
        fprintf(stderr, "Error on disconnect.\n");
        error++;
    }
    if (error == 0 && exit_code == EXIT_SUCCESS)
        printf ("\nG'day\n");

    exit(exit_code);
}


void myfflush()
{
  eb1 buf[50];

  fgets((char *) buf, 50, stdin);
}


