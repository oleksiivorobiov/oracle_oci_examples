#ifdef RCSID
static char *RCSid =
   "$Header: cdemo5.c 11-oct-2006.17:36:36 azhao Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1995, 2006, Oracle. All rights reserved.  
*/

/*
   NAME
     cdemo5.c - a small example of use of variable cursors
   MODIFIED   (MM/DD/YY)
    azhao      10/11/06  - case-senstive password change
    lzhao      04/04/05  - bug4184176
    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
    ehayes     05/29/97 -  Fix olint errors
    azhao      05/16/97 -  change ub1 to ub4 for hstb
    dchatter   07/15/96 -  hda is a ub4 array to prevent bus error
    plocke     11/18/95 -  to update for v7.3
    slari      04/25/95 -  merge changes from branch 1.1.720.1
    slari      04/20/95 -  Branch_for_patch
    slari      04/19/95 -  replace orlon with olog
    pvasterd   10/20/94 -  Changes to bring into line with other cdemos
    dchatter   10/17/94 -  Creation
 */
/*
 *     -- cdemo5.c --
 *
 *  An example program which demonstrates the use of
 *  Variable Cursors in an OCI program.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <oratypes.h>
#include <ocidfn.h>
#include <ocikpr.h>
#include <string.h>

 /* oparse flags */
#define DEFER_PARSE 1
#define VERSION_7 2

#define NPOS 16
#define DSCLEN 240

text *username = (text *) "SCOTT";
text *password = (text *) "tiger";

static  sword retval;
static  ub4   hstb[512];
static  text  errorb[4095];
static  text  cbuf[NPOS][DSCLEN];
static  sb4   cbufl[NPOS];
static  sb4   dbsize[NPOS];
static  sb4   dsize[NPOS];
static  sb2   dbtype[NPOS];
static  sb2   prec[NPOS];
static  sb2   scale[NPOS];
static  sb2   nullok[NPOS];

static Lda_Def lda1;

static text plsql_block[] =
      "begin \
          OPEN :cursor1 FOR select empno, ename, job, mgr, hiredate,sal,deptno\
               from emp where job=:job order by empno;\
      end;";

/* CLIENT CURSORS */
static Cda_Def cursor, cursor_emp;

/* Prototype */
void oracle_error();

int main ()
{
  Lda_Def *ldap = &lda1;

  ub4    empno;
  text   ename[11];
  text   job[10];
  ub4    mgr;
  text   hidate[10];
  ub4    sal;
  ub4    deptno;
  int    i;
  text   job_kind[50];
  ub4    pos;

  strcpy((char *) job_kind, (char *)"ANALYST");
  fprintf(stdout,"\n\nFETCHING for job=%s\n\n",job_kind);

  /*
   * Connect to Oracle as SCOTT/TIGER.
   * Exit on any error.
   */

  if (olog(ldap, (ub1 *)hstb, username, -1, password, -1,
           (text *) 0, -1, (ub4)OCI_LM_DEF))
  {
      printf("Unable to connect as %s\n", username);
      exit(EXIT_FAILURE);
  }
  printf("Connected to Oracle as %s\n\n", username);

  /*
   * Open a cursor for executing the PL/SQL block.
   */

  if (oopen(&cursor, ldap, (text *) 0, -1, 0, (text *) 0, -1))
  {
     oracle_error(&cursor);
     exit(EXIT_FAILURE);
  }

  /*
   * Parse the PL/SQL block.
   */

  if (oparse(&cursor, plsql_block, (sb4) -1, (sword) TRUE, (ub4) 2))
  {
     oracle_error(&cursor);
     exit(EXIT_FAILURE);
  }

  /*
   * Bind a variable of cursor datatype, the cursor will be opened
   * inside the PL/SQL block.
   */

  if (obndra(&cursor, (text *) ":cursor1", -1, (ub1 *) &cursor_emp,
             -1, SQLT_CUR, -1, (sb2 *) 0, (ub2 *) 0, (ub2 *) 0,
             (ub4) 0, (ub4 *) 0, (text *) 0, 0, 0))
  {
     oracle_error(&cursor);
     exit(EXIT_FAILURE);
  }

  /*
   * Bind a variable of string datatype.
   */

  if (obndra(&cursor, (text *) ":job", -1, (ub1 *) job_kind,
             -1, SQLT_STR, -1, (sb2 *) 0, (ub2 *) 0, (ub2 *) 0,
             (ub4) 0, (ub4 *) 0, (text *) 0, 0, 0))
  {
     oracle_error(&cursor);
     exit(EXIT_FAILURE);
  }

  /*
   * Execute the PL/SQL block.
   */

   if (oexec(&cursor))
   {
      oracle_error(&cursor);
      exit(EXIT_FAILURE);
   }

  /*
   * Close the cursor on which the PL/SQL block executed.
   */

  if (oclose(&cursor))
  {
     oracle_error(&cursor);
     exit(EXIT_FAILURE);
  }

  /*
   * Do describe on cursor initialized and returned from the PL/SQL block.
   */

  for (pos = 0; pos < NPOS; pos++)
  {
     cbufl[pos] = DSCLEN;
     if (odescr(&cursor_emp, (sword) (pos+1), &dbsize[pos], &dbtype[pos],
                (sb1 *) cbuf[pos], &cbufl[pos], &dsize[pos],
                &prec[pos], &scale[pos], &nullok[pos]))
     {
        if (cursor_emp.rc == 1007)
           break;
        oracle_error(&cursor_emp);
        exit(EXIT_FAILURE);
     }
  }

  printf("Describe select-list returns:\n\n");
  printf("----------------------------------------\n");
  printf("Item\t\tMaxSize\t\tType\n");
  printf("----------------------------------------\n");
  for (i = 0; i < pos; i++)
  {
     cbuf[i][cbufl[i]] = '\0';
     printf("%s\t\t%d\t\t%d\n", cbuf[i], dbsize[i], dbtype[i]);
  }

  /*
   * Do client defines.
   */

  if (odefin(&cursor_emp, 1, (ub1 *) &empno, (sword) sizeof(ub4), SQLT_INT,
             -1, (sb2 *) -1, (text *) 0, (sword) 0, (sword) 0, (ub2 *) 0,
             (ub2 *) 0))
  {
     oracle_error(&cursor_emp);
     exit(EXIT_FAILURE);
  }

   if (odefin(&cursor_emp, 2, (ub1 *) ename, (sword) sizeof(ename),
              SQLT_STR, -1, (sb2 *) -1, (text *)0, (sword) 0, (sword) 0,
              (ub2 *) 0, (ub2 *) 0))
   {
      oracle_error(&cursor_emp);
      exit(EXIT_FAILURE);
   }

  if (odefin(&cursor_emp, 3, (ub1 *) job, (sword) sizeof(job),
             SQLT_STR, -1, (sb2 *) -1, (text *) 0, (sword) 0, (sword) 0,
             (ub2 *) 0, (ub2 *) 0))
   {
      oracle_error(&cursor_emp);
      exit(EXIT_FAILURE);
   }

  if (odefin(&cursor_emp, 4, (ub1 *)&mgr, (sword) sizeof(ub4), SQLT_INT,
             -1, (sb2 *) -1, (text *)0, (sword) 0, (sword) 0, (ub2 *) 0,
             (ub2 *) 0))
   {
      oracle_error(&cursor_emp);
      exit(EXIT_FAILURE);
   }

  if (odefin(&cursor_emp, 5, (ub1 *)hidate, (sword) sizeof(hidate),
             SQLT_STR, -1, (sb2 *) -1, (text *) 0, (sword) 0, (sword) 0,
             (ub2 *) 0, (ub2 *) 0))
   {
      oracle_error(&cursor_emp);
      exit(EXIT_FAILURE);
   }

   if (odefin(&cursor_emp, 6, (ub1 *) &sal, (sword) sizeof(ub4), SQLT_INT,
              -1, (sb2 *) -1, (text *) 0, (sword) 0, (sword) 0, (ub2 *) 0,
              (ub2 *) 0))
   {
      oracle_error(&cursor_emp);
      exit(EXIT_FAILURE);
   }

   if (odefin(&cursor_emp, 7, (ub1 *) &deptno, (sword) sizeof(deptno),
              SQLT_INT, -1, (sb2 *) -1, (text *) 0, (sword) 0, (sword) 0,
              (ub2 *) 0, (ub2 *) 0))
   {
      oracle_error(&cursor_emp);
      exit(EXIT_FAILURE);
   }

  printf("\nFETCH from variable cursor:\n\n");
  printf("------------------------------------------------------------\n");
  printf("empno\tename\tjob\tmgr\thiredate\tsalary\tdept\n");
  printf("------------------------------------------------------------\n");

  /*
   * Now fetch the result set and display.
   */

  while (1)
  {
    sb4 err = 0;

    if (err = ofetch(&cursor_emp))
    {
      if (cursor_emp.rc == 1403)
         break;
      else
      {
         oracle_error(&cursor_emp);
         exit(EXIT_FAILURE);
      }
    }
    else
    {
      /* A row was returned; have to do the fetch. */
      fprintf(stdout, "%d\t%s\t%s\t%d\t%s\t%d\t%d\n\n",
              empno, ename, job, mgr, hidate, sal, deptno);
    }
  }

  /*
   * Log off.
   */

  if (ologof(ldap))
  {
     oracle_error(&cursor_emp);
     exit(EXIT_FAILURE);
  }
} /* end of main */

void oracle_error(lda)
Lda_Def * lda;
{
  char msgbuf[512];
  int n=oerhms(lda, lda->rc, (text *)msgbuf, (int) sizeof(msgbuf) );

  printf("\n\n%.*s\n",n,msgbuf);
}


