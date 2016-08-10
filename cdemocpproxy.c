/* Copyright (c) 2001, 2006, Oracle. All rights reserved.  */
/*

   NAME
     cdemocpproxy.c - OCI Connection Pooling, proxy functionality

   DESCRIPTION

     This program invokes multiple threads to insert MAXTHREAD records 
     into EMP table and displays all the inserted records. 
     
     For proxy login we should alter the user permission as the following
     ALTER USER scott GRANT CONNECT THROUGH appuser;
     
     This demo needs the cdemocpproxy.sql file for creating Pool user. 
     Before running this demo the cdemocpproxy.sql should be executed.

   MODIFIED   (MM/DD/YY)
   msowdaga    04/30/08 - Fix bug 6236196, multiple threads should not share
                          single error handle
   azhao       10/10/06 - case-senstive password change
   jchai       01/28/02 - Merged jchai_change_oci_sp_sc_cp_names
   kmohan      04/23/01 - Merged kmohan_cpdemo
   mpurayat    04/20/01 - Creation

*/

#include <oci.h> 
#include <stdio.h> 
#include <stdlib.h> 
#include <string.h> 
/* Maximum Number of threads  */ 
#define MAXTHREAD 10 
 
static OCIError   *errhp;
static OCIEnv     *envhp;
static OCICPool   *poolhp;

static int employeeNum[MAXTHREAD];

static OraText *poolName;
static sb4 poolNameLen;
static CONST OraText *database = (OraText *)"";
static CONST OraText *username =(OraText *)"SCOTT";
static CONST OraText *password =(OraText *)""; /* no password needed for proxy*/
static CONST OraText *appusername =(OraText *)"APPUSER"; 
static CONST OraText *apppassword =(OraText *)"apppassword";

/*  Values to be inserted into EMP table */
static char ename[MAXTHREAD][10]={"JOHN","JIMMY","JET","JIM","JEFREE","JAMES",
    "JAMMEE","JOSEP","JOHNY","JEGAN"};
static char ejob[MAXTHREAD][9]={"MANAGER","TYPIST","ASST MGR","CLERK","MANAGER",
   "ASST MGR","CLERK","CLERK","TYPIST","TYPIST"};
static float esal[MAXTHREAD]={10000.00
                             ,5000.00
                             ,8000.00
                             ,6000.00
                             ,10000.00
                             ,8000.00
                             ,6000.00
                             ,6000.00
                             ,5000.00
                             ,5000.00};
static unsigned int edept[MAXTHREAD] = {10,20,10,20,30,10,30,20,10,20};

/* Max,Min, and increment connections */
static ub4 conMin = 1;
static ub4 conMax = 3;
static ub4 conIncr = 1;
 
static void checkerr (OCIError *errhp, sword status);
static void threadFunction (dvoid *arg);
static void queryRows();
static void deleteRows();
 
int main(argc, argv)
int argc;
char *argv[];
{
    int i = 0;
    sword lstat;
    OCIEnvCreate (&envhp, OCI_THREADED, (dvoid *)0,  NULL,
      NULL, NULL, 0, (dvoid *)0);
 
    (void) OCIHandleAlloc((dvoid *) envhp, (dvoid **) &errhp, OCI_HTYPE_ERROR,
                     (size_t) 0, (dvoid **) 0);
 
    (void) OCIHandleAlloc((dvoid *) envhp, (dvoid **) &poolhp, OCI_HTYPE_CPOOL,
                          (size_t) 0, (dvoid **) 0);
 
    /* CREATE THE CONNECTION POOL */
    if (lstat = OCIConnectionPoolCreate(envhp,
                     errhp,poolhp, &poolName, &poolNameLen,
                     database,(sb4)strlen((const signed char *)database),
                     conMin, conMax, conIncr,
                     appusername,(sb4)strlen((const signed char *)appusername),
                     apppassword,(sb4)strlen((const signed char *)apppassword)
                     ,OCI_DEFAULT))
    {
      checkerr(errhp,lstat);
      exit(1);
    }
    /*  Delete Inserted rows by this demo previously */ 
    deleteRows();
    /* Multiple threads using the connection pool */
    {
      OCIThreadId     *thrid[MAXTHREAD];
      OCIThreadHandle *thrhp[MAXTHREAD];
 
      OCIThreadProcessInit ();
      checkerr (errhp, OCIThreadInit (envhp, errhp));
      for (i = 0; i < MAXTHREAD; ++i)
      {
        checkerr (errhp, OCIThreadIdInit (envhp, errhp, &thrid[i]));
        checkerr (errhp, OCIThreadHndInit (envhp, errhp, &thrhp[i]));
      }
      printf("Multiple threads inserting records into EMP table\n");
      for (i = 0; i < MAXTHREAD; ++i)
      {
        employeeNum[i]=i;
        /* Inserting into EMP table */
        checkerr (errhp, OCIThreadCreate (envhp, errhp, threadFunction,
            (dvoid *) &employeeNum[i], thrid[i], thrhp[i]));
      }
      for (i = 0; i < MAXTHREAD; ++i)
      {
        checkerr (errhp, OCIThreadJoin (envhp, errhp, thrhp[i]));
        checkerr (errhp, OCIThreadClose (envhp, errhp, thrhp[i]));
        checkerr (errhp, OCIThreadIdDestroy (envhp, errhp, &(thrid[i])));
        checkerr (errhp, OCIThreadHndDestroy (envhp, errhp, &(thrhp[i])));
      }
      checkerr (errhp, OCIThreadTerm (envhp, errhp));
    } /* ALL THE THREADS ARE COMPLETE */
    /* This function will select and display all the rows from EMP table */ 
    printf("Displaying the Inserted records\n");
    queryRows();
    checkerr(errhp, OCIConnectionPoolDestroy(poolhp, errhp, OCI_DEFAULT));
    checkerr(errhp, OCIHandleFree((dvoid *)poolhp, OCI_HTYPE_CPOOL));
    checkerr(errhp, OCIHandleFree((dvoid *)errhp, OCI_HTYPE_ERROR));
    return 0;
} /* end of main () */

/* Inserts records into EMP table */ 
static void threadFunction (dvoid *arg)
{
    int empno = *(int *)arg;
    OCISvcCtx *svchp = (OCISvcCtx *) 0;
    text insertst1[256];
    OCIStmt *stmthp = (OCIStmt *)0;
    OCIError  *errhp2 = (OCIError *) 0;
    sword lstat;
 
    (void) OCIHandleAlloc((dvoid *) envhp, (dvoid **) &errhp2, OCI_HTYPE_ERROR,
                     (size_t) 0, (dvoid **) 0);

    if (lstat = OCILogon2(envhp, errhp2, &svchp,
      (CONST OraText *)username, (ub4)strlen((const signed char *)username),
      (CONST OraText *)password, (ub4)strlen((const signed char *)password),
      (CONST OraText *)poolName, (ub4)poolNameLen,
                          OCI_CPOOL))
    {
      checkerr(errhp2,lstat);
      exit(1);
    }
    (void) sprintf(insertst1,"INSERT INTO emp(empno, ename, job, sal, deptno) \
         values (%d,'%s','%s',%7.2f,%d)",empno+1,ename[empno],ejob[empno],
         esal[empno],edept[empno]);
    
    printf("Inserting %s\n",ename[empno]);
 
    OCIHandleAlloc(envhp, (dvoid **)&stmthp, OCI_HTYPE_STMT, (size_t)0,
                   (dvoid **)0);
 
    checkerr(errhp2, OCIStmtPrepare (stmthp, errhp2, (CONST OraText *)insertst1,
             (ub4)strlen((const signed char *)insertst1), OCI_NTV_SYNTAX, 
             OCI_DEFAULT));
 
    checkerr(errhp2, OCIStmtExecute (svchp, stmthp, errhp2, (ub4)1, (ub4)0,
             (OCISnapshot *)0, (OCISnapshot *)0, OCI_DEFAULT ));
 
    checkerr(errhp2, OCITransCommit(svchp,errhp2,(ub4)0));
 
    checkerr(errhp2, OCIHandleFree((dvoid *) stmthp, OCI_HTYPE_STMT));
    checkerr(errhp2, OCILogoff((dvoid *) svchp, errhp2));
    OCIHandleFree((dvoid *)errhp2, OCI_HTYPE_ERROR);
} /* end of threadFunction (dvoid *) */
/* Delete rows which are inserted in the prvious invocation of this demo*/
static void deleteRows()
{
    OCISvcCtx *svchp = (OCISvcCtx *) 0;
    sword status = 0;
    text deletest1[256];
    sword lstat;
    OCIStmt *stmthp = (OCIStmt *)0;
    if (lstat = OCILogon2(envhp, errhp, &svchp,
       (CONST OraText *)username, (ub4)strlen((const signed char *)username),
       (CONST OraText *)password, (ub4)strlen((const signed char *)password),
       (CONST OraText *)poolName, (ub4)poolNameLen,
                          OCI_CPOOL))
    {
      checkerr(errhp,lstat);
      exit(1);
    }
    (void) sprintf(deletest1,
     "DELETE FROM emp WHERE empno BETWEEN 1 AND %d",MAXTHREAD); 
    OCIHandleAlloc(envhp, (dvoid **)&stmthp, OCI_HTYPE_STMT, (size_t)0,
                   (dvoid **)0);

    checkerr(errhp, OCIStmtPrepare (stmthp, errhp, (text *)deletest1,
             (ub4)strlen((const signed char *)deletest1), OCI_NTV_SYNTAX, 
             OCI_DEFAULT));
    checkerr(errhp, OCIStmtExecute (svchp, stmthp, errhp, (ub4)1, (ub4)0,
             (OCISnapshot *)0, (OCISnapshot *)0, OCI_DEFAULT ));
    checkerr(errhp, OCITransCommit(svchp,errhp,(ub4)0));

    checkerr(errhp, OCIHandleFree((dvoid *) stmthp, OCI_HTYPE_STMT));
    checkerr(errhp, OCILogoff((dvoid *) svchp, errhp));
} /* End of deleteRows */

/* Displays the contents of EMP table */
static void queryRows() 
{
    OCISvcCtx *svchp = (OCISvcCtx *) 0;
    sword status = 0;
    text selectst1[256];
    OCIStmt *stmthp = (OCIStmt *)0;
    sword lstat;
    /* Define all output variables */
    OCIDefine *defhp1 = (OCIDefine *)0;
    OCIDefine *defhp2 = (OCIDefine *)0;
    OCIDefine *defhp3 = (OCIDefine *)0;
    OCIDefine *defhp4 = (OCIDefine *)0;
    OCIDefine *defhp5 = (OCIDefine *)0;
    ub4 emp_no,emp_dept;
    text emp_name[10],emp_job[9];
    int i;
    float  emp_sal;
    /* Logon in Connection Pool mode */
    if (lstat = OCILogon2(envhp, errhp, &svchp,
      (CONST OraText *)username, (ub4)strlen((const signed char *)username),
      (CONST OraText *)password, (ub4)strlen((const signed char *)password),
      (CONST OraText *)poolName, (ub4)poolNameLen,
      OCI_CPOOL))
    {
      checkerr(errhp,lstat);
      exit(1);
    }

    (void) sprintf(selectst1,"SELECT empno, ename, job, sal, deptno FROM emp\
       WHERE empno between 1 and %d",MAXTHREAD);

    OCIHandleAlloc(envhp, (dvoid **)&stmthp, OCI_HTYPE_STMT, (size_t)0,
                   (dvoid **)0);

    checkerr(errhp, OCIStmtPrepare (stmthp, errhp, (text *)selectst1,
            (ub4)strlen((const signed char *)selectst1), OCI_NTV_SYNTAX, 
            OCI_DEFAULT));
    checkerr(errhp,OCIDefineByPos (stmthp, &defhp1, errhp, (ub4)1,
            (dvoid *)&emp_no, (sb4)sizeof(ub4), (ub2)SQLT_INT, (dvoid *)0,
            (ub2 *)0, (ub2 *)0, OCI_DEFAULT));
    checkerr(errhp,OCIDefineByPos (stmthp, &defhp2, errhp, (ub4)2,
            (dvoid *)&emp_name, (sb4)10, (ub2)SQLT_STR, 
            (dvoid *)0, (ub2 *)0, (ub2 *)0, OCI_DEFAULT));
    checkerr(errhp,OCIDefineByPos (stmthp, &defhp3, errhp, (ub4)3,
            (dvoid *)&emp_job, (sb4)9, (ub2)SQLT_STR, 
            (dvoid *)0, (ub2 *)0, (ub2 *)0, OCI_DEFAULT));
    checkerr(errhp,OCIDefineByPos (stmthp, &defhp4, errhp, (ub4)4,
            (dvoid *)&emp_sal,(sb4)sizeof(float),(ub2)SQLT_FLT,(dvoid *)0,
            (ub2 *)0, (ub2 *)0, OCI_DEFAULT));
    checkerr(errhp,OCIDefineByPos (stmthp, &defhp5, errhp, (ub4)5,
            (dvoid *)&emp_dept,(sb4)sizeof(ub4),(ub2)SQLT_INT,(dvoid *)0,
            (ub2 *)0, (ub2 *)0, OCI_DEFAULT));

    /* Execute the Query and Fetch MAXTHREAD records */
    if (lstat = OCIStmtExecute (svchp, stmthp, errhp, (ub4)0, 
            (ub4)0, (OCISnapshot *)0, (OCISnapshot *)0, OCI_DEFAULT ))
    {
      checkerr(errhp,lstat);
      exit(1);
    }
    /* Printing the values */
    status = OCIStmtFetch(stmthp,errhp,1,OCI_FETCH_NEXT,OCI_DEFAULT);
    while (status != OCI_NO_DATA)
    {
       printf("Emp No : %u Name : %s Job : %s Salary : %7.2f Dept No : %u\n",
            emp_no,emp_name,emp_job,emp_sal,emp_dept); 
       status = OCIStmtFetch(stmthp,errhp,1,OCI_FETCH_NEXT,OCI_DEFAULT);
    }

    checkerr(errhp, OCIHandleFree((dvoid *) stmthp, OCI_HTYPE_STMT));
    checkerr(errhp, OCILogoff((dvoid *) svchp, errhp));
} /* end of queryRows() */
 
/* This function prints the error */
void checkerr(errhp, status)
OCIError *errhp;
sword status;
{
    text errbuf[512];
    sb4 errcode = 0;
 
    switch (status)
    {
    case OCI_SUCCESS:
      break;
    case OCI_SUCCESS_WITH_INFO:
      (void) printf("Error - OCI_SUCCESS_WITH_INFO\n");
      break;
    case OCI_NEED_DATA:
      (void) printf("Error - OCI_NEED_DATA\n");
      break;
    case OCI_NO_DATA:
      (void) printf("Error - OCI_NODATA\n");
      break;
    case OCI_ERROR:
      (void) OCIErrorGet((dvoid *)errhp, (ub4) 1, (text *) NULL, &errcode,
                          errbuf, (ub4) sizeof(errbuf), OCI_HTYPE_ERROR);
      (void) printf("Error - %.*s\n", 512, errbuf);
      break;
    case OCI_INVALID_HANDLE:
      (void) printf("Error - OCI_INVALID_HANDLE\n");
      break;
    case OCI_STILL_EXECUTING:
      (void) printf("Error - OCI_STILL_EXECUTE\n");
      break;
    case OCI_CONTINUE:
      (void) printf("Error - OCI_CONTINUE\n");
      break;
    default:
      break;
    }
}
