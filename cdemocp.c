/* Copyright (c) 2001, 2006, Oracle. All rights reserved.  */
/*

   NAME
     cdemocp.c - Basic OCI Connection Pooling functionality

   DESCRIPTION

     This demo invokes multiple threads to insert MAXTHREAD records 
     into hr.employees table and displays all the inserted records. 

     Before running the demo, Please..
     - run rdbms/demo/cdemocp.sql to create pool users.
     - make sure that the common schemas are installed. This can
       be verified by connecting to hr/hr. If not installed, 
       please refer to the common schema documentation. 

     DMLs on the following columns of hr.employees is done.
     If the table structure changes in releases beyond 10iR1, the 
     demo *may* need to be changed accordingly.

     Name                                      Null?    Type
     ----------------------------------------- -------- ------------
     EMPLOYEE_ID                               NOT NULL NUMBER(6)
     LAST_NAME                                 NOT NULL VARCHAR2(25)
     SALARY                                             NUMBER(8,2)
     DEPARTMENT_ID                             NOT NULL VARCHAR2(10)
     JOB_ID                                    NOT NULL VARCHAR2(10)
     EMAIL                                     NOT NULL VARCHAR2(25)
     HIRE_DATE                                 NOT NULL DATE

   MODIFIED   (MM/DD/YY)
   msowdaga    04/30/08 - Fix bug 6236196, multiple threads should not share
                          single error handle
   azhao       10/10/06 - case-senstive password change
   arrajara    08/07/02 - Move to common schema (scott->hr) and cleanup
   jchai       01/28/02 - Merged jchai_change_oci_sp_sc_cp_names
   kmohan      04/23/01 - Merged kmohan_cpdemo
   mpurayat    04/20/01 - Creation

*/

#include <oci.h>
#include <stdio.h> 
#include <stdlib.h>
#include <string.h> 


/* Maximum number of threads  */ 
#define MAXTHREAD 10 
/* Number of columns in select list */
#define MAXBINDS 7
 
static OCIError   *errhp;
static OCIEnv     *envhp;
static OCICPool   *poolhp;
static OraText    *poolName;
static sb4         poolNameLen;

static int employeeNum[MAXTHREAD];

static CONST OraText *database = (OraText *)"";
static CONST OraText *username = (OraText *)"HR";
static CONST OraText *password = (OraText *)"hr";
static CONST OraText *appusername = (OraText *)"APPUSER"; 
static CONST OraText *apppassword = (OraText *)"apppassword";

/*  Values to be inserted into employees table */
static char ename[MAXTHREAD][26]=
    {"LASTNAME1","LASTNAME2","LASTNAME3","LASTNAME4","LASTNAME5",
     "LASTNAME6","LASTNAME7","LASTNAME8","LASTNAME9","LASTNAME10"};

static char ejob[MAXTHREAD][11]=
          {"AD_PRES", "AD_VP", "AD_ASST", "FI_MGR", "FI_ACCOUNT",
           "AC_MGR", "AC_ACCOUNT", "SA_MAN", "SA_REP", "PU_MAN"};

static float esal[MAXTHREAD]={1000.00, 2000.00, 3000.00, 4000.00, 5000.00,
                              6000.00, 7000.00, 8000.00, 9000.00, 10000.00};
static unsigned int edept[MAXTHREAD] = {10,20,10,20,30,10,30,20,10,20};

static char email[MAXTHREAD][25]= 
   {"name1@oracle.com", "name2@oracle.com", "name3@oracle.com",
    "name4@oracle.com", "name5@oracle.com", "name6@oracle.com",
    "name7@oracle.com", "name8@oracle.com", "name9@oracle.com",
    "name10@oracle.com"};

static char hdate[MAXTHREAD][10]= 
   {"21-DEC-01", "22-DEC-01", "23-DEC-01", "24-DEC-01", "25-DEC-01",
    "26-DEC-01", "27-DEC-01", "28-DEC-01", "29-DEC-01", "30-DEC-01"};

/* Max, min and increment connections */
static ub4 conMin = 1;
static ub4 conMax = 3;
static ub4 conIncr = 1;
 
/* Local functions */
static void checkerr (OCIError *errhp, sword status);
static void threadFunction (dvoid *arg);
static void queryRows();
static void deleteRows();
int main();
 
/*- main -------------------------------------------------------------------*/
int main()
{
    int i = 0;
    sword lstat;

    OCIEnvCreate (&envhp, OCI_THREADED | OCI_OBJECT, (dvoid *)0,  NULL,
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

    /*  Delete rows that are already inserted by this demo */ 
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
      printf("Multiple threads inserting rows into employees table\n");
      for (i = 0; i < MAXTHREAD; ++i)
      {
        employeeNum[i]=i;
        /* Insert into hr.employees table */
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

    /* Display inserted rows */ 
    queryRows();

    checkerr(errhp, OCIConnectionPoolDestroy(poolhp, errhp, OCI_DEFAULT));
    checkerr(errhp, OCIHandleFree((dvoid *)poolhp, OCI_HTYPE_CPOOL));
    checkerr(errhp, OCIHandleFree((dvoid *)errhp, OCI_HTYPE_ERROR));
    return 0;
} 
/* - end of main -----------------------------------------------------------*/


/* - Insert records into employees table -----------------------------------*/ 
static void threadFunction (dvoid *arg)
{
    OCISvcCtx *svchp  = (OCISvcCtx *) 0;
    OCIStmt   *stmthp = (OCIStmt *)0;
    OCIError  *errhp2 = (OCIError *) 0;
    int        empno  = *(int *)arg;
    sword      lstat  = 0;
    text       insertst1[256];
 
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
    (void) sprintf(insertst1,
      "INSERT INTO hr.employees(employee_id, last_name, job_id, salary, \
         department_id, email, hire_date) \
         VALUES (%d,'%s','%s',%7.2f,%d,'%s','%s')",
         empno+1,ename[empno],ejob[empno],
         esal[empno],edept[empno], email[empno], hdate[empno]);
    
    printf("Inserting details of %s\n",ename[empno]);
 
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
} 
/*- end of threadFunction --------------------------------------------------*/


/* Delete rows which are inserted in the previous invocation of this demo --*/
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
     "DELETE FROM hr.employees WHERE employee_id BETWEEN 1 AND %d",MAXTHREAD); 
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
    printf("Deleted all rows between 1 and %d\n", MAXTHREAD); 

} 
/*- end of deleteRows ------------------------------------------------------*/


/*- Display the contents of hr.employees table -----------------------------*/
#define DBUFLEN 20
static void queryRows() 
{
    OCISvcCtx *svchp  = (OCISvcCtx *) 0;
    OCIStmt   *stmthp = (OCIStmt *)0;
    OCIDefine *defhp[MAXBINDS];
    OCIDate    emp_hdate;
    sword      status = 0;
    sword      lstat  = 0;
    text       selectst1[256];
    text       emp_name[26];   
    text       emp_job[11]; 
    text       emp_email[26];
    text       datebuf[DBUFLEN];
    ub4        emp_no = 0;
    ub4        emp_dept = 0;
    ub4        datebuflen = DBUFLEN;
    float      emp_sal = 0.0;
    int        i = 0;

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

    (void) sprintf(selectst1,
       "SELECT employee_id, last_name, job_id, salary, department_id, \
               email, hire_date \
        FROM hr.employees WHERE employee_id between 1 and %d",MAXTHREAD);

    OCIHandleAlloc(envhp, (dvoid **)&stmthp, OCI_HTYPE_STMT, (size_t)0,
                   (dvoid **)0);

    checkerr(errhp, OCIStmtPrepare (stmthp, errhp, (text *)selectst1,
            (ub4)strlen((const signed char *)selectst1), OCI_NTV_SYNTAX, 
            OCI_DEFAULT));
    checkerr(errhp,OCIDefineByPos (stmthp, &defhp[0], errhp, (ub4)1,
            (dvoid *)&emp_no, (sb4)sizeof(ub4), (ub2)SQLT_INT, (dvoid *)0,
            (ub2 *)0, (ub2 *)0, OCI_DEFAULT));
    checkerr(errhp,OCIDefineByPos (stmthp, &defhp[1], errhp, (ub4)2,
            (dvoid *)&emp_name, (sb4)sizeof(emp_name), (ub2)SQLT_STR, 
            (dvoid *)0, (ub2 *)0, (ub2 *)0, OCI_DEFAULT));
    checkerr(errhp,OCIDefineByPos (stmthp, &defhp[2], errhp, (ub4)3,
            (dvoid *)&emp_job, (sb4)sizeof(emp_job), (ub2)SQLT_STR, 
            (dvoid *)0, (ub2 *)0, (ub2 *)0, OCI_DEFAULT));
    checkerr(errhp,OCIDefineByPos (stmthp, &defhp[3], errhp, (ub4)4,
            (dvoid *)&emp_sal,(sb4)sizeof(float),(ub2)SQLT_FLT,(dvoid *)0,
            (ub2 *)0, (ub2 *)0, OCI_DEFAULT));
    checkerr(errhp,OCIDefineByPos (stmthp, &defhp[4], errhp, (ub4)5,
            (dvoid *)&emp_dept,(sb4)sizeof(ub4),(ub2)SQLT_INT,(dvoid *)0,
            (ub2 *)0, (ub2 *)0, OCI_DEFAULT));
    checkerr(errhp,OCIDefineByPos (stmthp, &defhp[5], errhp, (ub4)6,
            (dvoid *)&emp_email,(sb4)sizeof(emp_email),(ub2)SQLT_STR,
            (dvoid *)0, (ub2 *)0, (ub2 *)0, OCI_DEFAULT));
    checkerr(errhp,OCIDefineByPos (stmthp, &defhp[6], errhp, (ub4)7,
            (dvoid *)&emp_hdate,(sb4)sizeof(emp_hdate),(ub2)SQLT_ODT,
            (dvoid *)0, (ub2 *)0, (ub2 *)0, OCI_DEFAULT));

    /* Execute the Query and Fetch MAXTHREAD records */
    if (lstat = OCIStmtExecute (svchp, stmthp, errhp, (ub4)0, 
            (ub4)0, (OCISnapshot *)0, (OCISnapshot *)0, OCI_DEFAULT ))
    {
      checkerr(errhp,lstat);
      exit(1);
    }
    /* Printing the values */

    printf("Displaying the inserted records\n");
    status = OCIStmtFetch(stmthp,errhp,1,OCI_FETCH_NEXT,OCI_DEFAULT);
    while (status != OCI_NO_DATA)
    {
       /* Convert OCIDate into text format for printing */
       lstat = OCIDateToText(errhp, &emp_hdate, (text *)0, (ub1)0,
                             (text*)"American", (ub4)8, &datebuflen, datebuf);
       if (lstat)
        checkerr(errhp, lstat);
       printf(
        "Empno:%u Name:%s Job:%s Sal:%7.2f Deptno:%u Email:%s Hiredate:%s\n",
         emp_no,emp_name,emp_job,emp_sal,emp_dept, emp_email, datebuf); 
       status = OCIStmtFetch(stmthp,errhp,1,OCI_FETCH_NEXT,OCI_DEFAULT);
    }

    checkerr(errhp, OCIHandleFree((dvoid *) stmthp, OCI_HTYPE_STMT));
    checkerr(errhp, OCILogoff((dvoid *) svchp, errhp));
} 
/*- end of queryRows --------------------------------------------------------*/
#undef  DBUFLEN
 

/* Handle oci error ---------------------------------------------------------*/
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
/* end of demo -------------------------------------------------------------*/
