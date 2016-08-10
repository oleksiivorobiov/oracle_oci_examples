/* Copyright (c) 2001, 2002, Oracle Corporation.  All rights reserved.  */
/*

   NAME
     cdemosp.c - Basic OCI Session Pooling functionality

   DESCRIPTION


     This program invokes multiple threads to insert MAXTHREAD records
     into EMPLOYEES table.
     This program assumes that sample HR schema is setup. 

   MODIFIED   (MM/DD/YY)
   msowdaga    04/30/08 - Fix bug 6236196, multiple threads should not share
                          single error handle
   sudsrini    12/26/02 - Use different empnos to insert, than 1-10
   jchai       01/28/02 - Merged jchai_change_oci_sp_sc_cp_names
   sudsrini    01/08/02 - Merged sudsrini_enable_sp_sc_suite
   sudsrini    12/30/01 - Lint Fix
   abande      12/07/01 - Creation

*/

#ifndef OCISP_ORACLE
# include <cdemosp.h> 
#endif

/* Maximum Number of threads  */ 
#define MAXTHREAD 10 
static ub4 sessMin = 3;
static ub4 sessMax = 8;
static ub4 sessIncr = 5;

static OCIError   *errhp;
static OCIEnv     *envhp;
static OCISPool   *poolhp=(OCISPool *) 0;
static int employeeNum[MAXTHREAD];

static OraText *poolName;
static ub4 poolNameLen;
static CONST OraText *database = (text *)"";
static CONST OraText *appusername =(text *)"hr";
static CONST OraText *apppassword =(text *)"hr";

/*  Values to be inserted into EMP table */
static char firstname[10][10]={"A","B","C","D","E","F","G","H","I","J"};
static char lastname[10][10]={"A","B","C","D","E","F","G","H","I","J"};
static char email[10][20]={"A@oracle.com","B@oracle.com","C@oracle.com",
                           "D@oracle.com","E@oracle.com","F@oracle.com",
                           "G@oracle.com","H@oracle.com","I@oracle.com",
                           "J@oracle.com"};

static char ejob[10][11]={"FI_ACCOUNT","AC_ACCOUNT","SA_MAN","PU_MAN",
                          "PU_CLERK","IT_PROG","MK_REP","AD_VP","AC_MGR",
                          "HR_REP"};
static float esal[10]={10000.00,
                       5000.00,
                       8000.00,
                       6000.00,
                       10000.00,
                       8000.00,
                       6000.00,
                       6000.00,
                       5000.00,
                       5000.00};

static char hiredate[10][10]={"07-JUN-94","08-JAN-94","18-JUL-97",
                              "21-FEB-96","02-JUL-96","13-AUG-99",
                              "28-DEC-98","27-SEP-96","01-JUL-99", "01-AUG-97"};

static unsigned int edept[10] = {10,20,10,20,30,10,30,20,10,20};

static void checkerr (OCIError *errhp, sword status);
static void threadFunction (dvoid *arg);

int main(void)
{
  int i = 0;
  sword lstat;
  int timeout =1;
  OCIEnvCreate (&envhp, OCI_THREADED, (dvoid *)0,  NULL,
                NULL, NULL, 0, (dvoid *)0);
 
  (void) OCIHandleAlloc((dvoid *) envhp, (dvoid **) &errhp, OCI_HTYPE_ERROR,
                        (size_t) 0, (dvoid **) 0);
 
  (void) OCIHandleAlloc((dvoid *) envhp, (dvoid **) &poolhp, OCI_HTYPE_SPOOL,
                        (size_t) 0, (dvoid **) 0);

  /* Create the session pool */
  checkerr(errhp, OCIAttrSet((dvoid *) poolhp,
           (ub4) OCI_HTYPE_SPOOL, (dvoid *) &timeout, (ub4)0, 
           OCI_ATTR_SPOOL_TIMEOUT, errhp));

  if (lstat = OCISessionPoolCreate(envhp, errhp,poolhp, (OraText **)&poolName, 
              (ub4 *)&poolNameLen, database, 
              (ub4)strlen((const signed char *)database),
              sessMin, sessMax, sessIncr,
              (OraText *)appusername,
              (ub4)strlen((const signed char *)appusername),
              (OraText *)apppassword,
              (ub4)strlen((const signed char *)apppassword),
              OCI_DEFAULT))
  {
    checkerr(errhp,lstat);
  }

  printf("Session Pool Created \n");

  /* Multiple threads using the session pool */
  {
    OCIThreadId *thrid[MAXTHREAD];
    OCIThreadHandle *thrhp[MAXTHREAD];
 
    OCIThreadProcessInit ();
    checkerr (errhp, OCIThreadInit (envhp, errhp));
    for (i = 0; i < MAXTHREAD; ++i)
    {
      checkerr (errhp, OCIThreadIdInit (envhp, errhp, &thrid[i]));
      checkerr (errhp, OCIThreadHndInit (envhp, errhp, &thrhp[i]));
    }
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
  lstat =  OCISessionPoolDestroy(poolhp, errhp, OCI_DEFAULT);

  printf("Session Pool Destroyed \n");
  
  if (lstat != OCI_SUCCESS)
    checkerr(errhp, lstat);
    
  checkerr(errhp, OCIHandleFree((dvoid *)poolhp, OCI_HTYPE_SPOOL));
    
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
  OCIAuthInfo *authp = (OCIAuthInfo *)0;
  sword lstat;
  text name[10];

  (void) OCIHandleAlloc((dvoid *) envhp, (dvoid **) &errhp2, OCI_HTYPE_ERROR,
                     (size_t) 0, (dvoid **) 0);

  lstat =  OCIHandleAlloc((dvoid *) envhp,
                          (dvoid **)&authp, (ub4) OCI_HTYPE_AUTHINFO,
                          (size_t) 0, (dvoid **) 0);
  if (lstat)
    checkerr(errhp2, lstat);

  checkerr(errhp2, OCIAttrSet((dvoid *) authp,(ub4) OCI_HTYPE_AUTHINFO, 
           (dvoid *) appusername, (ub4) strlen((char *)appusername),
           (ub4) OCI_ATTR_USERNAME, errhp2));

  checkerr(errhp2,OCIAttrSet((dvoid *) authp,(ub4) OCI_HTYPE_AUTHINFO, 
           (dvoid *) apppassword, (ub4) strlen((char *)apppassword),
           (ub4) OCI_ATTR_PASSWORD, errhp2));

  if  (lstat = OCISessionGet(envhp, errhp2, &svchp, authp,
               (OraText *)poolName, (ub4)strlen((char *)poolName), NULL, 
               0, NULL, NULL, NULL, OCI_SESSGET_SPOOL))
  {
    checkerr(errhp2,lstat);
  } 

  (void) sprintf(insertst1,"INSERT INTO EMPLOYEES (employee_id, first_name,\
                 last_name, email, hire_date, job_id, salary, department_id)\
                 values (%d, '%s', '%s', '%s', '%s', '%s',%7.2f, %d )",
                 empno+2345,firstname[empno],lastname[empno], email[empno], 
                 hiredate[empno], ejob[empno], esal[empno], edept[empno]);

  OCIHandleAlloc(envhp, (dvoid **)&stmthp, OCI_HTYPE_STMT, (size_t)0,
                 (dvoid **)0);

  checkerr(errhp2, OCIStmtPrepare (stmthp, errhp2, (CONST OraText *)insertst1, 
                                  (ub4)strlen((const signed char *)insertst1),
                                  OCI_NTV_SYNTAX, OCI_DEFAULT));

  checkerr(errhp2, OCIStmtExecute (svchp, stmthp, errhp2, (ub4)1, (ub4)0,
                                  (OCISnapshot *)0, (OCISnapshot *)0, 
                                  OCI_DEFAULT ));

  checkerr(errhp2, OCITransCommit(svchp,errhp2,(ub4)0));
  
  printf("Insert complete \n");

  checkerr(errhp2, OCIHandleFree((dvoid *) stmthp, OCI_HTYPE_STMT));
  checkerr(errhp2, OCISessionRelease(svchp, errhp2, NULL, 0, OCI_DEFAULT));
  OCIHandleFree((dvoid *)authp, OCI_HTYPE_AUTHINFO);
  OCIHandleFree((dvoid *)errhp2, OCI_HTYPE_ERROR);

} /* end of threadFunction (dvoid *) */

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
