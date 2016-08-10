/* Copyright (c) 2008, Oracle and/or its affiliates. All rights reserved. */

/*
  NAME
    cdemoqc2 - OCI Query Cache(result cache) table annotation functionality 
         
  DESCRIPTION
    For using client result cache, result_cache hint not necessarily needs to
    be used in query. If the table is created/altered with result_cache mode
    FORCE, cache will be enabled by default and it will be used even if
    result_cache hint is not used in the query. If result_cache mode is not
    mentioned while creating table, the default mode is DEFAULT. In this case,
    cache will be used only when result_cache hint is used in the query.

    Note that even when the result_cache mode is specified as FORCE, caching
    will be enabled only for those queries which are cache worthy.

    In the below table, 
      rows -> represent the result_cache hint usage in the query 
      columns -> represent, the mode with which table was created
      YES -> represents result cache will be used
      NO -> represents result cache will not be used

    -----------------------------------------------
    |                 | DEFAULT MODE | FORCE MODE |
    -----------------------------------------------
    | No Hint used    | NO           | YES        |
    | result_cache    | YES          | YES        |
    | no_result_cache | NO           | NO         |
    -----------------------------------------------

  NOTE:
    To use the query cache feature (and hence this program), database should
    have been brought up with client side result cache enabled. To bring the
    database with result cache enabled, add the following lines in
    initialization parameter file,
       client_result_cache_size=<cache size in bytes>
       client_result_cache_lag=<timeout value for cache in ms>
       compatible=11.0.0.0.0
    and bring up the database.

  MODIFIED     (MM/DD/YY)
     dgopal     10/10/08 - Created

*/

#include <oci.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define NO_HINT 0
#define CACHE_HINT 1
#define NO_CACHE_HINT 2

#define FORCE_MODE 0
#define DEFAULT_MODE 1

#define USER_TABLES 0
#define ALL_TABLES 1
#define DBA_TABLES 2

/* Function prototypes */
static void query_emp(int idx);
static void query_ssn(int idx);
static void alt_table(int idx);
static void print_stats(int idx);
static void dbLogon();
static void dbLogoff();
static void Checkerr(OCIError *errhp, sword status, text *msg);

int main();

static sword status = 0;
static OCISvcCtx *svchp;
static OCIError   *errhp;
static OCIEnv     *envhp;
static OCIAuthInfo *authhp;

/* Queries from qctable */
static text *query_emp_stmt[]={
(text *)"SELECT empno, ename, sal FROM qctable",
(text *)"SELECT /*+ result_cache */ empno, ename, sal FROM qctable",
(text *)"SELECT /*+ no_result_cache */ empno, ename, sal FROM qctable"
};

/* Queries from ssntable */
static text *query_ssn_stmt[]={
(text *)"SELECT ssn, empno FROM ssntable",
(text *)"SELECT /*+ result_cache */ ssn, empno FROM ssntable",
(text *)"SELECT /*+ no_result_cache */ ssn, empno FROM ssntable"
};

/* Alter table statements with result_cache annotations */
static text *alt_stmt[]={
(text *)"ALTER TABLE ssntable RESULT_CACHE (MODE FORCE)",
(text *)"ALTER TABLE qctable RESULT_CACHE (MODE DEFAULT)"
};

/* Table can also be created with result_cache annotations.
(e.g.)
1. CREATE TABLE ssntable(ssn NUMBER, empno NUMBER) RESULT_CACHE (MODE FORCE)
2. CREATE TABLE qctable(empno NUMBER, ename VARCHAR2(20), sal NUMBER) RESULT_CACHE (MODE DEFAULT)
*/

/* - main -------------------------------------------------------------------*/ 
int main ()
{
  printf ("Executing table annotation demo - cdemoqc2\n\n");

  /* Logging on to the database */
  dbLogon ();

  /* 
     Table was created by the cdemoqc.sql script without any result_cache mode
     parameter. In this case caching will happen only when RESULT_CACHE sql
     hint is used in the query.
  */
  printf ("\nQuerying qctable which was created without specifying any "
          "result_cache mode\n");
  printf ("Execute query with result_cache hint in query\n");
  query_emp(CACHE_HINT);
  printf ("Re-execute would fetch from local cache\n");
  query_emp(CACHE_HINT);

  /*
    If table RESULT_CACHE (MODE FORCE) is specified in CREATE or ALTER TABLE
    statement caching will be used even if RESULT_CACHE sql hint is not
    specified in query. However NO_RESULT_CACHE sql hint can be used to
    explicitily disable caching.

    This FORCE MODE can be specified for the table which hardly gets updated.
  */
  printf ("\nAltering ssntable with RESULT_CACHE (MODE FORCE)\n");
  alt_table(FORCE_MODE);
  printf ("Execute would create local client cache even if no hint is "
          "specified in query\n");
  query_ssn(NO_HINT);
  printf ("Re-execute would fetch from local cache\n");
  query_ssn(NO_HINT);

  printf ("Even if the table is created with RESULT_CACHE MODE FORCE, execute "
          "will not \n  cache the results if NO_RESULT_CACHE is specified in "
          "query\n");
  query_ssn(NO_CACHE_HINT);
  printf ("Re-execute would fetch from the server\n");
  query_ssn(NO_CACHE_HINT);

  /* 
    Creating or altering table with RESULT_CACHE (MODE DEFAULT) is as good as
    having table without any RESULT_CACHE hint. In this case caching will be
    done only when RESULT_CACHE sql hint is used in the query.
  */
  printf ("\nAltering qctable with RESULT_CACHE (MODE DEFAULT)\n");
  alt_table(DEFAULT_MODE);
  printf ("Execute query without any result_cache hint in query\n");
  query_emp(NO_HINT);
  printf ("Re-execute would fetch from the server\n");
  query_emp(NO_HINT);

  printf ("Execute query with sql result_cache hint\n");
  query_emp(CACHE_HINT);
  printf ("Re-execute would fetch from local cache\n");
  query_emp(CACHE_HINT);

  printf("\nRESULT_CACHE parameter from USER_TABLES,\n");
  print_stats(USER_TABLES);
  printf("\nRESULT_CACHE parameter from ALL_TABLES,\n");
  print_stats(ALL_TABLES);
  /*
    For quering DBA_TABLES, user needs to login as SYSDBA. Hence if you 
    uncomment this call, the program will prompt for sys user password. scanf()
    has been used for fetching the password from user and you may want to
    replace it with your platform specific function to hide the echoing of 
    password.
  */
  /* 
  printf("\nRESULT_CACHE parameter from ALL_TABLES,\n");
  print_stats(DBA_TABLES); 
  */

  dbLogoff ();
  return 0;
}

/* - Logon to the DB --------------------------------------------------------*/ 
static void dbLogon ()
{
  ub4 cachesize=10;
  OraText *connStr = (text *)"";
  OraText *username = (text *)"ocitest";
  OraText *password = (text *)"ocitest";

  OCIEnvCreate ((OCIEnv **)&envhp, (ub4)OCI_DEFAULT, (dvoid *)0,
                (dvoid * (*)(dvoid *, size_t))0,
                (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                (void (*)(dvoid *, dvoid *))0, (size_t)0, (dvoid **)0);

  OCIHandleAlloc ((dvoid *) envhp, (dvoid **) &errhp, OCI_HTYPE_ERROR,
                  (size_t) 0, (dvoid **) 0);

  OCIHandleAlloc ((dvoid *)envhp, (dvoid **)&authhp,
                  (ub4)OCI_HTYPE_AUTHINFO, (size_t)0, (dvoid **)0);

  /* Connecting to database */

  OCIAttrSet ((dvoid *)authhp, (ub4)OCI_HTYPE_AUTHINFO, (dvoid *)username,
     (ub4)strlen((char *)username), (ub4)OCI_ATTR_USERNAME,
     (OCIError *)errhp);

  OCIAttrSet ((dvoid *)authhp, (ub4)OCI_HTYPE_AUTHINFO, (dvoid *)password,
     (ub4)strlen((char *)password), (ub4)OCI_ATTR_PASSWORD,
     (OCIError *)errhp);

  Checkerr (errhp, 
     OCISessionGet ((OCIEnv *)envhp, (OCIError *)errhp,
     (OCISvcCtx **)&svchp, (OCIAuthInfo *)authhp, (OraText *)connStr,
     (ub4)strlen((char *)connStr), (OraText *)NULL, (ub4)0, (OraText **)0,
     (ub4 *)0, (boolean *)0,(ub4)OCI_DEFAULT),
     (oratext *)"OCISessionGet");

  printf ("Connected to Database Session\n");

  OCIAttrSet((dvoid *)svchp, OCI_HTYPE_SVCCTX, (dvoid *)&cachesize,
     (ub4)0,OCI_ATTR_STMTCACHESIZE,errhp);
}

/* - Execute SQL query (from qctable) ---------------------------------------*/ 
static void query_emp (int idx)
{
  OCIStmt *stmthp = (OCIStmt *)0; 
  OCIDefine *def1hp = (OCIDefine *)0;
  OCIDefine *def2hp = (OCIDefine *)0;
  OCIDefine *def3hp = (OCIDefine *)0;
  ub4 empno;
  text ename[100];
  ub4 sal;
  sb4 empnoSz = sizeof (empno);
  sb4 enameSz = sizeof (ename);
  sb4 salSz = sizeof (sal);
  ub4 prefetch = 0;

  Checkerr (errhp, 
    OCIStmtPrepare2 ((OCISvcCtx *)svchp,(OCIStmt **)&stmthp, (OCIError *)errhp,
    (text *)query_emp_stmt[idx], (ub4)strlen((char *)query_emp_stmt[idx]), 
    (oratext *)NULL, (ub4) 0, (ub4)OCI_NTV_SYNTAX, (ub4)OCI_DEFAULT),
    (oratext *)"OCIStmtPrepare-emp");

  /* Setting the prefetch count = 0 */
  Checkerr(errhp, 
    OCIAttrSet((OCIStmt *) stmthp, OCI_HTYPE_STMT, (dvoid *)&prefetch,
    sizeof(prefetch), OCI_ATTR_PREFETCH_ROWS, (OCIError *)errhp),
    (oratext *) "OCIAttrSet-prefetch-emp");

  Checkerr (errhp, 
    OCIDefineByPos ((OCIStmt *)stmthp, (OCIDefine **)&def1hp,
    (OCIError *)errhp, (ub4)1, (dvoid *)&(empno), (sb4)empnoSz, (ub2)SQLT_INT,
    (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)OCI_DEFAULT),
    (oratext *)"OCIDefByPos-emp");

  Checkerr (errhp, 
    OCIDefineByPos ((OCIStmt *)stmthp, (OCIDefine **)&def2hp,
    (OCIError *)errhp, (ub4)2, (dvoid *)&(ename), (sb4)enameSz, (ub2)SQLT_STR,
    (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)OCI_DEFAULT),
    (oratext *)"OCIDefByPos2-emp");

  Checkerr (errhp, 
    OCIDefineByPos ((OCIStmt *)stmthp, (OCIDefine **)&def3hp,
    (OCIError *)errhp, (ub4)3, (dvoid *)&(sal), (sb4)salSz, (ub2)SQLT_INT,
    (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)OCI_DEFAULT),
    (oratext *)"OCIDefByPos3-emp");

  if ((status = OCIStmtExecute ((OCISvcCtx *)svchp, (OCIStmt *)stmthp,
                  (OCIError *)errhp, (ub4)0, (ub4)0, (OCISnapshot *)0,
                  (OCISnapshot *)0, (ub4)OCI_DEFAULT)) != OCI_SUCCESS )
  {
    printf ("OCIStmtExecute for SELECT - Fail\n" );
    Checkerr (errhp, status,(oratext *)"Stmt Execute-emp");
  }
  else
  {
    do 
    {
      status = OCIStmtFetch2((OCIStmt *)stmthp, (OCIError *)errhp, (ub4)1,
                 (ub2)OCI_FETCH_NEXT, (sb4)0, (ub4)OCI_DEFAULT);

      if (status == OCI_ERROR || status == OCI_INVALID_HANDLE)
      {
        Checkerr(errhp, status, (oratext *)"OCIStmtFetch2-emp");
        break;
      }
      else if (status != OCI_NO_DATA)
      {
        /* 
        printf("EMPNO is %d, ENAME is %s, SAL is %d\n", empno, ename, sal);
        */
      } 
    }while(status !=  OCI_NO_DATA);
  }

  Checkerr (errhp,
    OCIStmtRelease ((OCIStmt *)stmthp, (OCIError *)errhp,(dvoid *)NULL, 0,
    OCI_DEFAULT), (oratext *)"StmtRelease-emp");
}

/* - Execute SQL query (from ssntable) --------------------------------------*/ 
static void query_ssn (int idx)
{
  OCIStmt *stmthp = (OCIStmt *)0; 
  OCIDefine *def1hp = (OCIDefine *)0;
  OCIDefine *def2hp = (OCIDefine *)0;
  ub4 ssn;
  ub4 empno;
  sb4 ssnSz = sizeof (ssn);
  sb4 empnoSz = sizeof (empno);
  ub4  prefetch = 0;

  Checkerr (errhp, 
    OCIStmtPrepare2 ((OCISvcCtx *)svchp,(OCIStmt **)&stmthp, (OCIError *)errhp,
    (text *)query_ssn_stmt[idx], (ub4)strlen((char *)query_ssn_stmt[idx]), 
    (oratext *)NULL, (ub4) 0, (ub4)OCI_NTV_SYNTAX, (ub4)OCI_DEFAULT),
    (oratext *)"OCIStmtPrepare-ssn");

  /* Setting the prefetch count = 0 */
  Checkerr(errhp, 
    OCIAttrSet((OCIStmt *) stmthp, OCI_HTYPE_STMT, (dvoid *)&prefetch,
    sizeof(prefetch), OCI_ATTR_PREFETCH_ROWS, (OCIError *)errhp),
    (oratext *) "OCIAttrSet-prefetch-ssn");

  Checkerr (errhp, 
    OCIDefineByPos ((OCIStmt *)stmthp, (OCIDefine **)&def1hp,
    (OCIError *)errhp, (ub4)1, (dvoid *)&(ssn), (sb4)ssnSz, (ub2)SQLT_INT,
    (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)OCI_DEFAULT),
    (oratext *)"OCIDefByPos1-ssn");

  Checkerr (errhp, 
    OCIDefineByPos ((OCIStmt *)stmthp, (OCIDefine **)&def2hp,
    (OCIError *)errhp, (ub4)2, (dvoid *)&(empno), (sb4)empnoSz, (ub2)SQLT_INT,
    (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)OCI_DEFAULT),
    (oratext *)"OCIDefByPos2-ssn");

  if ((status = OCIStmtExecute ((OCISvcCtx *)svchp, (OCIStmt *)stmthp,
                  (OCIError *)errhp, (ub4)0, (ub4)0, (OCISnapshot *)0,
                  (OCISnapshot *)0, (ub4)OCI_DEFAULT)) != OCI_SUCCESS )
  {
    printf ("OCIStmtExecute for SELECT - Fail\n" );
    Checkerr (errhp, status,(oratext *)"Stmt Execute-ssn");
  }
  else
  {
    do 
    {
      status = OCIStmtFetch2((OCIStmt *)stmthp, (OCIError *)errhp, (ub4)1,
                 (ub2)OCI_FETCH_NEXT, (sb4)0, (ub4)OCI_DEFAULT);

      if (status == OCI_ERROR || status == OCI_INVALID_HANDLE)
      {
        Checkerr(errhp, status, (oratext *)"OCIStmtFetch2-ssn");
        break;
      }
      else if (status != OCI_NO_DATA)
      {
        /* 
        printf("SSN is %d, EMPNO is %d\n", ssn, empno);
        */
      } 
    }while(status !=  OCI_NO_DATA);
  }

  Checkerr (errhp,
    OCIStmtRelease ((OCIStmt *)stmthp, (OCIError *)errhp,(dvoid *)NULL, 0,
    OCI_DEFAULT), (oratext *)"StmtRelease-ssn");
}

/* - Execute alter result_cache mode statement ------------------------------*/ 
void alt_table(int idx)
{
  OCIStmt *stmthp = (OCIStmt *)0;

  Checkerr (errhp,
    OCIHandleAlloc ((dvoid *)envhp, (dvoid **)&stmthp, (ub4)OCI_HTYPE_STMT, 
    (size_t)0, (dvoid **)0), (oratext *)"OCIHandleAlloc-ddl");

  Checkerr (errhp,
    OCIStmtPrepare ((OCIStmt *)stmthp, (OCIError *)errhp, (text *)alt_stmt[idx],
    (ub4)strlen((char *)alt_stmt[idx]), (ub4)OCI_NTV_SYNTAX, (ub4)OCI_DEFAULT),
    (oratext *)"OCIStmtPrepare-ddl");

  if (status = OCIStmtExecute ((OCISvcCtx *)svchp, (OCIStmt *)stmthp,
                 (OCIError *)errhp, (ub4)1, (ub4)0, (OCISnapshot *)0,
                 (OCISnapshot *)0, (ub4)OCI_COMMIT_ON_SUCCESS))
  {
    printf ("OCIStmtExecute update - Fail\n");
    Checkerr (errhp, status,(oratext *)"Stmt Execute Update-ddl");
  }

  Checkerr (errhp,
    OCIHandleFree ((dvoid *)stmthp, (ub4)OCI_HTYPE_STMT),
    (oratext *)"OCIHandleFree-ddl");
}

/* - Fetches and prints result cache statistics information -----------------*/ 
void print_stats(int idx)
{
  OCISvcCtx *statsvchp = NULL;
  OCIServer *srvhp = NULL;
  OCIStmt *stmthp = (OCIStmt *)0; 
  OCISession *sysauthhp = (OCISession *)0;
  OCIDefine *def1hp = (OCIDefine *)0;
  OCIDefine *def2hp = (OCIDefine *)0;
  text tablename[128];
  text cacheusage[128];
  sb4 tablenameSz = sizeof (tablename);
  sb4 cacheusageSz = sizeof (cacheusage);
  ub4  prefetch = 0;
  OraText *connStr = (text *)"";
  OraText *sysuser = (text *)"sys";
  OraText syspass[128];
  text StatQuery[128];

  /* For querying from USER_TABLES */
  if (idx==USER_TABLES)
  {
    strcpy ((char *)StatQuery, "SELECT table_name, result_cache FROM user_tables WHERE table_name='QCTABLE' OR table_name='SSNTABLE' ORDER BY table_name");
    statsvchp=svchp;
  } 
  /* For querying from ALL_TABLES */
  else if (idx=ALL_TABLES)
  {
    strcpy ((char *)StatQuery, "SELECT table_name, result_cache FROM all_tables WHERE table_name='QCTABLE' OR table_name='SSNTABLE' ORDER BY table_name");
    statsvchp=svchp;
  }
  /* For querying from DBA_TABLES */
  else if (idx==DBA_TABLES)
  {
    strcpy ((char *)StatQuery, "SELECT table_name, result_cache FROM dba_tables WHERE table_name='QCTABLE' OR table_name='SSNTABLE' ORDER BY table_name");

    /* To access DBA_TABLES sysdba perviledge is required */
    printf ("\nPlease enter password for sys user to continue:\n");
    scanf ("%s", (char *)syspass);

    OCIHandleAlloc ((dvoid *)envhp, (dvoid **)&srvhp, (ub4)OCI_HTYPE_SERVER, 
                    (size_t)0, (dvoid **)0);

    OCIHandleAlloc ((dvoid *)envhp, (dvoid **)&statsvchp, (ub4)OCI_HTYPE_SVCCTX,
                    (size_t)0, (dvoid **)0);

    Checkerr (errhp, 
      OCIServerAttach (srvhp, errhp, (text *)connStr,
      (sb4)strlen((char *)connStr), 0), (oratext *)"OCIServerAttach-stat");

    OCIAttrSet ((dvoid *) statsvchp, OCI_HTYPE_SVCCTX, (dvoid *)srvhp, (ub4) 0,
                OCI_ATTR_SERVER, (OCIError *) errhp);

    OCIHandleAlloc ((dvoid *)envhp, (dvoid **)&sysauthhp,
                    (ub4)OCI_HTYPE_SESSION, (size_t)0, (dvoid **)0);

    OCIAttrSet ((dvoid *)sysauthhp, (ub4)OCI_HTYPE_SESSION, (dvoid *)sysuser,
                (ub4)strlen((char *)sysuser), (ub4)OCI_ATTR_USERNAME,
                (OCIError *)errhp);

    OCIAttrSet ((dvoid *)sysauthhp, (ub4)OCI_HTYPE_SESSION, (dvoid *)syspass,
                (ub4)strlen((char *)syspass), (ub4)OCI_ATTR_PASSWORD,
                (OCIError *)errhp);

    /* Connecting to Database as sysdba */
    Checkerr (errhp, 
      OCISessionBegin (statsvchp,  errhp, sysauthhp, OCI_CRED_RDBMS,
      (ub4)OCI_SYSDBA), (oratext *)"OCISessionBegin-stat");

    OCIAttrSet ((dvoid *)statsvchp, OCI_HTYPE_SVCCTX, (dvoid *)sysauthhp, (ub4)0,
                OCI_ATTR_SESSION, errhp);
  }

  Checkerr (errhp,
    OCIStmtPrepare2 ((OCISvcCtx *)statsvchp,(OCIStmt **)&stmthp,
    (OCIError *)errhp, (text *)StatQuery, (ub4)strlen((char *)StatQuery),
    (oratext *)NULL, (ub4) 0, (ub4)OCI_NTV_SYNTAX, (ub4)OCI_DEFAULT),
    (oratext *)"OCIStmtPrepare-stat");

  Checkerr(errhp, 
    OCIAttrSet((OCIStmt *) stmthp, OCI_HTYPE_STMT, (dvoid *)&prefetch,
    sizeof(prefetch), OCI_ATTR_PREFETCH_ROWS, (OCIError *)errhp),
    (oratext *) "OCIAttrSet-prefetch-stat");

  Checkerr (errhp,
    OCIDefineByPos ((OCIStmt *)stmthp, (OCIDefine **)&def1hp,
    (OCIError *)errhp, (ub4)1, (dvoid *)&(tablename), (sb4)tablenameSz,
    (ub2)SQLT_STR, (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)OCI_DEFAULT),
    (oratext *)"OCIDefByPos-stat");

  Checkerr (errhp,
    OCIDefineByPos ((OCIStmt *)stmthp, (OCIDefine **)&def2hp,
    (OCIError *)errhp, (ub4)2, (dvoid *)&(cacheusage), (sb4)cacheusageSz,
    (ub2)SQLT_STR, (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)OCI_DEFAULT),
    (oratext *)"OCIDefByPos2-stat");

  /* Fetching the data from the stats table */
  if ((status = OCIStmtExecute ((OCISvcCtx *)statsvchp, (OCIStmt *)stmthp,
                                (OCIError *)errhp, (ub4)0, (ub4)0,
                                (OCISnapshot *)0, (OCISnapshot *)0, 
                                (ub4)OCI_DEFAULT)) != OCI_SUCCESS )
  {
    printf ("OCIStmtExecute for SELECT - Fail\n" );
    Checkerr (errhp, status,(oratext *)"Stmt Execute-stat");
  }
  else
  {
    printf("TABLE_NAME      RESULT_CACHE\n");
    printf("==========      ============\n");
    do 
    {
      status = OCIStmtFetch2((OCIStmt *)stmthp, (OCIError *)errhp, (ub4)1,
                             (ub2)OCI_FETCH_NEXT, (sb4)0, (ub4)OCI_DEFAULT);

      if (status == OCI_ERROR || status == OCI_INVALID_HANDLE)
      {
        Checkerr(errhp, status, (oratext *)"OCIStmtFetch2-stat");
        break;
      }
      else if (status != OCI_NO_DATA)
      {
        printf("%-15s %-15s\n", tablename, cacheusage); 
      } 
    }while(status !=  OCI_NO_DATA);
  }

  Checkerr (errhp,
    OCIStmtRelease ((OCIStmt *)stmthp, (OCIError *)errhp,(dvoid *)NULL, 0, 
    OCI_DEFAULT), (oratext *)"StmtRelease-stat");

  /* If sysdba session is created, terminate it */
  if (idx==DBA_TABLES)
  {
    Checkerr (errhp, 
      OCISessionEnd (statsvchp, errhp, sysauthhp, OCI_DEFAULT),
      (oratext *) "Session-End-stat");

    Checkerr (errhp,
      OCIServerDetach (srvhp, errhp, OCI_DEFAULT),
      (oratext *) "Server-detach-stat");

    OCIHandleFree((dvoid *)sysauthhp, OCI_HTYPE_SESSION);
    OCIHandleFree((dvoid *)statsvchp, OCI_HTYPE_SVCCTX);
    OCIHandleFree((dvoid *)srvhp, OCI_HTYPE_SERVER);
  }
}

/* - Session Logoff --------------------------------------------------------*/
static void dbLogoff ()
{
  printf ("\nLogging off from the connected session.\n");

  Checkerr (errhp, 
    OCISessionRelease(svchp, errhp, 0,0, OCI_DEFAULT),
    (oratext *) "Session-Release");

  OCIHandleFree((dvoid *)authhp, OCI_HTYPE_AUTHINFO);
  OCIHandleFree((dvoid *)errhp, OCI_HTYPE_ERROR);
}

/* - Error checking routing -------------------------------------------------*/
void Checkerr(OCIError *errhp, sword status, text *msg)
{
  text  msgbuf[512];
  sb4   errcode = 0;

  memset((void *) msgbuf, (int)'\0', (size_t)512);
  if(status!=OCI_SUCCESS)
  {
    printf("error msg: %s\n",msg);
  }

  switch (status)
  {
  case OCI_SUCCESS: break;
  case OCI_SUCCESS_WITH_INFO:
    printf("status = OCI_SUCCESS_WITH_INFO\n");
    OCIErrorGet((dvoid *) errhp, (ub4) 1, (text *) NULL, &errcode,
                       msgbuf, (ub4) sizeof(msgbuf), (ub4) OCI_HTYPE_ERROR);
    printf("ERROR CODE = %d\n", errcode);
    printf("%.*s\n", 512, msgbuf);
    if (errcode == 436 || errcode == 437 || errcode == 438 || errcode == 439)
      exit(1);
    break;
  case OCI_NEED_DATA:
    printf("status = OCI_NEED_DATA\n");
    break;
  case OCI_NO_DATA:
     printf("status = OCI_NO_DATA\n");
    break;
  case OCI_ERROR:
     printf("status = OCI_ERROR\n");
     OCIErrorGet((dvoid *) errhp, (ub4) 1, (text *) NULL, &errcode,
                       msgbuf, (ub4) sizeof(msgbuf), (ub4) OCI_HTYPE_ERROR);
     printf("ERROR CODE = %d\n", errcode);
     printf("%.*s\n", 512, msgbuf);
    if (errcode == 436 || errcode == 437 || errcode == 438 || errcode == 439)
      exit(1);
    break;
  case OCI_INVALID_HANDLE:
     OCIErrorGet((dvoid *) errhp, (ub4) 1, (text *) NULL, &errcode,
                       msgbuf, (ub4) sizeof(msgbuf), (ub4) OCI_HTYPE_ERROR);
     printf("ERROR CODE = %d\n", errcode);
     printf("%.*s\n", 512, msgbuf);
     printf("status = OCI_INVALID_HANDLE\n");
    break;
  case OCI_STILL_EXECUTING:
     printf("status = OCI_STILL_EXECUTE\n");
    break;
  case OCI_CONTINUE:
     printf("status = OCI_CONTINUE\n");
    break;
  default:
    break;
  }
  return;
}
