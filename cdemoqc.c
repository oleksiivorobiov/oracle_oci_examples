/* Copyright (c) 2007, 2008, Oracle and/or its affiliates.
 All rights reserved. */

/*
  NAME
    cdemoqc - Basic OCI Query Cache(result cache) functionality 
         
  DESCRIPTION
    This program uses multiple sessions to demonstrate the working of Query 
    cache. To use the query cache feature (and hence this program), database
    should have been brought up with client side result cache enabled. To bring
    the database with result cache enabled, add the following lines in
    initialization parameter file,
       client_result_cache_size=<cache size in bytes>
       client_result_cache_lag=<timeout value for cache in ms>
       compatible=11.0.0.0.0
    and bring up the database.

    After this, when the result cache hint is specified, fetched data will be
    cached locally. When the same query is executed again, data will be fetched
    from this local client cache rather than fetching the data from the server.
    Hence there will be substantial improvement in the performance if this 
    feature is used on the transactions where the same query is executed
    multiple times on the tables which will be rarely updated.

    When the contents of the table is changed, the next fetch statement will 
    fetch the data from the server and also the local cache will be updated.
    This updated cache will be used thereafter.

  NOTE:
  1.To check the performance improvement with this feature measure the time
    taken by this program using your operating specific commands. For example 
    in Linux, "time cdemoqc" will give you the time taken by this program for
    completion. After this repeat the same program without result_cache hint
    and measure the time taken. You should able to see some difference in these 
    times and this difference will be remarkable when the server is running 
    remotely.
  2.print_stats() will query the stats table. This function can be used to 
    check the cache usage statistics. The call to this funtion has been 
    commented out. If you want to see this statistics info please uncomment the 
    corresponding lines in main(). 
    For getting this statistics, user needs to login as SYSDBA. Hence if you 
    uncomment this call, the program will prompt for sys user password. scanf()
    has been used for fetching the password from user and you may want to
    replace it with your platform specific function to hide the echoing of 
    password.

  MODIFIED     (MM/DD/YY)
     dgopal     12/19/07 - Updated
     dgopal     05/08/07 - Created

*/

#if defined(WIN32COMMON) || defined(WIN32) || defined(_WIN32) 
#include <windows.h>
#define sleep(x) Sleep(1000*(x))
#endif

# include <oci.h>

# include <stdio.h>
# include <string.h>
# include <stdlib.h>

#if !defined(WIN32COMMON) && !defined(WIN32) && !defined(_WIN32) 
#include <unistd.h>
#endif

#define EMPLOYEE 0
#define HR 1

/* Function prototypes */
static void query_tab(ub4 idx);
static void upd_table(ub4 idx);
static void print_stats();
static void dbLogon();
static void dbLogoff();
static void Checkerr(OCIError *errhp, sword status, text *msg);

int main();

static sword status = 0;
static OCISvcCtx *svchp[2];
static OCIError   *errhp;
static OCIEnv     *envhp;
static OCIAuthInfo *authhp;

/* Queries with result cache hints */
static text *cache_query=
(text *)"SELECT /*+ result_cache */ empno, ename, sal FROM qctable";

static text *upd_stmt=
(text *)"UPDATE qctable SET sal=sal+50 WHERE empno=1";

/* - main -------------------------------------------------------------------*/ 
int main ()
{
  printf ("Query cache is enabled by using result_cache hints\n\n");

  /* Logging on to multiple sessions */
  dbLogon ();

  /* 
     Querying same table from different sessions. For the same queries result
     cache will be shared across sessions.
  */
  printf ("Employee: Execute will fetch rows from server\n");
  query_tab(EMPLOYEE);
  printf ("Employee: Execute will fetch rows from local cache\n");
  query_tab(EMPLOYEE);

  printf ("HR: Execute will cause a roundtrip during first execute, but the "
          "same result \n\tset created in Employee Session will be shared "
          "thereafter\n");
  query_tab(HR);
  printf ("HR: Execute will fetch rows from local cache\n");
  query_tab(HR);

  /* 
     Updating the table. This will invalidate the result caches of all the 
     sessions involving this table data.
  */
  printf ("\nHR: Updating the table\n\n");
  upd_table(HR);

  printf ("Employee: Execute will fetch rows from server and hence the local "
          "result set \n\twill be updated\n");
  query_tab(EMPLOYEE);
  printf ("HR: Execute will fetch rows from updated local cache\n");
  query_tab(HR);
  printf ("Employee: Execute will fetch rows from local cache\n");
  query_tab(EMPLOYEE);

  /* 
     Stats table will get updated only once in few seconds. So data should be 
     fetched from the stats table after sleeping for few seconds.
  */

/* Below lines should be uncommented if you want to see the cache usage stats */
  /*
  printf ("\nSleeping for few seconds to let the stat table to get "
          "updated\n\n");

  sleep (60);
  print_stats();
  */

  dbLogoff ();
  return 0;
}

/* - Logon to the DB in two sessions ----------------------------------------*/ 
static void dbLogon ()
{
  int idx;
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

  /* Connecting to session 1 */
  idx=0;

  OCIAttrSet ((dvoid *)authhp, (ub4)OCI_HTYPE_AUTHINFO, (dvoid *)username,
     (ub4)strlen((char *)username), (ub4)OCI_ATTR_USERNAME,
     (OCIError *)errhp);

  OCIAttrSet ((dvoid *)authhp, (ub4)OCI_HTYPE_AUTHINFO, (dvoid *)password,
     (ub4)strlen((char *)password), (ub4)OCI_ATTR_PASSWORD,
     (OCIError *)errhp);

  Checkerr (errhp, 
     OCISessionGet ((OCIEnv *)envhp, (OCIError *)errhp,
     (OCISvcCtx **)&svchp[idx], (OCIAuthInfo *)authhp, (OraText *)connStr,
     (ub4)strlen((char *)connStr), (OraText *)NULL, (ub4)0, (OraText **)0,
     (ub4 *)0, (boolean *)0,(ub4)OCI_DEFAULT),
     (oratext *)"OCISessionGet");

  printf ("Connected to Employee Session\n");

  OCIAttrSet((dvoid *)svchp[idx], OCI_HTYPE_SVCCTX, (dvoid *)&cachesize,
     (ub4)0,OCI_ATTR_STMTCACHESIZE,errhp);

  /* Connecting to session 2 */
  idx=1;

  OCIAttrSet ((dvoid *)authhp, (ub4)OCI_HTYPE_AUTHINFO, (dvoid *)username,
     (ub4)strlen((char *)username), (ub4)OCI_ATTR_USERNAME,
     (OCIError *)errhp);

  OCIAttrSet ((dvoid *)authhp, (ub4)OCI_HTYPE_AUTHINFO, (dvoid *)password,
     (ub4)strlen((char *)password), (ub4)OCI_ATTR_PASSWORD,
     (OCIError *)errhp);

  Checkerr (errhp, 
     OCISessionGet ((OCIEnv *)envhp, (OCIError *)errhp,
     (OCISvcCtx **)&svchp[idx], (OCIAuthInfo *)authhp, (OraText *)connStr,
     (ub4)strlen((char *)connStr), (OraText *)NULL, (ub4)0, (OraText **)0,
     (ub4 *)0, (boolean *)0,(ub4)OCI_DEFAULT),
     (oratext *)"OCISessionGet");

  printf ("Connected to HR Session\n\n");

  OCIAttrSet((dvoid *)svchp[idx], OCI_HTYPE_SVCCTX, (dvoid *)&cachesize,
     (ub4)0,OCI_ATTR_STMTCACHESIZE,errhp);
}

/* - Execute SQL query and prints the data ----------------------------------*/ 
static void query_tab (ub4 idx)
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
  ub2 datelen=0;
  ub4  prefetch = 0;

  Checkerr (errhp, 
    OCIStmtPrepare2 ((OCISvcCtx *)svchp[idx],(OCIStmt **)&stmthp,
    (OCIError *)errhp, (text *)cache_query, (ub4)strlen((char *)cache_query), 
    (oratext *)NULL, (ub4) 0, (ub4)OCI_NTV_SYNTAX, (ub4)OCI_DEFAULT),
    (oratext *)"OCIStmtPrepare");

  /* Setting the prefetch count = 0 */
  Checkerr(errhp, 
    OCIAttrSet((OCIStmt *) stmthp, OCI_HTYPE_STMT, (dvoid *)&prefetch,
    sizeof(prefetch), OCI_ATTR_PREFETCH_ROWS, (OCIError *)errhp),
    (oratext *) "OCIAttrSet-prefetch");

  Checkerr (errhp, 
    OCIDefineByPos ((OCIStmt *)stmthp, (OCIDefine **)&def1hp,
    (OCIError *)errhp, (ub4)1, (dvoid *)&(empno), (sb4)empnoSz, (ub2)SQLT_INT,
    (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)OCI_DEFAULT),
    (oratext *)"OCIDefByPos");

  Checkerr (errhp, 
    OCIDefineByPos ((OCIStmt *)stmthp, (OCIDefine **)&def2hp,
    (OCIError *)errhp, (ub4)2, (dvoid *)&(ename), (sb4)enameSz, (ub2)SQLT_STR,
    (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)OCI_DEFAULT),
    (oratext *)"OCIDefByPos2");

  Checkerr (errhp, 
    OCIDefineByPos ((OCIStmt *)stmthp, (OCIDefine **)&def3hp,
    (OCIError *)errhp, (ub4)3, (dvoid *)&(sal), (sb4)salSz, (ub2)SQLT_INT,
    (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)OCI_DEFAULT),
    (oratext *)"OCIDefByPos3");

  if ((status = OCIStmtExecute ((OCISvcCtx *)svchp[idx], (OCIStmt *)stmthp,
                  (OCIError *)errhp, (ub4)0, (ub4)0, (OCISnapshot *)0,
                  (OCISnapshot *)0, (ub4)OCI_DEFAULT)) != OCI_SUCCESS )
  {
    printf ("OCIStmtExecute for SELECT - Fail\n" );
    Checkerr (errhp, status,(oratext *)"Stmt Execute");
  }
  else
  {
    do 
    {
      status = OCIStmtFetch2((OCIStmt *)stmthp, (OCIError *)errhp, (ub4)1,
                 (ub2)OCI_FETCH_NEXT, (sb4)0, (ub4)OCI_DEFAULT);

      if (status == OCI_ERROR || status == OCI_INVALID_HANDLE)
      {
        Checkerr(errhp, status, (oratext *)"OCIStmtFetch2");
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
    OCI_DEFAULT), (oratext *)"StmtRelease");
}

/* - Execute DML statement --------------------------------------------------*/ 
void upd_table(ub4 idx)
{
  OCIStmt *stmthp = (OCIStmt *)0;

  Checkerr (errhp,
    OCIHandleAlloc ((dvoid *)envhp, (dvoid **)&stmthp, (ub4)OCI_HTYPE_STMT, 
    (size_t)0, (dvoid **)0), (oratext *)"OCIHandleAlloc");

  Checkerr (errhp,
    OCIStmtPrepare ((OCIStmt *)stmthp, (OCIError *)errhp, (text *)upd_stmt,
    (ub4)strlen((char *)upd_stmt), (ub4)OCI_NTV_SYNTAX, (ub4)OCI_DEFAULT),
    (oratext *)"OCIStmtPrepare");

  if (status = OCIStmtExecute ((OCISvcCtx *)svchp[idx], (OCIStmt *)stmthp,
                 (OCIError *)errhp, (ub4)1, (ub4)0, (OCISnapshot *)0,
                 (OCISnapshot *)0, (ub4)OCI_COMMIT_ON_SUCCESS))
  {
    printf ("OCIStmtExecute update - Fail\n");
    Checkerr (errhp, status,(oratext *)"Stmt Execute Update");
  }

  Checkerr (errhp,
    OCIHandleFree ((dvoid *)stmthp, (ub4)OCI_HTYPE_STMT),
    (oratext *)"OCIHandleFree");
}

/* - Fetches and prints result cache statistics information -----------------*/ 
void print_stats()
{
  OCISvcCtx *syssvchp = NULL;
  OCIServer *srvhp = NULL;
  OCIStmt *stmthp = (OCIStmt *)0; 
  OCISession *sysauthhp = (OCISession *)0;
  OCIDefine *def1hp = (OCIDefine *)0;
  OCIDefine *def2hp = (OCIDefine *)0;
  OCIDefine *def3hp = (OCIDefine *)0;
  OCIDefine *def4hp = (OCIDefine *)0;
  ub4 statid;
  text name[128];
  ub4 value;
  ub4 cacheid;
  sb4 statidSz = sizeof(statid);
  sb4 nameSz = sizeof (name);
  sb4 valueSz= sizeof(value);
  sb4 cacheidSz = sizeof(cacheid);
  ub4  prefetch = 0;
  OraText *connStr = (text *)"";
  OraText *sysuser = (text *)"sys";
  OraText syspass[128];
  text *StatQuery=
       (text *)"SELECT c1.stat_id, c1.name, c1.value, c1.cache_id \
                FROM client_result_cache_stats$ c1 \
                order by c1.cache_id, c1.stat_id";

  printf ("Please enter password for sys user to continue:\n");
  scanf ("%s", (char *)syspass);

  OCIHandleAlloc ((dvoid *)envhp, (dvoid **)&srvhp, (ub4)OCI_HTYPE_SERVER, 
                  (size_t)0, (dvoid **)0);

  OCIHandleAlloc ((dvoid *)envhp, (dvoid **)&syssvchp, (ub4)OCI_HTYPE_SVCCTX,
                  (size_t)0, (dvoid **)0);

  Checkerr (errhp, 
    OCIServerAttach (srvhp, errhp, (text *)connStr,
    (sb4)strlen((char *)connStr), 0), (oratext *)"OCIServerAttach");

  OCIAttrSet ((dvoid *) syssvchp, OCI_HTYPE_SVCCTX, (dvoid *)srvhp, (ub4) 0,
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
    OCISessionBegin (syssvchp,  errhp, sysauthhp, OCI_CRED_RDBMS,
    (ub4)OCI_SYSDBA), (oratext *)"OCISessionBegin");

  OCIAttrSet ((dvoid *)syssvchp, OCI_HTYPE_SVCCTX, (dvoid *)sysauthhp, (ub4)0,
              OCI_ATTR_SESSION, errhp);

  Checkerr (errhp,
    OCIStmtPrepare2 ((OCISvcCtx *)syssvchp,(OCIStmt **)&stmthp,
    (OCIError *)errhp, (text *)StatQuery, (ub4)strlen((char *)StatQuery),
    (oratext *)NULL, (ub4) 0, (ub4)OCI_NTV_SYNTAX, (ub4)OCI_DEFAULT),
    (oratext *)"OCIStmtPrepare");

  Checkerr(errhp, 
    OCIAttrSet((OCIStmt *) stmthp, OCI_HTYPE_STMT, (dvoid *)&prefetch,
    sizeof(prefetch), OCI_ATTR_PREFETCH_ROWS, (OCIError *)errhp),
    (oratext *) "OCIAttrSet-prefetch");

  Checkerr (errhp,
    OCIDefineByPos ((OCIStmt *)stmthp, (OCIDefine **)&def2hp,
    (OCIError *)errhp, (ub4)1, (dvoid *)&(statid), (sb4)statidSz,
    (ub2)SQLT_INT, (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)OCI_DEFAULT),
    (oratext *)"OCIDefByPos");

  Checkerr (errhp,
    OCIDefineByPos ((OCIStmt *)stmthp, (OCIDefine **)&def3hp, 
    (OCIError *)errhp, (ub4)2, (dvoid *)&(name), (sb4)nameSz, (ub2)SQLT_STR, 
    (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)OCI_DEFAULT),
    (oratext *)"OCIDefByPos");

  Checkerr (errhp, 
    OCIDefineByPos ((OCIStmt *)stmthp, (OCIDefine **)&def4hp,
    (OCIError *)errhp, (ub4)3, (dvoid *)&(value), (sb4)valueSz, (ub2)SQLT_INT,
    (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)OCI_DEFAULT),
    (oratext *)"OCIDefByPos");

  Checkerr (errhp,
    OCIDefineByPos ((OCIStmt *)stmthp, (OCIDefine **)&def1hp,
    (OCIError *)errhp, (ub4)4, (dvoid *)&(cacheid), (sb4)cacheidSz,
    (ub2)SQLT_INT, (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)OCI_DEFAULT),
    (oratext *)"OCIDefByPos");

  /* Fetching the data from the stats table */
  if ((status = OCIStmtExecute ((OCISvcCtx *)syssvchp, (OCIStmt *)stmthp,
                                (OCIError *)errhp, (ub4)0, (ub4)0,
                                (OCISnapshot *)0, (OCISnapshot *)0, 
                                (ub4)OCI_DEFAULT)) != OCI_SUCCESS )
  {
    printf ("OCIStmtExecute for SELECT - Fail\n" );
    Checkerr (errhp, status,(oratext *)"Stmt Execute");
  }
  else
  {
    printf("Contents of CLIENT_RESULT_CACHE_STATS$\n");
    printf("STAT_ID    NAME OF STATISTICS      VALUE  CACHE_ID\n"); 
    printf("=======    ==================      =====  ========\n"); 
    do 
    {
      status = OCIStmtFetch2((OCIStmt *)stmthp, (OCIError *)errhp, (ub4)1,
                             (ub2)OCI_FETCH_NEXT, (sb4)0, (ub4)OCI_DEFAULT);

      if (status == OCI_ERROR || status == OCI_INVALID_HANDLE)
      {
        Checkerr(errhp, status, (oratext *)"OCIStmtFetch2");
        break;
      }
      else if (status != OCI_NO_DATA)
      {
        printf("%5d      %-20s %8d %6d\n", statid, name, value, cacheid); 
      } 
    }while(status !=  OCI_NO_DATA);

  }

  Checkerr (errhp,
    OCIStmtRelease ((OCIStmt *)stmthp, (OCIError *)errhp,(dvoid *)NULL, 0, 
    OCI_DEFAULT), (oratext *)"StmtRelease");

  Checkerr (errhp, 
    OCISessionEnd (syssvchp, errhp, sysauthhp, OCI_DEFAULT),
    (oratext *) "Session-End");

  Checkerr (errhp,
    OCIServerDetach (srvhp, errhp, OCI_DEFAULT),
    (oratext *) "Server-detach");

  OCIHandleFree((dvoid *)sysauthhp, OCI_HTYPE_SESSION);
  OCIHandleFree((dvoid *)syssvchp, OCI_HTYPE_SVCCTX);
  OCIHandleFree((dvoid *)srvhp, OCI_HTYPE_SERVER);
}

/* - Session Logoff --------------------------------------------------------*/
static void dbLogoff ()
{
  int idx;

  printf ("\nLogging off all the connected sessions.\n");

  for (idx=0; idx<2; idx++)
    Checkerr (errhp, 
      OCISessionRelease(svchp[idx], errhp, 0,0, OCI_DEFAULT),
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
