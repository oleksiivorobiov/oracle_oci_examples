/* Copyright (c) 2001, 2002, Oracle Corporation.  All rights reserved.  */
/*
   NAME
     cdemostc.c- OCI Statement caching functionality - basic demo

   DESCRIPTION
    This program assumes that sample HR schema is setup. 
    Does the following steps:
    o prepare a statement
    o tag it 
    o retag the same statement and 
    o retreive the statement from the cache.

   MODIFIED  (MM/DD/YY)
   sprabhak   12/10/02 - Changed the mode to statement cache
   sudsrini   01/30/02 - Rename ocisc -> cdemostc
   gkirsur    01/25/02 - Merged gkirsur_stcache_tests
   sudsrini   01/24/02 - Removed argc and argv as its not being used
   sprabhak   01/22/02  -Incorporated review comments
   sprabhak   12/15/01  - Modified
   schandir   12/06/01  - Creation

*/
#ifndef OCISC_ORACLE
# include <cdemostc.h>
#endif
 
static OCIError    *errhp;
static OCIEnv      *envhp;
static  OCIServer   *svrhp = (OCIServer *)0;
static  OCISession  *sesnhp = (OCISession *)0;
static  OCISvcCtx   *svchp = (OCISvcCtx *)0;
static  OCIStmt     *stmthp = (OCIStmt *)0;

static CONST OraText *database = (OraText *)"";
static CONST OraText *username = (OraText *)"hr";
static CONST OraText *password = (OraText *)"hr"; 

 
int main(void)
{
  int i = 0;
  sword lstat;

  OCIEnvCreate ((OCIEnv **)&envhp, (ub4)OCI_THREADED, (dvoid *)0,
    (dvoid * (*) (dvoid *, size_t))0, (dvoid * (*)(dvoid *, dvoid *, size_t))0,
    (void (*)(dvoid *, dvoid *))0, (size_t)0, (dvoid **)0);

  (void) OCIHandleAlloc((dvoid *) envhp, (dvoid **) &errhp, OCI_HTYPE_ERROR,
                     (size_t) 0, (dvoid **) 0);
   lstat = OCILogon2(envhp, errhp, &svchp, username
            , (ub4)strlen ((char *)username),password
            , (ub4)strlen ((char *)password), database
            , (ub4)strlen((char *)database),OCI_LOGON2_STMTCACHE);

  if (lstat)
     checkerr(errhp, lstat);

  queryRows(svchp);

  /* close connection */
  lstat = OCILogoff(svchp, errhp);
  if (lstat)
      checkerr(errhp, lstat);

  checkerr(errhp, OCIHandleFree((dvoid *)errhp, OCI_HTYPE_ERROR));

  return 0;
} /* end of main () */


/* Displays the contents of EMP table */
static void queryRows(OCISvcCtx *svchp) 
{
  sword status = 0;
  static text *upd=(text *)
        "update employees set salary = 20000 WHERE employee_id = 10";
  int i;
  static CONST text *tag = (text *)"tagA";
  static CONST text *tag1 =(text *)"tagB";

  /* Calling OCIStmtPrepare2 with a SQLtext specified */

  status= OCIStmtPrepare2 ((OCISvcCtx *)svchp,(OCIStmt **)&stmthp, 
            (OCIError *)errhp, (text *)upd, (ub4)strlen((char *)upd), 
            NULL,0,OCI_NTV_SYNTAX,OCI_DEFAULT);

  if (status != OCI_SUCCESS )
  {
     printf("OCIStmtPrepare2 Failure...\n");
     checkerr(errhp, status);
  }
  else
  {
     printf("OCIStmtPrepare2 Success...\n");
  }

  /*Tag the statement prepared*/
  status=OCIStmtRelease ((OCIStmt *)stmthp, (OCIError *)errhp,tag ,
     (ub4)strlen((char *)tag),OCI_DEFAULT);

  if ( status != OCI_SUCCESS )
  {
     printf("OCIStmtRelease Failure...\n");
     checkerr(errhp, status);
  }
  else
  {
     printf("OCIStmtRelease Success...\n");
  }
  status= OCIStmtPrepare2 ((OCISvcCtx *)svchp,(OCIStmt **)&stmthp, 
        (OCIError *)errhp, (text *)upd, (ub4)strlen((char *)upd), tag,
        (ub4)strlen((char *)tag), OCI_NTV_SYNTAX, OCI_DEFAULT);

  if ( status != OCI_SUCCESS )
  {
     printf("OCIStmtPrepare2 Failure...\n");
     checkerr(errhp, status);
  }
  else
  {
     printf("OCIStmtPrepare2 Success...\n");
  }

  /*Retag the statement with a different tag*/
  status=OCIStmtRelease ((OCIStmt *)stmthp, (OCIError *)errhp,tag1,
    (ub4)strlen((char *)tag1), OCI_DEFAULT);
  if ( status != OCI_SUCCESS )
  {
     printf("OCIStmtRelease Failure...\n");
     checkerr(errhp, status);
  }
  else
  {
     printf("OCIStmtRelease Success...\n");
  }
  /*Trying to get the statement with the tag */
  status= OCIStmtPrepare2 ((OCISvcCtx *)svchp,(OCIStmt **)&stmthp, 
        (OCIError *)errhp, (text *)upd, (ub4)strlen((char *)upd), tag1,
        (ub4)strlen((char *)tag1), OCI_NTV_SYNTAX, OCI_DEFAULT);

  if ( status != OCI_SUCCESS )
  {
     printf("OCIStmtPrepare2 Failure...\n");
     checkerr(errhp, status);
  }
  else
  {
     printf("OCIStmtPrepare2 Success...\n");
  }
  /*Now releasing the stmt back to the cache*/
  status=OCIStmtRelease ((OCIStmt *)stmthp, (OCIError *)errhp,tag1,
    (ub4)strlen((char *)tag1), OCI_DEFAULT);
  if ( status != OCI_SUCCESS )
  {
     printf("OCIStmtRelease Failure...\n");
     checkerr(errhp, status);
  }
  else
  {
     printf("OCIStmtRelease Success...\n");
  }
 

} /* end of queryRows() */


/* This function prints the error */
void checkerr(OCIError *errhp,sword  status)
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

