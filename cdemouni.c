/* Copyright (c) 2001, 2002, Oracle Corporation.  All rights reserved.  */

/*

   NAME
     cdemouni.c - a simple program for OCI UTF16 API

   DESCRIPTION
     This is a demo for OCI UTF16 API. 

     UTF16 is the encoding scheme of choice for internationalized 
     application developers, due to the support provided by JAVA
     and MS windows platforms. This demo tries to illustrate the
     fixed width Unicode support for UTF-16 encoded strings.

   EXPORT FUNCTION(S)
     <external functions defined for use outside package - one-line descriptions>

   INTERNAL FUNCTION(S)
     <other external functions defined - one-line descriptions>

   STATIC FUNCTION(S)
     <static functions defined - one-line descriptions>

   NOTES
     <other useful comments, qualifications, etc.>

   MODIFIED   (MM/DD/YY)
   aliu        03/14/02 - add conn to report_error on line 409.
   anzhang     12/04/01 - fix bug 2116117
   anzhang     03/12/01 - Merged anzhang_utf16demo
   anzhang     03/12/01 - Creation

*/

#ifndef ORASTDLIB
# include <stdlib.h>
# define ORASTDLIB
#endif /* !ORASTDLIB */

#ifndef ORASTDIO
# include <stdio.h>
# define ORASTDIO
#endif /* ifndef ORASTDIO */

#ifndef ORASTRING
#include <string.h>
#define ORASTRING
#endif /* ifndef ORASTRING */

#ifndef  OCI_ORACLE
#include <oci.h>
#endif /* ifndef OCI_ORACLE */

/*---------------------------------------------------------------------------
                     PRIVATE TYPES AND CONSTANTS
  ---------------------------------------------------------------------------*/


#define DISCARD (void)

/* database connection context  */
struct db_conn {
  OCIEnv        * envhp;        /* OCI environment handle */
  OCIEnv        * cnvhp;       /* OCI environment handle for conversion */
  OCIServer        * srvhp;        /* OCI server handle */
  OCIError        * errhp;        /* OCI error handle */
  OCISvcCtx        * svchp;        /* OCI service context */
  OCISession        * authp;        /* OCI session handle */
  
  boolean          attached;        /* if server is attached */
  boolean          is_logged_on;        /* if we have logged on to server */
};
typedef struct db_conn db_conn;
                
/*---------------------------------------------------------------------------
                     STATIC FUNCTION DECLARATIONS 
  ---------------------------------------------------------------------------*/

/* Static Function */
static db_conn* db_conn_init();        /* initialize database connection context */

static void        db_conn_free();        /* Environment clean up */

static void        connect();        /* function to initialize connection */

static sb4        ustrlen();        /* calculate length for UTF16-string  */

static void        report_error();        /* print out error message */

static void        checkerr();        /* report error if there is one and exit */

static void     execute();        /* execute a statement */

/* display column data for select statement */
static void        display_column(); 

/* convert a native text to unicode */
static text*        text2unicode();        

/* convert a unicode string to native text */
static text*        unicode2text();        

/* ----------------------------------------------------------------- */
/*             db_conn_init: initialize a connection context         */
/* ----------------------------------------------------------------- */
db_conn*
db_conn_init()
{
  sword retval;
  /* allocate memory */
  db_conn * conn = (db_conn *)calloc(1,sizeof(db_conn));
  if (conn == NULL)
    return NULL;

  conn->envhp = (OCIEnv*)0;
  conn->cnvhp = (OCIEnv*)0;
  conn->srvhp = (OCIServer*)0;
  conn->svchp = (OCISvcCtx*)0;
  conn->authp = (OCISession*)0;
  conn->attached = FALSE;
  conn->is_logged_on = FALSE;

  /* create OCI environment handle */
  if ((retval = OCIEnvCreate((OCIEnv **) &conn->envhp, (ub4) OCI_UTF16,
                             (dvoid *)0,  (dvoid * (*)(dvoid *, size_t)) 0,
                             (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                             (void (*)(dvoid *, dvoid *)) 0, (size_t) 0,
                             (dvoid **) 0)) ||
      (retval = OCIEnvCreate((OCIEnv **) &conn->cnvhp, (ub4) OCI_DEFAULT,
                             (dvoid *)0,  (dvoid * (*)(dvoid *, size_t)) 0,
                             (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                             (void (*)(dvoid *, dvoid *)) 0, (size_t) 0,
                             (dvoid **) 0)))
  {
    DISCARD fprintf(stdout, "FAILED: OCIEnvCreate() retval = %d\n", retval);
    exit(OCI_ERROR);
  }

  /* allocation of service context handle */
  if (retval = OCIHandleAlloc((dvoid *) conn->envhp, (dvoid **) &conn->svchp,
                        (ub4) OCI_HTYPE_SVCCTX, (size_t) 0, (dvoid **) 0))
  {
    DISCARD fprintf(stdout, "FAILED: OCIHandleAlloc() on svchp, RC = %d\n", 
                    retval);
    db_conn_free(conn);
    exit(OCI_ERROR);
  }

  /* allocate error handle */
  if (retval = OCIHandleAlloc((dvoid *) conn->envhp, (dvoid **) &conn->errhp,
                        (ub4) OCI_HTYPE_ERROR, (size_t) 0, (dvoid **) 0))
  {
    DISCARD fprintf(stdout, "FAILED: OCIHandleAlloc() on errhp, RC = %d\n", 
                    retval);
    db_conn_free(conn);
    exit(OCI_ERROR);
  }

  /* allocate server handle */
  if (retval = OCIHandleAlloc((dvoid *) conn->envhp, (dvoid **) &conn->srvhp,
                        (ub4) OCI_HTYPE_SERVER, (size_t) 0, (dvoid **) 0))
  {
    DISCARD fprintf(stdout, "FAILED: OCIHandleAlloc() on srvhp, RC = %d\n", 
                    retval);
    db_conn_free(conn);
  }

  /* allocation of session handle */
  if (retval = OCIHandleAlloc((dvoid *) conn->envhp, (dvoid **) &conn->authp,
                        (ub4) OCI_HTYPE_SESSION, (size_t) 0, (dvoid **) 0))
  {
    DISCARD fprintf(stdout, "FAILED: OCIHandleAlloc() on authp, RC = %d\n", 
                    retval);
    db_conn_free(conn);
  }

  return conn;
}

/* ----------------------------------------------------------------- */
/*         db_conn_free: Free a connect context, clean up            */
/* ----------------------------------------------------------------- */
void
db_conn_free(conn)
     db_conn * conn;
{
  /* log off the user if we are logged on */
  if (conn->is_logged_on)
    DISCARD OCISessionEnd(conn->svchp, conn->errhp, conn->authp, (ub4) 0);

  /* detach server */
  if (conn->attached)
    DISCARD OCIServerDetach(conn->srvhp, conn->errhp, (ub4) OCI_DEFAULT);
  
  /* free server handle */
  if (conn->srvhp)
    DISCARD OCIHandleFree((dvoid *) conn->srvhp, (ub4) OCI_HTYPE_SERVER);
  /* free service context */
  if (conn->svchp)
    DISCARD OCIHandleFree((dvoid *) conn->svchp, (ub4) OCI_HTYPE_SVCCTX);
  /* free error handle */
  if (conn->errhp)
    DISCARD OCIHandleFree((dvoid *) conn->errhp, (ub4) OCI_HTYPE_ERROR);
  /* free environment handle */
  if (conn->authp)
    DISCARD OCIHandleFree((dvoid *) conn->authp, (ub4) OCI_HTYPE_SESSION);
  /* deallocate connect object */
  free(conn);
}

/* ----------------------------------------------------------------- */
/* initialize environment, allocate handles, make connection, etc.   */ 
/* ----------------------------------------------------------------- */
void 
connect(conn,dblink,username,password)
     db_conn        *conn;                /* database connection */
     text        *dblink;        /* Connection string */
     text        *username;        /* username */
     text        *password;        /* password */
{
  sword retval;                        /* return value of OCI funcs */

  /* log on to server */
  if (retval = OCIServerAttach(conn->srvhp, conn->errhp, dblink,
                               ustrlen((void *)dblink)*sizeof(utext), OCI_DEFAULT))
  {
    DISCARD fprintf(stdout, "FAILED: OCIServerAttach(), RC = %d\n", retval);
    db_conn_free(conn);
    exit(OCI_ERROR);
  }

  conn->attached = TRUE;

  checkerr(conn, OCIAttrSet((dvoid *) conn->svchp, (ub4) OCI_HTYPE_SVCCTX,
                              (dvoid *) conn->srvhp, (ub4) 0,
                              (ub4) OCI_ATTR_SERVER, conn->errhp));

  checkerr(conn, OCIAttrSet((dvoid *) conn->authp, (ub4) OCI_HTYPE_SESSION,
                              (dvoid *) username, (ub4) ustrlen((void *)username)*sizeof(utext),
                              (ub4) OCI_ATTR_USERNAME, conn->errhp));

  checkerr(conn, OCIAttrSet((dvoid *) conn->authp, (ub4) OCI_HTYPE_SESSION,
                              (dvoid *) password, (ub4) ustrlen((void *)password)*sizeof(utext),
                              (ub4) OCI_ATTR_PASSWORD, conn->errhp));

  /* log on to server */
  checkerr(conn, OCISessionBegin(conn->svchp, conn->errhp,conn->authp, 
                                   OCI_CRED_RDBMS, OCI_DEFAULT));
  conn->is_logged_on = TRUE;

  checkerr(conn, OCIAttrSet((dvoid *) conn->svchp, (ub4) OCI_HTYPE_SVCCTX,
                              (dvoid *) conn->authp, (ub4) 0,
                              (ub4) OCI_ATTR_SESSION, conn->errhp));
}

/* ----------------------------------------------------------------- */
/*        display_column: display the data after execution           */
/* ----------------------------------------------------------------- */
void 
display_column(stmhp, conn)
  OCIStmt *stmhp;
  db_conn *conn;
{
  text    *pcoln[20];                /* column name: OCI_ATTR_NAME */
  ub2     pcoll[20];                /* column size: OCI_ATTR_DATA_SIZE */
  ub2     podt[20];                /* data type:  OCI_ATTR_DATA_TYPE */
  ub1     isnull[20];                /* is this column NULL? */
  ub4     namelen[20];
 
  eword   i, pos;                /* iterators */
  ub4     parmcnt = 0;                /* parameter count */
  sword   retval = 0;                /* return value from OCI functions */
  OCIParam *parmdp;                /* a parameter handle */
 
  OCIDefine *dfnp[20];           /* define handle pointer */
  text *column[20];                /* buffers to hold data fetched */
 
  memset((void *) pcoln, 0, 20 * sizeof(text *));
  /* get parameter count */
  checkerr(conn, OCIAttrGet((dvoid *) stmhp, (ub4)OCI_HTYPE_STMT, 
                              (dvoid *) &parmcnt,
                              (ub4 *) 0, (ub4)OCI_ATTR_PARAM_COUNT, conn->errhp));
 
  /* iterate every column, display data */
  for (pos = 1; pos <= parmcnt; pos++)
  {
    retval = OCIParamGet((dvoid *)stmhp, (ub4)OCI_HTYPE_STMT, conn->errhp,
                      (dvoid **)&parmdp, (ub4) pos );

    if (retval)
    {
      DISCARD fprintf(stdout,"OCIParamGet RC=%d, position=%d\n", retval, pos);
      continue;
    }
 
    /* get the column name */
    checkerr(conn, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                                (dvoid*) &pcoln[pos-1],
                                (ub4 *) &namelen[pos-1], (ub4) OCI_ATTR_NAME,
                                (OCIError *) conn->errhp));
 
    /* get the data size */
    checkerr(conn, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                                (dvoid*) &pcoll[pos-1],
                                (ub4 *) 0, (ub4) OCI_ATTR_DATA_SIZE,
                                (OCIError *) conn->errhp));

    /* get the data type */
    checkerr(conn, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                                (dvoid*) &podt[pos-1],
                                (ub4 *) 0, (ub4) OCI_ATTR_DATA_TYPE,
                                (OCIError *) conn->errhp));
 
    /* is column null */
    checkerr(conn, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                                (dvoid*) &isnull[pos-1],
                                (ub4 *) 0, (ub4) OCI_ATTR_IS_NULL,
                                (OCIError *) conn->errhp));
 
  }
 
  for (i = 1; i <= parmcnt; i++)
  {
    /* print out the column name */
    DISCARD fprintf(stdout, "%-*s ", pcoll[i-1],
                    unicode2text(conn, (void *) pcoln[i-1], 
                                 ustrlen ((void *)pcoln[i-1])));
    column[i-1] = (text *) calloc( pcoll[i-1] + 1, sizeof(utext));
 
    checkerr(conn, OCIDefineByPos(stmhp, &dfnp[i-1], conn->errhp, (ub4)i,
                                    (dvoid *)column[i-1],
                                    (sb4)((pcoll[i-1]+1)*2),(ub2)SQLT_STR,
                                    (dvoid *)0, (ub2 *)0, (ub2 *)0,
                                    (ub4)OCI_DEFAULT));
  }
  DISCARD fprintf(stdout, "\n========================================\n");

  /* since we define after execute, we need to fetch */
  checkerr(conn, OCIStmtFetch(stmhp, conn->errhp, (ub4) 1,  (ub4) OCI_FETCH_NEXT, 
                              (ub4) OCI_DEFAULT));
  do
  { 
    for (i = 0; i < parmcnt; i++) 
    {
      if (ustrlen((void *)column[i]) != 0)
      {
        /* display the data */
        DISCARD fprintf(stdout, "%-*s ", pcoll[i],
                        unicode2text(conn, (void *)column[i],
                                     ustrlen((void *)column[i])));
        /* clean up data */
        memset(column[i], 0, (pcoll[i] + 1)*sizeof(utext)); 
      }
    }
    DISCARD fprintf(stdout, "\n");
  }
  /* continue to fetch */
  while ((retval = OCIStmtFetch(stmhp, conn->errhp, (ub4) 1,  (ub4) OCI_FETCH_NEXT, 
                                (ub4) OCI_DEFAULT)) == OCI_SUCCESS ||
         retval == OCI_SUCCESS_WITH_INFO); 


  /* Ignore NO DATA FOUND error */
  if (retval != OCI_NO_DATA)
  {
    checkerr(conn,retval);
  }
}

/* ----------------------------------------------------------------- */
/*            execute: parse and execute a sql statement             */
/* ----------------------------------------------------------------- */

void 
execute(conn,stmt,func)
     db_conn        *conn;
     text        *stmt;
     dvoid        (*func)(OCIStmt * stmhp, db_conn * conn);
{
  sword retval;
  OCIStmt * stmtp;

  /* allocate a statement handle */
  if ( retval = OCIHandleAlloc((dvoid *)conn->envhp, (dvoid **)&stmtp, 
                                    OCI_HTYPE_STMT, 0, (dvoid **) 0) )
  {
    DISCARD fprintf(stdout, "FAILED: OCIHandleAlloc() on stmtp, RC = %d\n", 
                retval);
    db_conn_free(conn);
    exit(OCI_ERROR);
  }

  /* Parse the statement */
  if (retval = OCIStmtPrepare (stmtp, conn->errhp, (CONST text *) stmt, 
                               (ub4) ustrlen((void *)stmt)*sizeof(utext),
                               OCI_NTV_SYNTAX, OCI_DEFAULT))
  {
    DISCARD fprintf(stdout, "FAILED: OCIStmtPrepare() on stmtp, RC = %d\n", 
                retval);
    report_error(conn);
    db_conn_free(conn);
    exit(OCI_ERROR);
  }
 
  if ( retval = OCIStmtExecute (conn->svchp, stmtp, conn->errhp, 
                                0, 0, (const OCISnapshot *) 0, (OCISnapshot *) 0, OCI_DEFAULT))
  {
    report_error(conn);
    db_conn_free(conn);
  }

  if (func != NULL)
    (*func)(stmtp, conn);

  /* free the statement handle */
  if (stmtp)
    DISCARD OCIHandleFree((dvoid *) stmtp, (ub4) OCI_HTYPE_STMT);

}

/* ----------------------------------------------------------------- */
/*        ustrlen: calculate the length of UTF16 string              */
/* ----------------------------------------------------------------- */

sb4 
ustrlen(vustr)
     void * vustr;
{
  utext *ustr = (utext *)vustr;

  utext * p = (utext*) ustr;
  while (*p)
    p++;
  return (sb4)(p - ustr);
}

/* ----------------------------------------------------------------- */
/*         report_error: get error code and message                  */
/* ----------------------------------------------------------------- */
void 
report_error(conn)
     db_conn * conn;
{
  text  msgbuf[1024];
  sb4   errcode;
 
  DISCARD OCIErrorGet ((dvoid *)conn->errhp, (ub4) 1, (text *) NULL, &errcode,
                       msgbuf, (ub4) sizeof(msgbuf), (ub4) OCI_HTYPE_ERROR);
  
  DISCARD fprintf(stdout,(const char*)
                  unicode2text(conn,(void*)msgbuf,1024));
}

/* ----------------------------------------------------------------- */
/*    checkerr: print error message and exit if OCI func failed      */
/* ----------------------------------------------------------------- */
void checkerr(conn, status)
     db_conn *conn;
     sword status;
{
  text errbuf[512];
  sb4 errcode = 0;

  switch (status)
  {
  case OCI_SUCCESS:
    return;
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
    report_error(conn);
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

  db_conn_free(conn);
  exit(1);
}

/* ----------------------------------------------------------------- */
/*        text2unicode: convert a string to UTF16 encoding           */
/* ----------------------------------------------------------------- */
text* text2unicode(conn,src,srclen)
     db_conn * conn;
     const text* src;
     size_t srclen;
{
  text* result = NULL;
  size_t resultlen = (srclen+1)*sizeof(utext);
  void *temp;

  result = (text*)calloc(resultlen,sizeof(text));
  temp = (void *)result;

  if (OCICharSetToUnicode((dvoid *)conn->cnvhp, (ub2 *)temp,
                          resultlen, src, srclen, NULL))
  {
    fprintf(stderr,"OCICharSetToUnicode failed\n");
    free(temp);
    return NULL;
  }
  result = (text *)temp;

  return result;
}

/* ----------------------------------------------------------------- */
/*           unicode2text: convert a given UTF16 text                */
/*                           string to native encoding                 */
/* ----------------------------------------------------------------- */
text* unicode2text(conn,src,srclen)
     db_conn * conn;
     void* src;
     size_t srclen; 
{
  text *result = NULL;
  size_t resultlen = ustrlen((void *)src) + 1;
  result = (text*)calloc(resultlen,sizeof(text));

  if (OCIUnicodeToCharSet((dvoid*)conn->cnvhp,result,
                          resultlen, (CONST ub2 *)src, srclen, NULL))
  {
    fprintf(stderr,"OCIUnicodeToCharSet failed\n");
    free(result);
    return NULL;
  }
  
  return result;
}

#define LINK ""
#define USER "hr"
#define PASS "hr"
#define SQLS "SELECT FIRST_NAME, LAST_NAME FROM EMPLOYEES"
                                        
/* ----------------------------------------------------------------- */
/*                         main routine                              */
/* ----------------------------------------------------------------- */
int main()
{
  db_conn *conn = NULL;                /* db connection object */
  text *user = NULL;                /* username */
  text *pass = NULL;                /* password */
  text *dblink = NULL;                /* database descriptor */
  text *sqlstmt = NULL;                /* sql statement to be excuted */

  conn = db_conn_init();        /* initilalize the connect object */
 
  /* convert the all text strings to UTF16 encoding */
  dblink  = text2unicode(conn,(text*)LINK, strlen(LINK));
  user    = text2unicode(conn,(text*)USER, strlen(USER));
  pass    = text2unicode(conn,(text*)PASS, strlen(PASS));
  sqlstmt = text2unicode(conn,(text*)SQLS, strlen(SQLS));

  /* connect to database */
  connect(conn,dblink,user,pass);
  execute(conn,sqlstmt,display_column);

  /* clean up */
  db_conn_free(conn);
  free(dblink);
  free(user);
  free(pass);
  free(sqlstmt);

  return OCI_SUCCESS;
}


/* end of file cdemouni.c */
