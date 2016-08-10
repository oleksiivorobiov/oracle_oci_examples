#ifdef RCSID
static char *RCSid =
   "$Header: cdemoupk.c 15-feb-2007.10:21:02 aliu Exp $ ";
#endif /* RCSID */

/* Copyright (c) 2007, Oracle. All rights reserved.  */

/*

   NAME
     cdemoupk.c - User callbacks demo program with multiple packages.

   DESCRIPTION
     This programs tests loading of multiple packages.

     The dynamic callback files cdemoup1.c and cdemoup2.c need
     to be linked into shared libraries using the make file provided,
     see comments in cdemoup1.c for more details.

     Callback "chaining" is demonstrated as follows: If
     ORA_OCI_UCBPKG is set then the dynamic callbacks are
     registered first for OCIStmtPrepare. The environment
     variable can be set to upto 5 package names, delimited
     by semi-colons.

   PUBLIC FUNCTION(S)
     None

   PRIVATE FUNCTION(S)
     As described below

   RETURNS

   NOTES
     Multiple packages can be specified by setting the
     environment variable ORA_OCI_UCBPKG as follows:

        setenv ORA_OCI_UCBPKG  "cdemoup1;cdemoup2"

     The above setting specifies two packages are to be used.
     A maximum of 5 packages can be specified separated by
     semi-colons.

   MODIFIED   (MM/DD/YY)
   aliu        02/15/07 - fix lrg 2863937: initialize variables
   mpjoshi     11/04/99 - do not use wrapper functions
   svedala     10/20/99 - Creation

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <oci.h>
#include <stdarg.h>

static text *username = (text *) "CDEMOUPK";
static text *password = (text *) "CDEMOUPK";

static text *crstmt = (text *) "CREATE TABLE TAB1 (COL1 VARCHAR2(40))";

static OCIEnv *envhp;
static OCIServer *srvhp;
static OCIError *errhp;
static OCISvcCtx *svchp;
static OCIStmt *stmthp;
static OCISession *authp = (OCISession *) 0;

struct stat_ctx_struct
{
  OCIUserCallback *entry_funcptr;
  dvoid *entry_ctxptr;
  OCIUserCallback *exit_funcptr;
  dvoid *exit_ctxptr;
};

typedef struct stat_ctx_struct  stat_ctx_struct;

static void checkerr(/*_ OCIError *errhp, sword status _*/);
static void cleanup(/*_ void _*/);

sword stmtprep_entry_statcbk_fn (dvoid *ctxp, dvoid *hndlp, ub4 type,
                                 ub4 fcode, ub4 when, sword returnCode,
                                 sb4 *errnop, va_list arglist);

sword stmtprep_exit_statcbk_fn (dvoid *ctxp, dvoid *hndlp, ub4 type,
                                ub4 fcode, ub4 when, sword returnCode,
                                sb4 *errnop, va_list arglist);


int main(/*_ int argc, char *argv[] _*/);

static sword status;

int main(argc, argv)
int argc;
char *argv[];
{

  static ub4 pos;
  ub4 attrval;
  ub4 attrsiz;
  text errbuf[512];
  sb4 errcode = 0;
  text sversion[512];

  stat_ctx_struct  *static_context = (stat_ctx_struct *) 0;

  (void) printf("Initializing and Connecting\n\n");

  if (init_handles())
  {
    (void) printf("FAILED: init_handles()\n");
    return OCI_ERROR;
  }

  if (log_on())
  {
    (void) printf("FAILED: log_on()\n");
    return OCI_ERROR;
  }

  if (status = OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &stmthp,
           OCI_HTYPE_STMT, (size_t) 0, (dvoid **) 0))
  {
    checkerr(errhp, status);
    cleanup();
    return OCI_ERROR;
  }


  /* Register the static callback routine "stmtprep_entry_statcbk_fn"  */
  /* for OCIStmtPrepare, entry-time. This will overwrite the           */
  /* of entry dynamic callback if one was registered                   */

  if (OCIUserCallbackRegister(envhp, OCI_HTYPE_ENV, errhp,
                              stmtprep_entry_statcbk_fn, static_context,
                              OCI_FNCODE_STMTPREPARE, OCI_UCBTYPE_ENTRY,
                              (OCIUcb *)0))
  {
    checkerr(errhp, status);
    cleanup();
    return OCI_ERROR;
  }


  /* Register the exit static callback function for OCIStmtPrepare */

  if (OCIUserCallbackRegister(envhp, OCI_HTYPE_ENV, errhp,
                              stmtprep_exit_statcbk_fn, static_context,
                              OCI_FNCODE_STMTPREPARE, OCI_UCBTYPE_EXIT,
                              (OCIUcb *)0))
  {
    checkerr(errhp, status);
    cleanup();
    return OCI_ERROR;
  }

  if (status = OCIStmtPrepare(stmthp, errhp, crstmt,
                                (ub4) strlen((char *) crstmt),
                                (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    checkerr(errhp, status);
    cleanup();
    return OCI_ERROR;
  }

  if ((status = OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
           (CONST OCISnapshot *) NULL, (OCISnapshot *) NULL, OCI_DEFAULT))
      && (status != OCI_NO_DATA))
  {
    checkerr(errhp, status);
    cleanup();
    return OCI_ERROR;
  }

  if (status = OCITransCommit(svchp, errhp, 0))
  {
    checkerr(errhp, status);
    cleanup();
    return OCI_ERROR;
  }


  if (status = OCISessionEnd(svchp, errhp, authp, (ub4) 0))
  {
    checkerr(errhp, status);
    cleanup();
    return OCI_ERROR;
  }

  (void) printf("\nDetach and deallocate handles\n");

  if (status = OCIServerDetach( srvhp, errhp, OCI_DEFAULT))
  {
    checkerr(errhp, status);
    cleanup();
    return OCI_ERROR;
  }

  cleanup();
}


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


/* ----------------------------------------------------------------- */
/* initialize environment, allocate handles, etc.                    */
/* ----------------------------------------------------------------- */

sb4 init_handles()
{
  if (OCIInitialize((ub4) OCI_DEFAULT, (dvoid *)0,
                    (dvoid * (*)(dvoid *, size_t)) 0,
                    (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                    (void (*)(dvoid *, dvoid *)) 0 ))
  {
    (void) printf("FAILED: OCIInitialize()\n");
    return OCI_ERROR;
  }

  /* initialize environment handle */
  if (OCIEnvInit((OCIEnv **) &envhp, (ub4) OCI_DEFAULT,
                 (size_t) 0, (dvoid **) 0 ))
  {
    (void) printf("FAILED: OCIEnvInit()\n");
    return OCI_ERROR;
  }

  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &svchp,
                     (ub4) OCI_HTYPE_SVCCTX, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc()\n");
    return OCI_ERROR;
  }

  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &errhp,
                     (ub4) OCI_HTYPE_ERROR, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc()\n");
    return OCI_ERROR;
  }

  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &stmthp,
                     (ub4) OCI_HTYPE_STMT, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc()\n");
    return OCI_ERROR;
  }

  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &srvhp,
                     (ub4) OCI_HTYPE_SERVER, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc()\n");
    return OCI_ERROR;
  }

  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &authp,
                     (ub4) OCI_HTYPE_SESSION, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc()\n");
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}



/* ----------------------------------------------------------------- */
/* attach to the server and log on                                   */
/* ----------------------------------------------------------------- */

sb4 log_on()
{
  text *uid = (text *)"CDEMOUPK";
  text *pwd = (text *)"CDEMOUPK";
  text *cstring = (text *) "";

  /* attach to the server */
  if (OCIServerAttach(srvhp, errhp, (text *) cstring,
                     (sb4) strlen((char *)cstring), (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIServerAttach()\n");
    return OCI_ERROR;
  }

  if (OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION,
                 (dvoid *) uid, (ub4) strlen((char *)uid),
                 (ub4) OCI_ATTR_USERNAME, errhp))
  {
    (void) printf("FAILED: OCIAttrSet()\n");
    return OCI_ERROR;
  }

  if (OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION,
                 (dvoid *) pwd, (ub4) strlen((char *)pwd),
                 (ub4) OCI_ATTR_PASSWORD, errhp))
  {
    (void) printf("FAILED: OCIAttrSet()\n");
    return OCI_ERROR;
  }

  /* set the server attribute in the service context */
  if (OCIAttrSet((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
                 (dvoid *) srvhp, (ub4) 0, (ub4) OCI_ATTR_SERVER, errhp))
  {
    (void) printf("FAILED: OCIAttrSet()\n");
    return OCI_ERROR;
  }

  /* log on */
  if (OCISessionBegin(svchp, errhp, authp, (ub4) OCI_CRED_RDBMS,
                     (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCISessionBegin()\n");
    return OCI_ERROR;
  }

  /* set the session attribute in the service context */
  if (OCIAttrSet((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX, (dvoid *) authp,
                 (ub4) 0, (ub4) OCI_ATTR_SESSION, errhp))
  {
    (void) printf("FAILED: OCIAttrSet()\n");
    return OCI_ERROR;
  }

  return OCI_SUCCESS;

}


/* ----------------------------------------------------------------- */
/* Exit program with an exit code.                                   */
/* ----------------------------------------------------------------- */
void cleanup()
{
  sword status;

  if (stmthp)
  {
    if (status = OCIHandleFree((dvoid *) stmthp, OCI_HTYPE_STMT))
    {
        (void) printf("Status = %d\n", status);
        checkerr(errhp, status);
    }
  }

  if (srvhp)
  {
    if (status = OCIHandleFree((dvoid *) srvhp, OCI_HTYPE_SERVER))
    {
        (void) printf("Status = %d\n", status);
        checkerr(errhp, status);
    }
  }

  if (errhp)
  {
    (void) OCIHandleFree((dvoid *) errhp, OCI_HTYPE_ERROR);
  }
  return;
}



/* ----------------------------------------------------------------- */
/* Entry callback function for OCIStmtPrepare. This function is      */
/* statically registered (i.e. from within this file).  Since, it    */
/* uses (via va_arg) the parametes passed to it, it calls a wrapper  */
/* function that recreates the arglist to be passed to the dynamic   */
/* callback.                                                         */
/* ----------------------------------------------------------------- */
sword stmtprep_entry_statcbk_fn (dvoid *ctxp, dvoid *hndlp, ub4 type,
                                 ub4 fcode, ub4 when, sword returnCode,
                                 sb4 *errnop, va_list arglist)
{
  stat_ctx_struct *loc_ctx;
  OCIStmt *stmthp;
  OCIError *errhp;
  text *sqlstmt;
  ub4   stmtlen;
  ub4   language;
  ub4   mode;
  sword retCode = OCI_CONTINUE;


  /* Retrieve all arguments of OCIStmtPrepare and save   */
  /* them so they can be passed on to dynamic-registered */
  /* callback if one was registered                      */

  stmthp   = va_arg(arglist, OCIStmt *);
  errhp    = va_arg(arglist, OCIError *);
  sqlstmt  = va_arg(arglist, text *);
  stmtlen  = va_arg(arglist, ub4);
  language = va_arg(arglist, ub4);
  mode     = va_arg(arglist, ub4);

  printf("In static entry callback routine for OCIStmtPrepare:\n");
  printf("sql_stmt = [%s]\n\n", sqlstmt);


  return retCode;
}


/* ----------------------------------------------------------------- */
/* Exit callback function (static) for OCIStmtPrepare.  Since it     */
/* does not utilize any parameters in the arglist, it can pass       */
/* the arglist directly to the dynamic callback.                     */
/* ----------------------------------------------------------------- */
sword stmtprep_exit_statcbk_fn (dvoid *ctxp, dvoid *hndlp, ub4 type,
                                ub4 fcode, ub4 when, sword returnCode,
                                sb4 *errnop, va_list arglist)
{
  stat_ctx_struct *loc_ctx;
  sword retCode = OCI_CONTINUE;


  printf("\nIn static exit callback routine for OCIStmtPrepare\n");

  return retCode;
}



/* end of file cdemoupk.c */

