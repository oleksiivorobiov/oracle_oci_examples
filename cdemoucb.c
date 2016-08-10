#ifdef RCSID
static char *RCSid =
   "$Header: cdemoucb.c 14-jul-99.13:18:11 mjaeger Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1998, 1999, Oracle Corporation.  All rights reserved.
*/

/*

   NAME
     cdemoucb.c - User callback demo program

   DESCRIPTION
     This program demonstrates using user callbacks. Both static
     and dynamic callbacks are demonstrated. Passing of
     arguments to callback routine is also demostrated.

     The program executes a create table command. Callback
     routine is registered for OCIStmtPrepare(), in the comments
     below OCIStmtPrepare() is mentioned but same applies to all
     calls for which callbacks can be registered.

     To use dynamic callback file cdemoucbl.c needs to be linked
     into a shared library using the make file provided, see
     comments in cdemoucbl.c for more details.

     If environment variable ORA_OCI_UCBPKG is set then routine
     OCIEnvCallback in cdemoucbl.c is called, which registers
     the callback function for OCIStmtPrepare().

     All arguments of routine OCIStmtPrepare are also passed to
     callback routines, this is verified by doing a va_start
     and then acquiring and printing sqlstmt passed to
     OCIStmtPrepare().

     Callback "chaining" is demonstrated as follows: If
     ORA_OCI_UCBPKG is set then the dynamic callbacks are
     registered first for OCIStmtPrepare. Calling the function
     OCIUserCallbackGet returns pointers to the registered
     callbacks. These pointers are saved and then static callback
     functions are registered for OCIStmtPrepare.  Since
     registering another entry and exit callback functions for
     OCIStmtPrepare will overwrite the previously registered
     dynamic callback functions, so the saved pointers are used to
     make calls to the dynamic functions.

     Note that the last argument to user callbacks is an arglist.
     Therefore, for callback chaining, a wrapper function is needed
     to simulate a function with variable number of parameters.
     The wrapper function takes all the parameters that have been
     passed to the static callback.  Since, it is a function with
     variable number of parametes, it can create the arglist that is
     to be passed to the dynamic callback.  This is demonstrated
     in the entry callback function.

     In the special case where the static callback does not need the
     parameters passed to it, the static callback can simply
     pass the arglist directly to the dynamic callback.  This is
     demonstrated in the exit callback function.

     This program will also work in "static-only" mode. In this
     case dynamic callback function will not get registered and
     this program will know that after call to
     OCIUserCallbackGet and will not attempt to call the
     dynamic callback function.

   PUBLIC FUNCTION(S)

   PRIVATE FUNCTION(S)

   RETURNS

   NOTES

   MODIFIED   (MM/DD/YY)
   slari       08/30/99 - add OCIUcb *
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   slari       11/22/98 - add wrapper for chained callback
   slari       11/21/98 - conform to new OCIUserCallback with arglist
   svedala     10/13/98 - Creation

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <oci.h>
#include <stdarg.h>

static text *username = (text *) "CDEMOUCB";
static text *password = (text *) "CDEMOUCB";

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

sword stmtprep_wrapper_cbk_fn (OCIUserCallback *funcptr, dvoid *ctxp, dvoid
                               *hndlp, ub4 type, ub4 fcode, ub4 when,
                               sword returnCode, sb4 *errnop, ...);


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

  stat_ctx_struct  *static_context;

  OCIUserCallback dyn_entry_funcptr;
  dvoid *dyn_entry_ctxptr;

  OCIUserCallback dyn_exit_funcptr;
  dvoid *dyn_exit_ctxptr;

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


  /* Call OCIUserCallbackGet() to check if an entry callback */
  /* is registered for OCIStmtPrepare. If a callback was     */
  /* registered dynamically its pointer will be returned.    */

  if (status = OCIUserCallbackGet(envhp, OCI_HTYPE_ENV, errhp,
                                  OCI_FNCODE_STMTPREPARE, OCI_UCBTYPE_ENTRY,
                                  &dyn_entry_funcptr, &dyn_entry_ctxptr,
                                  (OCIUcb *)0))
  {
    checkerr(errhp, status);
    cleanup();
    return OCI_ERROR;
  }


  /* Call OCIUserCallbackGet() to check if an exit callback  */
  /* is registered for OCIStmtPrepare. If a callback was     */
  /* registered dynamically its pointer will be returned.    */

  if (status = OCIUserCallbackGet(envhp, OCI_HTYPE_ENV, errhp,
                                  OCI_FNCODE_STMTPREPARE, OCI_UCBTYPE_EXIT,
                                  &dyn_exit_funcptr, &dyn_exit_ctxptr,
                                  (OCIUcb *)0))
  {
    checkerr(errhp, status);
    cleanup();
    return OCI_ERROR;
  }

  /* Allocate a structure for context to be passed          */
  /* to static callback function.                           */

  static_context = (stat_ctx_struct *) malloc(sizeof(stat_ctx_struct));


  /* Save the pointer to dynamic callback routine and the   */
  /* context associated with dynamic callback function      */

  if ((OCIUserCallback) *dyn_entry_funcptr)
  {
    static_context->entry_funcptr = &dyn_entry_funcptr;
    static_context->entry_ctxptr  = dyn_entry_ctxptr;
  }
  else
  {
    static_context->entry_funcptr = 0;
    static_context->entry_ctxptr  = 0;
  }


  if ((OCIUserCallback) *dyn_exit_funcptr)
  {
    static_context->exit_funcptr = &dyn_exit_funcptr;
    static_context->exit_ctxptr  = dyn_exit_ctxptr;
  }
  else
  {
    static_context->exit_funcptr = 0;
    static_context->exit_ctxptr  = 0;
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
  text *uid = (text *)"CDEMOUCB";
  text *pwd = (text *)"CDEMOUCB";
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


  /* If dynamic callback was registered, then make a    */
  /* call to the dynamic routine.                       */

  loc_ctx = ctxp;

  if (loc_ctx->entry_funcptr)
  {

    retCode = stmtprep_wrapper_cbk_fn(loc_ctx->entry_funcptr,
                                     loc_ctx->entry_ctxptr, hndlp, type, fcode,
                                      when, returnCode, errnop, stmthp, errhp,
                                      sqlstmt, stmtlen, language, mode);
  }

  return retCode;
}


/* ----------------------------------------------------------------- */
/* Entry wrapper callback function for OCIStmtPrepare.  This         */
/* function "recreates the arglist that is expected by the dynamic   */
/* callback.                                                         */
/* ----------------------------------------------------------------- */
sword stmtprep_wrapper_cbk_fn (OCIUserCallback *funcptr, dvoid *ctxp, dvoid
                               *hndlp, ub4 type, ub4 fcode, ub4 when,
                               sword returnCode, sb4 *errnop, ...)
{
  va_list  arglist;
  sword retCode;

  va_start(arglist, errnop);

  printf("In wrapper callback routine for OCIStmtPrepare:\n");

  retCode = (*funcptr)(ctxp, hndlp, type, fcode, when, returnCode, errnop,
                       arglist);
  va_end(arglist);

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

  /* If dynamic callback was registered, then make a    */
  /* call to the dynamic routine.                       */

  loc_ctx = ctxp;

  if (loc_ctx->exit_funcptr)
  {

    retCode = (*loc_ctx->exit_funcptr)(loc_ctx->exit_ctxptr, hndlp, type,
      fcode, when, returnCode, errnop, arglist);
  }

  return retCode;
}



/* end of file cdemoucb.c */

