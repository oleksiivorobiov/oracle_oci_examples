#ifdef RCSID
static char *RCSid =
   "$Header: cdemoup1.c 20-oct-99.14:12:18 svedala Exp $ ";
#endif /* RCSID */

/* Copyright (c) Oracle Corporation 1999, 2000. All Rights Reserved. */

/*

   NAME
     cdemoup1.c - User callback library routine 1.

   DESCRIPTION
     The library cdemoup1 for user callback is created using this
     file. The command for compiling and creating the library is:

       make -f ociucb.mk user_callback SHARED_LIBNAME=cdemoup1.so.1.0 \
                                                      OBJS=cdemoup1.o

     The make file ociucb.mk is located in the demo directory.


   PUBLIC FUNCTION(S)
     None

   PRIVATE FUNCTION(S)
     As described below

   RETURNS

   NOTES

   MODIFIED   (MM/DD/YY)
   jchai       04/26/00 - fix bug1238864
   svedala     10/20/99 - Creation

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <oci.h>
#include <stdarg.h>


  /* the forward declaration must be done so that OCIShareLibInit
     can be passed a pointer to this function */

sword cdemoup1EnvCallback(OCIEnv *env, ub4 mode, size_t xtramemsz,
                          dvoid *usrmemp, OCIUcb *ucbDesc);


static sword stmtprep_entry_dyncbk_fn (dvoid *ctxp, dvoid *hndlp, ub4 type,
                                       ub4 fcode, ub4 when, sword returnCode,
                                       sb4 *errnop, va_list arglist);


static sword stmtprep_replace_dyncbk_fn(dvoid *ctxp, dvoid *hndlp, ub4 type,
                                       ub4 fcode, ub4 when, sword returnCode,
                                       sb4 *errnop, va_list arglist);

static sword stmtprep_exit_dyncbk_fn (dvoid *ctxp, dvoid *hndlp, ub4 type,
                                      ub4 fcode, ub4 when, sword returnCode,
                                      sb4 *errnop, va_list arglist);

struct dyn_ctx_struct                             /* Context to be passed to */
{
   char str1[40];                               /* dynamic callback function */
};

static ub4     id = 1;                                                /* library id */

typedef struct dyn_ctx_struct  dyn_ctx_struct;



/*--------------------------- cdemoup1Init ---------------------------------*/

/*
   NAME:
       cdemoup1Init - OCI Shared Library CallBack Main function

   PARAMETERS:
        meta     - The metacpmtext for the program manager
        mycx     - The primary context for the package
        argfmt   - The behviour that package must undergo
        argc     - The number arguments passed
        argv     - The arguments to the package
        envCallback - pointer to user's environment callback function
      
   DESCRIPTION:
       This is called by OCI to load and initialize the shared library.

       The OCI shared library initialization is done by passing all the
       parameters passed to the the shared library initialization function to
       the OCISharedLibInit function.  User's environment callback function of
       type OCIEnvCallbackType is also passed to the OCISharedLibInit call.


   RETURNS:  
        the return code from OCISharedLibInit function.

   NOTES:  
*/

sword cdemoup1Init(metaCtx, libCtx, argfmt, argc, argv, envCallback)
dvoid   *metaCtx;                                         /* The metacontext */
dvoid   *libCtx;           /* The context for this program or package if you
                                                have previously been called. */ 
ub4      argfmt;                                /* What am I supposed to do? */
sword    argc;                     /* argc if I am being called as a program */
dvoid   *argv[];                   /* argv if I am being called as a program */
OCIEnvCallbackType envCallback;      /* user's environment callback function */
{
  return  (OCISharedLibInit(metaCtx, libCtx, argfmt, argc, argv,
                            cdemoup1EnvCallback));
}



/* ------------------------------------------------------------- */
/* Entry point for dynamic callback registration. This routine   */
/* is called if environment variable ORA_OCI_UCBPKG is set       */
/* ------------------------------------------------------------- */
sword cdemoup1EnvCallback(env, mode, xtramemsz, usrmemp, ucbDesc)
OCIEnv  *env;
ub4     mode;
size_t  xtramemsz;
dvoid   *usrmemp;
OCIUcb     *ucbDesc;
{
  OCISvcCtx *svchp;
  OCIError *errhp;
  dyn_ctx_struct  *dynamic_context;

  dynamic_context = (dyn_ctx_struct *) malloc(sizeof(dyn_ctx_struct));

  /* copy a string into element str1 of context, this value */
  /* will be printed in dynamic callback function           */

  strcpy(dynamic_context->str1, "Dynamic_Context_Test_String");


  /* register the dynamic callback function                */


  /* Note that the replacement callback function in the preferred
     way of using the ucbDesc passed-in in the EnvCallbackk function */

  if (OCIUserCallbackRegister(env, OCI_HTYPE_ENV, env,
                              stmtprep_replace_dyncbk_fn,
                              dynamic_context, OCI_FNCODE_STMTPREPARE,
                              OCI_UCBTYPE_REPLACE, ucbDesc))
  {
     printf("Library%d: OCIUserCallbackRegister returns error\n", id);
     return OCI_ERROR;
  }

  if (OCIUserCallbackRegister(env, OCI_HTYPE_ENV, env,
                              stmtprep_entry_dyncbk_fn,
                              dynamic_context, OCI_FNCODE_STMTPREPARE,
                              OCI_UCBTYPE_ENTRY, ucbDesc))
  {
     printf("Library%d: OCIUserCallbackRegister returns error\n", id);
     return OCI_ERROR;
  }

  if (OCIUserCallbackRegister(env, OCI_HTYPE_ENV, env,
                              stmtprep_exit_dyncbk_fn,
                              dynamic_context, OCI_FNCODE_STMTPREPARE,
                              OCI_UCBTYPE_EXIT, ucbDesc))
  {
     printf("Library%d: OCIUserCallbackRegister returns error\n", id);
     return OCI_ERROR;
  }

  return OCI_CONTINUE;
}



/* -------------------------------------------------------------------- */
/* Entry callback function registered for OCIStmtPrepare. This          */
/* function is registered dynamically from function cdemoup1EnvCallback */
/* -------------------------------------------------------------------- */
sword stmtprep_entry_dyncbk_fn (dvoid *ctxp, dvoid *hndlp, ub4 type,
                                ub4 fcode, ub4 when, sword returnCode,
                                sb4 *errnop, va_list arglist)
{
  dyn_ctx_struct *loc_ctx;
  text *sqlstmt;

  va_arg(arglist, dvoid *);
  va_arg(arglist, dvoid *);
  sqlstmt = va_arg(arglist, text *);

  loc_ctx = (dyn_ctx_struct *) ctxp;

  printf(
    "Library%d: In dynamic entry callback function for OCIStmtPrepare:\n", id);
  printf("Library%d: sql_stmt = [%s]\n", id, sqlstmt);
  printf("Library%d: Context string = [%s]\n", id, loc_ctx->str1);

  return OCI_CONTINUE;
}



/* -------------------------------------------------------------------- */
/* Replacement callback function registered for OCIStmtPrepare. This    */
/* function is registered dynamically from function cdemoup1EnvCallback */
/* -------------------------------------------------------------------- */
sword stmtprep_replace_dyncbk_fn (dvoid *ctxp, dvoid *hndlp, ub4 type,
                                  ub4 fcode, ub4 when, sword returnCode,
                                  sb4 *errnop, va_list arglist)
{
  dyn_ctx_struct *loc_ctx;
  text *sqlstmt;

  va_arg(arglist, dvoid *);
  va_arg(arglist, dvoid *);
  sqlstmt = va_arg(arglist, text *);

  loc_ctx = (dyn_ctx_struct *) ctxp;

  printf(
   "Library%d: In dynamic replacement callback function for OCIStmtPrepare:\n",
    id);
  printf("Library%d: sql_stmt = [%s]\n", id, sqlstmt);
  printf("Library%d: Context string = [%s]\n", id, loc_ctx->str1);

  return OCI_CONTINUE;
}



/* -------------------------------------------------------------------- */
/* Exit callback function registered for OCIStmtPrepare. This           */
/* function is registered dynamically from function cdemoup1EnvCallback */
/* -------------------------------------------------------------------- */
sword stmtprep_exit_dyncbk_fn (dvoid *ctxp, dvoid *hndlp, ub4 type,
                               ub4 fcode, ub4 when, sword returnCode,
                               sb4 *errnop, va_list arglist)
{
  dyn_ctx_struct *loc_ctx;
  text *sqlstmt;

  va_arg(arglist, dvoid *);
  va_arg(arglist, dvoid *);
  sqlstmt = va_arg(arglist, text *);

  loc_ctx = (dyn_ctx_struct *) ctxp;

  printf(
    "Library%d: In dynamic exit callback function for OCIStmtPrepare:\n", id);
  printf("Library%d: sql_stmt = [%s]\n", id, sqlstmt);
  printf("Library%d: Context string = [%s]\n", id, loc_ctx->str1);

  return OCI_CONTINUE;
}




/* end of file cdemoup1.c */

