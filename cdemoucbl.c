#ifdef RCSID
static char *RCSid =
   "$Header: cdemoucbl.c 14-jul-99.13:18:43 mjaeger Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1998, 1999, Oracle Corporation.  All rights reserved.
*/

/*

   NAME
     cdemoucbl.c - User callback demo program.

   DESCRIPTION
     This file contains the routine which is used to
     create the shared library to be used for dynamic
     callback registration.

     Makefile ociucb.mk is required to compile this file
     and for creating shared library. The make command is:

       make -f ociucb.mk user_callback SHARED_LIBNAME=ociucb.so.1.0 \
                                               OBJS=cdemoucbl.o

     For creating 64-bit shared library make command is:

       make -f ociucb.mk user_callback SHARED_LIBNAME=ociucb.so.1.0 \
                 LDFLAGS="\$(LDFLAGS64)" RDBMSLIB="\$(RDBMSLIB64)" \
                 LIBHOME="\$(LIBHOME64)" COMPFLAGS="\$(COMPFLAGS64)" \
                 LIBD4R=      OBJS=cdemoucbl.o


     The environment variable ORA_OCI_UCBPKG should be set to ociucb.

       setenv ORA_OCI_UCBPKG ociucb


   PUBLIC FUNCTION(S)

   PRIVATE FUNCTION(S)

   RETURNS

   NOTES

   MODIFIED   (MM/DD/YY)
   slari       09/10/99 - bg989981: remove envCallback from ociucbInit
   slari       08/30/99 - add OCIUcb *
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   slari       11/22/98 - use arglist instead of ellipsis
   slari       11/21/98 - conform to new OCIUserCallback with arglist
   svedala     10/13/98 - Creation

*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <oci.h>
#include <stdarg.h>


                    /* the forward declaration must be done so that
                       OCIShareLibInit can be passed a pointer to this function
                       */

sword ociucbEnvCallback(OCIEnv *env, ub4 mode, size_t xtramemsz, dvoid *usrmemp,
                        OCIUcb *ucbDesc);


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

typedef struct dyn_ctx_struct  dyn_ctx_struct;



/*--------------------------------- ociucbInit ---------------------------------*/

/*
   NAME:
       ociucbInit - OCI Shared Library CallBack Main function

   PARAMETERS:
        meta    - The metacontext
        libCtx  - The context for this package
        argfmt  - The package argument format
        argc    - The number of arguments passed
        argv    - The arguments to the package
      
   DESCRIPTION:
       This is called by OCI to load and initialize the shared library
       (package).

       The OCI shared library initialization is done by passing all the
       parameters passed to the the shared library initialization function to
       the OCISharedLibInit function.  User's environment callback function of
       type OCIEnvCallbackType is also passed to the OCISharedLibInit call.


   RETURNS:  
       the return code from OCISharedLibInit function.

   NOTES:  
*/

sword ociucbInit(metaCtx, libCtx, argfmt, argc, argv, envCallback)
dvoid *        metaCtx;                                  /* The metacontext */
dvoid *        libCtx;       /* The context for this program or package if you
                                 have previously been called. */ 
ub4            argfmt;                         /* What am I supposed to do? */
sword          argc;              /* argc if I am being called as a program */
dvoid *        argv[];            /* argv if I am being called as a program */
{
  return  (OCISharedLibInit(metaCtx, libCtx, argfmt, argc, argv,
                            ociucbEnvCallback));
}



/* ------------------------------------------------------------- */
/* Entry point for dynamic callback registration. This routine   */
/* is called if environment variable ORA_OCI_UCBPKG is set       */
/* ------------------------------------------------------------- */
sword ociucbEnvCallback(env, mode, xtramemsz, usrmemp, ucbDesc)
OCIEnv  *env;
ub4     mode;
size_t  xtramemsz;
dvoid   *usrmemp;
OCIUcb    *ucbDesc;
{
  OCISvcCtx *svchp;
  OCIError *errhp;
  dyn_ctx_struct  *dynamic_context;

  dynamic_context = (dyn_ctx_struct *) malloc(sizeof(dyn_ctx_struct));

  /* copy a string into element str1 of context, this value */
  /* will be printed in dynamic callback function           */

  strcpy(dynamic_context->str1, "Dynamic_Context_Test_String");


  /* register the dynamic callback function                */


                    /* Note that the replacement callback function in the
                       preferred way of using the ucbDesc passed-in in the
                       EnvCallbackk function */

  if (OCIUserCallbackRegister(env, OCI_HTYPE_ENV, env,
                              stmtprep_replace_dyncbk_fn,
                              dynamic_context, OCI_FNCODE_STMTPREPARE,
                              OCI_UCBTYPE_REPLACE, ucbDesc))
  {
     printf("cdemoucbl: OCIUserCallbackRegister returns error\n");
     return OCI_ERROR;
  }

                    /* The entry and exit callback functions are registered
                       using a NULL UCB Descriptor so that the application is
                       responsible for chaining them */

  if (OCIUserCallbackRegister(env, OCI_HTYPE_ENV, env,
                              stmtprep_entry_dyncbk_fn,
                              dynamic_context, OCI_FNCODE_STMTPREPARE,
                              OCI_UCBTYPE_ENTRY, (OCIUcb *)0))
  {
     printf("cdemoucbl: OCIUserCallbackRegister returns error\n");
     return OCI_ERROR;
  }

  if (OCIUserCallbackRegister(env, OCI_HTYPE_ENV, env,
                              stmtprep_exit_dyncbk_fn,
                              dynamic_context, OCI_FNCODE_STMTPREPARE,
                              OCI_UCBTYPE_EXIT, (OCIUcb *)0))
  {
     printf("cdemoucbl: OCIUserCallbackRegister returns error\n");
     return OCI_ERROR;
  }

  return OCI_CONTINUE;
}



/* ------------------------------------------------------------------ */
/* Entry callback function registered for OCIStmtPrepare. This        */
/* function is registered dynamically from function ociucbEnvCallback */
/* ------------------------------------------------------------------ */
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

  printf("In dynamic entry callback function for OCIStmtPrepare:\n");
  printf("sql_stmt = [%s]\n", sqlstmt);
  printf("Context string = [%s]\n", loc_ctx->str1);

  return OCI_CONTINUE;
}



/* ------------------------------------------------------------------ */
/* Replacement callback function registered for OCIStmtPrepare. This  */
/* function is registered dynamically from function ociucbEnvCallback */
/* ------------------------------------------------------------------ */
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

  printf("In dynamic replacement callback function for OCIStmtPrepare:\n");
  printf("sql_stmt = [%s]\n", sqlstmt);
  printf("Context string = [%s]\n", loc_ctx->str1);

  return OCI_CONTINUE;
}



/* ------------------------------------------------------------------ */
/* Exit callback function registered for OCIStmtPrepare. This         */
/* function is registered dynamically from function ociucbEnvCallback */
/* ------------------------------------------------------------------ */
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

  printf("In dynamic exit callback function for OCIStmtPrepare:\n");
  printf("sql_stmt = [%s]\n", sqlstmt);
  printf("Context string = [%s]\n", loc_ctx->str1);

  return OCI_CONTINUE;
}





/* end of file cdemoucbl.c */

