#ifdef RCSID
static char *RCSid =
   "$Header: cdemofo.c 16-jul-2003.09:08:43 srseshad Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1998, 2003, Oracle Corporation.  All rights reserved.  
*/

/*

   NAME
     cdemofo.c - C demo program for OCI callbacks for application failover

   DESCRIPTION
     This is an example program to demonstrate the registration
     and operation of OCI application failover callbacks.

   PUBLIC FUNCTION(S)
     main - main entry point

   PRIVATE FUNCTION(S)
     checkerr - check errors
     cleanup - cleanup resources
     register_callback - register failover callback
     callback_fn - callback function


   RETURNS
     EX_SUCESS on success
     EX_FAILURE on failure

   NOTES
     This is just a demo program for the failover callbacks.  A complete
     failover example would require someway of simulating the failure of the
     connection and switching to a new instance


   MODIFIED   (MM/DD/YY)
   srseshad    07/16/03 - remove direct ref to ocikp.h and ociap.h 
   porangas    10/08/02 - Fix for bug#2540468
   slari       05/15/02 - Change to fix core dumep during cleanup at demo exit..
   emendez     09/13/00 - fix top 5 olint errors
   mrhodes     01/25/00 - make sleep call nt compatible
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   slari       10/14/98 - remove OCI_FO_TXNAL
   slari       10/13/98 - demo program for oci application failover callbacks
   slari       10/13/98 - Creation

*/


/* for WINDOWS compatibility of 'sleep' call */
#if defined(WIN32COMMON) || defined(WIN32) || defined(_WIN32) 
#include <windows.h>
#define sleep(x) Sleep(1000*(x))
#endif


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <oci.h>

#define EX_SUCCESS 0
#define EX_FAILURE -1
#define FAILOVER_S "Application Failover Context"

static text *username = (text *) "SCOTT";
static text *password = (text *) "TIGER";
static text *dbname = (text *) "inst1";

                             /* Define SQL statements to be used in program. */
static text *sel1 = (text *) "SELECT empno FROM emp order by empno";

static OCIEnv *envhp = NULL;
static OCIServer *srvhp = NULL;
static OCIError *errhp = NULL;
static OCISvcCtx *svchp = NULL;
static OCIStmt  *stmthp = NULL;
static OCIDefine *defnp = (OCIDefine *) 0;

static OCIBind  *bnd1p = (OCIBind *) 0;

static sb4 checkerr(/*_ OCIError *errhp, sword status _*/);
static void cleanup(/*_ void _*/);

static sb4  register_callback();
static sb4  callback_fn();

int main(/*_ int argc, char *argv[] _*/);
static sword status;



int main(argc, argv)
int argc;
char *argv[];
{
  OCIFocbkStruct failover;
  int exitCode = EX_FAILURE;

  sword    i=0, empno=0, count=0, totcount=0;

  OCISession *authp = (OCISession *) 0;


  if (checkerr(NULL, OCIInitialize((ub4) OCI_DEFAULT, (dvoid *)0,
                               (dvoid * (*)(dvoid *, size_t)) 0,
                               (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                               (void (*)(dvoid *, dvoid *)) 0)))
    goto terminate;

  if (checkerr(NULL, OCIEnvInit((OCIEnv **) &envhp, OCI_DEFAULT, (size_t) 0,
                                (dvoid **) 0)))
    goto terminate;

  if (checkerr(envhp, OCIHandleAlloc((dvoid *) envhp, (dvoid **) &errhp,
                                    OCI_HTYPE_ERROR, (size_t) 0, (dvoid **)0)))
    goto terminate;

                                                          /* server contexts */
  if (checkerr(envhp, OCIHandleAlloc((dvoid *) envhp, (dvoid **) &srvhp,
                                     OCI_HTYPE_SERVER, (size_t) 0,
                                     (dvoid **) 0)))
    goto terminate;

  if (checkerr(envhp, OCIHandleAlloc((dvoid *) envhp, (dvoid **) &svchp,
                                     OCI_HTYPE_SVCCTX, (size_t) 0,
                                     (dvoid **) 0)))
    goto terminate;

  if (checkerr(errhp, OCIServerAttach(srvhp, errhp, (text *)dbname,
                                      strlen((char *)dbname), 0)))
    goto terminate;

                      /* set attribute server context in the service context */
  if (checkerr(errhp, OCIAttrSet((dvoid *) svchp, OCI_HTYPE_SVCCTX,
                                 (dvoid *)srvhp, (ub4) 0, OCI_ATTR_SERVER,
                                 (OCIError *) errhp)))
    goto terminate;

  if (checkerr(envhp, OCIHandleAlloc((dvoid *) envhp, (dvoid **)&authp,
                                     (ub4) OCI_HTYPE_SESSION, (size_t) 0,
                                     (dvoid **) 0)))
    goto terminate;

  if (checkerr(errhp, OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION,
                 (dvoid *) username, (ub4) strlen((char *)username),
                                 (ub4) OCI_ATTR_USERNAME, errhp)))
    goto terminate;

  if (checkerr(errhp, OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION,
                            (dvoid *) password, (ub4) strlen((char *)password),
                                 (ub4) OCI_ATTR_PASSWORD, errhp)))
    goto terminate;

  if (checkerr(errhp, OCISessionBegin(svchp,  errhp, authp, OCI_CRED_RDBMS,
                                      (ub4) OCI_DEFAULT)))
    goto terminate;

  if (checkerr(errhp, OCIAttrSet((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
                             (dvoid *) authp, (ub4) 0,
                                 (ub4) OCI_ATTR_SESSION, errhp)))
    goto terminate;

                         /* registers the OCI application failover callbacks */
  if (register_callback(srvhp, errhp,&failover))
    goto terminate;

  if (checkerr(errhp, OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &stmthp,
                                    OCI_HTYPE_STMT, (size_t) 0, (dvoid **) 0)))
    goto terminate;

                                                    /* process SQL statement */
  if (checkerr(errhp, OCIStmtPrepare(stmthp, errhp, sel1,
                                (ub4) strlen((char *) sel1),
                                     (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT)))
    goto terminate;

  if (checkerr(errhp, OCIDefineByPos(stmthp, &defnp, errhp, 1,
               (dvoid *) &empno,
                   (sword) sizeof(sword), SQLT_INT, (dvoid *) 0, (ub2 *)0,
                                     (ub2 *)0, OCI_DEFAULT)))
    goto terminate;

  status = OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                (CONST OCISnapshot *) NULL, (OCISnapshot *) NULL, OCI_DEFAULT);
  for(;;)
  {
    if (status != 0)
    {
      if (status == OCI_NO_DATA)
        break;
      else
      {
        checkerr(errhp, status);
        goto terminate;
      }
    }
    printf("empno: %d\n", empno);
                                                        /* execute and fetch */
    status=OCIStmtFetch(stmthp, errhp, (ub4) 1, (ub4) 0, (ub4) 0);
  }

  exitCode = EX_SUCCESS;

 terminate:

  if (exitCode == EX_SUCCESS)
    printf("No errors.\n");
  else
    printf("Failed due to errors.\n");

  cleanup();
  return exitCode;
}
                                  /* application failoever callback function */
sb4  callback_fn(svchp, envhp, fo_ctx, fo_type, fo_event )
dvoid * svchp;
dvoid * envhp;
dvoid *fo_ctx;
ub4 fo_type;
ub4 fo_event;
{
  switch (fo_event)
  {
   case OCI_FO_BEGIN:
   {
     printf(" Failing Over ... Please stand by \n");
     printf(" Failover type was found to be %s \n",
                     ((fo_type==OCI_FO_NONE) ? "NONE"
                     :(fo_type==OCI_FO_SESSION) ? "SESSION"
                     :(fo_type==OCI_FO_SELECT) ? "SELECT"
                     : "UNKNOWN!"));
     printf(" Failover Context is :%s\n",
            (fo_ctx?(char *)fo_ctx:"NULL POINTER!"));
     break;
   }

   case OCI_FO_ABORT:
   {
     printf(" Failover aborted. Failover will not take place.\n");
     break;
   }

   case OCI_FO_END:
   {
     printf(" Failover ended ...resuming services\n");
     break;
   }

   case OCI_FO_REAUTH:
   {
     printf(" Failed over user. Resuming services\n");

                    /* Application can check the OCI_ATTR_SESSION attribute of
                       the service handle to find out the user being
                       re-authenticated.

                       After this, the application can replay any ALTER SESSION
                       commands associated with this session.  These must have
                       been saved by the application in the fo_ctx
                     */
     break;
   }


   case OCI_FO_ERROR:
   {
     printf(" Failover error gotten. Sleeping...\n");
     sleep(3);
     return OCI_FO_RETRY;
     break;
   }

   default:
   {
     printf("Bad Failover Event: %d.\n",  fo_event);
     break;
   }
  }
  return 0;
}



                                           /* callback function registration */
sb4 register_callback(srvhp, errhp,failover)
dvoid    *srvhp;                                        /* the server handle */
OCIError *errhp;                                         /* the error handle */
OCIFocbkStruct *failover;
{
  
  if (!(failover->fo_ctx =
                       (dvoid *)malloc(strlen(FAILOVER_S) + 1)))
  {
    (void) printf("Error - could not allocate failover context\n");
    return EX_FAILURE;
  }

                                                  /* initialize the context. */
  strcpy((char *)failover->fo_ctx, FAILOVER_S);
  failover->callback_function = &callback_fn;
                                                      /* do the registration */
  if (checkerr(errhp, OCIAttrSet(srvhp, (ub4) OCI_HTYPE_SERVER,
                             (dvoid *) failover, (ub4) 0,
                                 (ub4) OCI_ATTR_FOCBK, errhp)))
  {
    printf("Failed to set failover attribute\n");
    return EX_FAILURE;
  }

  return EX_SUCCESS;
}


                                                            /* error handler */
sb4 checkerr(ehndlp, status)
OCIError *ehndlp;
sword status;
{
  text errbuf[512];
  sb4 errcode = 0;

  switch (status)
  {
  case OCI_SUCCESS:
    return EX_SUCCESS;

  case OCI_SUCCESS_WITH_INFO:
    printf("Error - OCI_SUCCESS_WITH_INFO\n");
    return EX_SUCCESS;

  case OCI_NEED_DATA:
    printf("Error - OCI_NEED_DATA\n");
    return EX_SUCCESS;

  case OCI_NO_DATA:
    printf("Error - OCI_NODATA\n");
    return EX_SUCCESS;

  case OCI_ERROR:
    if (ehndlp)
    {
      OCIErrorGet((dvoid *)ehndlp, (ub4) 1, (text *) NULL, &errcode,
                         errbuf, (ub4) sizeof(errbuf), OCI_HTYPE_ERROR);
      printf("Error - %.*s\n", 512, errbuf);
    }
    else
      printf("Error - OCI_ERROR\n");
    return EX_FAILURE;

  case OCI_INVALID_HANDLE:
    printf("Error - OCI_INVALID_HANDLE\n");
    return EX_FAILURE;

  case OCI_STILL_EXECUTING:
    printf("Error - OCI_STILL_EXECUTE\n");
    return EX_SUCCESS;

  case OCI_CONTINUE:
    printf("Error - OCI_CONTINUE\n");
    return EX_SUCCESS;

  default:
    printf("Error - Unknown error\n");
    return EX_FAILURE;
  }
}


                                                        /* cleanup resources */
void cleanup()
{
  if (stmthp)
    checkerr(errhp, OCIHandleFree((dvoid *) stmthp, OCI_HTYPE_STMT));

  if (errhp && srvhp)
    (void) OCIServerDetach( srvhp, errhp, OCI_DEFAULT );

  if (srvhp)
    checkerr(errhp, OCIHandleFree((dvoid *) srvhp, OCI_HTYPE_SERVER));

  if (svchp)
    (void) OCIHandleFree((dvoid *) svchp, OCI_HTYPE_SVCCTX);

  if (errhp)
    (void) OCIHandleFree((dvoid *) errhp, OCI_HTYPE_ERROR);

  if (envhp)
    (void) OCIHandleFree((dvoid *) envhp, OCI_HTYPE_ENV);
  return;
}



/* end of file cdemofo.c */

