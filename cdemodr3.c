#ifdef RCSID
static char *RCSid =
   "$Header: cdemodr3.c 10-oct-2006.14:39:59 azhao Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1997, 2006, Oracle. All rights reserved.  
*/

/*

   NAME
     cdemodr3.c - DML Returning Demo

   DESCRIPTION
     Demonstrate INSERT statement RETURNING REF

   PUBLIC FUNCTION(S)
     <list of external functions declared/defined - with one-line descriptions>

   PRIVATE FUNCTION(S)
     <list of static functions defined in .c file - with one-line descriptions>

   RETURNS
     <function return values, for .c file with single function>

   NOTES
     <other useful comments, qualifications, etc.>

   MODIFIED   (MM/DD/YY)
   azhao       10/10/06 - case-senstive password change
   svedala     10/18/99 -
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   azhao       08/19/97 - replace OCIStmtBindByName with OCIBindByName
   azhao       06/03/97 - Creation

*/

/*------------------------Inclusions-------------------------------*/

#include <cdemodr3.h>

static boolean logged_on = FALSE;

static OCIRef    *addrref[MAXITER];         /* for returned REF */
static OCIType   *addrtdo = (OCIType *) 0;

static text   in_state[MAXITER][2];
static text   in_zip[MAXITER][10];
static text  *out_zip[MAXITER];             /* for returned ZIP */

static short *ind[MAXCOLS][MAXITER];        /* indicators */
static ub2   *rc[MAXCOLS][MAXITER];         /* return codes */
static ub4   *rl[MAXCOLS][MAXITER];         /* return lengths */

/* Rows returned in each iteration */
static ub2 rowsret[MAXITER];

static ub4   pos[MAXCOLS];
static sb2   null_ind = -1;


static OCIEnv *envhp;
static OCIError *errhp;
static OCISvcCtx *svchp;

/*------------------------end of Inclusions-----------------------------*/


/*========================== UTILITY FUNCTIONS ======================*/
/*
 * These functions are generic functions that can be used in any
 * OCI program.
 */
/* ----------------------------------------------------------------- */
/* Initialize environment, allocate handles                          */
/* ----------------------------------------------------------------- */
sword init_handles(envhp, svchp, errhp, srvhp, authp, init_mode)
OCIEnv **envhp;
OCISvcCtx **svchp;
OCIError **errhp;
OCIServer **srvhp;
OCISession **authp;
ub4 init_mode;
{
  (void) printf("Environment setup ....\n");

  /* Initialize the OCI Process */
  if (OCIInitialize(init_mode, (dvoid *)0,
                    (dvoid * (*)(dvoid *, size_t)) 0,
                    (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                    (void (*)(dvoid *, dvoid *)) 0 ))
  {
    (void) printf("FAILED: OCIInitialize()\n");
    return OCI_ERROR;
  }

  /* Inititialize the OCI Environment */
  if (OCIEnvInit((OCIEnv **) envhp, (ub4) OCI_DEFAULT,
                 (size_t) 0, (dvoid **) 0 ))
  {
    (void) printf("FAILED: OCIEnvInit()\n");
    return OCI_ERROR;
  }

  /* Allocate a service handle */
  if (OCIHandleAlloc((dvoid *) *envhp, (dvoid **) svchp,
                     (ub4) OCI_HTYPE_SVCCTX, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc() on svchp\n");
    return OCI_ERROR;
  }

  /* Allocate an error handle */
  if (OCIHandleAlloc((dvoid *) *envhp, (dvoid **) errhp,
                     (ub4) OCI_HTYPE_ERROR, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc() on errhp\n");
    return OCI_ERROR;
  }

  /* Allocate a server handle */
  if (OCIHandleAlloc((dvoid *) *envhp, (dvoid **) srvhp,
                     (ub4) OCI_HTYPE_SERVER, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc() on srvhp\n");
    return OCI_ERROR;
  }

  /* Allocate a authentication handle */
  if (OCIHandleAlloc((dvoid *) *envhp, (dvoid **) authp,
                     (ub4) OCI_HTYPE_SESSION, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc() on authp\n");
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}

/* ----------------------------------------------------------------- */
/* Attach to server with a given mode.                               */
/* ----------------------------------------------------------------- */
sword attach_server(mode, srvhp, errhp, svchp)
ub4 mode;
OCIServer *srvhp;
OCIError *errhp;
OCISvcCtx *svchp;
{
  text *cstring = (text *)"";

  if (OCIServerAttach(srvhp, errhp, (text *) cstring,
                     (sb4) strlen((char *)cstring), (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIServerAttach()\n");
    return OCI_ERROR;
  }

  /* Set the server handle in the service handle */
  if (OCIAttrSet((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
                 (dvoid *) srvhp, (ub4) 0, (ub4) OCI_ATTR_SERVER, errhp))
  {
    (void) printf("FAILED: OCIAttrSet() server attribute\n");
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}
/* ----------------------------------------------------------------- */
/* Logon to the database using given username, password & credentials*/
/* ----------------------------------------------------------------- */
sword log_on(authp, errhp, svchp, uid, pwd, credt, mode)
OCISession *authp;
OCIError *errhp;
OCISvcCtx *svchp;
text *uid;
text *pwd;
ub4 credt;
ub4 mode;
{
  /* Set attributes in the authentication handle */
  if (OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION,
                 (dvoid *) uid, (ub4) strlen((char *) uid),
                 (ub4) OCI_ATTR_USERNAME, errhp))
  {
    (void) printf("FAILED: OCIAttrSet() userid\n");
    return OCI_ERROR;
  }
  if (OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION,
                 (dvoid *) pwd, (ub4) strlen((char *) pwd),
                 (ub4) OCI_ATTR_PASSWORD, errhp))
  {
    (void) printf("FAILED: OCIAttrSet() passwd\n");
    return OCI_ERROR;
  }

  (void) printf("Logging on as %s  ....\n", uid);

  if (OCISessionBegin(svchp, errhp, authp, credt, mode))
  {
    (void) printf("FAILED: OCIAttrSet() passwd\n");
    return OCI_ERROR;
  }

  (void) printf("%s logged on.\n", uid);

  /* Set the authentication handle in the Service handle */
  if (OCIAttrSet((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
                 (dvoid *) authp, (ub4) 0, (ub4) OCI_ATTR_SESSION, errhp))
  {
    (void) printf("FAILED: OCIAttrSet() session\n");
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}

/*---------------------------------------------------------------------*/
/* Allocate all required bind handles                                  */
/*---------------------------------------------------------------------*/

sword alloc_bind_handle(stmthp, bndhp, nbinds)
OCIStmt *stmthp;
OCIBind *bndhp[];
int  nbinds;
{
  int  i;
  /*
   * This function allocates the specified number of bind handles
   * from the given statement handle.
   */
  for (i = 0; i < nbinds; i++)
    if (OCIHandleAlloc((dvoid *)stmthp, (dvoid **)&bndhp[i],
                       (ub4)OCI_HTYPE_BIND, (CONST size_t) 0, (dvoid **) 0))
    {
      (void) printf("FAILED: OCIHandleAlloc() bind handle\n");
      return OCI_ERROR;
    }

  return OCI_SUCCESS;
}

/* ----------------------------------------------------------------- */
/*  Free the specified handles                                       */
/* ----------------------------------------------------------------- */
void free_handles(envhp, svchp, srvhp, errhp, authp, stmthp)
OCIEnv *envhp;
OCISvcCtx *svchp;
OCIServer *srvhp;
OCIError *errhp;
OCISession *authp;
OCIStmt *stmthp;
{
  (void) printf("Freeing handles ...\n");

  if (srvhp)
    (void) OCIHandleFree((dvoid *) srvhp, (ub4) OCI_HTYPE_SERVER);
  if (svchp)
    (void) OCIHandleFree((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX);
  if (errhp)
    (void) OCIHandleFree((dvoid *) errhp, (ub4) OCI_HTYPE_ERROR);
  if (authp)
    (void) OCIHandleFree((dvoid *) authp, (ub4) OCI_HTYPE_SESSION);
  if (stmthp)
    (void) OCIHandleFree((dvoid *) stmthp, (ub4) OCI_HTYPE_STMT);
  if (envhp)
    (void) OCIHandleFree((dvoid *) envhp, (ub4) OCI_HTYPE_ENV);

  return;
}

/* ----------------------------------------------------------------- */
/* Print the error message                                           */
/* ----------------------------------------------------------------- */
void report_error(errhp)
OCIError *errhp;
{
  text  msgbuf[512];
  sb4   errcode = 0;

  (void) OCIErrorGet((dvoid *) errhp, (ub4) 1, (text *) NULL, &errcode,
                       msgbuf, (ub4) sizeof(msgbuf), (ub4) OCI_HTYPE_ERROR);
  (void) printf("ERROR CODE = %d\n", errcode);
  (void) printf("%.*s\n", 512, msgbuf);
  return;
}

/*-------------------------------------------------------------------*/
/* Logout and detach from the server                                 */
/*-------------------------------------------------------------------*/
void logout_detach_server(svchp, srvhp, errhp, authp, userid)
OCISvcCtx *svchp;
OCIServer *srvhp;
OCIError *errhp;
OCISession *authp;
text *userid;
{
  if (OCISessionEnd(svchp, errhp, authp, (ub4) 0))
  {
    (void) printf("FAILED: OCISessionEnd()\n");
    report_error(errhp);
  }

  (void) printf("%s Logged off.\n", userid);

  if (OCIServerDetach(srvhp, errhp, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCISessionEnd()\n");
    report_error(errhp);
  }

  (void) printf("Detached from server.\n");

  return;
}

/*---------------------------------------------------------------------*/
/* Finish demo and clean up                                            */
/*---------------------------------------------------------------------*/
sword finish_demo(loggedon, envhp, svchp, srvhp, errhp, authp, stmthp, userid)
boolean loggedon;
OCIEnv *envhp;
OCISvcCtx *svchp;
OCIServer *srvhp;
OCIError *errhp;
OCISession *authp;
OCIStmt *stmthp;
text *userid;
{

  if (loggedon)
    logout_detach_server(svchp, srvhp, errhp, authp, userid);

  free_handles(envhp, svchp, srvhp, errhp, authp, stmthp);

  return OCI_SUCCESS;
}

/*===================== END OF UTILITY FUNCTIONS ======================*/



int main(argc, argv)
int argc;
char *argv[];
{
  text *username = (text *)"SCOTT";
  text *password = (text *)"tiger";

  OCIServer *srvhp;
  OCISession *authp;
  OCIStmt *stmthp = (OCIStmt *) NULL;
  OCIBind *bndhp[MAXBINDS];
  int i;

  if (init_handles(&envhp, &svchp, &errhp, &srvhp, &authp, (ub4)OCI_OBJECT))
  {
    (void) printf("FAILED: init_handles()\n");
    return finish_demo(logged_on, envhp, svchp, srvhp, errhp, authp,
                       stmthp, username);
  }

  if (attach_server((ub4) OCI_DEFAULT, srvhp, errhp, svchp))
  {
    (void) printf("FAILED: attach_server()\n");
    return finish_demo(logged_on, envhp, svchp, srvhp, errhp, authp,
                       stmthp, username);
  }

  if (log_on(authp, errhp, svchp, username, password,
             (ub4) OCI_CRED_RDBMS, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: log_on()\n");
    return finish_demo(logged_on, envhp, svchp, srvhp, errhp, authp,
                       stmthp, username);
  }

  logged_on = TRUE;

  if (OCIHandleAlloc((dvoid *)envhp, (dvoid **) &stmthp,
                     (ub4)OCI_HTYPE_STMT, (CONST size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: alloc statement handle\n");
    return finish_demo(logged_on, envhp, svchp, srvhp, errhp, authp,
                       stmthp, username);
  }

  /* bind handles will be implicitly allocated in the bind calls */
  /* need to initialize them to null prior to first usage in bind calls */

  for (i = 0; i < MAXBINDS; i++)
    bndhp[i] = (OCIBind *) 0;


  if (demo_insert(svchp, stmthp, bndhp, errhp))
    (void) printf("FAILED: demo_insert()\n");
  else
    (void) printf("PASSED: demo_insert()\n");

  return finish_demo(logged_on, envhp, svchp, srvhp, errhp, authp,
                     stmthp, username);
}


/* ----------------------------------------------------------------- */
sword bind_input(stmthp, bndhp, errhp)
OCIStmt *stmthp;
OCIBind *bndhp[];
OCIError *errhp;
{
  if (OCIBindByPos(stmthp, &bndhp[0], errhp, (ub4) 1,
                      (dvoid *) in_state[0], (sb4) 2, SQLT_CHR,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT)
  ||  OCIBindByPos(stmthp, &bndhp[1], errhp, (ub4) 2,
                      (dvoid *) in_zip[0], (sb4) 10, SQLT_CHR,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIBindByPos()\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  if (OCIBindArrayOfStruct(bndhp[0], errhp, 2, 0, 0, 0)
  ||  OCIBindArrayOfStruct(bndhp[1], errhp, 10, 0, 0, 0))
  {
    (void) printf("FAILED: OCIBindArrayOfStruct()\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}


/* ----------------------------------------------------------------- */
sword bind_output(stmthp, bndhp, errhp)
OCIStmt *stmthp;
OCIBind *bndhp[];
OCIError *errhp;
{
  ub4 i;

  if (OCITypeByName(envhp, errhp, svchp, (CONST text *) 0,
                   (ub4) 0, (CONST text *) "ADDRESS_OBJECT",
                   (ub4) strlen((CONST char *) "ADDRESS_OBJECT"),
                   (CONST text *) 0, (ub4) 0,
                    OCI_DURATION_SESSION, OCI_TYPEGET_HEADER, &addrtdo))
  {
    (void) printf("FAILED: OCITypeByName()\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  if (OCIBindByName(stmthp, &bndhp[2], errhp,
                       (text *) ":addref", (sb4) strlen((char *) ":addref"),
                       (dvoid *) 0, (sb4) sizeof(OCIRef *), SQLT_REF,
                       (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                       (ub4) 0, (ub4 *) 0, (ub4) OCI_DATA_AT_EXEC)
  ||  OCIBindByName(stmthp, &bndhp[3], errhp,
                       (text *) ":zip", (sb4) strlen((char *) ":zip"),
                       (dvoid *) 0, (sb4) MAXZIPLEN, SQLT_CHR,
                       (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                       (ub4) 0, (ub4 *) 0, (ub4) OCI_DATA_AT_EXEC))
  {
    (void) printf("FAILED: OCIBindByName()\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  if (OCIBindObject(bndhp[2], errhp, (OCIType *) addrtdo,
                   (dvoid **) &addrref[0], (ub4 *) 0, (dvoid **) 0, (ub4 *) 0))
  {
    (void) printf("FAILED: OCIBindObject()\n");
    report_error(errhp);
    return OCI_ERROR;
  }


  for (i = 0; i < MAXCOLS; i++)
    pos[i] = i;

  if (OCIBindDynamic(bndhp[2], errhp, (dvoid *) &pos[0], cbf_no_data,
                    (dvoid *) &pos[0], cbf_get_data)
  ||  OCIBindDynamic(bndhp[3], errhp, (dvoid *) &pos[1], cbf_no_data,
                    (dvoid *) &pos[1], cbf_get_data))
  {
    (void) printf("FAILED: OCIBindDynamic()\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}


/* ----------------------------------------------------------------- */
/* Intbind callback that does not do any data input.                 */
/* ----------------------------------------------------------------- */
sb4 cbf_no_data(ctxp, bindp, iter, index, bufpp, alenpp, piecep, indpp)
dvoid *ctxp;
OCIBind *bindp;
ub4 iter, index;
dvoid **bufpp;
ub4 *alenpp;
ub1 *piecep;
dvoid **indpp;
{
  *bufpp = (dvoid *) 0;
  *alenpp = 0;
   null_ind = -1;
  *indpp = (dvoid *) &null_ind;
  *piecep = OCI_ONE_PIECE;

  return OCI_CONTINUE;
}



/* ----------------------------------------------------------------- */
/* Outbind callback for returning data.                              */
/* ----------------------------------------------------------------- */
sb4 cbf_get_data(ctxp, bindp, iter, index, bufpp, alenp, piecep,
                         indpp, rcodepp)
dvoid *ctxp;
OCIBind *bindp;
ub4 iter, index;
dvoid **bufpp;
ub4 **alenp;
ub1 *piecep;
dvoid **indpp;
ub2 **rcodepp;
{
  static ub4  rows = 0;
  ub4    pos = *((ub4 *)ctxp);

  if (index == 0)
  {
    (void) OCIAttrGet((CONST dvoid *)bindp, OCI_HTYPE_BIND, (dvoid *)&rows,
                       (ub4 *) sizeof(ub4), OCI_ATTR_ROWS_RETURNED, errhp);
    rowsret[iter] = (ub2)rows;

    if (alloc_buffer(pos, iter, rows))
      return OCI_ERROR;
  }

  switch(pos)
  {
  case 0:
    rl[pos][iter][index] = sizeof(OCIRef *);
    *bufpp =  (dvoid *) (&addrref[iter]);
    break;
  case 1:
    rl[pos][iter][index] = (ub4) MAXZIPLEN;
    *bufpp =  (dvoid *) (out_zip[iter]+(index * MAXZIPLEN));
    break;
  default:
    *bufpp =  (dvoid *) 0;
    *alenp =  (ub4 *) 0;
    (void) printf("ERROR: invalid position number: %d\n", *((ub4 *)ctxp));
  }

  *piecep = OCI_ONE_PIECE;

  ind[pos][iter][index] = 0;
  *indpp = (dvoid *) &ind[pos][iter][index];

  rc[pos][iter][index] = 0;
  *rcodepp = &rc[pos][iter][index];

  *alenp = &rl[pos][iter][index];

  return OCI_CONTINUE;
}


/* ----------------------------------------------------------------- */
/* allocate buffers for callback.                                    */
/* ----------------------------------------------------------------- */
sword alloc_buffer(pos, iter, rows)
ub4 pos;
ub4 iter;
ub4 rows;
{
  switch(pos)
  {
  case 0:
    break;
  case 1:
    out_zip[iter] = (text *) malloc(rows * MAXZIPLEN);
    break;
  default:
    (void) printf("ERROR: invalid position number: %d\n", pos);
    return OCI_ERROR;
  }

  ind[pos][iter] = (short *) malloc(rows * sizeof(short));
  rc[pos][iter] = (ub2 *) malloc(rows * sizeof(ub2));
  rl[pos][iter] = (ub4 *) malloc(rows * sizeof(ub4));

  return OCI_SUCCESS;
}




/* ----------------------------------------------------------------- */
/* demo for INSERT with RETURNING clause.                            */
/* ----------------------------------------------------------------- */
sword demo_insert(svchp, stmthp, bndhp, errhp)
OCISvcCtx *svchp;
OCIStmt *stmthp;
OCIBind *bndhp[];
OCIError *errhp;
{
  int       i;
  address   *addr;
  text *sqlstmt = (text *) "INSERT INTO EXTADDR E VALUES (:1, :2) \
                                RETURNING REF(E), ZIP INTO :addref, :zip";

  for (i = 0; i < MAXITER; i++)
  {
    memset((void *) in_state[i], (int)'A' + i%26, (size_t) 2);
    (void) sprintf((char *) in_zip[i], "%d", 91200 + i);
    in_zip[i][5] = '\0';

    if (OCIObjectNew(envhp, errhp, svchp, OCI_TYPECODE_REF,
                    (OCIType *) 0, (dvoid *) 0, OCI_DURATION_SESSION,
                     FALSE, (dvoid **) &addrref[i]))
    {
      (void) printf("FAILED: OCIObjectNew()\n");
      report_error(errhp);
      return OCI_ERROR;
    }
  }

  if (OCIStmtPrepare(stmthp, errhp, sqlstmt, (ub4)strlen((char *)sqlstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() insert\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  if (bind_input(stmthp, bndhp, errhp))
    return OCI_ERROR;

  if (bind_output(stmthp, bndhp, errhp))
    return OCI_ERROR;


  if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) MAXITER, (ub4) 0,
                    (CONST OCISnapshot*) 0, (OCISnapshot*) 0,
                    (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtExecute() insert\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  for (i = 0; i < MAXITER; i++)
  {
    addr = (address *) 0;

    if (addrref[i])
    {
      if (OCIObjectPin(envhp, errhp, addrref[i], (OCIComplexObject *)0,
                       OCI_PIN_ANY, OCI_DURATION_SESSION, OCI_LOCK_NONE,
                       (dvoid **)&addr))
      {
        (void) printf("FAILED: OCIObjectPin()\n");
        report_error(errhp);
        return OCI_ERROR;
      }

      if (addr)
        (void) printf("address.state = %.2s address.zip = %.10s\n",
                        OCIStringPtr(envhp, addr->state),
                        OCIStringPtr(envhp, addr->zip));
      else
        (void) printf("Pinned address pointer is null\n");

      if (OCIObjectUnpin(envhp, errhp, (dvoid *) addr))
      {
        (void) printf("FAILED: OCIObjectUnPin()\n");
        report_error(errhp);
        return OCI_ERROR;
      }
    }
    else
      (void) printf("%dth Address REF is NULL\n", i+1);
  }


  for (i = 0; i < MAXITER; i++)
    (void) OCIObjectMarkDelete(envhp, errhp, (dvoid *) addrref[i]);

  return OCI_SUCCESS;
}


/* end of file cdemodr3.c */

