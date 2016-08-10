#ifdef RCSID
static char *RCSid =
   "$Header: cdemodr2.c 10-oct-2006.14:39:58 azhao Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1997, 2006, Oracle. All rights reserved.  
*/

/*

   NAME
     cdemodr2.c - DML Returning LOBs.

   DESCRIPTION
     Demonstrate INSERT/UPDATE/DELETE statements RETURNING LOBS

     demo table:  TAB2 (C1 INTEGER, C2 BLOB, C3 CLOB)

     Before running cdemodr2, run cdemodr2.sql to create and populate
     TAB2 with some initial data.

     demo_insert() -- inserts 10 rows (with C1 = 1, 2, 3, ..., 10)
                   in null BLOBs, CLOBs for C2, C3, respectively.
                   The returned lob locators corresponding to these
                   BLOBs, CLOBs are then used for lob read/write.

     select_locator() -- shows that the locators returned from the INSERT
                   statement in demo_insert() are valid.

     demo_update() -- updates the rows within 3 ranges of C1.  The
                   lob locators used for updating C2, C3 have been
                   returned from demo_insert(), used by select_locator()
                   to select the BLOBs, CLOBs from a different range of C1.

     demo_delete() -- deletes the rows within 3 ranges of C1.  The returned
                   lob locators can be read but not written because those
                   rows associated with the returned locators have be deleted.


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

#include <cdemodr2.h>

static boolean logged_on = FALSE;

/* TAB2 columns */
static int             in1[MAXITER];           /* for INTEGER    */
static OCILobLocator  *in2[MAXITER];           /* for BLOB       */
static OCILobLocator  *in3[MAXITER];           /* for CLOB       */

/* output buffers */
static int            *p1[MAXITER];            /* for INTEGER   */
static OCILobLocator  **p2[MAXITER];           /* for BLOB      */
static OCILobLocator  **p3[MAXITER];           /* for CLOB      */

static short *ind[MAXCOLS][MAXITER];           /* indicators */
static ub2   *rc[MAXCOLS][MAXITER];            /* return codes */
static ub4   *rl[MAXCOLS][MAXITER];            /* return lengths */

/* skip values for binding TAB2 */
static ub4   s1 = (ub4) sizeof(in1[0]);
static ub4   s2 = (ub4) sizeof(in2[0]);
static ub4   s3 = (ub4) sizeof(in3[0]);

/* Rows returned in each iteration */
static ub2 rowsret[MAXITER];

/*  indicator skips */
static ub4   indsk[MAXCOLS] = { 0, 0, 0 };
/*  return length skips */
static ub4   rlsk[MAXCOLS] = { 0, 0, 0 };
/*  return code skips */
static ub4   rcsk[MAXCOLS] = { 0, 0, 0 };

static int   lowc1[MAXITER], highc1[MAXITER];

static ub4   pos[MAXCOLS];

static OCIEnv *envhp;
static OCIError *errhp;

static sb2 null_ind = -1;


/*------------------------end of Global variables--------------------*/


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
/* Print the returned raw data.                                      */
/* ----------------------------------------------------------------- */
void print_raw(raw, rawlen)
ub1 *raw;
ub4 rawlen;
{
  ub4 i;
  ub4 lim;
  ub4 clen = 0;

  if (rawlen > 120)
  {
    ub4 llen = rawlen;

    while (llen > 120)
    {
      lim = clen + 120;
      for(i = clen; i < lim; ++i)
          (void) printf("%02.2x", (ub4) raw[i] & 0xFF);

      (void) printf("\n");
      llen -= 120;
      clen += 120;
    }
    lim = clen + llen;
  }
  else
    lim = rawlen;

  for(i = clen; i < lim; ++i)
    (void) printf("%02.2x", (ub4) raw[i] & 0xFF);

  (void) printf("\n");

  return;
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
  OCISvcCtx *svchp;
  OCISession *authp;
  OCIStmt *stmthp = (OCIStmt *) NULL;
  OCIBind *bndhp[MAXBINDS];
  int i;

  if (init_handles(&envhp, &svchp, &errhp, &srvhp, &authp, (ub4)OCI_DEFAULT))
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
    (void) printf("SUCCESS: demo_insert()\n");

  if (select_locator(svchp, stmthp, bndhp, errhp))
    (void) printf("FAILED: select_locator()\n");
  else
    (void) printf("SUCCESS: select_locator()\n");

  if (demo_update(svchp, stmthp, bndhp, errhp))
    (void) printf("FAILED: demo_update()\n");
  else
    (void) printf("SUCCESS: demo_update()\n");


  if (demo_delete(svchp, stmthp, bndhp, errhp))
    (void) printf("FAILED: demo_delete()\n");
  else
    (void) printf("SUCCESS: demo_delete()\n");

  locator_free(in2, MAXITER);
  locator_free(in3, MAXITER);

  return finish_demo(logged_on, envhp, svchp, srvhp, errhp, authp,
                     stmthp, username);
}




/* ----------------------------------------------------------------- */
/* bind all the columns of TAB2 by positions.                        */
/* ----------------------------------------------------------------- */
sword bind_in_name(stmthp, bndhp, errhp)
OCIStmt *stmthp;
OCIBind *bndhp[];
OCIError *errhp;
{

  if (OCIBindByName(stmthp, &bndhp[0], errhp,
                      (text *) ":c1", (sb4) strlen((char *) ":c1"),
                      (dvoid *) &in1[0], (sb4) sizeof(in1[0]), SQLT_INT,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT)
   || OCIBindByName(stmthp, &bndhp[1], errhp,
                      (text *) ":c2", (sb4) strlen((char *) ":c2"),
                      (dvoid *) &in2[0], (sb4) -1, SQLT_BLOB,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT)
   || OCIBindByName(stmthp, &bndhp[2], errhp,
                      (text *) ":c3", (sb4) strlen((char *) ":c3"),
                      (dvoid *) &in3[0], (sb4) -1, SQLT_CLOB,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIBindByName()\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}


/* ----------------------------------------------------------------- */
/* bind all the columns of TAB2 by name.                             */
/* ----------------------------------------------------------------- */
sword bind_name(stmthp, bndhp, errhp)
OCIStmt *stmthp;
OCIBind *bndhp[];
OCIError *errhp;
{

  if (OCIBindByName(stmthp, &bndhp[10], errhp,
                      (text *) ":out1", (sb4) strlen((char *) ":out1"),
                      (dvoid *) 0, (sb4) sizeof(int), SQLT_INT,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DATA_AT_EXEC)
   || OCIBindByName(stmthp, &bndhp[11], errhp,
                      (text *) ":out2", (sb4) strlen((char *) ":out2"),
                      (dvoid *) 0, (sb4) -1, SQLT_BLOB,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DATA_AT_EXEC)
   || OCIBindByName(stmthp, &bndhp[12], errhp,
                      (text *) ":out3", (sb4) strlen((char *) ":out3"),
                      (dvoid *) 0, (sb4) -1, SQLT_CLOB,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DATA_AT_EXEC))
  {
    (void) printf("FAILED: OCIBindByName()\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}


/* ----------------------------------------------------------------- */
/* bind array structs for TAB2 columns.                              */
/* ----------------------------------------------------------------- */
sword bind_array(bndhp, errhp)
OCIBind *bndhp[];
OCIError *errhp;
{
  if (OCIBindArrayOfStruct(bndhp[0], errhp, s1, indsk[0], rlsk[0], rcsk[0])
   || OCIBindArrayOfStruct(bndhp[1], errhp, s2, indsk[1], rlsk[1], rcsk[1])
   || OCIBindArrayOfStruct(bndhp[2], errhp, s3, indsk[2], rlsk[2], rcsk[2]))
  {
    (void) printf("FAILED: OCIBindArrayOfStruct()\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}


/* ----------------------------------------------------------------- */
/* bind dynamic for returning TAB2 columns.                          */
/* ----------------------------------------------------------------- */
sword bind_dynamic(bndhp, errhp)
OCIBind *bndhp[];
OCIError *errhp;
{
  int  i;

  for (i = 0; i < MAXCOLS; i++)
    pos[i] = i;

  if (OCIBindDynamic(bndhp[10], errhp, (dvoid *)&pos[0], cbf_no_data,
                    (dvoid *)&pos[0], cbf_get_data)
  ||  OCIBindDynamic(bndhp[11], errhp, (dvoid *)&pos[1], cbf_no_data,
                    (dvoid *)&pos[1], cbf_get_data)
  ||  OCIBindDynamic(bndhp[12], errhp, (dvoid *)&pos[2], cbf_no_data,
                    (dvoid *)&pos[2], cbf_get_data))
  {
    (void) printf("FAILED: OCIBindDynamic()\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}



/* ----------------------------------------------------------------- */
/* bind input variables.                                             */
/* ----------------------------------------------------------------- */
sword bind_input(stmthp, bndhp, errhp)
OCIStmt *stmthp;
OCIBind *bndhp[];
OCIError *errhp;
{
  /* by the input data by names */
  if (bind_in_name(stmthp, bndhp, errhp))
    return OCI_ERROR;

  /* by input array */
  return (bind_array(bndhp, errhp));
}



/* ----------------------------------------------------------------- */
/* bind output variables.                                            */
/* ----------------------------------------------------------------- */
sword bind_output(stmthp, bndhp, errhp)
OCIStmt *stmthp;
OCIBind *bndhp[];
OCIError *errhp;
{

  /* by the returning data by names */
  if (bind_name(stmthp, bndhp, errhp))
    return OCI_ERROR;

  /* by dynamic of returning data by names */
  return (bind_dynamic(bndhp, errhp));
}



/* ----------------------------------------------------------------- */
/* bind row indicator variables.                                     */
/* ----------------------------------------------------------------- */
sword bind_low_high(stmthp, bndhp, errhp)
OCIStmt *stmthp;
OCIBind *bndhp[];
OCIError *errhp;
{
  if (OCIBindByName(stmthp, &bndhp[23], errhp,
                      (text *) ":low", (sb4) strlen((char *) ":low"),
                      (dvoid *) &lowc1[0], (sb4) sizeof(lowc1[0]), SQLT_INT,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT)
   || OCIBindByName(stmthp, &bndhp[24], errhp,
                      (text *) ":high", (sb4) strlen((char *) ":high"),
                      (dvoid *) &highc1[0], (sb4) sizeof(highc1[0]), SQLT_INT,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIBindByName()\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  if (OCIBindArrayOfStruct(bndhp[23], errhp, s1, indsk[0], rlsk[0], rcsk[0])
   || OCIBindArrayOfStruct(bndhp[24], errhp, s1, indsk[0], rlsk[0], rcsk[0]))
  {
    (void) printf("FAILED: OCIBindArrayOfStruct()\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}




/* ----------------------------------------------------------------- */
/* test for INSERT with RETURNING clause.                            */
/* ----------------------------------------------------------------- */
sword demo_insert(svchp, stmthp, bndhp, errhp)
OCISvcCtx *svchp;
OCIStmt *stmthp;
OCIBind *bndhp[];
OCIError *errhp;
{
  int  i, j;
  int  num[MAXITER];
  text *sqlstmt =(text *)"INSERT INTO TAB2(C1,C2,C3) VALUES \
                              (:c1, empty_blob(), empty_clob()) \
                              RETURNING C1, C2, C3 INTO \
                              :out1, :out2, :out3";

  (void) printf("\n\n========== TESTING INSERT....RETURNING \n");
  for (i = 0; i < MAXITER; i++)
    num[i] = i;

  for (i = 0; i < 1; i++)
  {
    if (OCIStmtPrepare(stmthp, errhp, sqlstmt, (ub4)strlen((char *)sqlstmt),
                      (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
    {
      (void) printf("FAILED: OCIStmtPrepare() insert\n");
      report_error(errhp);
      return OCI_ERROR;
    }

    if (OCIBindByName(stmthp, &bndhp[0], errhp,
                      (text *) ":c1", (sb4) strlen((char *) ":c1"),
                      (dvoid *) &num[0], (sb4) sizeof(num[0]), SQLT_INT,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT) )
    {
      (void) printf("FAILED: OCIStmtPrepare() insert\n");
      report_error(errhp);
      return OCI_ERROR;
    }

    if (OCIBindArrayOfStruct(bndhp[0], errhp, s1, indsk[0], rlsk[0], rcsk[0]))
    {
      (void) printf("FAILED: OCIStmtPrepare() insert\n");
      report_error(errhp);
      return OCI_ERROR;
    }

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
  }

  (void) check_lob((int)MAXITER, svchp);

  return OCI_SUCCESS;
}



/* ----------------------------------------------------------------- */
/* test for UPDATE with RETURNING clause.                            */
/* ----------------------------------------------------------------- */
sword demo_update(svchp, stmthp, bndhp, errhp)
OCISvcCtx *svchp;
OCIStmt *stmthp;
OCIBind *bndhp[];
OCIError *errhp;
{
  int    i, j;
  int    range_size = 3;

  text *sqlstmt = (text *)
                   "UPDATE TAB2 SET C1 = C1 + :c1, C2 = :c2, C3 = :c3 \
                           WHERE C1 >= :low AND C1 <= :high \
                           RETURNING C1, C2, C3 INTO :out1, :out2, :out3";

  (void) printf("\n\n========== TESTING UPDATE....RETURNING \n");

  if (OCIStmtPrepare(stmthp, errhp, sqlstmt, (ub4)strlen((char *)sqlstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() update\n");
    report_error(errhp);
    return OCI_ERROR;
  }


  for (i = 0; i < MAXITER; i++)
  {
    in1[i] = 301 + i;

    /* in2[], in3[] have been loaded in select_locator() */

    rowsret[i] =0;
  }

  /* bind input */
  if (bind_input(stmthp, bndhp, errhp))
    return OCI_ERROR;

  /* bind output */
  if (bind_output(stmthp, bndhp, errhp))
    return OCI_ERROR;

  /* bind row indicator low, high */
  if (bind_low_high(stmthp, bndhp, errhp))
    return OCI_ERROR;

  /* update rows
         between 101 and 103;  --  expecting 3 rows returned.
         between 105 and 106;  --  expecting 2 rows returned.
         between 108 and 110;  --  expecting 3 rows returned.
  */

  lowc1[0] = 101;
  highc1[0] = 103;

  lowc1[1] = 105;
  highc1[1] = 106;

  lowc1[2] = 109;
  highc1[2] = 113;

  if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) range_size, (ub4) 0,
                    (CONST OCISnapshot*) 0, (OCISnapshot*) 0,
                    (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtExecute() update\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  (void) check_lob(range_size, svchp);

  return OCI_SUCCESS;
}




/* ----------------------------------------------------------------- */
/* test for DELETE with RETURNING clause.                            */
/* ----------------------------------------------------------------- */
sword demo_delete(svchp, stmthp, bndhp, errhp)
OCISvcCtx *svchp;
OCIStmt *stmthp;
OCIBind *bndhp[];
OCIError *errhp;
{
  int    i, range_size = 3;
  sword  retval;

  text *sqlstmt = (text *)
                   "DELETE FROM TAB2 WHERE C1 >= :low AND C1 <= :high \
                            RETURNING C1, C2, C3 INTO :out1, :out2, :out3";

  (void) printf("\n\n========== TESTING DELETE....RETURNING \n");

  if (OCIStmtPrepare(stmthp, errhp, sqlstmt, (ub4)strlen((char *)sqlstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() delete\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  /* bind output */
  if (bind_output(stmthp, bndhp, errhp))
    return OCI_ERROR;

  /* bind row indicator low, high */
  if (bind_low_high(stmthp, bndhp, errhp))
    return OCI_ERROR;


  /* delete rows
         between 201 and 202;  --  expecting 2 rows returned.
         between 204 and 206;  --  expecting 3 rows returned.
         between 208 and 209;  --  expecting 2 rows returned.
  */

  lowc1[0] = 201;
  highc1[0] = 202;

  lowc1[1] = 204;
  highc1[1] = 206;

  lowc1[2] = 208;
  highc1[2] = 209;


  for (i=0; i<MAXITER; i++)
    rowsret[i] = 0;

  if ((retval = OCIStmtExecute(svchp, stmthp, errhp, (ub4) range_size, (ub4) 0,
                              (CONST OCISnapshot*) 0, (OCISnapshot*) 0,
                              (ub4) OCI_DEFAULT)) != OCI_SUCCESS  &&
                               retval != OCI_SUCCESS_WITH_INFO)
  {
    (void) printf("FAILED: OCIStmtExecute() delete, retval = %d\n", retval);
    report_error(errhp);
    return OCI_ERROR;
  }

  (void) check_lob(range_size, svchp);

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
sb4 cbf_get_data(ctxp, bindp, iter, index, bufpp, alenp, piecep, indpp,
                 rcodepp)
dvoid *ctxp;
OCIBind *bindp;
ub4 iter, index;
dvoid **bufpp;
ub4 **alenp;
ub1 *piecep;
dvoid **indpp;
ub2 **rcodepp;
{
  static ub4  bufl = 0;
  static ub4  rows = 0;
  ub4    pos = *((ub4 *)ctxp);

  if (index == 0)
  {
    (void) OCIAttrGet((CONST dvoid *) bindp, OCI_HTYPE_BIND, (dvoid *)&rows,
                      (ub4 *)sizeof(ub2), OCI_ATTR_ROWS_RETURNED, errhp);
    rowsret[iter] = (ub2)rows;

    if (alloc_buffer(pos, iter, rows))
      return OCI_ERROR;
  }

  switch(pos)
  {
  case 0:
    rl[pos][iter][index] = sizeof(int);
    *bufpp =  (dvoid *) (p1[iter]+index);
    break;
  case 1:
    rl[pos][iter][index] = (ub4) 86;
    *bufpp =  (dvoid *) p2[iter][index];
    break;
  case 2:
    rl[pos][iter][index] = (ub4) 86;
    *bufpp =  (dvoid *) p3[iter][index];
    break;
  default:
    bufl = 0;
    *bufpp = (dvoid *) 0;
    *alenp = (ub4 *) 0;
    (void) printf("ERROR: invalid position number: %d\n", *((ub2 *)ctxp));
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
ub4 pos, iter, rows;
{
  ub4  i;

  switch(pos)
  {
  case 0:
    p1[iter] = (int *) malloc(sizeof(int) * rows);
    break;
  case 1:
    p2[iter] = (OCILobLocator **) malloc(rows * sizeof(OCILobLocator *));
    if (locator_alloc(p2[iter], rows))
      return OCI_ERROR;
    break;
  case 2:
    p3[iter] = (OCILobLocator **) malloc(rows * sizeof(OCILobLocator *));
    if (locator_alloc(p3[iter], rows))
      return OCI_ERROR;
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
/* allocate locators.                                                */
/* ----------------------------------------------------------------- */
sword locator_alloc(lob, rows)
OCILobLocator *lob[];
ub4 rows;
{
  ub4 i;

  for (i = 0; i < rows; i++)
  {
    if (OCIDescriptorAlloc((dvoid *) envhp, (dvoid **) &lob[i],
                           (ub4) OCI_DTYPE_LOB, (size_t) 0, (dvoid **) 0))
    {
      (void) printf("FAILED: OCIDescriptorAlloc()\n");
      return OCI_ERROR;
    }
  }

  return OCI_SUCCESS;
}



/* ----------------------------------------------------------------- */
/* free locators.                                                    */
/* ----------------------------------------------------------------- */
sword locator_free(lob, rows)
OCILobLocator *lob[];
ub4 rows;
{
  ub4 i;

  for (i = 0; i < rows; i++)
  {
    if (OCIDescriptorFree((dvoid *) lob[i], (ub4) OCI_DTYPE_LOB))
    {
      (void) printf("FAILED: OCIDescriptorFree()\n");
      return OCI_ERROR;
    }
  }

  return OCI_SUCCESS;
}



/* ----------------------------------------------------------------- */
/* select BLOB, CLOB between rows 201 and 210 and use them to        */
/* update the rows between 101 and 110.                              */
/* ----------------------------------------------------------------- */
sword select_locator(svchp, stmthp, bndhp, errhp)
OCISvcCtx *svchp;
OCIStmt *stmthp;
OCIBind *bndhp[];
OCIError *errhp;
{
  OCIDefine *dfnhp[3];

  text *sqlstmt = (text *)
               "SELECT C2, C3  FROM TAB2 WHERE C1 BETWEEN 201 AND 210";

  if (locator_alloc(in2, MAXITER)
  ||  locator_alloc(in3, MAXITER))
  {
    (void) printf("FAILED: locator_alloc()\n");
    return OCI_ERROR;
  }

  if (OCIStmtPrepare(stmthp, errhp, sqlstmt, (ub4)strlen((char *)sqlstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() select\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  dfnhp[0] = (OCIDefine *) 0;
  dfnhp[1] = (OCIDefine *) 0;
  dfnhp[2] = (OCIDefine *) 0;

  if (OCIDefineByPos(stmthp, &dfnhp[0], errhp, (ub4) 1,
                    (dvoid *) &in2[0], (sb4) -1, (ub2) SQLT_BLOB,
                    (dvoid *) 0, (ub2 *) 0, (ub2 *) 0, (ub4) OCI_DEFAULT)
  ||  OCIDefineByPos(stmthp, &dfnhp[1], errhp, (ub4) 2,
                    (dvoid *) &in3[0], (sb4) -1, (ub2) SQLT_CLOB,
                    (dvoid *) 0, (ub2 *) 0, (ub2 *) 0, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIDefineByPos()\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  if (OCIDefineArrayOfStruct(dfnhp[0], errhp, sizeof(in2[0]),
                             indsk[0], rlsk[0], rcsk[0])
  ||  OCIDefineArrayOfStruct(dfnhp[1], errhp, sizeof(in3[0]),
                             indsk[1], rlsk[1], rcsk[1]))
  {
    (void) printf("FAILED: OCIDefineArrayOfStruct()\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) MAXITER, (ub4) 0,
                    (CONST OCISnapshot*) 0, (OCISnapshot*) 0,
                    (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtExecute() select\n");
    report_error(errhp);
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}



/* ----------------------------------------------------------------- */
/* check the lob returned with read/write.                           */
/* ----------------------------------------------------------------- */
sword check_lob(iters, svchp)
int iters;
OCISvcCtx *svchp;
{
  int i, j;

  for (i = 0; i < iters; i++)
  {
    (void) printf("\n*** ITERATION *** : %d\n", i);
    (void) printf("(...returning %d rows)\n",  rowsret[i]);

    for (j = 0; j < rowsret[i]  ; j++)
    {
      /* Column 1 */
      (void) printf("COL1 [%d]: ind = %d, rc = %d, retl = %d\n",
                            j, ind[0][i][j], rc[0][i][j], rl[0][i][j]);
      if (ind[0][i][j] == -1)
        (void) printf("COL1 [%d]: NULL\n", j);
      else
        (void) printf("COL1 [%d]: %d\n", j, *(p1[i]+j) );

      /* Column 2, BLOB */
      (void) printf("COL2 [%d]: ind = %d, rc = %d, retl = %d\n",
                            j, ind[1][i][j], rc[1][i][j], rl[1][i][j]);
      if (ind[1][i][j] == -1)
        (void) printf("COL2 [%d]: NULL\n", j);
      else
      {
        (void) printf("COL2 [%d]: \n", j);
        (void) read_piece(svchp, p2[i][j], SQLT_BLOB);
        (void) write_piece(svchp, p2[i][j], SQLT_BLOB);
      }

      /* Column 3, CLOB */
      (void) printf("COL3 [%d]: ind = %d, rc = %d, retl = %d\n",
                            j, ind[2][i][j], rc[2][i][j], rl[2][i][j]);
      if (ind[2][i][j] == -1)
        (void) printf("COL3 [%d]: NULL\n", j);
      else
      {
        (void) printf("COL3 [%d]: \n", j);
        (void) read_piece(svchp, p3[i][j], SQLT_CLOB);
        (void) write_piece(svchp, p3[i][j], SQLT_CLOB);
      }

    }

    (void) locator_free(p2[i], rowsret[i]);
    (void) locator_free(p3[i], rowsret[i]);
  }
  return OCI_SUCCESS;
}



/* ----------------------------------------------------------------- */
/* read a piece of the lob/file.                                     */
/* ----------------------------------------------------------------- */
sword read_piece(svchp, lob, lobtype)
OCISvcCtx *svchp;
OCILobLocator *lob;
ub2 lobtype;
{
  ub1  buf[MAXBUFLEN];
  ub4  amt = MAXBUFLEN;
  ub4  offset = 1;


  memset((void *)buf, (int) '\0', (size_t) MAXBUFLEN);

  if (OCILobRead(svchp, errhp, lob, &amt, offset, (dvoid *) buf,
                (ub4) MAXBUFLEN, (dvoid *)0,
                (sb4 (*) (dvoid *, CONST dvoid *, ub4, ub1)) 0,
                (ub2) 0, (ub1) SQLCS_IMPLICIT))
  {
     (void) printf("FAILED: OCILobRead()\n");
     report_error(errhp);
     return OCI_ERROR;
  }

  if (lobtype == SQLT_CLOB)
    (void) printf("READ buf:%.*s\n", amt, buf);
  else
  {
    (void) printf("READ buf: ");
    print_raw(buf, amt);
  }

  return OCI_SUCCESS;
}


/* ----------------------------------------------------------------- */
/* write a piece of the lob/file.                                    */
/* ----------------------------------------------------------------- */
sword write_piece(svchp, lob, lobtype)
OCISvcCtx *svchp;
OCILobLocator *lob;
ub2 lobtype;
{
  ub1  buf[MAXBUFLEN];
  ub4  amt = MAXBUFLEN;
  ub4  offset = 1;
  ub4  i;


  if (SQLT_CLOB)
    memset((void *)buf, (int) 'A', (size_t) MAXBUFLEN);
  else
    for (i = 0; i < MAXBUFLEN; i++)
      buf[i] = (ub1) (i%0x10);

  if (OCILobWrite(svchp, errhp, lob, &amt, offset, (dvoid *) buf,
                 (ub4) MAXBUFLEN, OCI_ONE_PIECE, (dvoid *)0,
                 (sb4 (*)(dvoid *, dvoid *, ub4 *, ub1 *)) 0,
                 (ub2) 0, (ub1) SQLCS_IMPLICIT))
  {
     report_error(errhp);
     (void) printf("FAILED: OCILobWrite()\n");
     return OCI_ERROR;
  }

  return OCI_SUCCESS;
}

/* end of file cdemodr2.c */

