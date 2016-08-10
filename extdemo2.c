#ifdef RCSID
static char *RCSid =
   "$Header: extdemo2.c 05-apr-2005.06:32:23 yhu Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1998, 2005, Oracle. All rights reserved.  
*/

/*

   NAME
     extdemo2.c - Extensible Indexing example implemented as C routines

   DESCRIPTION
     This file contains the definitions of the DML and Query routines
     for the extensible indexing example that implements a simple btree
     (sbtree). See extdemo2.sql for the SQL script that defines the
     indextype.

   PUBLIC FUNCTION(S)
     qxiqtbs - QXIQT Btree Start routine
     qxiqtbf - QXIQT Btree Fetch routine
     qxiqtbc - QXIQT Btree Close routine
     qxiqtbi - QXIQT Btree Insert routine
     qxiqtbd - QXIQT Btree Delete routine
     qxiqtbu - QXIQT Btree Update routine

   PRIVATE FUNCTION(S)
     qxiqtbe - QXIQT error reporting routine

   NOTES
     <other useful comments, qualifications, etc.>

   MODIFIED   (MM/DD/YY)
   yhu         04/04/05 - #4184410: fix porting exceptions in HP VMS 
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   spsundar    10/21/98 - fix ODCIIndexInsert routine
   rmurthy     06/16/98 - Creation

*/

#ifndef EXTDEMO2_ORACLE
# include "extdemo2.h"
#endif
#include <string.h> 
#include <stdio.h>

/*---------------------------------------------------------------------------
                     TYPES AND CONSTANTS
  ---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------
                           PUBLIC FUNCTIONS
  ---------------------------------------------------------------------------*/

/* ODCIIndexInsert function */
OCINumber *qxiqtbi(ctx, ix, ix_ind, rid, rid_ind,
                   newval, newval_ind)
OCIExtProcContext *ctx;
ODCIIndexInfo     *ix;
ODCIIndexInfo_ind *ix_ind;
char              *rid;
short             rid_ind;
char              *newval;
short             newval_ind;
{
  OCIEnv *envhp = (OCIEnv *) 0;                /* env. handle */
  OCISvcCtx *svchp = (OCISvcCtx *) 0;          /* service handle */
  OCIError *errhp = (OCIError *) 0;            /* error handle */
  OCIStmt *stmthp = (OCIStmt *) 0;             /* statement handle */
  OCIBind *bndp = (OCIBind *) 0;               /* bind handle */
  OCIBind *bndp1 = (OCIBind *) 0;              /* bind handle */

  int retval = (int)ODCI_SUCCESS;              /* return from this function */
  OCINumber *rval = (OCINumber *)0;
  ub4 key;                                     /* key value set in "self" */

  char insstmt[2000];                          /* sql insert statement */

  /* allocate memory for OCINumber first */
  rval = (OCINumber *)OCIExtProcAllocCallMemory(ctx, sizeof(OCINumber));

  /* Get oci handles */
  if (qxiqtce(ctx, errhp, OCIExtProcGetEnv(ctx, &envhp, &svchp, &errhp)))
    return(rval);

  /* set up return code */
  if (qxiqtce(ctx, errhp, OCINumberFromInt(errhp, (dvoid *)&retval,
                                           sizeof(retval),
                                           OCI_NUMBER_SIGNED, rval)))
    return(rval);

  /******************************
   * Construct insert Statement *
   ******************************/

   sprintf(insstmt,
                  "INSERT into %s.%s_sbtree values (:newval, :mrid)",
                  OCIStringPtr(envhp, ix->IndexSchema),
                  OCIStringPtr(envhp, ix->IndexName));

  /****************************************
   * Parse and Execute Create Statement   *
   ****************************************/

  /* allocate stmt handle */
  if (qxiqtce(ctx, errhp, OCIHandleAlloc((dvoid *)envhp,
                                         (dvoid **)&stmthp,
                                         (ub4)OCI_HTYPE_STMT, (size_t)0,
                                         (dvoid **)0)))
    return(rval);

  /* prepare the statement */
  if (qxiqtce(ctx, errhp, OCIStmtPrepare(stmthp, errhp, (text *)insstmt,
                                         (ub4)strlen(insstmt), OCI_NTV_SYNTAX,
                                         OCI_DEFAULT)))
    return(rval);



  /* Set up bind for newval */
  if (qxiqtce(ctx, errhp, OCIBindByPos(stmthp, &bndp, errhp, (ub4)1,
                                       (dvoid *)newval,
                                       (sb4)(strlen(newval)+1),
                                       (ub2)SQLT_STR, (dvoid *)0, (ub2 *)0,
                                       (ub2 *)0, (ub4)0, (ub4 *)0,
                                       (ub4)OCI_DEFAULT)))
    return(rval);

  /* Set up bind for rid */
  if (qxiqtce(ctx, errhp, OCIBindByPos(stmthp, &bndp, errhp, (ub4)2,
                                       (dvoid *)rid,
                                       (sb4)(strlen(rid)+1),
                                       (ub2)SQLT_STR, (dvoid *)0, (ub2 *)0,
                                       (ub2 *)0, (ub4)0, (ub4 *)0,
                                       (ub4)OCI_DEFAULT)))
    return(rval);


  /* Execute statement */
  if (qxiqtce(ctx, errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4)1,
                                         (ub4)0, (OCISnapshot *)NULL,
                                         (OCISnapshot *)NULL,
                                         (ub4)OCI_DEFAULT)))
    return(rval);

  return(rval);
}

/* ODCIIndexDelete function */
OCINumber *qxiqtbd(ctx, ix, ix_ind, rid, rid_ind,
                   oldval, oldval_ind)
OCIExtProcContext *ctx;
ODCIIndexInfo     *ix;
ODCIIndexInfo_ind *ix_ind;
char              *rid;
short             rid_ind;
char              *oldval;
short             oldval_ind;
{
  OCIEnv *envhp = (OCIEnv *) 0;                /* env. handle */
  OCISvcCtx *svchp = (OCISvcCtx *) 0;          /* service handle */
  OCIError *errhp = (OCIError *) 0;            /* error handle */
  OCIStmt *stmthp = (OCIStmt *) 0;             /* statement handle */
  OCIBind *bndp = (OCIBind *) 0;               /* bind handle */
  OCIBind *bndp1 = (OCIBind *) 0;              /* bind handle */

  int retval = (int)ODCI_SUCCESS;              /* return from this function */
  OCINumber *rval = (OCINumber *)0;
  ub4 key;                                     /* key value set in "self" */

  char delstmt[2000];                          /* sql insert statement */

  /* Get oci handles */
  if (qxiqtce(ctx, errhp, OCIExtProcGetEnv(ctx, &envhp, &svchp, &errhp)))
    return(rval);

  /* set up return code */
  rval = (OCINumber *)OCIExtProcAllocCallMemory(ctx, sizeof(OCINumber));
  if (qxiqtce(ctx, errhp, OCINumberFromInt(errhp, (dvoid *)&retval,
                                           sizeof(retval),
                                           OCI_NUMBER_SIGNED, rval)))
    return(rval);

  /******************************
   * Construct delete Statement *
   ******************************/

   sprintf(delstmt,
                  "DELETE FROM %s.%s_sbtree WHERE f2 = :rr",
                  OCIStringPtr(envhp, ix->IndexSchema),
                  OCIStringPtr(envhp, ix->IndexName));

  /****************************************
   * Parse and Execute delete Statement   *
   ****************************************/

  /* allocate stmt handle */
  if (qxiqtce(ctx, errhp, OCIHandleAlloc((dvoid *)envhp,
                                         (dvoid **)&stmthp,
                                         (ub4)OCI_HTYPE_STMT, (size_t)0,
                                         (dvoid **)0)))
    return(rval);

  /* prepare the statement */
  if (qxiqtce(ctx, errhp, OCIStmtPrepare(stmthp, errhp, (text *)delstmt,
                                         (ub4)strlen(delstmt), OCI_NTV_SYNTAX,
                                         OCI_DEFAULT)))
    return(rval);



  /* Set up bind for rid */
  if (qxiqtce(ctx, errhp, OCIBindByPos(stmthp, &bndp, errhp, (ub4)1,
                                       (dvoid *)rid,
                                       (sb4)(strlen(rid)+1),
                                       (ub2)SQLT_STR, (dvoid *)0, (ub2 *)0,
                                       (ub2 *)0, (ub4)0, (ub4 *)0,
                                       (ub4)OCI_DEFAULT)))
    return(rval);


  /* Execute statement */
  if (qxiqtce(ctx, errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4)1,
                                         (ub4)0, (OCISnapshot *)NULL,
                                         (OCISnapshot *)NULL,
                                         (ub4)OCI_DEFAULT)))
    return(rval);

  return(rval);
}

/* ODCIIndexUpdate function */
OCINumber *qxiqtbu(ctx, ix, ix_ind, rid, rid_ind,
                   oldval, oldval_ind, newval, newval_ind)
OCIExtProcContext *ctx;
ODCIIndexInfo     *ix;
ODCIIndexInfo_ind *ix_ind;
char              *rid;
short             rid_ind;
char              *oldval;
short             oldval_ind;
char              *newval;
short             newval_ind;
{
  OCIEnv *envhp = (OCIEnv *) 0;                /* env. handle */
  OCISvcCtx *svchp = (OCISvcCtx *) 0;          /* service handle */
  OCIError *errhp = (OCIError *) 0;            /* error handle */
  OCIStmt *stmthp = (OCIStmt *) 0;             /* statement handle */
  OCIBind *bndp = (OCIBind *) 0;               /* bind handle */
  OCIBind *bndp1 = (OCIBind *) 0;              /* bind handle */

  int retval = (int)ODCI_SUCCESS;              /* return from this function */
  OCINumber *rval = (OCINumber *)0;
  ub4 key;                                     /* key value set in "self" */

  char updstmt[2000];                          /* sql insert statement */

  /* Get oci handles */
  if (qxiqtce(ctx, errhp, OCIExtProcGetEnv(ctx, &envhp, &svchp, &errhp)))
    return(rval);

  /* set up return code */
  rval = (OCINumber *)OCIExtProcAllocCallMemory(ctx, sizeof(OCINumber));
  if (qxiqtce(ctx, errhp, OCINumberFromInt(errhp, (dvoid *)&retval,
                                           sizeof(retval),
                                           OCI_NUMBER_SIGNED, rval)))
    return(rval);

  /******************************
   * Construct update Statement *
   ******************************/

   sprintf(updstmt,
           "UPDATE %s.%s_sbtree SET f1 = :newval, f2 = :rr WHERE f1 = :oldval",
                  OCIStringPtr(envhp, ix->IndexSchema),
                  OCIStringPtr(envhp, ix->IndexName));

  /****************************************
   * Parse and Execute Create Statement   *
   ****************************************/

  /* allocate stmt handle */
  if (qxiqtce(ctx, errhp, OCIHandleAlloc((dvoid *)envhp,
                                         (dvoid **)&stmthp,
                                         (ub4)OCI_HTYPE_STMT, (size_t)0,
                                         (dvoid **)0)))
    return(rval);

  /* prepare the statement */
  if (qxiqtce(ctx, errhp, OCIStmtPrepare(stmthp, errhp, (text *)updstmt,
                                         (ub4)strlen(updstmt), OCI_NTV_SYNTAX,
                                         OCI_DEFAULT)))
    return(rval);


  /* Set up bind for newval */
  if (qxiqtce(ctx, errhp, OCIBindByPos(stmthp, &bndp, errhp, (ub4)1,
                                       (dvoid *)newval,
                                       (sb4)(strlen(newval)+1),
                                       (ub2)SQLT_STR, (dvoid *)0, (ub2 *)0,
                                       (ub2 *)0, (ub4)0, (ub4 *)0,
                                       (ub4)OCI_DEFAULT)))
    return(rval);

  /* Set up bind for rid */
  if (qxiqtce(ctx, errhp, OCIBindByPos(stmthp, &bndp, errhp, (ub4)2,
                                       (dvoid *)rid,
                                       (sb4)(strlen(rid)+1),
                                       (ub2)SQLT_STR, (dvoid *)0, (ub2 *)0,
                                       (ub2 *)0, (ub4)0, (ub4 *)0,
                                       (ub4)OCI_DEFAULT)))
    return(rval);

  /* Set up bind for oldval */
  if (qxiqtce(ctx, errhp, OCIBindByPos(stmthp, &bndp, errhp, (ub4)3,
                                       (dvoid *)oldval,
                                       (sb4)(strlen(oldval)+1),
                                       (ub2)SQLT_STR, (dvoid *)0, (ub2 *)0,
                                       (ub2 *)0, (ub4)0, (ub4 *)0,
                                       (ub4)OCI_DEFAULT)))
    return(rval);

  /* Execute statement */
  if (qxiqtce(ctx, errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4)1,
                                         (ub4)0, (OCISnapshot *)NULL,
                                         (OCISnapshot *)NULL,
                                         (ub4)OCI_DEFAULT)))
    return(rval);

  return(rval);
}

/* ODCIIndexStart function */
OCINumber *qxiqtbs(ctx, sctx, sctx_ind, ix, ix_ind, pr, pr_ind, qy, qy_ind,
                   strt, strt_ind, stop, stop_ind, cmpval, cmpval_ind)
OCIExtProcContext *ctx;
qxiqtim           *sctx;
qxiqtin           *sctx_ind;
ODCIIndexInfo     *ix;
dvoid             *ix_ind;
ODCIPredInfo      *pr;
dvoid             *pr_ind;
ODCIQueryInfo     *qy;
dvoid             *qy_ind;
OCINumber         *strt;
short             strt_ind;
OCINumber         *stop;
short             stop_ind;
char              *cmpval;
short             cmpval_ind;
{
  sword status;
  OCIEnv *envhp = (OCIEnv *) 0;                               /* env. handle */
  OCISvcCtx *svchp = (OCISvcCtx *) 0;                      /* service handle */
  OCIError *errhp = (OCIError *) 0;                          /* error handle */
  OCISession *usrhp = (OCISession *) 0;                       /* user handle */
  qxiqtcx *icx = (qxiqtcx *) 0;         /* state to be saved for later calls */

  int strtval;                                                /* start bound */
  int stopval;                                                 /* stop bound */

  int errnum = 29400;                     /* choose some oracle error number */
  char errmsg[512];                                  /* error message buffer */
  size_t errmsglen;                               /* Length of error message */

  char relop[3];                     /* relational operator used in sql stmt */
  char selstmt[2000];                                /* sql select statement */

  int retval = (int)ODCI_SUCCESS;               /* return from this function */
  OCINumber *rval = (OCINumber *)0;
  ub4 key;                                        /* key value set in "sctx" */

  /* Get oci handles */
  if (qxiqtce(ctx, errhp, OCIExtProcGetEnv(ctx, &envhp, &svchp, &errhp)))
    return(rval);

  /* set up return code */
  rval = (OCINumber *)OCIExtProcAllocCallMemory(ctx, sizeof(OCINumber));
  if (qxiqtce(ctx, errhp, OCINumberFromInt(errhp, (dvoid *)&retval,
                                           sizeof(retval),
                                           OCI_NUMBER_SIGNED, rval)))
    return(rval);

  /* get the user handle */
  if (qxiqtce(ctx, errhp, OCIAttrGet((dvoid *)svchp, (ub4)OCI_HTYPE_SVCCTX,
                                     (dvoid *)&usrhp, (ub4 *)0,
                                     (ub4)OCI_ATTR_SESSION,
                                     errhp)))
    return(rval);


  /**********************************************/
  /* Allocate memory to hold index scan context */
  /**********************************************/
  if (qxiqtce(ctx, errhp, OCIMemoryAlloc((dvoid *)usrhp, errhp,
                                         (dvoid **)&icx,
                                         OCI_DURATION_STATEMENT,
                                         (ub4)(sizeof(qxiqtcx)),
                                         OCI_MEMORY_CLEARED)))
    return(rval);

  icx->stmthp = (OCIStmt *)0;
  icx->defnp = (OCIDefine *)0;
  icx->bndp = (OCIBind *)0;

  /***********************************/
  /* Check that the bounds are valid */
  /***********************************/
  /* convert from oci numbers to native numbers */
  if (qxiqtce(ctx, errhp, OCINumberToInt(errhp, strt,
                                         sizeof(strtval), OCI_NUMBER_SIGNED,
                                         (dvoid *)&strtval)))
    return(rval);
  if (qxiqtce(ctx, errhp, OCINumberToInt(errhp, stop,
                                        sizeof(stopval),
                                        OCI_NUMBER_SIGNED, (dvoid *)&stopval)))
    return(rval);

  /* verify that strtval/stopval are both either 0 or 1 */
  if (!(((strtval == 0) && (stopval == 0)) ||
        ((strtval == 1) && (stopval == 1))))
    {
      strcpy(errmsg, (char *)"Incorrect predicate for sbtree operator");
      errmsglen = (size_t)strlen(errmsg);
      if (OCIExtProcRaiseExcpWithMsg(ctx, errnum, (text *)errmsg, errmsglen)
          != OCIEXTPROC_SUCCESS)
        /* Use cartridge error services here */;
      return(rval);
    }

  /*********************************************/
  /* Generate the SQL statement to be executed */
  /*********************************************/
  if (memcmp((dvoid *)OCIStringPtr(envhp, pr->ObjectName), (dvoid *)"EQ", 2)
      == 0)
    if (strtval == 1)
      strcpy(relop, (char *)"=");
    else
      strcpy(relop, (char *)"!=");
  else if (memcmp((dvoid *)OCIStringPtr(envhp, pr->ObjectName), (dvoid *)"LT",
                  2) == 0)
    if (strtval == 1)
      strcpy(relop, (char *)"<");
    else
      strcpy(relop, (char *)">=");
  else
    if (strtval == 1)
      strcpy(relop, (char *)">");
    else
      strcpy(relop, (char *)"<=");

  sprintf(selstmt, "select f2 from %s.%s_sbtree where f1 %s :val",
                  OCIStringPtr(envhp, ix->IndexSchema),
                  OCIStringPtr(envhp, ix->IndexName), relop);

  /***********************************/
  /* Parse, bind, define and execute */
  /***********************************/
  /* allocate stmt handle */
  if (qxiqtce(ctx, errhp,
              OCIHandleAlloc((dvoid *)envhp, (dvoid **)&(icx->stmthp),
                             (ub4)OCI_HTYPE_STMT, (size_t)0,
                             (dvoid **)0)))
    return(rval);
  /* prepare the statement */
  if (qxiqtce(ctx, errhp, OCIStmtPrepare(icx->stmthp, errhp, (text *)selstmt,
                                         (ub4)strlen(selstmt), OCI_NTV_SYNTAX,
                                         OCI_DEFAULT)))
    return(rval);

  /* Set up bind */
  if (qxiqtce(ctx, errhp,
              OCIBindByPos(icx->stmthp, &(icx->bndp), errhp, (ub4)1,
                           (dvoid *)cmpval,
                           (sb4)(strlen(cmpval)+1),
                           (ub2)SQLT_STR, (dvoid *)0, (ub2 *)0,
                           (ub2 *)0, (ub4)0, (ub4 *)0,
                           (ub4)OCI_DEFAULT)))
    return(rval);

  /* Set up define */
  if (qxiqtce(ctx, errhp, OCIDefineByPos(icx->stmthp, &(icx->defnp), errhp,
                                         (ub4)1, (dvoid *)(icx->ridp),
                                         (sb4) sizeof(icx->ridp),
                                         (ub2)SQLT_STR, (dvoid *)0, (ub2 *)0,
                                         (ub2 *)0, (ub4)OCI_DEFAULT)))
    return(rval);

  /* execute */
  if (qxiqtce(ctx, errhp, OCIStmtExecute(svchp, icx->stmthp, errhp, (ub4)0,
                                         (ub4)0, (OCISnapshot *)NULL,
                                         (OCISnapshot *)NULL,
                                         (ub4)OCI_DEFAULT)))
    return(rval);

  /************************************/
  /* Set index context to be returned */
  /************************************/
  /* generate a key */
  if (qxiqtce(ctx, errhp, OCIContextGenerateKey((dvoid *)usrhp, errhp, &key)))
    return(rval);
  /* set the memory address of the struct to be saved in the context */
  if (qxiqtce(ctx, errhp, OCIContextSetValue((dvoid *)usrhp, errhp,
                                             OCI_DURATION_STATEMENT,
                                             (ub1 *)&key, (ub1)sizeof(key),
                                             (dvoid *)icx)))
    return(rval);
  /* set the key as the member of "sctx" */
  if (qxiqtce(ctx, errhp, OCIRawAssignBytes(envhp, errhp, (ub1 *)&key,
                                            (ub4)sizeof(key),
                                            &(sctx->sctx_qxiqtim))))
    return(rval);

  sctx_ind->atomic_qxiqtin = OCI_IND_NOTNULL;
  sctx_ind->scind_qxiqtin = OCI_IND_NOTNULL;

  return(rval);
}


/* ODCIIndexFetch function */
OCINumber *qxiqtbf(ctx, self, self_ind, nrows, nrows_ind, rids, rids_ind)
OCIExtProcContext *ctx;
qxiqtim           *self;
qxiqtin           *self_ind;
OCINumber         *nrows;
short             nrows_ind;
OCIArray          **rids;
short             *rids_ind;
{
  sword status;
  OCIEnv *envhp = (OCIEnv *) 0;                               /* env. handle */
  OCISvcCtx *svchp = (OCISvcCtx *) 0;                      /* service handle */
  OCIError *errhp = (OCIError *) 0;                          /* error handle */
  OCISession *usrhp = (OCISession *) 0;                       /* user handle */
  qxiqtcx *icx = (qxiqtcx *) 0;         /* state to be saved for later calls */

  int idx = 1;
  int nrowsval;

  OCIArray *ridarrp = *rids;                             /* rowid collection */
  OCIString *ridstr = (OCIString *)0;

  int done = 0;
  int retval = (int)ODCI_SUCCESS;
  OCINumber *rval = (OCINumber *)0;

  ub1 *key;                                       /* key to retrieve context */
  ub4 keylen;                                               /* length of key */

  /*******************/
  /* Get OCI handles */
  /*******************/
  if (qxiqtce(ctx, errhp, OCIExtProcGetEnv(ctx, &envhp, &svchp, &errhp)))
      return(rval);

  /* set up return code */
  rval = (OCINumber *)OCIExtProcAllocCallMemory(ctx, sizeof(OCINumber));
  if (qxiqtce(ctx, errhp,
              OCINumberFromInt(errhp, (dvoid *)&retval, sizeof(retval),
                                           OCI_NUMBER_SIGNED, rval)))
    return(rval);

  /* get the user handle */
  if (qxiqtce(ctx, errhp, OCIAttrGet((dvoid *)svchp, (ub4)OCI_HTYPE_SVCCTX,
                                     (dvoid *)&usrhp, (ub4 *)0,
                                     (ub4)OCI_ATTR_SESSION, errhp)))
    return(rval);

  /********************************/
  /* Retrieve context from key    */
  /********************************/
  key = OCIRawPtr(envhp, self->sctx_qxiqtim);
  keylen = OCIRawSize(envhp, self->sctx_qxiqtim);

  if (qxiqtce(ctx, errhp, OCIContextGetValue((dvoid *)usrhp, errhp,
                                             key, (ub1)keylen,
                                             (dvoid **)&(icx))))
    return(rval);

  /* get value of nrows */
  if (qxiqtce(ctx, errhp, OCINumberToInt(errhp, nrows, sizeof(nrowsval),
                                       OCI_NUMBER_SIGNED, (dvoid *)&nrowsval)))
    return(rval);

  /****************/
  /* Fetch rowids */
  /****************/
  while (!done)
  {
    if (idx > nrowsval)
      done = 1;
    else
    {
      status = OCIStmtFetch(icx->stmthp, errhp, (ub4)1, (ub2) 0,
                            (ub4)OCI_DEFAULT);
      if (status == OCI_NO_DATA)
      {
        short col_ind = OCI_IND_NULL;
        /* have to create dummy oci string */
        OCIStringAssignText(envhp, errhp, (text *)"dummy",
                            (ub2)5, &ridstr);
        /* append null element to collection */
        if (qxiqtce(ctx, errhp, OCICollAppend(envhp, errhp,(dvoid *)ridstr,
                                              (dvoid *)&col_ind,
                                              (OCIColl *)ridarrp)))
          return(rval);
        done = 1;
      }
      else if (status == OCI_SUCCESS)
      {
        OCIStringAssignText(envhp, errhp, (text *)icx->ridp,
                            (ub2)18, (OCIString **)&ridstr);
        /* append rowid to collection */
        if (qxiqtce(ctx, errhp, OCICollAppend(envhp, errhp, (dvoid *)ridstr,
                                              (dvoid *)0, (OCIColl *)ridarrp)))
          return(rval);
        idx++;
      }
      else if (qxiqtce(ctx, errhp, status))
        return(rval);
    }
  }

  /* free ridstr finally */
  if (ridstr &&
      (qxiqtce(ctx, errhp, OCIStringResize(envhp, errhp, (ub4)0,
                                           &ridstr))))
    return(rval);

  *rids_ind = OCI_IND_NOTNULL;

  return(rval);
}


/* ODCIIndexClose function */
OCINumber *qxiqtbc(ctx, self, self_ind)
OCIExtProcContext *ctx;
qxiqtim           *self;
qxiqtin           *self_ind;
{
  sword status;
  OCIEnv *envhp = (OCIEnv *) 0;                               /* env. handle */
  OCISvcCtx *svchp = (OCISvcCtx *) 0;                      /* service handle */
  OCIError *errhp = (OCIError *) 0;                          /* error handle */
  OCISession *usrhp = (OCISession *) 0;                       /* user handle */
  qxiqtcx *icx = (qxiqtcx *) 0;         /* state to be saved for later calls */

  int retval = (int) ODCI_SUCCESS;
  OCINumber *rval = (OCINumber *)0;

  ub1 *key;                                       /* key to retrieve context */
  ub4 keylen;                                               /* length of key */

  if (qxiqtce(ctx, errhp, OCIExtProcGetEnv(ctx, &envhp, &svchp, &errhp)))
      return(rval);

  /* set up return code */
  rval = (OCINumber *)OCIExtProcAllocCallMemory(ctx, sizeof(OCINumber));
  if (qxiqtce(ctx, errhp, OCINumberFromInt(errhp, (dvoid *)&retval,
                                           sizeof(retval),
                                           OCI_NUMBER_SIGNED, rval)))
    return(rval);

  /* get the user handle */
  if (qxiqtce(ctx, errhp, OCIAttrGet((dvoid *)svchp, (ub4)OCI_HTYPE_SVCCTX,
                                     (dvoid *)&usrhp, (ub4 *)0,
                                     (ub4)OCI_ATTR_SESSION, errhp)))
    return(rval);

  /********************************/
  /* Retrieve context using key   */
  /********************************/
  key = OCIRawPtr(envhp, self->sctx_qxiqtim);
  keylen = OCIRawSize(envhp, self->sctx_qxiqtim);

  if (qxiqtce(ctx, errhp, OCIContextGetValue((dvoid *)usrhp, errhp,
                                             key, (ub1)keylen,
                                             (dvoid **)&(icx))))
    return(rval);

  /* Free handles and memory */
  if (qxiqtce(ctx, errhp, OCIHandleFree((dvoid *)icx->stmthp,
                                        (ub4)OCI_HTYPE_STMT)))
    return(rval);

  if (qxiqtce(ctx, errhp, OCIMemoryFree((dvoid *)usrhp, errhp, (dvoid *)icx)))
    return(rval);

  return(rval);
}


/*---------------------------------------------------------------------------
                          PRIVATE FUNCTIONS
  ---------------------------------------------------------------------------*/
static int qxiqtce(ctx, errhp, status)
OCIExtProcContext *ctx;
OCIError *errhp;
sword status;
{
  text errbuf[512];
  sb4 errcode = 0;
  int errnum = 29400;                     /* choose some oracle error number */
  int rc = 0;

  switch (status)
  {
  case OCI_SUCCESS:
    rc = 0;
    break;
  case OCI_ERROR:
    (void) OCIErrorGet((dvoid *)errhp, (ub4)1, (text *)NULL, &errcode,
                       errbuf, (ub4)sizeof(errbuf), OCI_HTYPE_ERROR);
    OCIExtProcRaiseExcpWithMsg(ctx, errnum, errbuf, strlen((char *)errbuf));
    rc = 1;
    break;
  default:
    (void) sprintf((char *)errbuf, "Warning - some error\n");
    OCIExtProcRaiseExcpWithMsg(ctx, errnum, errbuf, strlen((char *)errbuf));
    rc = 1;
    break;
  }
  return (rc);
}

/* end of file extdemo2.c */

