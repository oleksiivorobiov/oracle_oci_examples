#ifdef RCSID
static char *RCSid =
   "$Header: cdemo82.c 04-apr-2005.17:08:37 lzhao Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1996, 2005, Oracle. All rights reserved.  
*/

/*

   NAME
     cdemo82.c - oci adt sample program ; run cdemo82.sql

   MODIFIED   (MM/DD/YY)
   lzhao       04/04/05 - bug4184289
   akatti      11/05/99 - [987191]:consider ret value of OCIDescribeAny
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   svedala     09/09/98 - lines longer than 79 chars reformatted - bug 722491
   svedala     02/17/98 - OCI obsoletion changes
   echen       06/03/97 - fix name resolution problem
   cchau       05/19/97 - change OCITypeByName to OCIDescribeAny
   azhao       01/30/97 - fix lint error
   azhao       01/11/97 - also check OCI_SUCCESS_WITH_INFO in fetch
   echen       01/03/97 - remove obsoleve type
   azhao       07/18/96 - use OCI_PIN_ANY,OCI_PIN_LATEST for ogiopin
   dchatter    07/18/96 - change ifdef for including cdemo82.h
   slari       07/15/96 - Creation

*/

#ifndef CDEMO82_ORACLE
#include <cdemo82.h>
#endif

#include <string.h>

#define SCHEMA "CDEMO82"


/*****************************************************************************/
static void pin_display_addr(envhp, errhp, addrref)
OCIEnv *envhp;
OCIError *errhp;
OCIRef *addrref;
{
  sword status;
  address *addr = (address *)0;

  checkerr(errhp, OCIObjectPin(envhp, errhp, addrref, (OCIComplexObject *)0,
                   OCI_PIN_ANY, OCI_DURATION_SESSION, OCI_LOCK_NONE,
                   (dvoid **)&addr));

  if (addr)
  {
    printf("address.state = %.2s address.zip = %.10s\n",
            OCIStringPtr(envhp, addr->state), OCIStringPtr(envhp, addr->zip));
  }
  else
  {
    printf("Pinned address pointer is null\n");
  }

  checkerr(errhp, OCIObjectUnpin(envhp, errhp, (dvoid *) addr));
}

/*****************************************************************************/
static void checkerr(errhp, status)
OCIError *errhp;
sword status;
{
  text errbuf[512];
  ub4 buflen;
  ub4 errcode;

  switch (status)
  {
  case OCI_SUCCESS:
    break;
  case OCI_SUCCESS_WITH_INFO:
    printf("Error - OCI_SUCCESS_WITH_INFO\n");
    break;
  case OCI_NEED_DATA:
    printf("Error - OCI_NEED_DATA\n");
    break;
  case OCI_NO_DATA:
    printf("Error - OCI_NO_DATA\n");
    break;
  case OCI_ERROR:
    OCIErrorGet ((dvoid *) errhp, (ub4) 1, (text *) NULL, (sb4 *)&errcode,
            errbuf, (ub4) sizeof(errbuf), (ub4) OCI_HTYPE_ERROR);
    printf("Error - %s\n", errbuf);
    break;
  case OCI_INVALID_HANDLE:
    printf("Error - OCI_INVALID_HANDLE\n");
    break;
  case OCI_STILL_EXECUTING:
    printf("Error - OCI_STILL_EXECUTE\n");
    break;
  case OCI_CONTINUE:
    printf("Error - OCI_CONTINUE\n");
    break;
  default:
    break;
  }
}

/*****************************************************************************/
/*
** Execute "selvalstmt" statement which selects records
** from a table with an ADT.
*/
static void selectval(envhp, svchp, stmthp, errhp)
OCIEnv *envhp;
OCISvcCtx *svchp;
OCIStmt *stmthp;
OCIError *errhp;
{
  OCIType *addr_tdo = (OCIType *) 0;
  OCIDefine *defn1p = (OCIDefine *) 0, *defn2p = (OCIDefine *) 0;
  address *addr = (address *)NULL;
  sword custno =0;
  int i = 0;
  OCIRef *addrref = (OCIRef *) 0;
  OCIRef *type_ref = (OCIRef *) 0;
  sb4 status;
  OCIDescribe *dschp = (OCIDescribe *) 0;
  OCIParam *parmp;
  sword retval;

  /* allocate describe handle for OCIDescribeAny */
  checkerr(errhp, OCIHandleAlloc((dvoid *) envhp, (dvoid **) &dschp,
                        (ub4) OCI_HTYPE_DESCRIBE,
                        (size_t) 0, (dvoid **) 0));

  /* define the application request  */
  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) selvalstmt,
                         (ub4) strlen((char *)selvalstmt),
                         (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  /* bind the input variable */
  checkerr(errhp, OCIDefineByPos(stmthp, &defn1p, errhp,
                          (ub4) 1, (dvoid *) &custno,
                          (sb4) sizeof(sword), SQLT_INT, (dvoid *) 0, (ub2 *)0,
                          (ub2 *)0, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIDefineByPos(stmthp, &defn2p, errhp, (ub4) 2, (dvoid *) 0,
                          (sb4) 0, SQLT_NTY, (dvoid *) 0, (ub2 *)0,
                          (ub2 *)0, (ub4) OCI_DEFAULT));

 /*  checkerr(errhp, OCITypeByName(envhp, errhp, svchp, (const text *) 0,
                   (ub4) 0, (const text *) "ADDRESS_VALUE",
                   (ub4) strlen((const char *) "ADDRESS_VALUE"),
                   (CONST text *) 0, (ub4) 0,
                   OCI_DURATION_SESSION,  OCI_TYPEGET_HEADER,
                   &addr_tdo)); */

  /* Bug 987191 - if describe call fails, no OCIAttrGet on desc handle */
  if ((retval = OCIDescribeAny(svchp, errhp, (text *)"ADDRESS_VALUE",
                  (ub4) strlen((char *)"ADDRESS_VALUE"), OCI_OTYPE_NAME,
                  (ub1)1, (ub1) OCI_PTYPE_TYPE, dschp)) != OCI_SUCCESS)
  {
     if (retval == OCI_NO_DATA)
     {
       printf("NO DATA: OCIDescribeAny on ADDRESS_VALUE\n");
     }
     else
     {
       printf( "ERROR: OCIDescribeAny on ADDRESS_VALUE\n");
       checkerr(errhp, retval);
       return;
     }
  }
  else
  {
     checkerr(errhp, OCIAttrGet((dvoid *) dschp, (ub4) OCI_HTYPE_DESCRIBE,
                  (dvoid *)&parmp, (ub4 *)0, (ub4)OCI_ATTR_PARAM, errhp));

     checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &type_ref, (ub4 *) 0,
                    (ub4) OCI_ATTR_REF_TDO, (OCIError *) errhp));
  }

  checkerr(errhp, OCIObjectPin(envhp, errhp, type_ref, (OCIComplexObject *) 0,
               OCI_PIN_ANY, OCI_DURATION_SESSION, OCI_LOCK_NONE,
               (dvoid **)&addr_tdo));

  if(!addr_tdo)
  {
    printf("NULL tdo returned\n");
    goto done_selectval;
  }


  checkerr(errhp, OCIDefineObject(defn2p, errhp, addr_tdo, (dvoid **) &addr,
                          (ub4 *) 0, (dvoid **) 0, (ub4 *) 0));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                         (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));

  /* execute and fetch */
  do
  {
    if (addr)
      printf("custno = %d address.state = %.2s address.zip = %.10s\n", custno,
             OCIStringPtr(envhp, addr->state),
             OCIStringPtr(envhp, addr->zip));
    else
      printf("custno = %d fetched address is NULL\n", custno);

    addr = (address *)NULL;
  }
  while ((status = OCIStmtFetch(stmthp, errhp, (ub4) 1, (ub4) OCI_FETCH_NEXT,
                               (ub4) OCI_DEFAULT)) == OCI_SUCCESS ||
                                status == OCI_SUCCESS_WITH_INFO);


  if ( status!= OCI_NO_DATA )
    checkerr(errhp, status);

  printf("\n\n");

 done_selectval:

 i = 0;  /* dummy statement, need something after label */
/*
  checkerr(errhp, OCIHandleFree((dvoid *) defn1p, (ub4) OCI_HTYPE_DEFINE));
  checkerr(errhp, OCIHandleFree((dvoid *) defn2p, (ub4) OCI_HTYPE_DEFINE));
*/

}

/*****************************************************************************/
/*
** Execute "selobjstmt" statement which selects records
** from a table with a ref.
*/
static void selectobj(envhp, svchp, stmthp, errhp)
OCIEnv *envhp;
OCISvcCtx *svchp;
OCIStmt *stmthp;
OCIError *errhp;
{
  OCIType *addr_tdo = (OCIType *) 0;
  OCIDefine *defn1p = (OCIDefine *) 0, *defn2p = (OCIDefine *) 0;
  sword status;
  OCIRef *addrref = (OCIRef *) 0, *addrref1 = (OCIRef *) 0;
  sword custno =0;
  int i = 0;
  address *addr;
  ub4     ref_len;


  /* define the application request  */
  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) selobjstmt,
                         (ub4) strlen((char *)selobjstmt),
                         (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIDefineByPos(stmthp, &defn1p, errhp, (ub4) 1,
                          (dvoid *) &custno,
                          (sb4) sizeof(sword), SQLT_INT, (dvoid *) 0, (ub2 *)0,
                          (ub2 *)0, (ub4) OCI_DEFAULT));

  addrref = (OCIRef *)NULL;
  checkerr(errhp, OCIDefineByPos(stmthp, &defn2p, errhp, (ub4) 2,
                          (dvoid *) NULL,
                          (sb4) 0, SQLT_REF, (dvoid *) 0, (ub2 *)0,
                          (ub2 *)0, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIDefineObject(defn2p, errhp, (OCIType *)NULL,
                          (dvoid **)&addrref,
                          &ref_len, (dvoid **)0, (ub4 *)0));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          (ub4) OCI_DEFAULT));

  do
  {
    printf("custno = %d fetched address\n", custno);

    if ( addrref )
    {
      pin_display_addr(envhp, errhp, addrref);
    }
    else
      printf("Address ref is NULL\n");

  }
  while ((status = OCIStmtFetch(stmthp, errhp, (ub4) 1,  (ub4) OCI_FETCH_NEXT,
                               (ub4) OCI_DEFAULT)) == OCI_SUCCESS ||
                                status == OCI_SUCCESS_WITH_INFO);


  if ( status != OCI_NO_DATA )
    checkerr(errhp, status);

  printf("\n\n");
/*
  checkerr(errhp, OCIHandleFree((dvoid *) defn1p, (ub4) OCI_HTYPE_DEFINE));
  checkerr(errhp, OCIHandleFree((dvoid *) defn2p, (ub4) OCI_HTYPE_DEFINE));
*/

}


/*****************************************************************************/
/*
 ** execute "insstmt"
 **
 */
static void insert(envhp, svchp, stmthp, errhp, insstmt, nrows)
OCIEnv *envhp;
OCISvcCtx *svchp;
OCIStmt *stmthp;
OCIError *errhp;
text *insstmt;
ub2   nrows;
{
  OCIType *addr_tdo = (OCIType *) 0;
  address  addrs;
  null_address naddrs;
  address *addr = &addrs;
  null_address *naddr = &naddrs;
  sword custno =300;
  OCIBind *bnd1p = (OCIBind *) 0, *bnd2p = (OCIBind *) 0;
  char buf[20];
  ub2 i;
  OCIRef *type_ref = (OCIRef *) 0;
  OCIDescribe *dschp = (OCIDescribe *) 0;
  OCIParam *parmp;
  sword retval;

  /* allocate describe handle for OCIDescribeAny */
  checkerr(errhp, OCIHandleAlloc((dvoid *) envhp, (dvoid **) &dschp,
                        (ub4) OCI_HTYPE_DESCRIBE,
                        (size_t) 0, (dvoid **) 0));

  /* define the application request  */
  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) insstmt,
                         (ub4) strlen((char *)insstmt),
                         (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  /* bind the input variable */
  checkerr(errhp, OCIBindByName(stmthp, &bnd1p, errhp, (text *) ":custno",
                          (sb4) -1, (dvoid *) &custno,
                          (sb4) sizeof(sword), SQLT_INT,
                          (dvoid *) 0, (ub2 *)0, (ub2 *)0, (ub4) 0, (ub4 *) 0,
                          (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIBindByName(stmthp, &bnd2p, errhp, (text *) ":addr",
                          (sb4) -1, (dvoid *) 0,
                          (sb4) 0, SQLT_NTY, (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                          (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT));

  /* checkerr(errhp, OCITypeByName(envhp, errhp, svchp, (const text *) 0,
                   (ub4) 0, (const text *) "ADDRESS_VALUE",
                   (ub4) strlen((const char *) "ADDRESS_VALUE"),
                   (CONST text *) 0, (ub4) 0,
                   OCI_DURATION_SESSION,  OCI_TYPEGET_HEADER,
                   &addr_tdo)); */

  /* Bug 987191-if describe call fails, don't call OCIAttrGet on dsc handle */
  if ((retval = OCIDescribeAny(svchp, errhp, (text *)"ADDRESS_VALUE",
                  (ub4) strlen((char *)"ADDRESS_VALUE"), OCI_OTYPE_NAME,
                  (ub1)1, (ub1) OCI_PTYPE_TYPE, dschp)) != OCI_SUCCESS)
  {
     if (retval == OCI_NO_DATA)
     {
       printf("NO DATA: OCIDescribeAny on ADDRESS_VALUE\n");
     }
     else
     {
       printf( "ERROR: OCIDescribeAny on ADDRESS_VALUE\n");
       checkerr(errhp, retval);
       return;
     }
  }
  else
  {
     checkerr(errhp, OCIAttrGet((dvoid *) dschp, (ub4) OCI_HTYPE_DESCRIBE,
                  (dvoid *)&parmp, (ub4 *)0, (ub4)OCI_ATTR_PARAM, errhp));

     checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &type_ref, (ub4 *) 0,
                    (ub4) OCI_ATTR_REF_TDO, (OCIError *) errhp));
  }

  checkerr(errhp, OCIObjectPin(envhp, errhp, type_ref, (OCIComplexObject *) 0,
               OCI_PIN_ANY, OCI_DURATION_SESSION, OCI_LOCK_NONE,
               (dvoid **)&addr_tdo));

  if(!addr_tdo)
  {
    printf("Null tdo returned\n");
    goto done_insert;
  }

  checkerr(errhp, OCIBindObject(bnd2p, errhp, addr_tdo, (dvoid **) &addr,
                          (ub4 *) 0, (dvoid **) &naddr, (ub4 *) 0));

  for(i = 0; i <= nrows; i++)
  {
    addr->state = (OCIString *) 0;
    sprintf(buf, "%cA", 65+i%27);
    checkerr(errhp, OCIStringAssignText(envhp, errhp,
                                        (CONST text*) buf, 2, &addr->state));
    addr->zip = (OCIString *) 0;
    sprintf(buf, "94%d    ", i+455);
    checkerr(errhp, OCIStringAssignText(envhp, errhp, (CONST text*) buf, 10,
                          &addr->zip));

    naddr->null_adt   = 0;
    naddr->null_state = 0;
    naddr->null_zip = 0;

    checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                            (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                            (ub4) OCI_DEFAULT));
  }
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));

 done_insert:

 i = 0;  /* dummy statement, need something after label */

/*
  checkerr(errhp, OCIHandleFree((dvoid *) bnd1p, (ub4) OCI_HTYPE_BIND));
  checkerr(errhp, OCIHandleFree((dvoid *) bnd2p, (ub4) OCI_HTYPE_BIND));
*/

}

/*****************************************************************************/
int main()
{
  OCIEnv *envhp;
  OCIServer *srvhp;
  OCIError *errhp;
  OCISvcCtx *svchp;
  OCIStmt *stmthp;
  OCISession *usrhp;

  OCIInitialize((ub4) OCI_THREADED | OCI_OBJECT, (dvoid *)0,
                (dvoid * (*)()) 0, (dvoid * (*)()) 0,  (void (*)()) 0 );

  OCIHandleAlloc( (dvoid *) NULL, (dvoid **) &envhp, (ub4) OCI_HTYPE_ENV,
           52, (dvoid **) &tmp);

  OCIEnvInit( &envhp, (ub4) OCI_DEFAULT, 21, (dvoid **) &tmp  );

  OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &errhp, (ub4) OCI_HTYPE_ERROR,
           52, (dvoid **) &tmp);
  OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &srvhp, (ub4) OCI_HTYPE_SERVER,
           52, (dvoid **) &tmp);

  OCIServerAttach( srvhp, errhp, (text *) 0, (sb4) 0, (ub4) OCI_DEFAULT);

  OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &svchp, (ub4) OCI_HTYPE_SVCCTX,
           52, (dvoid **) &tmp);

  /* set attribute server context in the service context */
  OCIAttrSet( (dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
           (dvoid *) srvhp, (ub4) 0,
           (ub4) OCI_ATTR_SERVER, (OCIError *) errhp);

  /* allocate a user context handle */
  OCIHandleAlloc((dvoid *)envhp, (dvoid **)&usrhp, (ub4) OCI_HTYPE_SESSION,
           (size_t) 0, (dvoid **) 0);

  OCIAttrSet((dvoid *)usrhp, (ub4)OCI_HTYPE_SESSION,
           (dvoid *)"cdemo82", (ub4)strlen((char *)"cdemo82"),
           OCI_ATTR_USERNAME, errhp);

  OCIAttrSet((dvoid *)usrhp, (ub4)OCI_HTYPE_SESSION,
           (dvoid *)"cdemo82", (ub4)strlen((char *)"cdemo82"),
           OCI_ATTR_PASSWORD, errhp);

  checkerr(errhp, OCISessionBegin (svchp, errhp, usrhp,
           OCI_CRED_RDBMS, OCI_DEFAULT));

  OCIAttrSet((dvoid *)svchp, (ub4)OCI_HTYPE_SVCCTX,
           (dvoid *)usrhp, (ub4)0,
           OCI_ATTR_SESSION, errhp);

  checkerr(errhp, OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &stmthp,
                           (ub4) OCI_HTYPE_STMT, 50, (dvoid **) &tmp));

  /* execute "insstmt" */
  printf("--- Test insertion into extent table.\n");
  insert(envhp, svchp, stmthp, errhp, insstmt, 26);

  /* execute "selstmt" */
  printf("--- Test selection of a table with one ADT column.\n");
  selectval(envhp, svchp, stmthp, errhp);


  /* execute "selobjstmt" */
  printf("--- Test selection of a table with one ADT REF.\n");
  selectobj(envhp, svchp, stmthp, errhp);


  checkerr(errhp, OCIHandleFree((dvoid *) stmthp, (ub4) OCI_HTYPE_STMT));

  OCISessionEnd(svchp, errhp, usrhp, (ub4)OCI_DEFAULT);
  OCIServerDetach( srvhp, errhp, (ub4) OCI_DEFAULT );
  checkerr(errhp, OCIHandleFree((dvoid *) srvhp, (ub4) OCI_HTYPE_SERVER));
  checkerr(errhp, OCIHandleFree((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX));
  checkerr(errhp, OCIHandleFree((dvoid *) errhp, (ub4) OCI_HTYPE_ERROR));

}

/* end of file cdemo82.c */

