#ifdef RCSID
static char *RCSid =
   "$Header: cdemocor.c 18-feb-2005.15:07:29 aliu Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1995, 2005, Oracle. All rights reserved.  
*/

/*
   NAME
     cdemocor.c
   DESCRIPTION
     Demo COR user interface.  Run cdemocor.sql bfirst.

   NOTES
     see routines.

   MODIFIED   (MM/DD/YY)
    aliu     02/17/05 - lint issues 
    aliu     02/16/05 - fix bug 4184560 
    emendez  09/13/00 - fix top 5 olint errors
    mpjoshi  07/29/99 - updating file from 806 (Add OCICacheUnpin: HSBEDI)
    mjaeger  07/14/99 - bug 808870: OCCS: convert tabs, no long lines
    svedala  09/09/98 - lines longer than 79 chars reformatted - bug 722491
    svedala  07/02/98 - no handlealloc required for define handles
    tanguyen 08/19/97 -
    echen    06/04/97 -  fix warning mesg
    echen    05/31/97 -  Creation
*/

#ifndef CDEMOCOR_ORACLE
#include "cdemocor.h"
#endif

/*
 * COR is a prefetching mechanism which does not add any more
 * functionality but improves performance. This demo shows how
 * you can use COR to improve the performance of your application
 * by cutting down on the number of network roundtrips. Since it
 * may not be easy for you to track the actual network roundtrips
 * you can turn VERIFY_COR to TRUE which lets you verify that the
 * pin calls find the prefetched objects in the object cache and
 * thus do not incur a n/w roundtrip. By setting VERIFY_COR to
 * TRUE, you modify the data corresponding to the prefetched
 * objects (in the object cache) in the server. When you access
 * these prefetched objects on the client side, you get the ones
 * locally cached in the object cache (and not the new modified
 * ones in the server).
 */
#define VERIFY_COR FALSE

static const text *const selref = (text *)
            "SELECT REF(po) from po_table po where po.po_number = 1";
static const text *const selall = (text *)
            "SELECT REF(po) from po_table po";
static const text *sch_name_arr[] = {(const text *)0, (const text *)0};
static ub4   sch_name_len_arr[] = {(ub4)0, (ub4)0};
static const text *type_name_arr[] =
            {(const text *)"PURCHASE_ORDER", (const text *)"CUSTOMER"};
static ub4   type_name_len_arr[] = {(ub4)14, (ub4)8};
static const text *version_arr[] = {(const text *)0, (const text *)0};
static ub4   version_len_arr[] = {(ub4)0, (ub4)0};

/*****************************************************************************/
void checkerr1(errhp, status, file, line)
OCIError *errhp;
sword status;
const char *file;
int line;
{
  text errbuf[512];
  sb4 errcode;

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
    (void) printf("Error - OCI_NO_DATA\n");
    break;
  case OCI_ERROR:
    (void) OCIErrorGet ((dvoid *) errhp, (ub4) 1,
                    (text *) NULL, (sb4 *) &errcode,
                    errbuf, (ub4) sizeof(errbuf),
                    (ub4) OCI_HTYPE_ERROR);
    (void) printf("Error - %s, file %s, line %d\n",
                    errbuf, file, line);
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

static void demo_cor_1(envhp, svchp, stmthp, errhp)
OCIEnv         *envhp;
OCISvcCtx         *svchp;
OCIStmt         *stmthp;
OCIError         *errhp;
{
  OCIRef          *poref = (OCIRef *)0;
  OCIIter         *itr;
  boolean          eoc;
  purchase_order  *po = (purchase_order *)0;
  purchase_order  *related_po = (purchase_order *)0;
  customer        *cust = (customer *)0;
  OCIDefine         *defnp;
  OCIComplexObject         *corhp, *corhp3;
  OCIComplexObjectComp         *cordp1, *cordp3;
  OCIType         *custtdo, *custtdo2;
  OCIRef          *custref = (OCIRef *)0;
  OCIComplexObjectComp         *cordp2;
  OCIType         *po_tdo;
  ub4              po_size;
  OCIRef          *po_ref = (OCIRef *)0;
  ub4              level = 0;
  ub4              mylevel, siz, paramcount = 0;
  dvoid           *tdo_arr[2];
  dvoid           *tmp;
  sword            status = OCI_SUCCESS;
  OCIParam         *parmp;
  ub1              outofline = FALSE;
  int              age, po_num;
  address         *addr = (address *) 0;
  sb4              index;
  boolean          exist;
#ifdef VERIFY_COR
  text             *stmt11 = (text *)
  "update cust_table set name = 'Anil' where name = 'JOHN'";
  text             *stmt12 = (text *)
  "update cust_table set name = 'Chin' where name = 'MIKE'";
  text             *stmt13 = (text *)
  "insert into the (select addr from cust_table where name='Anil') \
    select 'ca', '90417', ref(x) from person_table x where name='JOHN2'";
  text             *stmt14 = (text *)
  "insert into the (select addr from cust_table where name='Chin') \
    select 'ca', '90417', ref(x) from person_table x where name='JOHN2'";
#endif

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) selref,
                           (ub4) strlen((char *) selref),
                           (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIDefineByPos(stmthp, &defnp, errhp, (ub4) 1,
                           (dvoid *) 0, (sb4) 0, SQLT_REF,
                           (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                           (ub4) OCI_DEFAULT));

  /* get type of purchase_order to set in COR descriptor */
  checkerr(errhp, OCITypeArrayByName(envhp, errhp, svchp, 2,
                         (CONST oratext **) sch_name_arr,
                         sch_name_len_arr,  (CONST oratext **) type_name_arr,
                         type_name_len_arr, (CONST oratext **) version_arr,
                         version_len_arr, OCI_DURATION_SESSION,
                         OCI_TYPEGET_ALL, (OCIType **) tdo_arr));
  po_tdo = (OCIType *)tdo_arr[0];
  custtdo = (OCIType *)tdo_arr[1];

  checkerr(errhp, OCIDefineObject(defnp, errhp, po_tdo, (dvoid **) &poref,
                  &po_size, (dvoid **) 0, (ub4 *) 0));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 0,
                  (ub4) 0, (OCISnapshot *)NULL,
                  (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtFetch(stmthp, errhp, (ub4) 1,  (ub4) OCI_FETCH_NEXT,
                  (ub4) OCI_DEFAULT));

  /*---------------------------- C O R --------------------------------*/
  /* get COR Handle */
  checkerr(errhp, OCIHandleAlloc((dvoid *)envhp, (dvoid **)&corhp,
                 (ub4)OCI_HTYPE_COMPLEXOBJECT, 0,
                 (dvoid **)0));

  /*---------------------------- put customer -------------------------*/
  /* get COR descriptor for type customer */
  checkerr(errhp, OCIDescriptorAlloc((dvoid *)envhp, (dvoid **)&cordp1,
                 (ub4)OCI_DTYPE_COMPLEXOBJECTCOMP, 0,
                 (dvoid **)0));

  /* set customer type in COR descriptor */
  checkerr(errhp, OCIAttrSet((dvoid *)cordp1,
                 (ub4)OCI_DTYPE_COMPLEXOBJECTCOMP, (dvoid *)custtdo,
                 (ub4)sizeof(dvoid *), (ub4)OCI_ATTR_COMPLEXOBJECTCOMP_TYPE,
                 (OCIError *)errhp));

  level = 2;
  /* set depth level for customer in COR descriptor */
  checkerr(errhp, OCIAttrSet((dvoid *)cordp1,
               (ub4)OCI_DTYPE_COMPLEXOBJECTCOMP,
               (dvoid *)&level, (ub4)sizeof(ub4),
               (ub4)OCI_ATTR_COMPLEXOBJECTCOMP_TYPE_LEVEL, (OCIError *)errhp));

  /* set attributes to COR handle */
  if (OCIAttrSet((dvoid *)corhp,(ub4) OCI_HTYPE_COMPLEXOBJECT,
                 (dvoid *)&outofline, (ub4)sizeof(ub1),
                 (ub4)OCI_ATTR_COMPLEXOBJECT_COLL_OUTOFLINE,
                 (OCIError *)errhp))
    (void) printf("BUG: set COR attribute out of line\n");

  if (OCIAttrSet((dvoid *)corhp,(ub4) OCI_HTYPE_COMPLEXOBJECT,
                 (dvoid *)&outofline, (ub4)sizeof(ub1), (ub4)OCI_ATTR_SERVER,
                 (OCIError *)errhp) != OCI_ERROR)
    (void) printf("BUG: set COR attribute to server\n");

  /* get attributes from COR handle */
  if (OCIAttrGet((dvoid *)corhp, (ub4)OCI_HTYPE_COMPLEXOBJECT,
                 (dvoid *) &mylevel, (ub4 *) &siz,
                 (ub4)OCI_ATTR_COMPLEXOBJECT_LEVEL, (OCIError *)errhp))
    (void) printf("BUG: get COR attribute\n");

  if (OCIAttrGet((dvoid *)corhp, (ub4)OCI_HTYPE_COMPLEXOBJECT,
                 (dvoid *) &paramcount, (ub4 *) &siz,
                 (ub4)OCI_ATTR_PARAM_COUNT, (OCIError *)errhp))
    (void) printf("BUG: get COR attribute\n");

  if (OCIAttrGet((dvoid *)corhp, (ub4)OCI_HTYPE_COMPLEXOBJECT,
                 (dvoid *) &outofline, (ub4 *) 0,
                 (ub4)OCI_ATTR_COMPLEXOBJECT_COLL_OUTOFLINE,
                 (OCIError *)errhp))
    (void) printf("BUG: get COR attribute\n");

  if (OCIAttrGet((dvoid *)corhp, (ub4)OCI_HTYPE_COMPLEXOBJECT,
                 (dvoid *) &outofline, (ub4 *) 0,
                 (ub4)OCI_ATTR_SCALE, (OCIError *)errhp) == OCI_SUCCESS)
    (void) printf("BUG: get COR attribute\n");


  /* get attributes from COR descriptor */
  if (OCIAttrGet((dvoid *)cordp1, (ub4)OCI_DTYPE_COMPLEXOBJECTCOMP,
                 (dvoid *) &custtdo2, (ub4 *) 0,
                 (ub4)OCI_ATTR_COMPLEXOBJECTCOMP_TYPE, (OCIError *)errhp))
    (void) printf("BUG: get COR attribute\n");

  if (OCIAttrGet((dvoid *)cordp1, (ub4)OCI_DTYPE_COMPLEXOBJECTCOMP,
                 (dvoid *) &custtdo2, (ub4 *) 0,
                 (ub4)OCI_ATTR_SCALE, (OCIError *)errhp) == OCI_SUCCESS)
    (void) printf("BUG: get COR attribute\n");

  if (OCIAttrGet((dvoid *)cordp1, (ub4)OCI_DTYPE_COMPLEXOBJECTCOMP,
                 (dvoid *) &mylevel, (ub4 *) 0,
                 (ub4)OCI_ATTR_COMPLEXOBJECTCOMP_TYPE_LEVEL,
                 (OCIError *)errhp))
    (void) printf("BUG: get COR attribute\n");

  if (mylevel != level)
    (void) printf("BUG: mylevel = %d, level = %d\n", mylevel, level);


  /* put COR descriptor in COR handle */
  checkerr(errhp, OCIParamSet(corhp, OCI_HTYPE_COMPLEXOBJECT, errhp,
                     cordp1, OCI_DTYPE_COMPLEXOBJECTCOMP, 1));

  if (OCIParamGet(corhp, OCI_HTYPE_COMPLEXOBJECT, errhp, (dvoid *) &corhp3, 1)
                != OCI_SUCCESS)
     (void) printf("BUG: get parm on corhp\n");

  if (OCIParamGet(stmthp, OCI_HTYPE_STMT, errhp, (dvoid *) &parmp, 1)
                != OCI_SUCCESS)
     (void) printf("BUG: get parm on stmthp with position 1\n");


  /*------------------------- put purchase_order ------------------------*/
  /* get COR descriptor for type purchase_order */
  checkerr(errhp, OCIDescriptorAlloc((dvoid *)envhp,
                 (dvoid **)&cordp2, (ub4)OCI_DTYPE_COMPLEXOBJECTCOMP, 0,
                 (dvoid **)0));

  /* set customer type in COR descriptor */
  checkerr(errhp, OCIAttrSet((dvoid *)cordp2,
                  (ub4)OCI_DTYPE_COMPLEXOBJECTCOMP, (dvoid *)po_tdo,
                  (ub4)sizeof(dvoid *), (ub4)OCI_ATTR_COMPLEXOBJECTCOMP_TYPE,
                  (OCIError *)errhp));

  level = 2;

  /* set depth level for customer in COR descriptor */
  checkerr(errhp, OCIAttrSet((dvoid *)cordp2,
                  (ub4)OCI_DTYPE_COMPLEXOBJECTCOMP, (dvoid *)&level,
                  (ub4)sizeof(ub4), (ub4)OCI_ATTR_COMPLEXOBJECTCOMP_TYPE_LEVEL,
                  (OCIError *)errhp));

  /* put COR descriptor in COR handle */
  checkerr(errhp, OCIParamSet(corhp, OCI_HTYPE_COMPLEXOBJECT,
                  errhp, cordp2, OCI_DTYPE_COMPLEXOBJECTCOMP, 2));


  if (OCIParamGet(corhp, OCI_HTYPE_COMPLEXOBJECT, errhp, (dvoid *) &cordp3, 1))
     (void) printf("BUG: get parm on cordp\n");


  /* pin the purchase order */
  checkerr(errhp, OCIObjectPin(envhp, errhp, poref, corhp, OCI_PIN_ANY,
          OCI_DURATION_SESSION, OCI_LOCK_NONE, (dvoid **)&po));

  checkerr(errhp, OCINumberToInt(errhp, &po->po_number,
                        sizeof(po_num), OCI_NUMBER_SIGNED,
                        (dvoid *) &po_num));
  (void) printf("     The po number is %d\n", po_num);

#ifdef VERIFY_COR
  /* Modify the data */
  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt11,
                        (ub4) strlen((char *) stmt11),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt12,
                        (ub4) strlen((char *) stmt12),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt13,
                        (ub4) strlen((char *) stmt13),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt14,
                        (ub4) strlen((char *) stmt14),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));
#endif

  /* pin the customer (shouldn't go over the n/w) */
  /* you should see MIKE and JOHN and should not see newly inserted records */
  checkerr(errhp, OCIObjectPin(envhp, errhp, po->cust,
          (OCIComplexObject *)0, OCI_PIN_ANY,
          OCI_DURATION_SESSION, OCI_LOCK_NONE,
          (dvoid **)&cust));

  (void) printf("The name is %s\n", OCIStringPtr(envhp, cust->name));
  checkerr(errhp, OCINumberToInt(errhp, &cust->age, sizeof(age),
                                  OCI_NUMBER_SIGNED, (dvoid *) &age));
  (void) printf("     The age is %d\n", age);

  (void) printf("\n---> Dump the table from the top to the bottom.\n");
  /* go to the first element and print out the index */
  checkerr(errhp, OCITableFirst(envhp, errhp, cust->addr, &index));
  (void) printf("     The index of the first element is : %d.\n", index);
  /* print out the element */
  checkerr(errhp, OCICollGetElem(envhp, errhp, (OCIColl *) cust->addr, index,
                         &exist, (void **) &addr, (dvoid **) 0));

  (void) printf("     address.state = %.2s address.zip = %.10s\n",
                       OCIStringPtr(envhp, addr->state),
                       OCIStringPtr(envhp, addr->zip));

  for(;!OCITableNext(envhp, errhp, index, cust->addr,
                                                     &index, &exist) && exist;)
  {
    (void) printf("     The index of the next element is : %d.\n", index);
    /* print out the element */
    checkerr(errhp, OCICollGetElem(envhp, errhp, (OCIColl *) cust->addr, index,
                         &exist, (void **) &addr, (dvoid **) 0));
    (void) printf("     address.state = %.2s address.zip = %.10s\n",
                       OCIStringPtr(envhp, addr->state),
                       OCIStringPtr(envhp, addr->zip));
  }

  /* pin the purchase_order (shouldn't go over the n/w) */
  checkerr(errhp, OCIObjectPin(envhp, errhp, po->related_orders,
          (OCIComplexObject *)0, OCI_PIN_ANY,
          OCI_DURATION_SESSION,
          OCI_LOCK_NONE, (dvoid **)&related_po));

  /* pin the customer (shouldn't go over the n/w) */
  checkerr(errhp, OCIObjectPin(envhp, errhp, related_po->cust,
          (OCIComplexObject *)0, OCI_PIN_ANY,
          OCI_DURATION_SESSION, OCI_LOCK_NONE,
          (dvoid **)&cust));

  (void) printf("The name is %s\n", OCIStringPtr(envhp, cust->name));
  checkerr(errhp, OCINumberToInt(errhp, &cust->age, sizeof(age),
                                 OCI_NUMBER_SIGNED, (dvoid *) &age));
  (void) printf("     The age is %d\n", age);

  (void) printf("\n---> Dump the table from the top to the bottom.\n");
  /* go to the first element and print out the index */
  checkerr(errhp, OCITableFirst(envhp, errhp, cust->addr, &index));
  (void) printf("     The index of the first element is : %d.\n", index);
  /* print out the element */
  checkerr(errhp, OCICollGetElem(envhp, errhp, (OCIColl *) cust->addr, index,
                         &exist, (void **) &addr, (dvoid **) 0));

  (void) printf("     address.state = %.2s address.zip = %.10s\n",
                       OCIStringPtr(envhp, addr->state),
                       OCIStringPtr(envhp, addr->zip));

  for(;!OCITableNext(envhp, errhp, index, cust->addr,
                                                     &index, &exist) && exist;)
  {
    (void) printf("     The index of the next element is : %d.\n", index);
    /* print out the element */
    checkerr(errhp, OCICollGetElem(envhp, errhp, (OCIColl *) cust->addr, index,
                         &exist, (void **) &addr, (dvoid **) 0));
    (void) printf("     address.state = %.2s address.zip = %.10s\n",
                       OCIStringPtr(envhp, addr->state),
                       OCIStringPtr(envhp, addr->zip));
  }
  
  checkerr(errhp, OCICacheUnpin(envhp, errhp, svchp)); 
  checkerr(errhp, OCICacheFree(envhp, errhp, svchp));

  /* free COR descriptor and COR handle */
  checkerr(errhp, OCIDescriptorFree((dvoid *)cordp1,
          (ub4)OCI_DTYPE_COMPLEXOBJECTCOMP));
  checkerr(errhp, OCIDescriptorFree((dvoid *)cordp2,
          (ub4)OCI_DTYPE_COMPLEXOBJECTCOMP));
  checkerr(errhp, OCIHandleFree((dvoid *)corhp,
          (ub4)OCI_HTYPE_COMPLEXOBJECT));
}


static void demo_cor_2(envhp, svchp, stmthp, errhp)
OCIEnv          *envhp;
OCISvcCtx       *svchp;
OCIStmt         *stmthp;
OCIError        *errhp;
{
  OCIType         *po_tdo;
  ub4              po_size;
  OCIRef          *poref = (OCIRef *)0;
  purchase_order  *po = (purchase_order *)0;
  purchase_order  *related_po = (purchase_order *)0;
  customer        *cust = (customer *)0;
  OCIDefine         *defnp;
  OCIComplexObject         *corhp;
  ub4              level = 0;
  dvoid           *tmp;
  sword            status = OCI_SUCCESS;
  int              age, po_num;
  address         *addr = (address *) 0;
  sb4              index;
  boolean          exist;
#ifdef VERIFY_COR
  text             *stmt11 = (text *)
  "update cust_table set name = 'JOHN' where name = 'Anil'";
  text             *stmt12 = (text *)
  "update cust_table set name = 'MIKE' where name = 'Chin'";
  text             *stmt13 = (text *)
  "insert into the (select addr from cust_table where name='JOHN') \
    select 'ca', '90418', ref(x) from person_table x where name='JOHN3'";
  text             *stmt14 = (text *)
  "insert into the (select addr from cust_table where name='MIKE') \
    select 'ca', '90418', ref(x) from person_table x where name='JOHN3'";
#endif

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *)
                          selref, (ub4) strlen((char *) selref),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIDefineByPos(stmthp, &defnp, errhp, (ub4) 1,
                          (dvoid *) 0, (sb4) 0, SQLT_REF,
                          (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                          (ub4) OCI_DEFAULT));

  /* get type of purchase_order to set in COR descriptor */
  OCITypeByName(envhp, errhp, svchp, (text *)0, (ub4)0,
          (text *)"PURCHASE_ORDER",
          (ub4)strlen((char *)"PURCHASE_ORDER"),
          (text *) 0, (ub4) 0, OCI_DURATION_SESSION,
          OCI_TYPEGET_HEADER, &po_tdo);

  checkerr(errhp, OCIDefineObject(defnp, errhp, po_tdo,
          (dvoid **) &poref, &po_size, (dvoid **) 0,
          (ub4 *) 0));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 0,
          (ub4) 0, (OCISnapshot *)NULL,
          (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtFetch(stmthp, errhp, (ub4) 1,  (ub4) OCI_FETCH_NEXT,
                  (ub4) OCI_DEFAULT));

  /*---------------------------- C O R --------------------------------*/
  /* get COR Handle */
  checkerr(errhp, OCIHandleAlloc((dvoid *)envhp, (dvoid **)&corhp,
           (ub4)OCI_HTYPE_COMPLEXOBJECT, 0,
           (dvoid **)0));

  level = 5;
  /* put level in COR handle */
  checkerr(errhp, OCIAttrSet((dvoid *)corhp, (ub4)OCI_HTYPE_COMPLEXOBJECT,
           (dvoid *)&level, (ub4)sizeof(ub4),
           (ub4)OCI_ATTR_COMPLEXOBJECT_LEVEL, (OCIError *)errhp));

  /* pin the purchase order */
  checkerr(errhp, OCIObjectPin(envhp, errhp, poref, corhp, OCI_PIN_ANY,
          OCI_DURATION_SESSION, OCI_LOCK_NONE, (dvoid **)&po));

#ifdef VERIFY_COR
  /* Modify the data */
  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt11,
                        (ub4) strlen((char *) stmt11),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt12,
                        (ub4) strlen((char *) stmt12),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt13,
                        (ub4) strlen((char *) stmt13),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt14,
                        (ub4) strlen((char *) stmt14),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));
#endif

  /* pin the customer (shouldn't go over the n/w) */
  /* you should see Anil and Chin                 */
  checkerr(errhp, OCIObjectPin(envhp, errhp, po->cust,
          (OCIComplexObject *)0, OCI_PIN_ANY,
          OCI_DURATION_SESSION, OCI_LOCK_NONE,
          (dvoid **)&cust));

  (void) printf("The name is %s\n", OCIStringPtr(envhp, cust->name));
  checkerr(errhp, OCINumberToInt(errhp, &cust->age, sizeof(age),
                                 OCI_NUMBER_SIGNED, (dvoid *) &age));
  (void) printf("     The age is %d\n", age);

  (void) printf("\n---> Dump the table from the top to the bottom.\n");
  /* go to the first element and print out the index */
  checkerr(errhp, OCITableFirst(envhp, errhp, cust->addr, &index));
  (void) printf("     The index of the first element is : %d.\n", index);
  /* print out the element */
  checkerr(errhp, OCICollGetElem(envhp, errhp, (OCIColl *) cust->addr, index,
                         &exist, (void **) &addr, (dvoid **) 0));

  (void) printf("     address.state = %.2s address.zip = %.10s\n",
                       OCIStringPtr(envhp, addr->state),
                       OCIStringPtr(envhp, addr->zip));

  for(;!OCITableNext(envhp, errhp, index, cust->addr,
                                                     &index, &exist) && exist;)
  {
    (void) printf("     The index of the next element is : %d.\n", index);
    /* print out the element */
    checkerr(errhp, OCICollGetElem(envhp, errhp, (OCIColl *) cust->addr, index,
                         &exist, (void **) &addr, (dvoid **) 0));
    (void) printf("     address.state = %.2s address.zip = %.10s\n",
                       OCIStringPtr(envhp, addr->state),
                       OCIStringPtr(envhp, addr->zip));
  }

  /* pin the purchase_order (shouldn't go over the n/w) */
  checkerr(errhp, OCIObjectPin(envhp, errhp, po->related_orders,
          (OCIComplexObject *)0, OCI_PIN_ANY,
          OCI_DURATION_SESSION,
          OCI_LOCK_NONE, (dvoid **)&related_po));

  /* pin the customer (shouldn't go over the n/w) */
  checkerr(errhp, OCIObjectPin(envhp, errhp, related_po->cust,
          (OCIComplexObject *)0, OCI_PIN_ANY,
          OCI_DURATION_SESSION, OCI_LOCK_NONE,
          (dvoid **)&cust));

  (void) printf("The name is %s\n", OCIStringPtr(envhp, cust->name));
  checkerr(errhp, OCINumberToInt(errhp, &cust->age, sizeof(age),
                                 OCI_NUMBER_SIGNED, (dvoid *) &age));
  (void) printf("     The age is %d\n", age);

  (void) printf("\n---> Dump the table from the top to the bottom.\n");
  /* go to the first element and print out the index */
  checkerr(errhp, OCITableFirst(envhp, errhp, cust->addr, &index));
  (void) printf("     The index of the first element is : %d.\n", index);
  /* print out the element */
  checkerr(errhp, OCICollGetElem(envhp, errhp, (OCIColl *) cust->addr, index,
                         &exist, (void **) &addr, (dvoid **) 0));

  (void) printf("     address.state = %.2s address.zip = %.10s\n",
                       OCIStringPtr(envhp, addr->state),
                       OCIStringPtr(envhp, addr->zip));

  for(;!OCITableNext(envhp, errhp, index, cust->addr,
                                                     &index, &exist) && exist;)
  {
    (void) printf("     The index of the next element is : %d.\n", index);
    /* print out the element */
    checkerr(errhp, OCICollGetElem(envhp, errhp, (OCIColl *) cust->addr, index,
                         &exist, (void **) &addr, (dvoid **) 0));
    (void) printf("     address.state = %.2s address.zip = %.10s\n",
                       OCIStringPtr(envhp, addr->state),
                       OCIStringPtr(envhp, addr->zip));
  }
  
  checkerr(errhp, OCICacheUnpin(envhp, errhp, svchp)); 
  checkerr(errhp, OCICacheFree(envhp, errhp, svchp));

  /* free COR handle */
  checkerr(errhp, OCIHandleFree((dvoid *)corhp, (ub4)OCI_HTYPE_COMPLEXOBJECT));
}

static void demo_cor_3(envhp, svchp, stmthp, errhp)
OCIEnv         *envhp;
OCISvcCtx         *svchp;
OCIStmt         *stmthp;
OCIError         *errhp;
{
  OCIType         *po_tdo;
  ub4              po_size;
  OCIRef          *poref = (OCIRef *)0;
  purchase_order  *po = (purchase_order *)0;
  purchase_order  *related_po = (purchase_order *)0;
  customer        *cust = (customer *)0;
  OCIDefine         *defnp;
  OCIComplexObject         *corhp;
  ub1              outofline;
  dvoid           *tmp;
  sword            status = OCI_SUCCESS;
  dvoid *elem = (dvoid *)0;
  dvoid *elemind = (dvoid *)0;
  sb4 collsiz;
  boolean exist;
  address *addr = (address *) 0;
  null_address *null_addr = (null_address *) 0;
  sb4 index = 0, counter = 0;
#ifdef VERIFY_COR
  text             *stmt11 = (text *)
  "update cust_table set name = 'Anil' where name = 'JOHN'";
  text             *stmt12 = (text *)
  "update cust_table set name = 'Chin' where name = 'MIKE'";
  text             *stmt13 = (text *)
  "insert into the (select addr from cust_table where name='Anil') \
    select NULL, '90419', ref(x) from person_table x where name='MIKE1'";
  text             *stmt14 = (text *)
  "insert into the (select addr from cust_table where name='Chin') \
    select NULL, '90419', ref(x) from person_table x where name='MIKE1'";
#endif

  /*-------------------------- get po ref -----------------------------*/
  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) selref,
                          (ub4) strlen((char *) selref),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIDefineByPos(stmthp, &defnp, errhp, (ub4) 1, (dvoid *) 0,
                          (sb4) 0, SQLT_REF, (dvoid *) 0, (ub2 *)0,
                          (ub2 *)0, (ub4) OCI_DEFAULT));

  /* get type of purchase_order to set in COR descriptor */
  OCITypeByName(envhp, errhp, svchp, (text *)0, (ub4)0,
          (text *)"PURCHASE_ORDER",
          (ub4)strlen((char *)"PURCHASE_ORDER"),
          (text *) 0, (ub4) 0, OCI_DURATION_SESSION,
          OCI_TYPEGET_HEADER, &po_tdo);

  checkerr(errhp, OCIDefineObject(defnp, errhp, po_tdo,
          (dvoid **) &poref, &po_size, (dvoid **) 0,
          (ub4 *) 0));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 0,
          (ub4) 0, (OCISnapshot *)NULL,
          (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtFetch(stmthp, errhp, (ub4) 1,  (ub4) OCI_FETCH_NEXT,
                  (ub4) OCI_DEFAULT));

  /*---------------------------- C O R --------------------------------*/
  /* get COR Handle */
  checkerr(errhp, OCIHandleAlloc((dvoid *)envhp, (dvoid **)&corhp,
           (ub4)OCI_HTYPE_COMPLEXOBJECT, 0,
           (dvoid **)0));

  /* pin the purchase order */
  checkerr(errhp, OCIObjectPin(envhp, errhp, poref,
          (OCIComplexObject *)0, OCI_PIN_ANY,
          OCI_DURATION_SESSION, OCI_LOCK_NONE, (dvoid **)&po));

  outofline = TRUE;
  /* put level in COR handle */
  checkerr(errhp, OCIAttrSet((dvoid *)corhp, (ub4)OCI_HTYPE_COMPLEXOBJECT,
           (dvoid *)&outofline, (ub4)sizeof(ub1),
           (ub4)OCI_ATTR_COMPLEXOBJECT_COLL_OUTOFLINE,
           (OCIError *)errhp));

  /* pin the customer (shouldn't go over the n/w) */
  checkerr(errhp, OCIObjectPin(envhp, errhp, po->cust, corhp, OCI_PIN_ANY,
               OCI_DURATION_SESSION, OCI_LOCK_NONE,
               (dvoid **)&cust));

#ifdef VERIFY_COR
  /* Modify the data */
  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt11,
                        (ub4) strlen((char *) stmt11),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt12,
                        (ub4) strlen((char *) stmt12),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));

  /* add new records */
  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt13,
                        (ub4) strlen((char *) stmt13),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt14,
                        (ub4) strlen((char *) stmt14),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));
#endif

  /* should see the new records */
  (void) printf("\n---> Dump the table from the top to the bottom.\n");
  /* go to the first element and print out the index */
  checkerr(errhp, OCITableFirst(envhp, errhp, cust->addr, &index));
  (void) printf("     The index of the first element is : %d.\n", index);
  /* print out the element */
  checkerr(errhp, OCICollGetElem(envhp, errhp, (OCIColl *) cust->addr, index,
                          &exist, (void **) &elem, &elemind));
  addr = (address *)elem;
  null_addr = (null_address *)elemind;
  (void) printf("     address.state = %.2s address.zip = %.10s\n",
                       OCIStringPtr(envhp, addr->state),
                       OCIStringPtr(envhp, addr->zip));
  (void) printf("     atomic null indicator is %d\n", null_addr->null_address);
  (void) printf("     zip null indicator is %d\n", null_addr->null_zip);
  (void) printf("     state null indicator is %d\n", null_addr->null_state);


  for(;!OCITableNext(envhp, errhp, index, cust->addr,
                                                     &index, &exist) && exist;)
  {
    (void) printf("     The index of the next element is : %d.\n", index);
    /* print out the element */
    checkerr(errhp, OCICollGetElem(envhp, errhp, (OCIColl *) cust->addr, index,
                            &exist, (void **) &elem, &elemind));
    addr = (address *)elem;
    null_addr = (null_address *)elemind;
    (void) printf("     address.state = %.2s address.zip = %.10s\n",
                         OCIStringPtr(envhp, addr->state),
                         OCIStringPtr(envhp, addr->zip));
    (void) printf("     atomic null indicator is %d\n",
                                                     null_addr->null_address);
    (void) printf("     zip null indicator is %d\n", null_addr->null_zip);
    (void) printf("     state null indicator is %d\n", null_addr->null_state);
  }


  /* pin the purchase_order (shouldn't go over the n/w) */
  checkerr(errhp, OCIObjectPin(envhp, errhp, po->related_orders,
          (OCIComplexObject *)0, OCI_PIN_ANY,
          OCI_DURATION_SESSION,
          OCI_LOCK_NONE, (dvoid **)&related_po));

  checkerr(errhp, OCICacheUnpin(envhp, errhp, svchp)); 
  checkerr(errhp, OCICacheFree(envhp, errhp, svchp));

  /* free COR handle */
  checkerr(errhp, OCIHandleFree((dvoid *)corhp, (ub4)OCI_HTYPE_COMPLEXOBJECT));
}

static void demo_cor_4(envhp, svchp, stmthp, errhp)
OCIEnv          *envhp;
OCISvcCtx       *svchp;
OCIStmt         *stmthp;
OCIError        *errhp;
{
  OCIType         *po_tdo;
  ub4              po_size[3], i;
  OCIRef          *poref[3];
  purchase_order  *po[3];
  purchase_order  *related_po = (purchase_order *)0;
  customer        *cust = (customer *)0;
  OCIDefine         *defnp;
  OCIComplexObject  *corhp[3];
  ub4              level = 0;
  dvoid           *tmp;
  sword            status = OCI_SUCCESS;
  int              age, po_num;
  address         *addr = (address *) 0;
  sb4              index;
  boolean          exist;
#ifdef VERIFY_COR
  text             *stmt11 = (text *)
  "update cust_table set name = 'JOHN' where name = 'Anil'";
  text             *stmt12 = (text *)
  "update cust_table set name = 'MIKE' where name = 'Chin'";
  text             *stmt13 = (text *)
  "insert into the (select addr from cust_table where name='JOHN') \
    select 'ca', '90418', ref(x) from person_table x where name='JOHN3'";
  text             *stmt14 = (text *)
  "insert into the (select addr from cust_table where name='MIKE') \
    select 'ca', '90418', ref(x) from person_table x where name='JOHN3'";
#endif

  for (i=0; i<3; i++)
    poref[i] = (OCIRef *) 0;

  for (i=0; i<3; i++)
    po[i] = (purchase_order *) 0;

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) selall,
                (ub4) strlen((char *) selall),
                (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIDefineByPos(stmthp, &defnp, errhp, (ub4) 1,
          (dvoid *) 0, (sb4) 0, SQLT_REF,
          (dvoid *) 0, (ub2 *)0, (ub2 *)0, (ub4) OCI_DEFAULT));

  /* get type of purchase_order to set in COR descriptor */
  OCITypeByName(envhp, errhp, svchp, (text *)0, (ub4)0,
          (text *)"PURCHASE_ORDER",
          (ub4)strlen((char *)"PURCHASE_ORDER"),
          (text *) 0, (ub4) 0, OCI_DURATION_SESSION,
          OCI_TYPEGET_HEADER, &po_tdo);

  checkerr(errhp, OCIDefineObject(defnp, errhp, po_tdo,
          (dvoid **) poref, po_size, (dvoid **) 0,
          (ub4 *) 0));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp,
          (ub4) 0, (ub4) 0, (OCISnapshot *)NULL,
          (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtFetch(stmthp, errhp, (ub4) 3,  (ub4) OCI_FETCH_NEXT,
                  (ub4) OCI_DEFAULT));

  /*---------------------------- C O R --------------------------------*/
  /* get COR Handle */
  for (i=0; i<3; i++)
    OCIHandleAlloc((dvoid *)envhp, (dvoid **)&corhp[i],
                   (ub4)OCI_HTYPE_COMPLEXOBJECT, 0,
                   (dvoid **)0);

  level = 1;
  /* put level in COR handle */
  for (i=0; i<3; i++)
    OCIAttrSet((dvoid *)corhp[i], (ub4)OCI_HTYPE_COMPLEXOBJECT,
           (dvoid *)&level, (ub4)sizeof(ub4),
           (ub4)OCI_ATTR_COMPLEXOBJECT_LEVEL, (OCIError *)errhp);

  checkerr(errhp, OCIObjectArrayPin(envhp, errhp, poref, (ub4) 3,
                corhp, (ub4) 3,
                OCI_PIN_LATEST, OCI_DURATION_SESSION,
                OCI_LOCK_NONE, (dvoid **) po, 0));

#ifdef VERIFY_COR
  /* modify the records */
  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt11,
                        (ub4) strlen((char *) stmt11),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt12,
                        (ub4) strlen((char *) stmt12),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));

  /* add new records */
  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt13,
                        (ub4) strlen((char *) stmt13),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) stmt14,
                        (ub4) strlen((char *) stmt14),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                         (ub4) 0, (OCISnapshot *) NULL,
                         (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));
#endif

  /* pin the purchase order */
  for (i=0; i<3; i++)
  {
    /* pin the customer (shouldn't go over the n/w) */
    /* you should see Anil and Chin                 */
    checkerr(errhp, OCIObjectPin(envhp, errhp, po[i]->cust,
            (OCIComplexObject *)0, OCI_PIN_ANY,
            OCI_DURATION_SESSION, OCI_LOCK_NONE,
            (dvoid **)&cust));

    (void) printf("The name is %s\n", OCIStringPtr(envhp, cust->name));
    checkerr(errhp, OCINumberToInt(errhp, &cust->age, sizeof(age),
                                   OCI_NUMBER_SIGNED, (dvoid *) &age));
    (void) printf("     The age is %d\n", age);

    (void) printf("\n---> Dump the table from the top to the bottom.\n");
    /* go to the first element and print out the index */
    checkerr(errhp, OCITableFirst(envhp, errhp, cust->addr, &index));
    (void) printf("     The index of the first element is : %d.\n", index);
    /* print out the element */
    checkerr(errhp, OCICollGetElem(envhp, errhp, (OCIColl *) cust->addr, index,
                           &exist, (void **) &addr, (dvoid **) 0));

    (void) printf("     address.state = %.2s address.zip = %.10s\n",
                         OCIStringPtr(envhp, addr->state),
                         OCIStringPtr(envhp, addr->zip));

    for(;!OCITableNext(envhp, errhp, index, cust->addr,
                                                     &index, &exist) && exist;)
    {
      (void) printf("     The index of the next element is : %d.\n", index);
      /* print out the element */
      checkerr(errhp, OCICollGetElem(envhp, errhp, (OCIColl *) cust->addr,
                              index, &exist, (void **) &addr, (dvoid **) 0));
      (void) printf("     address.state = %.2s address.zip = %.10s\n",
                         OCIStringPtr(envhp, addr->state),
                         OCIStringPtr(envhp, addr->zip));
    }

    /* pin the purchase_order (shouldn't go over the n/w) */
    checkerr(errhp, OCIObjectPin(envhp, errhp, po[i]->related_orders,
            (OCIComplexObject *)0, OCI_PIN_ANY,
            OCI_DURATION_SESSION,
            OCI_LOCK_NONE, (dvoid **)&related_po));

    /* pin the customer (shouldn't go over the n/w) */
    checkerr(errhp, OCIObjectPin(envhp, errhp, related_po->cust,
            (OCIComplexObject *)0, OCI_PIN_ANY,
            OCI_DURATION_SESSION, OCI_LOCK_NONE,
            (dvoid **)&cust));

    (void) printf("The name is %s\n", OCIStringPtr(envhp, cust->name));
    checkerr(errhp, OCINumberToInt(errhp, &cust->age, sizeof(age),
                                   OCI_NUMBER_SIGNED, (dvoid *) &age));
    (void) printf("     The age is %d\n", age);

    (void) printf("\n---> Dump the table from the top to the bottom.\n");
    /* go to the first element and print out the index */
    checkerr(errhp, OCITableFirst(envhp, errhp, cust->addr, &index));
    (void) printf("     The index of the first element is : %d.\n", index);
    /* print out the element */
    checkerr(errhp, OCICollGetElem(envhp, errhp, (OCIColl *) cust->addr, index,
                           &exist, (void **) &addr, (dvoid **) 0));

    (void) printf("     address.state = %.2s address.zip = %.10s\n",
                         OCIStringPtr(envhp, addr->state),
                         OCIStringPtr(envhp, addr->zip));

    for(;!OCITableNext(envhp, errhp, index, cust->addr,
                                                     &index, &exist) && exist;)
    {
      (void) printf("     The index of the next element is : %d.\n", index);
      /* print out the element */
      checkerr(errhp, OCICollGetElem(envhp, errhp, (OCIColl *) cust->addr,
                              index, &exist, (void **) &addr, (dvoid **) 0));
      (void) printf("     address.state = %.2s address.zip = %.10s\n",
                         OCIStringPtr(envhp, addr->state),
                         OCIStringPtr(envhp, addr->zip));
    }

  }

  checkerr(errhp, OCICacheUnpin(envhp, errhp, svchp)); 
  checkerr(errhp, OCICacheFree(envhp, errhp, svchp));

  /* free COR handle */
  for (i=0; i<3; i++)
    checkerr(errhp, OCIHandleFree((dvoid *)corhp[i],
                    (ub4)OCI_HTYPE_COMPLEXOBJECT));
}


int main(int argc, char *argv[])
{
  OCIEnv *envhp;
  OCISvcCtx *svchp;
  OCIError *errhp;
  OCIServer *srvhp;
  OCISession *usrhp;
  OCIStmt *stmthp;
  sword status = OCI_SUCCESS;
  dvoid *tmp = (dvoid *) 0;

  (void) OCIInitialize((ub4) OCI_OBJECT | OCI_DEFAULT,
                (dvoid *)0,  (dvoid * (*)()) 0,
                (dvoid * (*)()) 0,  (void (*)()) 0 );

  (void) OCIEnvInit( &envhp, (ub4) OCI_DEFAULT, 21, (dvoid **) &tmp  );

  (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &errhp,
                   (ub4) OCI_HTYPE_ERROR,
                   52, (dvoid **) &tmp);

  checkerr(errhp, OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &srvhp,
                   (ub4) OCI_HTYPE_SERVER,
                   52, (dvoid **) &tmp));

  checkerr(errhp, OCIServerAttach( srvhp, errhp, (text *) "",
                   (sb4) strlen((char *)""), (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &svchp,
                   (ub4) OCI_HTYPE_SVCCTX,
                   52, (dvoid **) &tmp));

  /* set attribute server context in the service context */
  checkerr(errhp, OCIAttrSet( (dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
                    (dvoid *) srvhp, (ub4) 0,
                    (ub4) OCI_ATTR_SERVER, (OCIError *) errhp));

  checkerr(errhp, OCIHandleAlloc((dvoid *)envhp, (dvoid **)&usrhp,
                   (ub4)OCI_HTYPE_SESSION, 0, (dvoid **)0));

  checkerr(errhp, OCIAttrSet((dvoid *) usrhp, (ub4)OCI_HTYPE_SESSION,
                   (dvoid *)"cdemocor", (ub4)strlen((char *)"cdemocor"),
                   (ub4)OCI_ATTR_USERNAME, errhp));

  checkerr(errhp, OCIAttrSet((dvoid *) usrhp, (ub4)OCI_HTYPE_SESSION,
                   (dvoid *)"cdemocor", (ub4)strlen((char *)"cdemocor"),
                   (ub4)OCI_ATTR_PASSWORD, errhp));

  checkerr(errhp, OCISessionBegin (svchp, errhp, usrhp, (ub4) OCI_CRED_RDBMS,
                   (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIAttrSet((dvoid *)svchp, (ub4)OCI_HTYPE_SVCCTX,
                   (dvoid *)usrhp, (ub4)0,
                   (ub4)OCI_ATTR_SESSION, errhp));

  /*-------------------------- get po ref -----------------------------*/
  checkerr(errhp, OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &stmthp,
                  (ub4) OCI_HTYPE_STMT, 50,
                  (dvoid **) &tmp));

  (void) printf("Complex object retrieval demo # 1\n");
  (void) printf("   --> Set level in COR descriptor\n");
  demo_cor_1(envhp, svchp, stmthp, errhp);
  (void) printf("Complex object retrieval demo # 2\n");
  (void) printf("   --> Set level in COR handle\n");
  demo_cor_2(envhp, svchp, stmthp, errhp);
  (void) printf("Complex object retrieval demo # 3\n");
  (void) printf("   --> Set outofline equal to TRUE\n");
  demo_cor_3(envhp, svchp, stmthp, errhp);
  (void) printf("Complex object retrieval demo # 4\n");
  (void) printf("   --> ArrayPin with COR\n");
  demo_cor_4(envhp, svchp, stmthp, errhp);

  OCIDescriptorFree((dvoid *)envhp, (ub4)OCI_HTYPE_ENV);

}

