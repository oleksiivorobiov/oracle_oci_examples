/* Copyright (c) 1996, 2001, Oracle Corporation.  All rights reserved.  */

/* 

  NAME
    cdemoin1 - Demo program which modifies an inherited type in a table and
    displays a record from the table.

  DESCRIPTION
    This program pins an inherited instance in the object cache and displays
    the attributes in it. It also updates a record of a table.

  NOTES 
    dependent files :
      cdemoin1.h     - Header file
      cdemoin1.sql   - SQL script to be run before execution
      cdemoin1.tsc   - test script (optional)

  Program Notes :
 
  MODIFIED (MM/DD/YY) 
    pzwu     05/18/01             - Fix bug1786565
    jchai    04/29/01            -  change schema
    rdwajan         08/22/00                   - Creation 
*/

#define NUMREC 3
#include "cdemoin1.h"

static OCIError    *errhp = (OCIError *)0;
static OCIEnv      *envhp = (OCIEnv *)0;
static OCIServer   *svrhp = (OCIServer *)0;
static OCISession  *sesnhp = (OCISession *)0;
static OCISvcCtx   *svchp = (OCISvcCtx *)0;
static OCIStmt     *stmthp = (OCIStmt *)0;

static OCIRef *workadd[NUMREC]; /* Array of REFs for workadd to be populated */
static text *database =(text *)"";
static text *username =(text *)"scott";
static text *password =(text *)"tiger";

static text *selectSql = (text *)
  "SELECT REF(a) FROM i_manager_tab a WHERE VALUE(a) IS OF (ONLY i_manager)";

static text *updateSql = (text *)
  "SELECT REF(a) FROM i_manager_tab a WHERE VALUE(a) IS OF (ONLY i_manager) FOR UPDATE";

static text *insertSql = (text *)
  "INSERT INTO i_people_tab VALUES (:v1)";

static text *selRef = (text *)
  "SELECT REF(a) FROM i_residence_tab a";

int main(/*void*/);
/* Free the allocated handles */
static void cleanup (void);

/* Function to modify a record of an inherited object table */
static void modifyFunction();

/* Function to select the ref of an inherited object */
static void selectFunction ();

/* Function to insert an instance of the subtype into a supertype table */
static void insertFunction();

/* Function to display the attributes of the object */
static void display (i_manager *sub_obj);

/* Function to get an array of REFs */
static void getRef();

static sword status = 0;

int main (void)
{
  printf
  ("cdemoin1 - Demonstrating select, insert and update on a subtype table\n");
  /* Initializing the environment in the Object mode*/

  OCIEnvCreate((OCIEnv **) &envhp, OCI_OBJECT, (dvoid *)0,
    (dvoid * (*)(dvoid *, size_t)) 0, (dvoid * (*)(dvoid *, dvoid *, size_t))0,
    (void (*)(dvoid *, dvoid *)) 0, (size_t) 0, (dvoid **) 0 );

  OCIHandleAlloc (envhp, (dvoid **)&errhp, OCI_HTYPE_ERROR, (size_t)0, 
    (dvoid **)0);

  OCIHandleAlloc(envhp, (dvoid **)&svrhp, OCI_HTYPE_SERVER, (size_t)0,
    (dvoid **)0);

  status = OCIServerAttach(svrhp, errhp, (text *)database,
    (sb4)strlen((char *)database), OCI_DEFAULT);

  if (status != OCI_SUCCESS)
  {
    printf("OCIServerAttach failed \n");
  }
  else
    printf("OCIServerAttach - Success \n");

  OCIHandleAlloc(envhp, (dvoid **)&svchp, OCI_HTYPE_SVCCTX, (size_t)0, 
    (dvoid **)0);
  OCIAttrSet(svchp, OCI_HTYPE_SVCCTX, (dvoid *)svrhp, (ub4)0, OCI_ATTR_SERVER, 
    errhp);

  OCIHandleAlloc(envhp, (dvoid **)&sesnhp, OCI_HTYPE_SESSION, (size_t)0, 
    (dvoid **)0);

  OCIAttrSet(sesnhp, OCI_HTYPE_SESSION, (dvoid *)username, 
    (ub4)strlen((char *)username), OCI_ATTR_USERNAME, errhp);

  OCIAttrSet(sesnhp, OCI_HTYPE_SESSION, (dvoid*)password,
    (ub4)strlen((char *)password), OCI_ATTR_PASSWORD, errhp);

  printf("Connecting as %s/%s@%s\n",username,password,database);

  status = OCISessionBegin(svchp, errhp, sesnhp, OCI_CRED_RDBMS, OCI_DEFAULT);
  if (status != OCI_SUCCESS)
  {
    printf("Connection  failed \n");
  }
  else
    printf("Connection - Success \n");

  OCIAttrSet(svchp, OCI_HTYPE_SVCCTX, sesnhp, (ub4)0, OCI_ATTR_SESSION, errhp);

  /* Calling function to get an array of REFs to populate the varray */
  getRef(); 

  /* Function to insert an instance of the subtype into a supertype table */
  insertFunction();
 
  /* Function to modify the data of an inherited object */
  modifyFunction();  

  /* Function to display data from the table */
  selectFunction();  

  /* Free the allocated handles */
  cleanup();

  printf("cdemoin1 - Done\n");
  return 1;
} /* End of main() */

/* Function to insert an instance of the subtype into a supertype table */
static void insertFunction()
{
  OCIBind *bindhp = (OCIBind *)0;
  i_residence elem;
  i_residence *elemptr = &elem;
  i_manager *sub_obj = (i_manager *)0;
  i_manager_ind ind;
  OCIType *i_manager_tdo = (OCIType *)0;

  ub4 subSize = 0;
  ub4 sizeUB4 = sizeof (ub4);
  sb4 size = 0;
  text *name = (text*)"JENNY";
  ub4 name_len =(ub4)( strlen( (char * ) name));
  ub4 ssn = 303; /* Data for ssn */
  ub4 addr_hno = 33; /*Data for  hno of addr */
  text *addr_street = (text *)"T33"; /*Data for street of addr */
  ub4 addr_streetLen = (ub4)(strlen( (char *)addr_street));
  ub4 altadrs_hno = 333; /*data for hno of altadrs */
  text *altadrs_street = (text *)"T333"; /*Data for street of altadrs */
  ub4 altadrs_streetLen = (ub4)(strlen( (char *)altadrs_street));
  ub4 empno = 3333; /* data for empno */

  sb4 index1 = 0; /* Index for the starting point of the scan */
  sb4 index2 = 0; /* Index of the next existing element */
  boolean eoc = TRUE; /* For getting the status for the availability of
                         the next index */
  ub4 count = 0; /* Loop counter */

  memset(&ind, 0, sizeof(ind));
  memset(elemptr, 0, sizeof(elem));
  OCIHandleAlloc (envhp, (dvoid **)&stmthp, OCI_HTYPE_STMT, (size_t)0,
    (dvoid **)0);

  if ((status = OCIStmtPrepare (stmthp, errhp, (text *)insertSql, 
    (ub4)strlen((char *)insertSql),OCI_NTV_SYNTAX, OCI_DEFAULT)) != OCI_SUCCESS)
  {
    printf ("OCIStmtPrepare - Fail\n");
  }

  if ((status = OCIBindByName(stmthp, &bindhp, errhp, (text *)":v1",
      (sb4) -1, (dvoid *)0, (sb4)0, SQLT_NTY, (dvoid *)0, (ub2 *)0, (ub2 *)0,
      (ub4)0, (ub4 *)0, OCI_DEFAULT)) != OCI_SUCCESS)
  {
    printf ("OCIBindByName - Failure \n");
  }

  if ((status = OCITypeByName (envhp, errhp, svchp, (CONST text *)0, (ub4)0,
    (CONST text *)"I_MANAGER", (ub4)strlen ((char *)"I_MANAGER"),
    (CONST text *)0, (ub4)0, OCI_DURATION_SESSION, OCI_TYPEGET_HEADER,
    &i_manager_tdo)) != OCI_SUCCESS)
  {
    printf ("OCITypeByName - Fail\n");
  }

  if (( status = OCIObjectNew(envhp, errhp, svchp, OCI_TYPECODE_OBJECT,
    (OCIType *)i_manager_tdo, (dvoid *)0, OCI_DURATION_SESSION, TRUE,
    (dvoid **) &sub_obj)) != OCI_SUCCESS)
  {
    printf ("OCIObjectNew - Failure\n");
  }
  OCINumberFromInt (errhp, (dvoid *)&ssn, sizeof(ssn), OCI_NUMBER_UNSIGNED,
    &(sub_obj->_super.ssn));
  OCINumberFromInt (errhp, (dvoid *)&empno, sizeof(empno), OCI_NUMBER_UNSIGNED,
    &(sub_obj->empno));
  
  OCINumberFromInt (errhp, (dvoid *)&addr_hno, sizeof(addr_hno),OCI_NUMBER_UNSIGNED,
    &(sub_obj->_super.addr.hno));
      
  OCIStringAssignText (envhp, errhp, (text *)name, (ub2)name_len,
    &(sub_obj->_super.name));
  OCIStringAssignText (envhp, errhp, (text *)addr_street, (ub2)addr_streetLen,
    &(sub_obj->_super.addr.street));
  for (count = 0; count < NUMREC; ++count)
  {
    OCIStringAssignText (envhp, errhp, (text *)altadrs_street, (ub2)altadrs_streetLen,
      &(elemptr->street));  
    OCINumberFromInt (errhp, (dvoid *)&altadrs_hno, sizeof(altadrs_hno),
      OCI_NUMBER_UNSIGNED, &(elemptr->hno));
    if (( status = OCICollAppend (envhp, errhp, (dvoid *)elemptr, (dvoid *)&ind,
      (OCIColl *)sub_obj->_super.altadrs)) != OCI_SUCCESS)
    {
      printf ("OCICollAppend - Fail\n");
    }
    altadrs_hno ++;
  } 
  for (count = 0; count < NUMREC; ++count)
  {
    if (( status = OCICollAppend (envhp, errhp, (dvoid *)workadd[count], 
      (dvoid *)&ind, (OCIColl *)sub_obj->workadd)) != OCI_SUCCESS)
    {
      printf ("OCICollAppend - Fail\n");
    }
  } 
  OCITableSize(envhp, errhp,(CONST OCITable*) (sub_obj->_super.altadrs), &size);
  OCITableSize(envhp, errhp,(CONST OCITable*) (sub_obj->workadd), &size);

  if (OCIBindObject(bindhp, errhp, i_manager_tdo,
    (dvoid **) &sub_obj, (ub4 *)0, (dvoid **)&ind , (ub4 *) 0) != OCI_SUCCESS)
  {
    printf("OCIBindObject - Failure \n");
  }

  printf ("\nExecuting the statement:%s\n", insertSql); 
  if (OCIStmtExecute (svchp, stmthp, errhp, (ub4)1, (ub4)0,
    (OCISnapshot *)0, (OCISnapshot *)0, OCI_COMMIT_ON_SUCCESS ) != OCI_SUCCESS)
  {
    printf("OCIStmtExecute - Failure \n");
  }
  else
    printf("OCIStmtExecute - Success\n");

  OCIHandleFree((dvoid *) stmthp, OCI_HTYPE_STMT);
}/* End on insertFunction() */

/* Function to modify the data of an inherited object */
static void modifyFunction ()
{ 
  OCIDefine *defhp = (OCIDefine *)0; 
  OCIRef *sub_ref = (OCIRef *)0;
  i_residence elem;
  i_residence *elemptr = (i_residence *)0;
  i_manager *sub_obj = (i_manager *)0;
  ub4 subSize = 0;
  ub4 sizeUB4 = sizeof (ub4);
  OCIType *mtype = (OCIType *)0;

  text *name = (text*)"JENNY";
  ub4 name_len=(ub4)strlen( (char * ) name);
  ub4 ssn = 808; /* Data for ssn */
  ub4 addr_hno = 800; /*Data for  hno of addr */
  text *addr_street = (text *)"Laurel Road"; /*Data for street of addr */
  ub4 addr_streetLen = (ub4)strlen( (char *)addr_street);
  ub4 altadrs_hno = 800; /*data for hno of altadrs */
  text *altadrs_street = (text *)"Shoreline Street"; /*Data for street of altadrs */
  ub4 altadrs_streetLen = (ub4)strlen( (char *)altadrs_street);
  ub4 empno = 8888; /* data for empno */

  sb4 index1 = 0; /* Index for the starting point of the scan */
  sb4 index2 = 0; /* Index of the next existing element */
  boolean eoc = TRUE; /* For getting the status for the availability of
                         the next index */
  ub4 count = 0; /* Loop counter */
  OCIHandleAlloc (envhp, (dvoid **)&stmthp, OCI_HTYPE_STMT, (size_t)0,
    (dvoid **)0);

  if ((status = OCIStmtPrepare (stmthp, errhp, (text *)updateSql, 
    (ub4)strlen((char *)updateSql),OCI_NTV_SYNTAX, OCI_DEFAULT)) != OCI_SUCCESS)
  {
    printf ("OCIStmtPrepare - Fail\n");
  }

  if (( status = OCIObjectNew(envhp, errhp, svchp, OCI_TYPECODE_REF,
    (OCIType *)0, (dvoid *)0, OCI_DURATION_DEFAULT, FALSE, 
    (dvoid **) &sub_ref)) != OCI_SUCCESS)
  {
    printf ("OCIObjectNew - Failure\n");
  }

  if ((status = OCIDefineByPos(stmthp, &defhp, errhp, (ub4) 1,
    (dvoid *) 0, (sb4) 0, SQLT_REF, (dvoid *) 0,
    (ub2 *)0, (ub2 *)0, (ub4) OCI_DEFAULT)) != OCI_SUCCESS)
  {
    printf ("OCIDefineByPos - Failure \n");
  }

  if ((status = OCIDefineObject(defhp, errhp, (OCIType *)0,
    (dvoid **)&sub_ref, (ub4 *)&subSize, (dvoid **) 0, (ub4 *) 0))
    != OCI_SUCCESS)
  {
    printf ("OCIDefineObject - Failure \n");
  }
 
  printf ("\nExecuting the statement:%s\n", updateSql); 
  if (OCIStmtExecute (svchp, stmthp, errhp, (ub4)1, (ub4)0,
    (OCISnapshot *)0, (OCISnapshot *)0, OCI_COMMIT_ON_SUCCESS ) != OCI_SUCCESS)
  {
    printf("OCIStmtExecute - Failure \n");
  }
  else
  {
    printf("OCIStmtExecute - Success\n");
    if ((status = OCIObjectPin(envhp, errhp, sub_ref, (OCIComplexObject *)0,
      OCI_PIN_ANY, OCI_DURATION_SESSION, OCI_LOCK_NONE, 
      (dvoid **) &sub_obj)) != OCI_SUCCESS)
    {
      printf("OCIObjectPin - Failure \n");
    }
    else
    {
      printf ("\nDisplaying the original data from the employee table \n");
      display (sub_obj);
      printf ("\nModifying the data present in the object\n");
      if ((status = OCIObjectMarkUpdate (envhp, errhp, (dvoid *)sub_obj))
        != OCI_SUCCESS)
      {
        printf ("OCIObjectMarkUpdate - Fail\n");
      }
      else
      {
        printf ("OCIObjectMarkUpdate - Success\n");
      } 
      OCINumberFromInt (errhp, (dvoid *)&ssn, sizeof(ssn), OCI_NUMBER_UNSIGNED,
        &(sub_obj->_super.ssn));
      OCINumberFromInt (errhp, (dvoid *)&empno, sizeof(empno), 
                       OCI_NUMBER_UNSIGNED, &(sub_obj->empno));
      OCINumberFromInt (errhp, (dvoid *)&addr_hno, sizeof(addr_hno),
               OCI_NUMBER_UNSIGNED, &(sub_obj->_super.addr.hno));
      OCIStringAssignText (envhp, errhp, (text *)name, (ub2)name_len,
        &(sub_obj->_super.name));
      OCIStringAssignText (envhp, errhp, (text *)addr_street, (ub2)addr_streetLen,
        &(sub_obj->_super.addr.street));
      OCITableFirst(envhp, errhp, (CONST OCITable*)sub_obj->_super.altadrs, 
         &index1);
      OCICollGetElem(envhp, errhp, (OCIColl *)sub_obj->_super.altadrs, 
        index1, (boolean *)&eoc, (dvoid **)&elemptr, (dvoid **)0);
      OCINumberFromInt (errhp, (dvoid *)&altadrs_hno, sizeof(altadrs_hno),
        OCI_NUMBER_UNSIGNED, &(elemptr->hno));
      OCIStringAssignText (envhp, errhp, (text *)altadrs_street, (ub2)altadrs_streetLen,
        &(elemptr->street));
      for (count = 0; count < NUMREC-1; ++count)
      {
        altadrs_hno += count;
        OCITableNext(envhp, errhp, index1,
         (CONST OCITable*) sub_obj->_super.altadrs, &index2, &eoc);
        OCICollGetElem(envhp, errhp, (OCIColl *)sub_obj->_super.altadrs, 
          index2, (boolean *)&eoc, (dvoid **)&elemptr, (dvoid **)0);
        OCINumberFromInt (errhp, (dvoid *)&altadrs_hno, sizeof(altadrs_hno),
          OCI_NUMBER_UNSIGNED, &(elemptr->hno));
        OCIStringAssignText (envhp, errhp, (text *)altadrs_street, (ub2)altadrs_streetLen,
          &(elemptr->street));
        index1 = index2;
      } 
      index1 = 0;
      index2 = 0;
      
      OCITableFirst(envhp, errhp, (CONST OCITable*)sub_obj->workadd, &index1);
      OCICollAssignElem (envhp, errhp, index1, (dvoid *)workadd[count],
       (dvoid *)0, (OCIColl *)sub_obj->workadd);
      for (count = 0; count < NUMREC; ++count)
      {
        OCITableNext(envhp, errhp, index1,(CONST OCITable*) sub_obj->workadd,
          &index2, &eoc);
        OCICollAssignElem (envhp, errhp, index1, (dvoid *)workadd[count],
          (dvoid *)0, (OCIColl *)sub_obj->workadd);
        index1 = index2;
      } 
      
      if ((status = OCIObjectFlush(envhp, errhp, sub_obj)) != OCI_SUCCESS)
      {
        printf ("OCIObjectFlush - Fail\n");
      }
      else
      {
        printf ("OCIObjectFlush - Success\n");
      }
      printf ("Refreshing the object\n");
      if ((status = OCIObjectRefresh(envhp, errhp, (dvoid *)sub_obj))
        != OCI_SUCCESS)
      {
        printf ("OCIObjectRefresh - Fail\n");
      }
      else
      {
        printf ("OCIObjectRefresh - Success\n");
      }
      printf ("\nDisplaying the data in the employee table after the refresh\n");
      display (sub_obj);

      printf ("\nModifying the data present in the object once again\n");
      ssn = 606; /* Data for ssn */
      addr_hno = 200; /*Data for  hno of addr */
      addr_street = (text *)"Main Street"; /*Data for street of addr */
      addr_streetLen = (ub4)strlen( (char *)addr_street);
      altadrs_hno = 600; /*data for hno of altadrs */
      altadrs_street = (text *)"Shell Blvd";/*Data for street of altadrs*/
      altadrs_streetLen = (ub4)strlen( (char *)altadrs_street);
      empno = 6666; /* data for empno */

      if ((status = OCIObjectMarkUpdate (envhp, errhp, (dvoid *)sub_obj))
        != OCI_SUCCESS)
      {
        printf ("OCIObjectMarkUpdate - Fail\n");
      }
      else
      {
        printf ("OCIObjectMarkUpdate - Success\n");
      }
      index1 = 0;
      index2 = 0;
      OCINumberFromInt (errhp, (dvoid *)&ssn, sizeof(ssn), OCI_NUMBER_UNSIGNED,
        &(sub_obj->_super.ssn));
      OCINumberFromInt (errhp, (dvoid *)&empno, sizeof(empno), OCI_NUMBER_UNSIGNED,
        &(sub_obj->empno));
      OCINumberFromInt (errhp, (dvoid *)&addr_hno, sizeof(addr_hno),
        OCI_NUMBER_UNSIGNED, &(sub_obj->_super.addr.hno));
      OCIStringAssignText (envhp, errhp, (text *)addr_street, 
         (ub2)addr_streetLen, &(sub_obj->_super.addr.street));
      OCITableFirst(envhp, errhp,(CONST OCITable*)sub_obj->_super.altadrs,
          &index1);
      OCICollGetElem(envhp, errhp, (OCIColl *)sub_obj->_super.altadrs, 
        index1, (boolean *)&eoc, (dvoid **)&elemptr, (dvoid **)0);
      OCINumberFromInt (errhp, (dvoid *)&altadrs_hno, sizeof(altadrs_hno),
        OCI_NUMBER_UNSIGNED, &(elemptr->hno));
      OCIStringAssignText (envhp, errhp, (text *)altadrs_street, (ub2)altadrs_streetLen,
        &(elemptr->street));
      for (count = 0; count < NUMREC-1; ++count)
      {
        altadrs_hno += count;
        OCITableNext(envhp, errhp, index1,(CONST OCITable*) sub_obj->_super.altadrs, &index2, 
          &eoc);
        OCICollGetElem(envhp, errhp, (OCIColl *)sub_obj->_super.altadrs, 
          index2, (boolean *)&eoc, (dvoid **)&elemptr, (dvoid **)0);
        OCINumberFromInt (errhp, (dvoid *)&altadrs_hno, sizeof(altadrs_hno),
          OCI_NUMBER_UNSIGNED, &(elemptr->hno));
        OCIStringAssignText (envhp, errhp, (text *)altadrs_street, (ub2)altadrs_streetLen,
          &(elemptr->street));
        index1 = index2;
      } 
      index1 = 0;
      index2 = 0;
      
      OCITableFirst(envhp, errhp, (CONST OCITable*)sub_obj->workadd, &index1);
      OCICollAssignElem (envhp, errhp, index1, (dvoid *)workadd[count],
       (dvoid *)0, (OCIColl *)sub_obj->workadd);
      for (count = 0; count < NUMREC; ++count)
      {
        OCITableNext(envhp, errhp, index1, (CONST OCITable*)sub_obj->workadd,
          &index2, &eoc);
        OCICollAssignElem (envhp, errhp, index1, (dvoid *)workadd[count],
          (dvoid *)0, (OCIColl *)sub_obj->workadd);
        index1 = index2;
      } 
      if ((status = OCICacheFlush (envhp, errhp, svchp, NULL, NULL, &sub_ref))
        != OCI_SUCCESS)
      {
        printf ("OCICacheFlush - Fail\n");
      }
      else
      {
        printf ("OCICacheFlush - Success\n");
      }
    }
        
    if ((status = OCITransCommit (svchp, errhp, OCI_DEFAULT)) != OCI_SUCCESS)
    {
      printf ("OCITransCommit - Fail\n");
    }
    else
      printf ("OCITransCommit - Success\n");
  }
  OCIHandleFree((dvoid *) stmthp, OCI_HTYPE_STMT);

} /* end of modifyFunction() */

/* Function to display data from the table */
static void selectFunction ()
{ 
  OCIDefine *defhp = (OCIDefine *)0;
  OCIRef *sub_ref = (OCIRef *)0;
  i_manager *sub_obj = (i_manager *)0;
  ub4 subSize = 0;
  ub4 sizeUB4 = sizeof (ub4);
  OCIType *mtype = (OCIType *)0;

  OCIHandleAlloc (envhp, (dvoid **)&stmthp, OCI_HTYPE_STMT, (size_t)0,
    (dvoid **)0);

  if ((status = OCIStmtPrepare (stmthp, errhp, (text *)selectSql, 
    (ub4)strlen((char *)selectSql),OCI_NTV_SYNTAX, OCI_DEFAULT)) != OCI_SUCCESS)
  {
    printf ("OCIStmtPrepare - Fail\n");
  }

  if (( status = OCIObjectNew(envhp, errhp, svchp, OCI_TYPECODE_REF,
    (OCIType *)0, (dvoid *)0, OCI_DURATION_DEFAULT, FALSE, 
    (dvoid **) &sub_ref)) != OCI_SUCCESS)
  {
    printf ("OCIObjectNew - Failure\n");
  }

  if ((status = OCIDefineByPos(stmthp, &defhp, errhp, (ub4) 1,
    (dvoid *) 0, (sb4) 0, SQLT_REF, (dvoid *) 0,
    (ub2 *)0, (ub2 *)0, (ub4) OCI_DEFAULT)) != OCI_SUCCESS)
  {
    printf ("OCIDefineByPos - Failure \n");
  }

  if ((status = OCIDefineObject(defhp, errhp, (OCIType *) 0,
    (dvoid **)&sub_ref, (ub4 *)&subSize, (dvoid **) 0, (ub4 *) 0))
    != OCI_SUCCESS)
  {
    printf ("OCIDefineObject - Failure \n");
  }
  
  printf ("\nExecuting the statement:%s\n", selectSql);
  if (OCIStmtExecute (svchp, stmthp, errhp, (ub4)1, (ub4)0,
    (OCISnapshot *)0, (OCISnapshot *)0, OCI_DEFAULT ) != OCI_SUCCESS)
  {
    printf("OCIStmtExecute - Failure \n");
  }
  else
  {
    printf("OCIStmtExecute - Success\n");
    if ((status = OCIObjectPin(envhp, errhp, sub_ref, (OCIComplexObject *)0,
      OCI_PIN_ANY, OCI_DURATION_SESSION, OCI_LOCK_NONE,
      (dvoid **) &sub_obj)) != OCI_SUCCESS)
    {
      printf("OCIObjectPin - Failure \n");
    }
    else
    {
      printf ("\nDisplaying the contents of the object\n");
      display (sub_obj);
    }
  }
} /* end of selectFunction() */

/* Function to display the attributes of the object */
static void display (i_manager *sub_obj)
{
  ub4 sizeUB4 = sizeof (ub4);
  ub4 ssn = 0; /* Variable to fetch ssn */
  ub4 addr_hno = 0; /*Variable to fetch hno of addr */
  text *name; /*Variable to fetch name */
  ub4 name_len = 0;
  text *addr_street; /*Variable to fetch street of addr */
  ub4 addr_streetLen = 0;
  ub4 altadrs_hno = 0; /*Variable to fetch hno of altadrs */
  text *altadrs_street; /*Variable to fetch street of altadrs */
  ub4 altadrs_streetLen = 0;
  ub4 empno = 0; /* Variable to fetch empno */
  ub4 workadd_hno = 0; /*Variable to fetch hno of workadd */
  text *workadd_street; /*Variable to fetch street of workadd */
  ub4 workadd_streetLen = 0;

  sb4 size = 111;
  sb4 index1 = 0; /* Index for the starting point of the scan */
  sb4 index2 = 0; /* Index of the next existing element */
  boolean eoc = TRUE; /* For getting the status for the availability of
                         the next index */
  ub4 count = 0;
  i_residence *elem = (i_residence *)0; /* To get the element from the 
                                           nested table */
  i_residence_ref *elemref = (i_residence_ref *)0;
  i_residence *objptr = (i_residence *)0;

  name = OCIStringPtr(envhp, sub_obj->_super.name);
  name_len =  OCIStringSize (envhp, sub_obj->_super.name);
  printf("name is : %.*s\n", name_len, name);
  OCINumberToInt (errhp, &(sub_obj->_super.ssn), sizeof(ub4),
    OCI_NUMBER_UNSIGNED, (dvoid *)&ssn);
  printf("SSN: %d\n", ssn);
  OCINumberToInt (errhp, &(sub_obj->_super.addr.hno), sizeof(ub4),
    OCI_NUMBER_UNSIGNED, (dvoid *)&addr_hno);
  addr_street = OCIStringPtr(envhp, sub_obj->_super.addr.street);
  addr_streetLen =  OCIStringSize (envhp, sub_obj->_super.addr.street);
  printf("Primary address:   %d %.*s\n", addr_hno,addr_streetLen, addr_street);

  OCITableSize(envhp, errhp, (CONST OCITable*)(sub_obj->_super.altadrs), &size);
  OCITableFirst(envhp, errhp,(CONST OCITable*)sub_obj->_super.altadrs, &index1);
  status = OCICollGetElem(envhp, errhp, (OCIColl *)sub_obj->_super.altadrs, 
    index1, (boolean *)&eoc, (dvoid **)&elem, (dvoid **)0);
  OCINumberToInt (errhp, &(elem->hno), sizeof(ub4),
    OCI_NUMBER_UNSIGNED, (dvoid *)&altadrs_hno);
  altadrs_street = OCIStringPtr(envhp, elem->street);
  altadrs_streetLen = OCIStringSize (envhp, elem->street);
  printf("Other addresses:\n");
  printf("   %d %.*s\n", altadrs_hno, altadrs_streetLen, 
           altadrs_street);
  for (count = 1; count < size; count++)
  {
     OCITableNext(envhp, errhp, index1, 
      (CONST OCITable*)sub_obj->_super.altadrs, &index2, &eoc);
     OCICollGetElem(envhp, errhp,(OCIColl *)sub_obj->_super.altadrs, index2,
       (boolean *)&eoc, (dvoid **)&elem, (dvoid **)0);
     OCINumberToInt (errhp, &(elem->hno), sizeof(ub4), OCI_NUMBER_UNSIGNED, 
       (dvoid *)&altadrs_hno);
     altadrs_street = OCIStringPtr(envhp, elem->street);
     altadrs_streetLen = OCIStringSize (envhp, elem->street);
     printf("   %d %.*s\n", altadrs_hno, altadrs_streetLen, 
           altadrs_street);
     index1 = index2;
  }

  OCINumberToInt (errhp, &(sub_obj->empno), sizeof(ub4),
    OCI_NUMBER_UNSIGNED, (dvoid *)&empno);
  printf("EMPNO: %d\n", empno);

  index1 = 1;
  eoc = TRUE;
  OCITableSize(envhp, errhp, (CONST OCITable*) sub_obj->workadd, &size);
  OCITableFirst(envhp, errhp,(CONST OCITable*) sub_obj->workadd, &index1);
  status = OCICollGetElem(envhp, errhp, (OCIColl *)(sub_obj->workadd),
    index1, (boolean *)&eoc, (dvoid **)&elemref, (dvoid **)0);
  if (status != OCI_SUCCESS)
  {
    printf("OCICollGetElem - Failure \n");
  }
  elemref = *( OCIRef **)elemref;
  
  if ((status = OCIObjectPin(envhp, errhp, elemref, 
    (OCIComplexObject *)0,
    OCI_PIN_ANY, OCI_DURATION_SESSION, OCI_LOCK_NONE, 
    (dvoid **) &objptr)) != OCI_SUCCESS)
  {
    printf("OCIObjectPin - Failure \n");
  }

  OCINumberToInt (errhp, &(objptr->hno), sizeof(ub4), OCI_NUMBER_UNSIGNED,
       (dvoid *)&workadd_hno);

  workadd_street = OCIStringPtr(envhp, objptr->street);
  workadd_streetLen = OCIStringSize (envhp, objptr->street);
  printf("Work Addresses:\n");
  printf("   %d %.*s\n", workadd_hno,workadd_streetLen,
              workadd_street);
  for (count = 1; count < size; count++)
  {
    OCITableNext(envhp, errhp, index1, 
       (CONST OCITable*)sub_obj->workadd, &index2, &eoc);
    status = OCICollGetElem(envhp, errhp, (OCIColl *)(sub_obj->workadd),
    index2, (boolean *)&eoc, (dvoid **)&elemref, (dvoid **)0);
    if (status != OCI_SUCCESS)
    {
      printf("OCICollGetElem - Failure \n");
    }
    elemref = *( OCIRef **)elemref;
    
    if ((status = OCIObjectPin(envhp, errhp, elemref, 
      (OCIComplexObject *)0,
      OCI_PIN_ANY, OCI_DURATION_SESSION, OCI_LOCK_NONE,
      (dvoid **) &objptr)) != OCI_SUCCESS)
    {
      printf("OCIObjectPin - Failure \n");
    }
    OCINumberToInt (errhp, &(objptr->hno), sizeof(ub4), OCI_NUMBER_UNSIGNED,
       (dvoid *)&workadd_hno);

    workadd_street = OCIStringPtr(envhp, objptr->street);
    workadd_streetLen = OCIStringSize (envhp, objptr->street);
    printf("   %d %.*s\n", workadd_hno,workadd_streetLen,
              workadd_street);
    index1 = index2;
  } 
} /* end of display ((i_manager *) */

/* Function to get an array of REFs to populate the varray of REFs*/
static void getRef()
{
  OCIDefine *defhp = (OCIDefine *) 0; /* For the REF column */
  i_residence *sub_obj[NUMREC];
  ub4 subsize[NUMREC];
  ub4 pos = 0;
  ub4 arraySize = NUMREC;

  OCIHandleAlloc(envhp, (dvoid **) &stmthp, OCI_HTYPE_STMT, (size_t) 0,
    ((dvoid **) 0));

  if((status = OCIStmtPrepare(stmthp, errhp, (text *) selRef,
    (ub4) strlen((char *) selRef), OCI_NTV_SYNTAX, OCI_DEFAULT)) != OCI_SUCCESS)
  {
    printf("OciStmtPrepare - Failure\n");
  }

  if((status = OCIObjectNew(envhp, errhp, svchp, OCI_TYPECODE_REF,
    (OCIType *) 0, (dvoid *) 0, OCI_DURATION_DEFAULT, FALSE,
    (dvoid **) workadd)) != OCI_SUCCESS)
  {
    printf("OCIObjectNew - Failure\n");
  }

  if((status = OCIDefineByPos(stmthp, &defhp, errhp, (ub4) 1, (dvoid *) 0,
    (sb4) 0, SQLT_REF, (dvoid *) 0, (ub2 *) 0, (ub2 *) 0, (ub4) OCI_DEFAULT ))
    != OCI_SUCCESS)
  {
    printf("OCIDefineByPos - Failure\n");
  }

  if((status = OCIDefineObject(defhp, errhp, (OCIType *) 0, (dvoid **) workadd,
    subsize, (dvoid **) 0, (ub4 *) 0)) != OCI_SUCCESS)
  {
    printf("OCIDefineObject - failure\n");
  }

  if((status = OCIStmtExecute(svchp, stmthp, errhp, (ub4) NUMREC, (ub4) 0,
    (OCISnapshot *) 0, (OCISnapshot *) 0, OCI_DEFAULT)) != OCI_SUCCESS)
  {
    printf("OCIStmtExecute - Failure\n");
  }
  else
  {
    if((status = OCIObjectArrayPin(envhp, errhp, workadd, (ub4) arraySize,
      (OCIComplexObject **) 0, 0, OCI_PIN_ANY, OCI_DURATION_SESSION,
      OCI_LOCK_NONE, (dvoid **) sub_obj, (ub4 *) &pos)) != OCI_SUCCESS)
    {
      printf("OCIObjectArrayPin - Failure\n");
    }
  }
  OCIHandleFree((dvoid *) stmthp, OCI_HTYPE_STMT);
} /* End of getRef() */

/* Freeing the allocated handles */
static void cleanup ()
{
  OCISessionEnd (svchp, errhp, sesnhp, OCI_DEFAULT );
  OCIHandleFree((dvoid *) sesnhp, OCI_HTYPE_SESSION);
  OCIServerDetach(svrhp, errhp, OCI_DEFAULT );
  OCIHandleFree((dvoid *) svrhp, OCI_HTYPE_SERVER);
  OCIHandleFree((dvoid *) svchp, OCI_HTYPE_SVCCTX);
  OCIHandleFree((dvoid *) errhp, OCI_HTYPE_ERROR);
  return;
}/* End of cleanup() */


