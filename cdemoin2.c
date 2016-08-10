/* Copyright (c) Oracle Corporation 1996, 1997, 1998. All Rights Reserved. */ 

/* 

  NAME 
    cdemoin2 - Demo program to perform attribute substitutability.

  DESCRIPTION
    This program demonstrates attribute substitutability, wherein a column
    which is of REF to a supertype is substituted with a REF to a subtype. 
    All the data from the table are then displayed.

 
  NOTES 
    dependent files :
      cdemoin2.h     - Header file
      cdemoin2.sql   - SQL script to be run before execution
      cdemoin2.tsc   - test script (optional)

  Program Notes :
 
  MODIFIED (MM/DD/YY) 
    rdwajan	 08/25/00 	          - Creation 
*/

#include "cdemoin2.h"

static OCIError    *errhp = (OCIError *)0;
static OCIEnv      *envhp = (OCIEnv *)0;
static OCIServer   *svrhp = (OCIServer *)0;
static OCISession  *sesnhp = (OCISession *)0;
static OCISvcCtx   *svchp = (OCISvcCtx *)0;
static OCIStmt     *stmthp = (OCIStmt *)0;

static text *database =(text *)"";
static text *username =(text *)"scott";
static text *password =(text *)"tiger";

static text *updateSql1 = (text *)
  "UPDATE cdemoin2_person_tab SET vacation_home = :v1 WHERE ssn = 999";

static text *updateSql2 = (text *)
  "UPDATE cdemoin2_person_tab SET first_home = :v1 WHERE ssn = 999";

static text *getRefSql1 = (text *)
  "SELECT REF(a) FROM cdemoin2_address_tab a WHERE a.hno = 300";

static text *getRefSql2 = (text *)
  "SELECT REF(a) FROM cdemoin2_sec_address_tab a WHERE a.hno = 100";

static text *displaySql = (text *)
"SELECT vacation_home, first_home FROM cdemoin2_person_tab";
/* where deref(vacation_home) is of (cdemoin2_sec_address, cdemoin2_address)";*/

/* Free the allocated handles */
static void cleanup (void);

/* Check for errors */
void checkerr (OCIError *errhp, sword status);

/* Function to display the records before updation*/
static void displayFunction1();

/* Function to display the records after updation*/
static void displayFunction2();

/* Function to modify the REF column to show substitutability */
static void updateFunction(text *getRefSql, text *updateSql);

sword status = 0;

int main (void)
{
  printf("cdemoin2 - Demo program to perform attribute substitutability\n");
  /* Initializing the environment in the Object mode*/
  OCIEnvCreate (&envhp, OCI_OBJECT, (dvoid *)0,  (dvoid * (*)()) 0,
    (dvoid * (*)()) 0, (dvoid (*)()) 0, 0, (dvoid *)0);

  OCIHandleAlloc (envhp, (dvoid **)&errhp, OCI_HTYPE_ERROR, (size_t)0, 
    (dvoid **)0);

  OCIHandleAlloc(envhp, (dvoid **)&svrhp, OCI_HTYPE_SERVER, (size_t)0,
    (dvoid **)0);

  status = OCIServerAttach(svrhp, errhp, (text *)database,
    (sb4)strlen((char *)database), OCI_DEFAULT);

  if (status != OCI_SUCCESS)
  {
    printf("OCIServerAttach failed \n");
    checkerr(errhp, status);
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
    checkerr(errhp, status);
  }
  else
    printf("Connection - Success \n");

  OCIAttrSet(svchp, OCI_HTYPE_SVCCTX, sesnhp, (ub4)0, OCI_ATTR_SESSION, errhp);

  /* Function to display records before updation*/
  printf ("Displaying records before updation\n");
  displayFunction1();

  /* Substituting a REF of a subtype with a REF of a supertype */ 
  updateFunction(getRefSql1, updateSql1);

  /* Substituting a REF of a supertype with a REF of a subtype */
  updateFunction(getRefSql2, updateSql2);

  /* Function to display records after updation*/
  printf ("Displaying records after updation\n");
  displayFunction2();

  /* Free the allocated handles */
  cleanup();

  printf("cdemoin2 - Done\n");
} /* End of main() */

/* Function to display the contents of the table before updation*/
static void displayFunction1 ()
{ 
  OCIDefine *def1hp = (OCIDefine *)0; /* For vacation_home */
  OCIDefine *def2hp = (OCIDefine *)0; /* For first_home */
  OCIRef *s_ref = (OCIRef *)0;
  OCIRef *ref = (OCIRef *)0;
  cdemoin2_sec_address *s_obj = (cdemoin2_sec_address *)0;
  cdemoin2_address *obj = (cdemoin2_address *)0;
  ub4 subSize = 0;
  sb4 size = 0;

  ub4 hno = 0; /* Variable to fetch hno */
  ub4 sizeUB4 = sizeof (ub4);
  text *street; /* Variable to fetch street */
  ub4 streetLength = 0;
  text *city; /* Variable to fetch city */
  ub4 cityLength = 0;
  text *state; /* Variable to fetch state */
  ub4 stateLength = 0;

  OCIHandleAlloc (envhp, (dvoid **)&stmthp, OCI_HTYPE_STMT, (size_t)0,
    (dvoid **)0);

  if ((status = OCIStmtPrepare (stmthp, errhp, (text *)displaySql, 
    (ub4)strlen(displaySql), OCI_NTV_SYNTAX, OCI_DEFAULT)) != OCI_SUCCESS)
  {
    printf ("OCIStmtPrepare - Fail\n");
    checkerr (errhp, status);
  }
  
  if (status = OCIDefineByPos(stmthp, &def1hp, errhp, (ub4)1, (dvoid *)0,
    (ub4)0, SQLT_REF, (dvoid *)0, (ub2 *)0, (ub2 *)0,
    OCI_DEFAULT) != OCI_SUCCESS)
  {
    printf("OCIDefineByPos - Failure \n");
    checkerr(errhp, status);
  }

  if (status = OCIDefineByPos(stmthp, &def2hp, errhp, (ub4)2, (dvoid *)0,
    (ub4)0, SQLT_REF, (dvoid *)0, (ub2 *)0, (ub2 *)0,
    OCI_DEFAULT) != OCI_SUCCESS)
  {
    printf("OCIDefineByPos - Failure \n");
    checkerr(errhp, status);
  }

  if (status = OCIDefineObject(def1hp, errhp, (OCIType *)NULL, 
    (dvoid **)&s_ref, (ub4 *)0, (dvoid **)0, (ub4)0) != OCI_SUCCESS)
  {
    printf("OCIDefineObject - Failure \n");
    checkerr(errhp, status);
  }
  
  if (status = OCIDefineObject(def2hp, errhp, (OCIType *)NULL, 
    (dvoid **)&ref, (ub4 *)0, (dvoid **)0, (ub4)0) != OCI_SUCCESS)
  {
    printf("OCIDefineObject - Failure \n");
    checkerr(errhp, status);
  }
  
  printf ("Executing the statement:%s\n", displaySql); 
  if (status = OCIStmtExecute (svchp, stmthp, errhp, (ub4)1, (ub4)0,
    (OCISnapshot *)0, (OCISnapshot *)0, OCI_DEFAULT ) != OCI_SUCCESS)
  {
    printf("OCIStmtExecute - Failure \n");
    checkerr (errhp, status);
  }
  else
  {
    printf("OCIStmtExecute - Success\n");
    if (status = OCIObjectPin(envhp, errhp, ref,
      (OCIComplexObject *)0, OCI_PIN_ANY, OCI_DURATION_SESSION,
      OCI_LOCK_NONE, (dvoid **)&obj) != OCI_SUCCESS)
    {
       printf("OCIObjectPin - Failure \n");
       checkerr(errhp, status);
    }
    if (status = OCIObjectPin(envhp, errhp, s_ref,
      (OCIComplexObject *)0, OCI_PIN_ANY, OCI_DURATION_SESSION,
      OCI_LOCK_NONE, (dvoid **)&s_obj) != OCI_SUCCESS)
    {
       printf("OCIObjectPin - Failure \n");
       checkerr(errhp, status);
    }
    printf ("Col2 data\n");
    OCINumberToInt (errhp, &(s_obj->_super.hno), sizeUB4,
      OCI_NUMBER_UNSIGNED, (dvoid *)&hno);
    printf("HNO: %d\n", hno);
    street = OCIStringPtr(envhp, s_obj->_super.street);
    streetLength =  OCIStringSize (envhp, s_obj->_super.street);
    printf("STREET: %.*s\n", streetLength, street);
    city = OCIStringPtr(envhp, s_obj->city);
    cityLength =  OCIStringSize (envhp, s_obj->city);
    printf("CITY: %.*s\n", cityLength, city);
    state = OCIStringPtr(envhp, s_obj->state);
    stateLength =  OCIStringSize (envhp, s_obj->state);
    printf("STATE: %.*s\n", stateLength, state);
    printf ("Col3 data\n");
    OCINumberToInt (errhp, &(obj->hno), sizeUB4,
      OCI_NUMBER_UNSIGNED, (dvoid *)&hno);
    printf("HNO: %d\n", hno);
    street = OCIStringPtr(envhp, obj->street);
    streetLength =  OCIStringSize (envhp, obj->street);
    printf("STREET: %.*s\n", streetLength, street);
  }
  OCIHandleFree((dvoid *) stmthp, OCI_HTYPE_STMT);
} /* end of displayFunction1() */

/* Function to display the contents of the table after updation*/
static void displayFunction2 ()
{ 
  OCIDefine *def1hp = (OCIDefine *)0; /* For vacation_home */
  OCIDefine *def2hp = (OCIDefine *)0; /* For first_home */
  OCIRef *s_ref = (OCIRef *)0;
  OCIRef *ref = (OCIRef *)0;
  cdemoin2_sec_address *s_obj = (cdemoin2_sec_address *)0;
  cdemoin2_address *obj = (cdemoin2_address *)0;
  ub4 subSize = 0;
  sb4 size = 0;

  ub4 hno = 0; /* Variable to fetch hno */
  ub4 sizeUB4 = sizeof (ub4);
  text *street; /* Variable to fetch street */
  ub4 streetLength = 0;
  text *city; /* Variable to fetch city */
  ub4 cityLength = 0;
  text *state; /* Variable to fetch state */
  ub4 stateLength = 0;

  OCIHandleAlloc (envhp, (dvoid **)&stmthp, OCI_HTYPE_STMT, (size_t)0,
    (dvoid **)0);

  if ((status = OCIStmtPrepare (stmthp, errhp, (text *)displaySql, 
    (ub4)strlen(displaySql), OCI_NTV_SYNTAX, OCI_DEFAULT)) != OCI_SUCCESS)
  {
    printf ("OCIStmtPrepare - Fail\n");
    checkerr (errhp, status);
  }
  
  if (status = OCIDefineByPos(stmthp, &def1hp, errhp, (ub4)1, (dvoid *)0,
    (ub4)0, SQLT_REF, (dvoid *)0, (ub2 *)0, (ub2 *)0,
    OCI_DEFAULT) != OCI_SUCCESS)
  {
    printf("OCIDefineByPos - Failure \n");
    checkerr(errhp, status);
  }

  if (status = OCIDefineByPos(stmthp, &def2hp, errhp, (ub4)2, (dvoid *)0,
    (ub4)0, SQLT_REF, (dvoid *)0, (ub2 *)0, (ub2 *)0,
    OCI_DEFAULT) != OCI_SUCCESS)
  {
    printf("OCIDefineByPos - Failure \n");
    checkerr(errhp, status);
  }

  if (status = OCIDefineObject(def1hp, errhp, (OCIType *)NULL, 
    (dvoid **)&ref, (ub4 *)0, (dvoid **)0, (ub4)0) != OCI_SUCCESS)
  {
    printf("OCIDefineObject - Failure \n");
    checkerr(errhp, status);
  }
  
  if (status = OCIDefineObject(def2hp, errhp, (OCIType *)NULL, 
    (dvoid **)&s_ref, (ub4 *)0, (dvoid **)0, (ub4)0) != OCI_SUCCESS)
  {
    printf("OCIDefineObject - Failure \n");
    checkerr(errhp, status);
  }
  
  printf ("Executing the statement:%s\n", displaySql); 
  if (status = OCIStmtExecute (svchp, stmthp, errhp, (ub4)1, (ub4)0,
    (OCISnapshot *)0, (OCISnapshot *)0, OCI_DEFAULT ) != OCI_SUCCESS)
  {
    printf("OCIStmtExecute - Failure \n");
    checkerr (errhp, status);
  }
  else
  {
    printf("OCIStmtExecute - Success\n");
    if (status = OCIObjectPin(envhp, errhp, ref,
      (OCIComplexObject *)0, OCI_PIN_ANY, OCI_DURATION_SESSION,
      OCI_LOCK_NONE, (dvoid **)&obj) != OCI_SUCCESS)
    {
       printf("OCIObjectPin - Failure \n");
       checkerr(errhp, status);
    }
    if (status = OCIObjectPin(envhp, errhp, s_ref,
      (OCIComplexObject *)0, OCI_PIN_ANY, OCI_DURATION_SESSION,
      OCI_LOCK_NONE, (dvoid **)&s_obj) != OCI_SUCCESS)
    {
       printf("OCIObjectPin - Failure \n");
       checkerr(errhp, status);
    }
    printf ("Col2 data\n");
    OCINumberToInt (errhp, &(obj->hno), sizeUB4,
      OCI_NUMBER_UNSIGNED, (dvoid *)&hno);
    printf("HNO: %d\n", hno);
    street = OCIStringPtr(envhp, obj->street);
    streetLength =  OCIStringSize (envhp, obj->street);
    printf("STREET: %.*s\n", streetLength, street);
    printf ("Col3 data\n");
    OCINumberToInt (errhp, &(s_obj->_super.hno), sizeUB4,
      OCI_NUMBER_UNSIGNED, (dvoid *)&hno);
    printf("HNO: %d\n", hno);
    street = OCIStringPtr(envhp, s_obj->_super.street);
    streetLength =  OCIStringSize (envhp, s_obj->_super.street);
    printf("STREET: %.*s\n", streetLength, street);
    city = OCIStringPtr(envhp, s_obj->city);
    cityLength =  OCIStringSize (envhp, s_obj->city);
    printf("CITY: %.*s\n", cityLength, city);
    state = OCIStringPtr(envhp, s_obj->state);
    stateLength =  OCIStringSize (envhp, s_obj->state);
    printf("STATE: %.*s\n", stateLength, state);
  }
  OCIHandleFree((dvoid *) stmthp, OCI_HTYPE_STMT);
} /* end of displayFunction2() */

static void updateFunction(text *getRefSql, text *updateSql)
{
  OCIRef *ref = (OCIRef *)0;
  OCIDefine *defhp = (OCIDefine *)0;
  ub4 subSize = 0;
  OCIBind *bindhp = (OCIBind *)0; 
  sb4 sizeRef = sizeof (OCIRef *);
  OCIHandleAlloc (envhp, (dvoid **)&stmthp, OCI_HTYPE_STMT, (size_t)0,
    (dvoid **)0);

  if ((status = OCIStmtPrepare (stmthp, errhp, (text *)getRefSql,
    (ub4)strlen(getRefSql), OCI_NTV_SYNTAX, OCI_DEFAULT)) != OCI_SUCCESS)
  {
    printf ("OCIStmtPrepare - Fail\n");
    checkerr (errhp, status);
  }
  if (( status = OCIObjectNew(envhp, errhp, svchp, OCI_TYPECODE_REF,
    (OCIType *)0, (dvoid *)0, OCI_DURATION_DEFAULT, FALSE,
    (dvoid **) &ref)) != OCI_SUCCESS)
  {
    printf ("OCIObjectNew - Failure\n");
    checkerr (errhp, status);
  }

  if ((status = OCIDefineByPos(stmthp, &defhp, errhp, (ub4) 1, (dvoid *) 0,
    (sb4) 0, SQLT_REF, (dvoid *) 0, (ub2 *)0, (ub2 *)0,
    (ub4) OCI_DEFAULT)) != OCI_SUCCESS)
  {
    printf ("OCIDefineByPos - Failure \n");
    checkerr (errhp, status);
  }

  if ((status = OCIDefineObject(defhp, errhp, (OCIType *) 0,
    (dvoid **) &ref, &subSize, (dvoid **) 0, (ub4 *) 0)) != OCI_SUCCESS)
  {
    printf ("OCIDefineObject - Failure \n");
    checkerr (errhp, status);
  }
  if (status = OCIStmtExecute (svchp, stmthp, errhp, (ub4)1, (ub4)0,
    (OCISnapshot *)0, (OCISnapshot *)0, OCI_DEFAULT ) != OCI_SUCCESS)
  {
    printf("OCIStmtExecute - Failure \n");
    checkerr (errhp, status);
  }
  else
  {
    if ((status = OCIStmtPrepare (stmthp, errhp, (text *)updateSql,
    (ub4)strlen(updateSql), OCI_NTV_SYNTAX, OCI_DEFAULT)) != OCI_SUCCESS)
    {
      printf ("OCIStmtPrepare - Fail\n");
      checkerr (errhp, status);
    }

    if ((status = OCIBindByPos(stmthp, &bindhp, errhp, (ub4) 1, (dvoid *) 0,
      sizeRef, SQLT_REF, (dvoid *) 0, (ub2 *)0, (ub2 *)0, (ub4)0,
      (ub4 *)0, (ub4) OCI_DEFAULT)) != OCI_SUCCESS)
    {
      printf ("OCIBindByPos - Failure \n");
      checkerr (errhp, status);
    }

    if (status = OCIBindObject(bindhp, errhp, (OCIType *)0, (dvoid **) &ref,
      (ub4 *) &subSize, (dvoid **)0 , (ub4 *) 0) != OCI_SUCCESS)
    {
      printf("OCIBindObject - Failure \n");
      checkerr(errhp, status);
    }
    printf ("Executing the statement:%s\n", updateSql);
    if (status = OCIStmtExecute (svchp, stmthp, errhp, (ub4)1, (ub4)0,
      (OCISnapshot *)0, (OCISnapshot *)0, OCI_DEFAULT ) != OCI_SUCCESS)
    {
      printf("OCIStmtExecute - Failure \n");
      checkerr (errhp, status);
    }
    else
    {
      printf("OCIStmtExecute - Success\n");
    }
  }
  OCIHandleFree((dvoid *) stmthp, OCI_HTYPE_STMT);
} /* end of updateFunction(text *, text *) */ 

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

/* check for the errors */
void checkerr (OCIError *err, sword status)
{
	text errbuf[514];
	sb4 errcode;

	switch (status)
	{
		case OCI_SUCCESS :
			break;
		case OCI_SUCCESS_WITH_INFO :
			OCIErrorGet (err, 1, (text *)0, 
			(sb4 *)&errcode, errbuf, 514,
			OCI_HTYPE_ERROR);
			printf ("Warning %d : %.*s\n", errcode, 514, errbuf);
			break;
		case OCI_NO_DATA :
			printf ("*** No Data Found *** \n");
			break;
		case OCI_INVALID_HANDLE :
			printf ("*** Invalid Handle *** \n");
			exit (-5);
		case OCI_ERROR :
			OCIErrorGet (err, 1, (text *)0, 
			(sb4 *)&errcode, errbuf, 514,
			OCI_HTYPE_ERROR);
			printf ("ERROR %d : %.*s\n", errcode, 514, errbuf);
			break;
		default :
			printf ("*** Unknown Status *** \n");
			break;
	}
}

