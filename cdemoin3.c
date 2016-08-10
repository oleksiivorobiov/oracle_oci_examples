/* Copyright (c) 1996, 2001, Oracle Corporation.  All rights reserved.  */

/* 

  NAME 
    cdemoin3 - Demo program to describe an object, inherited types, object
    table and subtable.

  DESCRIPTION
    The program describes an object, an inherited object, methods of an 
    object, object table and a subtable and prints out the new type level
    attributes, the new method level attributes and the new table level 
    attributes.

  NOTES 
    dependent files :
      cdemoin3.h     - Header file
      cdemoin3.sql   - SQL script to be run before execution
      cdemoin3.tsc   - test script (optional)

  Program Notes :
 
  MODIFIED (MM/DD/YY) 
    rdwajan        08/07/00             - Creation 
*/

#include "cdemoin3.h"

static OCIError    *errhp = (OCIError *)0;
static OCIEnv      *envhp = (OCIEnv *)0;
static OCIServer   *svrhp = (OCIServer *)0;
static OCISession  *sesnhp = (OCISession *)0;
static OCISvcCtx   *svchp = (OCISvcCtx *)0;
static OCIStmt     *stmthp = (OCIStmt *)0;

static text *database =(text *)"";
static text *username =(text *)"scott";
static text *password =(text *)"tiger";

/* Free the allocated handles */
static void cleanup (void);

/* Check for errors */
void checkerr (OCIError *errhp, sword status);

/* describe a given object */
void describe_obj ( OCIEnv *envhp, OCISvcCtx *svchp, OCIStmt *stmthp,
OCIError *errhp, text *obj);

/* describe an object table */
void describe_tab ( OCIEnv *envhp, OCISvcCtx *svchp, OCIStmt *stmthp,
OCIError *errhp, text *table);

/* Function to describe the methods of an object */
void desc_obj_meth (OCIParam *parmhp);

/* print the type of the enumerated type */
void printtypename (ub2 type, text *name, ub2 typeSize,  ub2 typenameSize);

sword status = 0;

int main (void)
{
  printf("cdemoin3 - Demo program for describing objects \n");
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

  /* describing the object I_PERSON */
  describe_obj  (envhp, svchp, stmthp, errhp, (text *)"I_PERSON");

  /* describing the object I_STUDENT */
  describe_obj  (envhp, svchp, stmthp, errhp, (text *)"I_STUDENT");

  /* describing the object I_EMPLOYEE */
  describe_obj  (envhp, svchp, stmthp, errhp, (text *)"I_EMPLOYEE");

  /* describing the table I_PERSON_TAB */
  describe_tab ( envhp, svchp, stmthp, errhp, (text*)"I_PERSON_TAB");

  /* describing the table I_STUDENT_TAB */
  describe_tab ( envhp, svchp, stmthp, errhp, (text*)"I_STUDENT_TAB");

  /* describing the table I_EMPLOYEE_TAB */
  describe_tab ( envhp, svchp, stmthp, errhp, (text*)"I_EMPLOYEE_TAB");

  /* describing the table I_PEOPLE_TAB1 */
  describe_tab ( envhp, svchp, stmthp, errhp, (text*)"I_PEOPLE_TAB1");

  /* describing the table I_PEOPLE_TAB2 */
  describe_tab ( envhp, svchp, stmthp, errhp, (text*)"I_PEOPLE_TAB2");

  /* describing the storage table of nested table I_PEOPLE_TAB3_NT_TAB */
  describe_tab ( envhp, svchp, stmthp, errhp, (text*)"I_PEOPLE_TAB3_NT_TAB");

  /* describing the table I_PEOPLE_TAB3 */
  describe_tab ( envhp, svchp, stmthp, errhp, (text*)"I_PEOPLE_TAB3");

  /* Free the allocated handles */
  cleanup();

  printf("cdemoin3 - Done\n");
} /* End of main() */

/* describing an object */
void describe_obj ( OCIEnv *envhp, OCISvcCtx *svchp, OCIStmt *stmthp, 
OCIError *errhp, text *obj ) 
{
  ub4 objlen = strlen((char *)obj);
  OCIParam *parmhp = (OCIParam *)0; /* parameter handle */
  OCIParam *attrlshp = (OCIParam *)0; /* list of attributes */
  OCIParam *attrhp = (OCIParam *)0; /* attributes handle */
  OCIDescribe *deschp = (OCIDescribe *)0;

  ub4 in = 0; /* counter in the loop */
  ub2 numattr = 0; /* To store the number of attributes */

  ub2 type = 0; /* To get the type of the attribute */
  text  *typename; /* To get the typename of the attribute */
  ub2 typeSize = 0; /* To get column size */
  ub4 typenameSize = 0;
  ub4 ntynameSize = 0;
  text *ntyname = (text *)0; /* To get the typename of the attribute if the 
                                type is SQLT_NTY */
  ub4 sizeSuper = 0; /* To get the size of the Supertype */
  ub4 sizeSchm = 0; /* To get the size of the Supertype schema */

  ub1 isFinal = 0; /* To check if type is final */
  ub1 isInst = 0; /* To check if type is instantiable */
  ub1 isSubtype = 0; /* To check if type is a subtype */
  ub1 isInhattr = 0; /* To check if attribute is inherited */
  ub4 local_attr = 0; /* To get the number of local attributes */
  ub4 local_meth = 0; /* To get the number of local methods */
  text *superType; /* To get the name of the supertype */
  text *superSchema; /* To get the name of the schema of the supertype */
  
  printf ("Describing object %s \n", obj);

  OCIHandleAlloc((dvoid *)envhp, (dvoid **)&deschp,
    (ub4)OCI_HTYPE_DESCRIBE, (size_t)0, (dvoid **)0);

  /* get the describe handle for the object */
  if ((status = OCIDescribeAny(svchp, errhp, (dvoid *)obj, objlen, 
    OCI_OTYPE_NAME, 0, OCI_PTYPE_TYPE, deschp)) != OCI_SUCCESS )
  {
    printf ("Describe - Fail\n");
    checkerr (errhp, status); 
  }
  else 
    printf ("Describe - Success \n");

  /* get the parameter handle */
  if ((status = OCIAttrGet(deschp, OCI_HTYPE_DESCRIBE, &parmhp, 0, 
    OCI_ATTR_PARAM, errhp)) != OCI_SUCCESS)
  {
    printf ("Getting parameter Handle - Fail \n");
    checkerr (errhp, status); 
  }

  /* get the number of attributes in the object */
  if ( (status = OCIAttrGet(parmhp, OCI_DTYPE_PARAM, &numattr, 0, 
    OCI_ATTR_NUM_TYPE_ATTRS, errhp)) != OCI_SUCCESS)
  {
    printf ("Getting the number of attributes - Fail \n");
    checkerr (errhp, status); 
  }
  printf ("Number of attributes = %d \n", numattr);

  /* get whether the type is final*/
  if ( (status = OCIAttrGet(parmhp, OCI_DTYPE_PARAM, &isFinal, 0, 
    OCI_ATTR_IS_FINAL_TYPE, errhp)) != OCI_SUCCESS)
  {
    printf ("Getting whether type is final - Fail \n");
    checkerr (errhp, status); 
  }
  if (isFinal)
    printf ("Type is Final\n");
  else
    printf ("Type is not Final\n");
  
  /* get whether the type is instantiable*/
  if ( (status = OCIAttrGet(parmhp, OCI_DTYPE_PARAM, &isInst, 0, 
    OCI_ATTR_IS_INSTANTIABLE_TYPE, errhp)) != OCI_SUCCESS)
  {
    printf ("Getting whether type is instantiable - Fail \n");
    checkerr (errhp, status); 
  }
  if (isInst)
    printf ("Type is instantiable\n");
  else
    printf ("Type is not instantiable\n");

  /* get whether the type is subtype*/
  if ( (status = OCIAttrGet(parmhp, OCI_DTYPE_PARAM, &isSubtype, 0, 
    OCI_ATTR_IS_SUBTYPE, errhp)) != OCI_SUCCESS)
  {
    printf ("Getting whether type is a subtype - Fail \n");
    checkerr (errhp, status); 
  }
  if (isSubtype)
    printf ("Type is a subtype\n");
  else
    printf ("Type is not a subtype\n");

  if (isSubtype)
  {
    /* get the name of the schema of the supertype */
    if ( (status = OCIAttrGet(parmhp, OCI_DTYPE_PARAM, (dvoid *)&superSchema, 
      (dvoid *)&sizeSchm, OCI_ATTR_SUPERTYPE_SCHEMA_NAME, errhp)) 
      != OCI_SUCCESS)
    {
      printf ("Getting the name of the schema of supertype - Fail \n");
      checkerr (errhp, status); 
    }
    printf ("Name of schema of supertype: %.*s\n", sizeSchm, superSchema);

    /* get the name of the supertype */
    if ( (status = OCIAttrGet(parmhp, OCI_DTYPE_PARAM, (dvoid *)&superType, 
      (dvoid *)&sizeSuper, OCI_ATTR_SUPERTYPE_NAME, errhp)) != OCI_SUCCESS)
    {
      printf ("Getting the name of the supertype - Fail \n");
      checkerr (errhp, status);
    }
    printf ("Name of supertype: %.*s\n", sizeSuper, superType);
  }

  /* get handle for parameter list */
  if ( (status = OCIAttrGet(parmhp, OCI_DTYPE_PARAM, &attrlshp, 0, 
    OCI_ATTR_LIST_TYPE_ATTRS, errhp)) != OCI_SUCCESS)
  {
    printf ("Getting attributes list Handle - Fail \n");
    checkerr (errhp, status); 
  }

  for (in = 1; in <= numattr; ++in)
  {
    if (status = OCIParamGet (attrlshp, OCI_DTYPE_PARAM, errhp, 
    (dvoid **)&attrhp, in) != OCI_SUCCESS)
    {
      printf ("Getting parameter Handle for the attr - Failed \n");
      checkerr (errhp, status);
    }
  
    if (status = OCIAttrGet ((dvoid *)attrhp, OCI_DTYPE_PARAM, &type, 
      0, OCI_ATTR_DATA_TYPE, errhp) != OCI_SUCCESS)
    {
      printf ("Getting column type - Failed \n");
      checkerr (errhp, status);
    }
      
    if (status = OCIAttrGet ((dvoid *)attrhp, OCI_DTYPE_PARAM, 
      &typeSize, 0, OCI_ATTR_DATA_SIZE, errhp) != OCI_SUCCESS)
    {
      printf ("Getting column size - Failed \n");
      checkerr (errhp, status);
    }

#ifdef CDEMOIN3_LATER
    /* check if the attribute is inherited */
    if (status = OCIAttrGet ((dvoid *)attrhp, OCI_DTYPE_PARAM,
      &isInhattr, 0, OCI_ATTR_IS_INHERITED_ATTR, errhp) != OCI_SUCCESS)
    {
      printf ("Checking if attribute is inherited - Failed \n");
      checkerr (errhp, status);
    }
    else
    {
      if (isInhattr)
        printf ("Attribute is inherited \n");
      else
        printf ("Attribute is not inherited\n");
    }
#endif /* #ifdef CDEMOIN3_LATER */

    if (status = OCIAttrGet (attrhp, OCI_DTYPE_PARAM, &typename, 
      (ub4 *)&typenameSize, OCI_ATTR_NAME, errhp) != OCI_SUCCESS)
    {
      printf ("Getting attribute name - Failed \n");
      checkerr (errhp, status);
    }
    else  
    {
      if (type == SQLT_NTY)
      {
        if (status = OCIAttrGet ((dvoid *)attrhp, OCI_DTYPE_PARAM, &ntyname, 
        (ub4 *)&ntynameSize, OCI_ATTR_TYPE_NAME, errhp) != OCI_SUCCESS)
        {
          printf ("Getting type name - Failed \n");
          checkerr (errhp, status);
        }
        else
        {
          printf ("%.*s - Named Data Type :", typenameSize, typename);
          printf ("%.*s\n", ntynameSize, ntyname);
        }
      }
      else
        printtypename (type, typename, typeSize ,typenameSize);
    }
  } /* end of for loop */

  /* Calling function to describe the object methods */
  desc_obj_meth(parmhp); 
} /* end of describe_obj () */

/* Function to describe the methods of an object */
void desc_obj_meth(OCIParam *parmhp)
{
  OCIParam *methlshp; /* list of methods */
  OCIParam *methhp; /* methods handle */

  ub4 in = 0; /* Loop counter */
  ub2 nummeth = 111; /* To get the number of methods */

  ub1 isFinal = 0; /* To check if method is final */
  ub1 isInst = 0; /* To check if method is instantiable */
  ub1 isInhmeth = 0; /* To check if method is inherited */
 
  ub4 methSize = 0; /* To get the size of the method name */
  text *methname; /* To get the method name */

  /* get the number of methods in the object */
  if ( (status = OCIAttrGet(parmhp, OCI_DTYPE_PARAM, &nummeth, 0,
    OCI_ATTR_NUM_TYPE_METHODS, errhp)) != OCI_SUCCESS)
  {
    printf ("Getting the number of methods - Fail \n");
    checkerr (errhp, status);
  }
  printf ("Number of methods = %d \n", nummeth);

  if (nummeth > 0)
  { 
    /* get handle for parameter list */
    if ( (status = OCIAttrGet(parmhp, OCI_DTYPE_PARAM, &methlshp, 0,
      OCI_ATTR_LIST_TYPE_METHODS, errhp)) != OCI_SUCCESS)
    {
      printf ("Getting methods list Handle - Fail \n");
      checkerr (errhp, status);
    }

    for (in = 1; in <= nummeth; ++in)
    {
      if (status = OCIParamGet (methlshp, OCI_DTYPE_PARAM, errhp,
      (dvoid **)&methhp, in) != OCI_SUCCESS)
      {
        printf ("Getting parameter Handle for the method - Failed \n");
        checkerr (errhp, status);
      }
 
      if (status = OCIAttrGet ((dvoid *)methhp, OCI_DTYPE_PARAM, (dvoid *)
        &methname, (ub4 *)&methSize, OCI_ATTR_NAME, errhp) != OCI_SUCCESS)
      {
        printf ("Getting method name - Failed \n");
        checkerr (errhp, status);
      }
      else
      {
        printf ("Method name:%.*s\n", methSize, methname);
      }

      /* get whether the method is final*/
      if ((status = OCIAttrGet(methhp, OCI_DTYPE_PARAM, &isFinal, 0, 
        OCI_ATTR_IS_FINAL_METHOD, errhp)) != OCI_SUCCESS)
      {
        printf ("Getting whether method is final - Fail \n");
        checkerr (errhp, status); 
      }
      if (isFinal)
        printf ("Method is Final\n");
      else
        printf ("Method is not Final\n");
  
      /* get whether the method is instantiable*/
      if ( (status = OCIAttrGet(methhp, OCI_DTYPE_PARAM, &isInst, 0, 
        OCI_ATTR_IS_INSTANTIABLE_METHOD, errhp)) != OCI_SUCCESS)
      {
        printf ("Getting whether method is instantiable - Fail \n");
        checkerr (errhp, status); 
      }
      if (isInst)
        printf ("Method is instantiable\n");
      else
        printf ("Method is not instantiable\n");

      /* get whether the method is overriding*/
      if ( (status = OCIAttrGet(methhp, OCI_DTYPE_PARAM, &isInst, 0, 
        OCI_ATTR_IS_OVERRIDING_METHOD, errhp)) != OCI_SUCCESS)
      {
        printf ("Getting whether method is overriding - Fail \n");
        checkerr (errhp, status); 
      }
      if (isInst)
        printf ("Method is overriding\n");
      else
        printf ("Method is not overriding\n");
#ifdef CDEMOIN3_LATER
      /* check if the method is inherited */
      if (status = OCIAttrGet ((dvoid *)methhp, OCI_DTYPE_PARAM,
        &isInhmeth, 0, OCI_ATTR_IS_INHERITED_METHOD, errhp) != OCI_SUCCESS)
      {
        printf ("Checking if method is inherited - Failed \n");
        checkerr (errhp, status);
      }
      else
      {
        if (isInhmeth)
          printf ("Method is inherited \n");
        else
          printf ("Method is not inherited\n");
      }
#endif /* #ifdef CDEMOIN3_LATER */
    } /*  end of for loop*/ 
  }
} /* End of desc_obj_meth (OCIParam *) */

/* describe the given table */
void describe_tab ( OCIEnv *envhp, OCISvcCtx *svchp, OCIStmt *stmthp,
OCIError *errhp, text *table)
{
  ub4 tablen = strlen(table);
  OCIParam *parmhp; /* parameter handle */
  OCIParam *agrlshp; /* list of args */
  OCIParam *arg; /* argument handle */
  OCIDescribe *deschp = (OCIDescribe *)0;

  ub4 in = 0; /* Loop counter */
  ub2 numcol = 111; /* To get the number of columns */

  ub2 type = 111; /* To get the datatype */
  text  *typename; /* To get the attribute name */
  ub2 typeSize = 111;
  
  ub4 len = 0; /*typename length */
  ub1 isSubtable = 0; /* To check if the table is a subtable */
  text superSchema[25]; /* to get the schema name of the super type */
  text superTable[25]; /* to get the name of the super table */

  if (strcmp(table,(text *)"I_PEOPLE_TAB3_NT_TAB") ==0 )
    printf ("Describing the storage table of a nested table %s\n", table);
  else
    printf ("Describing the table %s \n", table);
    

  OCIHandleAlloc((dvoid *)envhp, (dvoid **)&deschp,
  (ub4)OCI_HTYPE_DESCRIBE, (size_t)0, (dvoid **)0);

  /* get the describe handle for the table */
  if ((status = OCIDescribeAny(svchp, errhp, (dvoid *)table, tablen,
    OCI_OTYPE_NAME, 0, OCI_PTYPE_TABLE, deschp)) != OCI_SUCCESS )
    checkerr (errhp, status);
  else
     printf ("Describe - Success \n");

  /* get the parameter handle */
   if ((status = OCIAttrGet(deschp, OCI_HTYPE_DESCRIBE, &parmhp, 0,
     OCI_ATTR_PARAM, errhp)) != OCI_SUCCESS)
     checkerr (errhp, status);

   /* get the number of columns in the table */
   if ( (status = OCIAttrGet(parmhp, OCI_DTYPE_PARAM, &numcol, 0,
   OCI_ATTR_NUM_COLS, errhp)) != OCI_SUCCESS)
     checkerr (errhp, status);

   printf ("Number of Columns:%d \n", numcol);

#ifdef CDEMOIN3_LATER
  /* get whether the table is subtable*/
  if ( (status = OCIAttrGet(parmhp, OCI_DTYPE_PARAM, &isSubtable, 0,
    OCI_ATTR_IS_SUBOBJECT, errhp)) != OCI_SUCCESS)
  {
    printf ("Getting whether table is a subtable - Fail \n");
    checkerr (errhp, status);
  }
  if (isSubtable)
    printf ("Table is a subtable\n");
  else
    printf ("Table is not a subtable\n");

  /* get the name of the schema of the supertable */
  if ( (status = OCIAttrGet(parmhp, OCI_DTYPE_PARAM, (dvoid *)superSchema, 0,
    OCI_ATTR_SUPEROBJECT_SCHEMA_NAME, errhp)) != OCI_SUCCESS)
  {
    printf ("Getting the name of the schema of supertable - Fail \n");
    checkerr (errhp, status);
  }
  printf ("Name of schema of supertable: %s\n", superSchema);

  /* get the name of the supertable */
  if ( (status = OCIAttrGet(parmhp, OCI_DTYPE_PARAM, (dvoid *)superTable, 0,
    OCI_ATTR_SUPEROBJECT_NAME, errhp)) != OCI_SUCCESS)
  {
    printf ("Getting the name of the supertable - Fail \n");
    checkerr (errhp, status);
  }
  printf ("Name of supertable: %s\n", superTable);
#endif /* #ifdef CDEMOIN3_LATER */

  /* get handle for parameter list */
  if ( (status = OCIAttrGet(parmhp, OCI_DTYPE_PARAM, &agrlshp, 0,
  OCI_ATTR_LIST_COLUMNS, errhp)) != OCI_SUCCESS)
    checkerr (errhp, status);
  for (in = 1; in <= numcol; ++in)
  {
    if (status = OCIParamGet (agrlshp, OCI_DTYPE_PARAM, errhp,
    (dvoid **)&arg, in) != OCI_SUCCESS)
    {
      printf ("Getting parameter Handle for the arguments ");
      printf ("- Failed \n");
      checkerr (errhp, status);
    }

    if (status = OCIAttrGet ((dvoid *)arg, OCI_DTYPE_PARAM, &type,
    0, OCI_ATTR_DATA_TYPE, errhp) != OCI_SUCCESS)
    {
      printf ("Getting column type - Failed \n");
      checkerr (errhp, status);
    }

    if (status = OCIAttrGet ((dvoid *)arg, OCI_DTYPE_PARAM,
    &typeSize, 0, OCI_ATTR_DATA_SIZE, errhp) != OCI_SUCCESS)
    {
      printf ("Getting column type - Failed \n");
      checkerr (errhp, status);
    }

    if (status = OCIAttrGet (arg, OCI_DTYPE_PARAM, &typename, (ub4 *)&len,
    OCI_ATTR_NAME, errhp) != OCI_SUCCESS)
    {
      printf ("Getting type name - Failed \n");
      checkerr (errhp, status);
    }
    else
      printtypename (type, typename, typeSize, len);
  } /* end for loop */
} /* end of describe_tab () */

/* printing typenames for given typecodes */
void printtypename (ub2 type, text *name, ub2 typeSize, ub2 typenameSize)
{
  printf ("%.*s:", typenameSize, name);
  switch (type)
  {
    case SQLT_CHR: printf ("VARCHAR2");
              break; 
    case SQLT_AFC: printf ("CHAR");
              break;
    case SQLT_DAT: printf ("DATE");
              break; 
    case SQLT_INT: printf ("SIGNED INTEGER");
              break; 
    case SQLT_UIN: printf ("UNSIGNED INTEGER");
              break; 
    case SQLT_FLT: printf ("REAL");
              break; 
    case SQLT_PDN: printf ("PACKED DECIMAL");
              break; 
    case SQLT_BIN: printf ("BINARY DATA");
              break; 
    case SQLT_NUM: printf ("NUMBER");
              break; 
    case SQLT_BLOB : printf ("BLOB");
              break; 
    case SQLT_CLOB : printf ("CLOB");
              break; 
    case SQLT_FILE : printf ("BFILE");
              break; 
    case SQLT_NTY : printf ("NAMED DATA TYPE");
              break; 
    case SQLT_REF : printf ("REF to a Named Data Type");
              break;
    default : printf ("*** UNKNOWN *** ");
              break; 
  }
  printf ("(%3d) \n", typeSize);
} /* end of printtypebyname () */


/* cleaning up the allocated handles */
void cleanup ()
{
  if (stmthp)
    checkerr(errhp, OCIHandleFree((dvoid *) stmthp, OCI_HTYPE_STMT));
  if (svrhp)
    (void) OCIServerDetach( svrhp, errhp, OCI_DEFAULT );
  if (svrhp)
    checkerr(errhp, OCIHandleFree((dvoid *) svrhp, OCI_HTYPE_SERVER));
  if (sesnhp)
  {
    OCISessionEnd (svchp, errhp, sesnhp, OCI_DEFAULT );
    OCIHandleFree((dvoid *) sesnhp, OCI_HTYPE_SESSION);
  }
  if (svchp)
    (void) OCIHandleFree((dvoid *) svchp, OCI_HTYPE_SVCCTX);
  if (errhp)
    (void) OCIHandleFree((dvoid *) errhp, OCI_HTYPE_ERROR);
  return;
} /* end of cleanup () */

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
