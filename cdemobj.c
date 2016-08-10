#ifdef RCSID
static char *RCSid =
   "$Header: cdemobj.c 18-feb-2005.15:01:08 aliu Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1995, 2005, Oracle. All rights reserved.  
*/

/*
   NAME
     cdemobj.c
   DESCRIPTION
     Demo of selection of a REF and display the pinned object through
     navigational interface.

   NOTES
     see routines.

   MODIFIED   (MM/DD/YY)
     aliu     02/17/05 - lint issues 
     aliu     02/16/05 - fix bug 4184313 
     aliu     07/17/03 - Free dschp after printing in dump_adt 
     nireland 04/11/00 - Fix porting exception. #1264324
     mpjoshi  02/15/00 - bug 531229
     mjaeger  07/14/99 - bug 808870: OCCS: convert tabs, no long lines
     svedala  09/09/98 - lines longer than 79 chars reformatted - bug 722491
     svedala  02/18/98 - OCI obsoletion changes
     svedala  04/13/98 -
     echen    06/04/97 - remove TypeTypeCode call
     echen    06/02/97 - fix a bug
     echen    06/02/97 - remove obsolete ORT code
     skmishra 05/14/97 - stdcc compatibility changes
     azhao    03/28/97 - add AS OBJECT to create type
     echen    03/24/97 - fix a sequent bug
     azhao    03/05/97 - fix compile errors
     echen    03/05/97 - remove unnecessary code
     cchau    03/03/97 - change functions to long names
     cxcheng  02/10/97 - remove short ORO names
     echen    01/09/97 - modify the sql syntax
     echen    01/03/97 - fix test bugs
     echen    11/15/96 - change obsolete type code
     skrishna 11/07/96 - continue OCICollGetElem interface change
     jboonl10/30/96 -  new ori interface
     echen 11/06/96 -  fix OCICollGetElem
     echen 07/19/96 -  remove obsolete pin option
     echen 07/19/96 -  remove obsolete pin option
     dchatt07/18/96 -  delete Oracle spec code like static
     echen 07/16/96 -  Creation
*/

#ifndef CDEMOBJ_ORACLE
#include "cdemobj.h"
#endif

/* statement to select a ref from an extent table customer_tab */
static const text *const selref = (text *)
          "SELECT REF(c) from customer_tab c";

/* statement to create the type address */
static const text *const create_type_address = (text *)
"CREATE TYPE address AS OBJECT (\
  no       NUMBER,\
  street   VARCHAR(60),\
  state    CHAR(2),\
  zip      CHAR(10)\
)";

/* statement to create the typed table address_tab */
static const text *const create_type_addr_tab = (text *)
"create type addr_tab is table of address";

/* statement to create the type person */
static const text *const create_type_person = (text *)
"CREATE TYPE person AS OBJECT (\
  firstname         CHAR(20),\
  lastname          varchar(20),\
  age               int,\
  salary            float,\
  bonus             double precision,\
  retirement_fund   int,\
  number_of_kids    smallint,\
  years_of_school   numeric(10, 2),\
  preaddr           addr_tab,\
  birthday          date,\
  number_of_pets    real,\
  comment1          raw(200),\
  comment2          clob,\
  comment3          varchar2(200),\
  addr              ADDRESS\
)";

/* statement to create the type customer */
static const text *const create_type_customer = (text *)
"CREATE TYPE customer AS OBJECT (\
  account       char(20),\
  aperson       REF person\
)";

/* statement to create the typed table person */
static const text *const create_table_person = (text *)
"create table person_tab of person nested table preaddr \
 store as person_preaddr_table";

/* statement to create the typed table customer_tab */
static const text *const create_table_customer = (text *)
"create table customer_tab of customer";

/* statement to insert data into table customer_tab */
static const text *const insert_customer = (text *)
"insert into customer_tab values('00001', null)";

/* statement to insert data into table person_tab */
static const text *const insert_person = (text *)
"insert into person_tab values('Sandy', 'Wood', 25, 32000, 10000, 20000, 3,\
                          15, addr_tab(),\
                          to_date('1961 08 23', 'YYYY MM DD'), 2,\
                          '1234567890', 'This is a test', 'This is a test',\
                          ADDRESS(8888, 'Fenley Road', 'CA', '91406'))";

/* statement to insert data into the nested table in person_tab */
static const text *const insert_address1 = (text *)
"insert into the (select preaddr from person_tab where\
                firstname='Sandy') values\
                (715, 'South Henry', 'ca', '95117')";
static const text *const insert_address2 = (text *)
"insert into the (select preaddr from person_tab where\
                firstname='Sandy') values\
                (6830, 'Woodley Ave', 'ca', '90416')";

/* statement to update the ref in the table customer_tab */
static const text *const update_customer = (text *)
"update customer_tab set aperson = (select ref(p)\
                              from person_tab p where\
                              p.firstname = 'Sandy')";

/***************************************************************************
*  Check the error and display the error message                           *
****************************************************************************/
static void checkerr(errhp, status)
OCIError *errhp;
sword status;
{
  text errbuf[512];
  sb4 errcode;

  switch (status)
  {
  case OCI_SUCCESS:
    break;
  case OCI_SUCCESS_WITH_INFO:
    break;
  case OCI_NEED_DATA:
    break;
  case OCI_NO_DATA:
    break;
  case OCI_ERROR:  /* get the error back and display on the screen */
    (void) OCIErrorGet ((dvoid *) errhp, (ub4) 1, (text *) NULL, &errcode,
                    errbuf, (ub4) sizeof(errbuf), (ub4) OCI_HTYPE_ERROR);
    (void) printf("Error - %s\n", errbuf);
    break;
  case OCI_INVALID_HANDLE:
    break;
  case OCI_STILL_EXECUTING:
    break;
  case OCI_CONTINUE:
    break;
  default:
    break;
  }
}
/****************************************************************************
*  Display attribute value of an ADT                                        *
****************************************************************************/
static void display_attr_val(envhp, errhp, names, typecode, attr_value)
OCIEnv *envhp;            /* environment handle        */
OCIError *errhp;            /* error handle              */
text    *names;            /* the name of the attribute */
OCITypeCode   typecode;          /* the type code             */
dvoid   *attr_value;       /* the value pointer         */
{
  text           str_buf[200];
  double         dnum;
  ub4            text_len, str_len;
  OCIRaw         *raw = (OCIRaw *) 0;
  OCIString        *vs = (OCIString *) 0;
  ub1            *temp = (ub1 *)0;
  ub4            rawsize = 0;
  ub4            i = 0;

  /* display the data based on the type code */
  switch (typecode)
  {
     case OCI_TYPECODE_DATE :                         /* fixed length string */
         str_len = 200;
         (void) OCIDateToText(errhp, (CONST OCIDate *) attr_value,
            (CONST text*) "Month dd, SYYYY, HH:MI A.M.",
            (ub1) 27, (CONST text*) "American", (ub4) 8,
             (ub4 *)&str_len, str_buf);
         str_buf[str_len+1] = '\0';
         (void) printf("attr %s = %s\n", names, (text *) str_buf);
         break;
     case OCI_TYPECODE_RAW :                                          /* RAW */
         raw = *(OCIRaw **) attr_value;
         temp = OCIRawPtr(envhp, raw);
         rawsize = OCIRawSize (envhp, raw);
         (void) printf("attr %s = ", names);
         for (i=0; i < rawsize; i++)
         {
           (void) printf("0x%x ", temp[i]);
         }
         (void) printf("\n");
         break;
     case OCI_TYPECODE_CHAR :                         /* fixed length string */
     case OCI_TYPECODE_VARCHAR :                                 /* varchar  */
     case OCI_TYPECODE_VARCHAR2 :                                /* varchar2 */
         vs = *(OCIString **) attr_value;
         (void) printf("attr %s = %s\n",
                       names, (text *) OCIStringPtr(envhp, vs));
         break;
     case OCI_TYPECODE_SIGNED8 :                              /* BYTE - sb1  */
         (void) printf("attr %s = %d\n", names, *(sb1 *) attr_value);
         break;
     case OCI_TYPECODE_UNSIGNED8 :                   /* UNSIGNED BYTE - ub1  */
         (void) printf("attr %s = %d\n", names, *(ub1 *) attr_value);
         break;
     case OCI_TYPECODE_OCTET :                                       /* OCT  */
         (void) printf("attr %s = %d\n", names, *(ub1 *) attr_value);
         break;
     case OCI_TYPECODE_UNSIGNED16 :                       /* UNSIGNED SHORT  */
     case OCI_TYPECODE_UNSIGNED32 :                        /* UNSIGNED LONG  */
     case OCI_TYPECODE_REAL :                                     /* REAL    */
     case OCI_TYPECODE_DOUBLE :                                   /* DOUBLE  */
     case OCI_TYPECODE_INTEGER :                                     /* INT  */
     case OCI_TYPECODE_SIGNED16 :                                  /* SHORT  */
     case OCI_TYPECODE_SIGNED32 :                                   /* LONG  */
     case OCI_TYPECODE_DECIMAL :                                 /* DECIMAL  */
     case OCI_TYPECODE_FLOAT :                                   /* FLOAT    */
     case OCI_TYPECODE_NUMBER :                                  /* NUMBER   */
     case OCI_TYPECODE_SMALLINT :                                /* SMALLINT */
         (void) OCINumberToReal(errhp, (CONST OCINumber *) attr_value,
                                (uword) sizeof(dnum), (dvoid *) &dnum);
         (void) printf("attr %s = %f\n", names, dnum);
         break;
     default:
         (void) printf("attr %s - typecode %d\n", names, typecode);
         break;
    }
}


/****************************************************************************
*  Dump the info of any ADT                                                 *
****************************************************************************/
static void dump_adt(envhp, errhp, svchp, tdo, obj, null_obj)
OCIEnv *envhp;             /* environment handle           */
OCIError *errhp;           /* error handle                 */
OCISvcCtx *svchp;          /* service handle               */
OCIType  *tdo;             /* type descriptor             */
dvoid   *obj;              /* object pointer               */
dvoid   *null_obj;         /* parallel null struct pointer */
{
  text           *names[50];
  text           *lengths[50];
  text           *indexes[50];
  ub2             count, pos;
  OCITypeElem         *ado;
  ub4             text_len, str_len;
  ub4             i;
  OCITypeCode           typecode;
  OCIInd          attr_null_status;
  dvoid          *attr_null_struct;
  dvoid          *attr_value;
  OCIType         *attr_tdo, *element_type;
  dvoid          *object;
  dvoid          *null_object;
  OCIType         *object_tdo;
  ub1             status;
  OCIRef         *type_ref;
  text           str_buf[200];
  double         dnum;
  dvoid          *element = (dvoid *) 0, *null_element = (dvoid *) 0;
  boolean        exist, eoc, boc;
  sb4            index;
  OCIDescribe    *dschp = (OCIDescribe *) 0, *dschp1 = (OCIDescribe *) 0;
  text           *namep, *typenamep;
  dvoid          *list_attr;
  OCIIter        *itr = (OCIIter *) 0;
  dvoid          *parmp = (dvoid *) 0,
                 *parmdp = (dvoid *) 0,
                 *parmp1 = (dvoid *) 0,
                 *parmp2 = (dvoid *) 0;
  OCIRef         *elem_ref = (OCIRef *) 0;

  checkerr(errhp, OCIHandleAlloc((dvoid *) envhp, (dvoid **) &dschp,
                        (ub4) OCI_HTYPE_DESCRIBE,
                        (size_t) 0, (dvoid **) 0));

  checkerr(errhp, OCIDescribeAny(svchp, errhp, (dvoid *) tdo,
                  (ub4) 0, OCI_OTYPE_PTR, (ub1)1,
                  (ub1) OCI_PTYPE_TYPE, dschp));

  checkerr(errhp, OCIAttrGet((dvoid *) dschp, (ub4) OCI_HTYPE_DESCRIBE,
                  (dvoid *)&parmp, (ub4 *)0, (ub4)OCI_ATTR_PARAM, errhp));

  checkerr(errhp, OCIAttrGet((dvoid*) parmp,(ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &typenamep, (ub4 *) &str_len,
                    (ub4) OCI_ATTR_NAME, (OCIError *) errhp));
  typenamep[str_len] = '\0';

  printf("starting displaying instance of type '%s'\n", typenamep);

  /* loop through all attributes in the type */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &count, (ub4 *) 0,
                    (ub4) OCI_ATTR_NUM_TYPE_ATTRS, (OCIError *) errhp));

  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&list_attr, (ub4 *)0,
                    (ub4)OCI_ATTR_LIST_TYPE_ATTRS, (OCIError *)errhp));

  /* loop through all attributes in the type */
  for (pos = 1; pos <= count; pos++)
  {

    checkerr(errhp, OCIParamGet((dvoid *) list_attr,
                       (ub4) OCI_DTYPE_PARAM, errhp,
                       (dvoid *)&parmdp, (ub4) pos));

    checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &namep, (ub4 *) &str_len,
                    (ub4) OCI_ATTR_NAME, (OCIError *) errhp));
    namep[str_len] = '\0';

    /* get the attribute */
    if (OCIObjectGetAttr(envhp, errhp, obj, null_obj, tdo,
                  (CONST oratext **)&namep, &str_len, 1,
                  (ub4 *)0, 0, &attr_null_status, &attr_null_struct,
                  &attr_value, &attr_tdo) != OCI_SUCCESS)
      (void) printf("BUG -- OCIObjectGetAttr, expect OCI_SUCCESS.\n");

    /* get the type code of the attribute */
    checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid*) &typecode, (ub4 *) 0,
                      (ub4) OCI_ATTR_TYPECODE,
                      (OCIError *) errhp));

    /* support only fixed length string, ref and embedded ADT */
    switch (typecode)
    {
       case OCI_TYPECODE_OBJECT :                            /* embedded ADT */
           printf("attribute %s is an embedded ADT. Display instance ....\n",
                    namep);
           /* recursive call to dump nested ADT data */
           dump_adt(envhp, errhp, svchp, attr_tdo, attr_value,
                            attr_null_struct);
           break;
       case OCI_TYPECODE_REF :                               /* embedded ADT */
           printf("attribute %s is a ref. Pin and display instance ...\n",
                     namep);
           /* pin the object */
           if (OCIObjectPin(envhp, errhp, *(OCIRef **)attr_value,
                     (OCIComplexObject *)0, OCI_PIN_ANY,
                     OCI_DURATION_SESSION, OCI_LOCK_NONE,
                     (dvoid **)&object) != OCI_SUCCESS)
             (void) printf("BUG -- OCIObjectPin, expect OCI_SUCCESS.\n");
           /* allocate the ref */
           if (( status = OCIObjectNew(envhp, errhp, svchp,
                    OCI_TYPECODE_REF, (OCIType *)0,
                    (dvoid *)0, OCI_DURATION_DEFAULT, TRUE,
                    (dvoid **) &type_ref)) != OCI_SUCCESS)
             (void) printf("BUG -- OCIObjectNew, expect OCI_SUCCESS.\n");
           /* get the ref of the type from the object */
           if (( status = OCIObjectGetTypeRef(envhp, errhp, object, type_ref))
                    != OCI_SUCCESS)
             (void) printf("BUG -- ORIOGTR, expect OCI_SUCCESS.\n");
           /* pin the type ref to get the type object */
           if (OCIObjectPin(envhp, errhp, type_ref,  (OCIComplexObject *)0,
                     OCI_PIN_ANY, OCI_DURATION_SESSION, OCI_LOCK_NONE,
                     (dvoid **) &object_tdo) != OCI_SUCCESS)
             (void) printf("BUG -- OCIObjectPin, expect OCI_SUCCESS.\n");
           /* get null struct of the object */
           if (( status = OCIObjectGetInd(envhp, errhp, object,
                               &null_object)) != OCI_SUCCESS)
             (void) printf("BUG -- ORIOGNS, expect OCI_SUCCESS.\n");
           /* call the function recursively to dump the pinned object */
           dump_adt(envhp, errhp, svchp, object_tdo, object,
                    null_object);
       case OCI_TYPECODE_NAMEDCOLLECTION :
           checkerr(errhp, OCIHandleAlloc((dvoid *) envhp, (dvoid **) &dschp1,
                        (ub4) OCI_HTYPE_DESCRIBE,
                        (size_t) 0, (dvoid **) 0));

           checkerr(errhp, OCIDescribeAny(svchp, errhp, (dvoid *) attr_tdo,
                  (ub4) 0, OCI_OTYPE_PTR, (ub1)1,
                  (ub1) OCI_PTYPE_TYPE, dschp1));

           checkerr(errhp, OCIAttrGet((dvoid *) dschp1,
                    (ub4) OCI_HTYPE_DESCRIBE,
                    (dvoid *)&parmp1, (ub4 *)0, (ub4)OCI_ATTR_PARAM, errhp));

           /* get the collection type code of the attribute */
           checkerr(errhp, OCIAttrGet((dvoid*) parmp1, (ub4) OCI_DTYPE_PARAM,
                      (dvoid*) &typecode, (ub4 *) 0,
                      (ub4) OCI_ATTR_COLLECTION_TYPECODE,
                      (OCIError *) errhp));
           switch (typecode)
           {
             case OCI_TYPECODE_VARRAY :                    /* variable array */
               (void) printf
               ("\n---> Dump the table from the top to the bottom.\n");
               checkerr(errhp, OCIAttrGet((dvoid*) parmp1,
                       (ub4) OCI_DTYPE_PARAM,
                       (dvoid*) &parmp2, (ub4 *) 0,
                       (ub4) OCI_ATTR_COLLECTION_ELEMENT,
                       (OCIError *) errhp));
               checkerr(errhp, OCIAttrGet((dvoid*) parmp2,
                       (ub4) OCI_DTYPE_PARAM,
                       (dvoid*) &elem_ref, (ub4 *) 0,
                       (ub4) OCI_ATTR_REF_TDO,
                       (OCIError *) errhp));
               checkerr(errhp, OCITypeByRef(envhp, errhp, elem_ref,
                       OCI_PIN_DEFAULT, (OCITypeGetOpt)0, &element_type));
               /* initialize the iterator */
               checkerr(errhp, OCIIterCreate(envhp, errhp,
                       (CONST OCIColl*) attr_value, &itr));
               /* loop through the iterator */
               for(eoc = FALSE;!OCIIterNext(envhp, errhp, itr,
                               (dvoid **) &element,
                               (dvoid **)&null_element, &eoc) && !eoc;)
               {
                 /* if type is named type, call the same function recursively
                 */
                 if (typecode == OCI_TYPECODE_OBJECT)
                   dump_adt(envhp, errhp, svchp, element_type, element,
                          null_element);
                 else  /* else, display the scaler type attribute */
                   display_attr_val(envhp, errhp, namep, typecode, element);
               }
               break;

             case OCI_TYPECODE_TABLE :                       /* nested table */
               (void) printf
               ("\n---> Dump the table from the top to the bottom.\n");
               /* go to the first element and print out the index */
               checkerr(errhp, OCIAttrGet((dvoid*) parmp1,
                       (ub4) OCI_DTYPE_PARAM,
                       (dvoid*) &parmp2, (ub4 *) 0,
                       (ub4) OCI_ATTR_COLLECTION_ELEMENT,
                       (OCIError *) errhp));
               checkerr(errhp, OCIAttrGet((dvoid*) parmp2,
                       (ub4) OCI_DTYPE_PARAM,
                       (dvoid*) &elem_ref, (ub4 *) 0,
                       (ub4) OCI_ATTR_REF_TDO,
                       (OCIError *) errhp));
               checkerr(errhp, OCITypeByRef(envhp, errhp, elem_ref,
                       OCI_DURATION_SESSION,
                       OCI_TYPEGET_HEADER, &element_type));
               attr_value = *(dvoid **)attr_value;
               /* move to the first element in the nested table */
               checkerr(errhp, OCITableFirst(envhp, errhp,
                       (CONST OCITable*) attr_value, &index));
               (void) printf
               ("     The index of the first element is : %d.\n", index);
               /* print out the element */
               checkerr(errhp, OCICollGetElem(envhp, errhp,
                                       (CONST OCIColl *) attr_value, index,
                                       &exist, (dvoid **) &element,
                                       (dvoid **) &null_element));
               /* if it is named type, recursively call the same function */
               checkerr(errhp, OCIAttrGet((dvoid*) parmp2,
                       (ub4) OCI_DTYPE_PARAM,
                       (dvoid*) &typecode, (ub4 *) 0,
                       (ub4) OCI_ATTR_TYPECODE,
                       (OCIError *) errhp));
               if (typecode == OCI_TYPECODE_OBJECT)
                 dump_adt(envhp, errhp, svchp, element_type,
                        (dvoid *)element, (dvoid *)null_element);
               else
                 display_attr_val(envhp, errhp, namep, typecode, element);

               for(;!OCITableNext(envhp, errhp, index,
                             (CONST OCITable *) attr_value,
                             &index, &exist) && exist;)
               {
                  checkerr(errhp, OCICollGetElem(envhp, errhp,
                                         (CONST OCIColl *) attr_value, index,
                                         &exist, (dvoid **) &element,
                                         (dvoid **) &null_element));
                  if (typecode == OCI_TYPECODE_OBJECT)
                     dump_adt(envhp, errhp, svchp, element_type,
                           (dvoid *)element, (dvoid *)null_element);
                  else
                     display_attr_val(envhp, errhp, namep, typecode, element);
               }
               break;
             default:
               break;
           }
           checkerr(errhp, OCIHandleFree((dvoid *) dschp1,
                                        (ub4) OCI_HTYPE_DESCRIBE));
           break;
       default:   /* scaler type, display the attribute value */
           if (attr_null_status == OCI_IND_NOTNULL)
           {
              display_attr_val(envhp, errhp, namep, typecode, attr_value);
           }
           else
              printf("attr %s is null\n", namep);
           break;
    }
  }

  printf("finishing displaying instance of type '%s'\n", typenamep);
  checkerr(errhp, OCIHandleFree((dvoid *) dschp, (ub4) OCI_HTYPE_DESCRIBE));
}


/****************************************************************************
*  Setup the schema and insert the data                                     *
*****************************************************************************/
void setup(envhp, svchp, stmthp, errhp)
OCIEnv *envhp;
OCISvcCtx *svchp;
OCIStmt *stmthp;
OCIError *errhp;
{
  /* create the schema and populate the data */
  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) create_type_address,
                          (ub4) strlen((char *) create_type_address),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) create_type_addr_tab,
                          (ub4) strlen((char *) create_type_addr_tab),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) create_type_person,
                          (ub4) strlen((char *) create_type_person),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) create_type_customer,
                          (ub4) strlen((char *) create_type_customer),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) create_table_person,
                          (ub4) strlen((char *) create_table_person),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) create_table_customer,
                          (ub4) strlen((char *) create_table_customer),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) insert_customer,
                          (ub4) strlen((char *) insert_customer),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) insert_person,
                          (ub4) strlen((char *) insert_person),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) insert_address1,
                          (ub4) strlen((char *) insert_address1),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) insert_address2,
                          (ub4) strlen((char *) insert_address2),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) update_customer,
                          (ub4) strlen((char *) update_customer),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          (ub4) OCI_DEFAULT));

  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));
}


/*****************************************************************************/
void select_pin_display(envhp, svchp, stmthp, errhp)
OCIEnv *envhp;
OCISvcCtx *svchp;
OCIStmt *stmthp;
OCIError *errhp;
{
  sword status = OCI_SUCCESS;
  OCIDefine *defnp;
  OCIRef *custref = (OCIRef *) 0, *per_type_ref = (OCIRef *) 0;
  OCIRef *cust_type_ref = (OCIRef *) 0;
  ub4    custsize;
  customer *cust = (customer *) 0, *custnew = (customer *) 0;
  null_customer *null_cust = (null_customer *) 0,
                *null_custnew = (null_customer *) 0;
  person *per = (person *) 0;
  null_person *null_per = (null_person *) 0;
  null_address *nt_null = (null_address *) 0;
  OCIType *pertdo = (OCIType *) 0, *custtdo = (OCIType *) 0;
  address *addr = (address *) 0;
  sb4 index;
  boolean exist;
  dvoid *tabobj = (dvoid *) 0;

  (void) printf("\n=============================================\n");

  /* allocate ref */
  if (( status = OCIObjectNew(envhp, errhp, svchp, OCI_TYPECODE_REF,
                         (OCIType *)0, (dvoid *)0, OCI_DURATION_DEFAULT,
                         TRUE, (dvoid **) &per_type_ref))
                  != OCI_SUCCESS)
     (void) printf("BUG -- OCIObjectNew, expect OCI_SUCCESS.\n");

  /* allocate ref */
  if (( status = OCIObjectNew(envhp, errhp, svchp, OCI_TYPECODE_REF,
                         (OCIType *)0, (dvoid *)0, OCI_DURATION_DEFAULT,
                         TRUE, (dvoid **) &cust_type_ref))
                  != OCI_SUCCESS)
     (void) printf("BUG -- OCIObjectNew, expect OCI_SUCCESS.\n");

  /* define the application request  */
  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) selref,
                          (ub4) strlen((char *) selref),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

/*
  checkerr(errhp, OCIHandleAlloc( (dvoid *) stmthp, (dvoid **) &defnp,
                           (ub4) OCI_HTYPE_DEFINE,
                           0, (dvoid **) 0));
*/

  checkerr(errhp, OCIDefineByPos(stmthp, &defnp, errhp, (ub4) 1, (dvoid *) 0,
                   (sb4) 0, SQLT_REF, (dvoid *) 0, (ub2 *)0,
                   (ub2 *)0, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIDefineObject(defnp, errhp, (OCIType *) 0,
                            (dvoid **) &custref,
                            &custsize, (dvoid **) 0, (ub4 *) 0));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 0,
                         (ub4) 0, (OCISnapshot *)
                         NULL, (OCISnapshot *) NULL,
                         (ub4) OCI_DEFAULT));

  while ((status = OCIStmtFetch(stmthp, errhp, (ub4) 1,  (ub4) OCI_FETCH_NEXT,
                         (ub4) OCI_DEFAULT)) == 0)
  {

    (void) printf("\n-----------------------------------------\n");

    /* pin the ref and get the typed table to get to person */
    checkerr(errhp, OCIObjectPin(envhp, errhp, custref,
                          (OCIComplexObject *)0,
                          OCI_PIN_ANY, OCI_DURATION_SESSION,
                          OCI_LOCK_NONE, (dvoid **) &cust));
    (void) printf("The customer account number is %s\n",
                   OCIStringPtr(envhp, cust->account));
    if (( status = OCIObjectGetInd(envhp, errhp, (dvoid *) cust,
                               (dvoid **) &null_cust)) != OCI_SUCCESS)
    {
      (void) printf("BUG -- ORIOGNS, expect OCI_SUCCESS.\n");
    }
    else
    {
      (void) printf("null_cus = %d, null_account = %d, null_aperson = %d\n",
                      null_cust->null_cus, null_cust->null_account,
                      null_cust->null_aperson);
    }

    checkerr(errhp, OCIObjectPin(envhp, errhp, cust->aperson,
                          (OCIComplexObject *)0,
                          OCI_PIN_ANY, OCI_DURATION_SESSION,
                          OCI_LOCK_NONE, (dvoid **) &per));

    if (( status = OCIObjectGetInd(envhp, errhp, (dvoid *) per,
                               (dvoid **) &null_per)) != OCI_SUCCESS)
    {
      (void) printf("BUG -- ORIOGNS, expect OCI_SUCCESS.\n");
    }
    else
    {
      checkerr(errhp, OCIObjectGetTypeRef(envhp, errhp, (dvoid *)per,
                          per_type_ref));
      checkerr(errhp, OCIObjectPin(envhp, errhp, per_type_ref,
                          (OCIComplexObject *)0, OCI_PIN_ANY,
                          OCI_DURATION_SESSION, OCI_LOCK_NONE,
                          (dvoid **) &pertdo));
      dump_adt(envhp, errhp, svchp, pertdo, (dvoid *) per,
                          (dvoid *) null_per);
    }
  }

  if ( status != OCI_NO_DATA )
    checkerr(errhp, status);

  (void) printf("\n\n");
}


/****************************************************************************
*  Clean up the schema and the data                                         *
*****************************************************************************/
void cleanup(envhp, svchp, stmthp, errhp)
OCIEnv *envhp;
OCISvcCtx *svchp;
OCIStmt *stmthp;
OCIError *errhp;
{
  /* clean up the schema */
  checkerr(errhp, OCIStmtPrepare(stmthp, errhp,
                        (text *) "drop table customer_tab",
                        (ub4) strlen((char *)"drop table customer_tab" ),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                          (ub4) 0, (OCISnapshot *)
                          NULL, (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp,
                        (text *) "drop table person_tab",
                        (ub4) strlen((char *)"drop table person_tab" ),
                        (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                          (ub4) 0, (OCISnapshot *)
                          NULL, (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) "drop type customer",
                          (ub4) strlen((char *)"drop table customer" ),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                          (ub4) 0, (OCISnapshot *)
                          NULL, (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) "drop type person",
                          (ub4) strlen((char *)"drop table person" ),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                          (ub4) 0, (OCISnapshot *)
                          NULL, (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) "drop type addr_tab",
                          (ub4) strlen((char *)"drop table addr_tab" ),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                          (ub4) 0, (OCISnapshot *)
                          NULL, (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, (text *) "drop type address",
                          (ub4) strlen((char *) "drop type address"),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1,
                          (ub4) 0, (OCISnapshot *)
                          NULL, (OCISnapshot *) NULL, (ub4) OCI_DEFAULT));

  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));
}


/*****************************************************************************/
int main()
{
  OCIEnv *envhp;
  OCIServer *srvhp;
  OCIError *errhp;
  OCISvcCtx *svchp;
  OCISession *usrhp;
  OCIStmt *stmthp;
  dvoid *tmp;

  /* initialize the process */
  (void) OCIInitialize((ub4) OCI_THREADED | OCI_OBJECT,
                (dvoid *)0,  (dvoid * (*)()) 0,
                (dvoid * (*)()) 0,  (void (*)()) 0 );

  /* initialize the environmental handle */
  (void) OCIEnvInit( &envhp, (ub4) OCI_DEFAULT, 21, (dvoid **) &tmp  );

  /* get the error handle */
  (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &errhp,
                   (ub4) OCI_HTYPE_ERROR,
                   52, (dvoid **) &tmp);

  /* two server contexts */
  (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &srvhp,
                   (ub4) OCI_HTYPE_SERVER,
                   52, (dvoid **) &tmp);
  /* attach the server */
  (void) OCIServerAttach( srvhp, errhp, (text *) "", (sb4) 0,
                                                     (ub4) OCI_DEFAULT);

  /* get the service handle */
  (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &svchp,
                   (ub4) OCI_HTYPE_SVCCTX,
                   52, (dvoid **) &tmp);

  /* set attribute server context in the service context */
  (void) OCIAttrSet( (dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
                    (dvoid *) srvhp, (ub4) 0,
                    (ub4) OCI_ATTR_SERVER, (OCIError *) errhp);

  /* get the user handle */
  (void) OCIHandleAlloc((dvoid *)envhp, (dvoid **)&usrhp,
                   (ub4)OCI_HTYPE_SESSION, 0, (dvoid **)0);

  /* set the attribute user name */
  (void) OCIAttrSet((dvoid *) usrhp, (ub4)OCI_HTYPE_SESSION,
                   (dvoid *)"scott", (ub4)strlen((char *)"scott"),
                   (ub4)OCI_ATTR_USERNAME, errhp);

  /* set the attribute password */
  (void) OCIAttrSet((dvoid *) usrhp, (ub4)OCI_HTYPE_SESSION,
                   (dvoid *)"tiger", (ub4)strlen((char *)"tiger"),
                   (ub4)OCI_ATTR_PASSWORD, errhp);

  /* authenticate */
  checkerr(errhp, OCISessionBegin (svchp, errhp, usrhp,
                                   OCI_CRED_RDBMS, OCI_DEFAULT));

  /* set the attribute user context of the service handle */
  (void) OCIAttrSet((dvoid *)svchp, (ub4)OCI_HTYPE_SVCCTX,
                   (dvoid *)usrhp, (ub4)0,
                   (ub4)OCI_ATTR_SESSION, errhp);

  /* get the statement handle */
  checkerr(errhp, OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &stmthp,
           (ub4) OCI_HTYPE_STMT, 50, (dvoid **) &tmp));

  (void) printf("\n*********************************************\n");
  (void) printf("--- Setup the schema and insert the data.\n");
  setup(envhp, svchp, stmthp, errhp);

  (void) printf("\n*********************************************\n");
  (void) printf("--- Select a REF, pin the REF, then display the object.\n");
  select_pin_display(envhp, svchp, stmthp, errhp);

  (void) printf("\n*********************************************\n");
  (void) printf("--- Clean up the schema and the data.\n");
  cleanup(envhp, svchp, stmthp, errhp);

  checkerr(errhp, OCISessionEnd (svchp, errhp, usrhp, OCI_DEFAULT));

  /* dettach */
  (void) OCIServerDetach( srvhp, errhp, (ub4) OCI_DEFAULT );
  checkerr(errhp, OCIHandleFree((dvoid *) stmthp, (ub4) OCI_HTYPE_STMT));
  checkerr(errhp, OCIHandleFree((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX));
  checkerr(errhp, OCIHandleFree((dvoid *) errhp, (ub4) OCI_HTYPE_ERROR));
  checkerr(errhp, OCIHandleFree((dvoid *) srvhp, (ub4) OCI_HTYPE_SERVER));

  return (0);
}

