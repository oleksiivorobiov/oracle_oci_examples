/* Copyright (c) 2001, 2002, Oracle Corporation.  All rights reserved.  */

/*

   NAME
     cdemoAnyData1.c - OCI demo program for ANYDATA.

   DESCRIPTION
     An example program which inserts and selects rows of data to
     and from anydatatab.
     Rows are inserted into anydatatab by converting to ANYDATA
     from the following types: NUMBER, VARCHAR2, DATE, 
     ADDRESS_OBJECT, CHAR, RAW, and COLLECTION in the above oder.
     Then rows are fetched and COL2 accessed from ANYDATA 
     in the same order.     
     SQL> describe anydatatab;
     Name                      Null?    Type
     ------------------------- -------- ---------------
     COL1                               NUMBER
     COL2                               SYS.ANYDATA
     SQL> describe address_object;
     Name                      Null?    Type
     ------------------------- -------- ---------------
     STATE                              VARCHAR2(3)
     ZIP                                VARCHAR2(13)

   EXPORT FUNCTION(S)
     <external functions defined for use outside package - one-line descriptions>

   INTERNAL FUNCTION(S)
     <other external functions defined - one-line descriptions>

   STATIC FUNCTION(S)
     <static functions defined - one-line descriptions>

   NOTES
     Dependent Files:
       cdemoAnyData1.sql  - SQL script to be run before execution.

   MODIFIED   (MM/DD/YY)
   jchai       06/13/02 - fix bug 2360431
   ani         09/17/01 - OCIAnyDataAccess changes for Number,Date
   ani         04/30/01 - Merged ani_ocidemo
   ani         04/24/01 - Creation

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef OCI_ORACLE
#include <oci.h>
#endif

#define BUFLEN 20 
#define ADDR_SETNOTNULL(nind) (memset(nind, (char)0, 3*sizeof(OCIInd)));

typedef struct cdemoanctx
{
  OCIEnv *envhp;
  OCIServer *srvhp;
  OCISvcCtx *svchp;
  OCIError *errhp;
  OCISession *authp;
  OCIStmt *stmthp;
} cdemoanctx;
     
struct address
{
  OCIString *state; /* text   state[3]; */
  OCIString *zip; /* text   zip[11]; */
};
typedef struct address address;
  
struct null_address
{
  sb2    null_address;
  sb2    null_state;
  sb2    null_zip;
};     
typedef struct null_address null_address;

/*---------------------------------------------------------------------------
                     PRIVATE TYPES AND CONSTANTS
  ---------------------------------------------------------------------------*/
static text *username = (text *) "scott";
static text *password = (text *) "tiger";
   
/* Define SQL statements to be used in program. */
static text *inst1 = (text *)"INSERT INTO anydatatab VALUES (:first_col, :oan_buffer)";
static text *sel1 = (text *)"SELECT * FROM anydatatab WHERE col1 = ";        

/*---------------------------------------------------------------------------
                     STATIC FUNCTION DECLARATIONS
  ---------------------------------------------------------------------------*/
static void initialize(cdemoanctx *ctxptr);
static void create_table(cdemoanctx *ctxptx);
static void drop_table(cdemoanctx *ctxptx);
static void sql_exec_insert(/*_ cdemoanctx *ctxptr, ub2 typecode, ub4 index _*/);
static void sql_exec_select(/*_ cdemoanctx *ctxptr, OCITypeCode typecode, ub4 recnum _*/);
static void cleanup(cdemoanctx *ctxptr);
static void checkerr(OCIError *errhp, sword status);
int main();

     
int main()
{
  cdemoanctx ctx;

  printf("\n ######## start executing test ############ \n");

  initialize(&ctx);

  /* execute sql statement */
  sql_exec_insert(&ctx, OCI_TYPECODE_NUMBER, 1);
  sql_exec_insert(&ctx, OCI_TYPECODE_VARCHAR2, 2);
  sql_exec_insert(&ctx, OCI_TYPECODE_DATE, 3);
  sql_exec_insert(&ctx, OCI_TYPECODE_OBJECT, 4);
  sql_exec_insert(&ctx, OCI_TYPECODE_CHAR, 5);
  sql_exec_insert(&ctx, OCI_TYPECODE_RAW, 6);
  sql_exec_insert(&ctx, OCI_TYPECODE_NAMEDCOLLECTION, 7);

  sql_exec_select(&ctx, OCI_TYPECODE_NUMBER, 1);
  sql_exec_select(&ctx, OCI_TYPECODE_VARCHAR2, 2);
  sql_exec_select(&ctx, OCI_TYPECODE_DATE, 3);
  sql_exec_select(&ctx, OCI_TYPECODE_OBJECT, 4);
  sql_exec_select(&ctx, OCI_TYPECODE_CHAR, 5);
  sql_exec_select(&ctx, OCI_TYPECODE_RAW, 6);
  sql_exec_select(&ctx, OCI_TYPECODE_NAMEDCOLLECTION, 7);

  /* clean things up before exhit */
  cleanup(&ctx);
  return 1; 

} /*end main*/
 

/*********************************************************************
 * execute insert statement                                          *
 * This function converts data of given type specified by parameter  *
 * "typecode" to OCIANYDATA, then insert OCIANYDATA into datatabase  *
 *  table named anydatatab.                                          *
 *********************************************************************/
void sql_exec_insert(ctxptr, typecode, index)
cdemoanctx *ctxptr;
ub2 typecode;
ub4 index;
{
  OCIBind *bndp1 = (OCIBind *) NULL;
  OCIBind *bndp2 = (OCIBind *) NULL;

  OCIType *any_tdo = (OCIType *)0;
  OCIInd indp = OCI_IND_NOTNULL;
  OCIInd *any_indp = (OCIInd *)0;
  OCIAnyData *oan_buffer = (OCIAnyData *)0;
  OCIType *addr_tdo = (OCIType *)0;
  dvoid   *addr = (dvoid *)0;
  address *addr_obj = (address *)0;

  ub4 first_col = index;
  ub4 any_number, i;
  OCINumber num;
  OCIString *str = (OCIString *) 0;
  OCIDate date;

  OCIDuration dur;
  OCIParam *type_info = (OCIParam *)0;
  OCIRaw *raw_col = (OCIRaw *)0;
  ub1* raw_ptr;
  ub1 lbuf1[BUFLEN];
  dvoid *nind = (dvoid *)0;
  /*text zip[6] = "00000";*/
  char zip[6] = "00000";

  OCITable *addr_tab = (OCITable *) 0;
  OCIType *addr_tab_tdo = (OCIType *)0;
  sb4 collsiz;
  /*address *addr = (address *)0, *addr2 = (address *)0;*/
  boolean exist;
  /*sb4 index;*/
  dvoid *elem = (dvoid *)0; 
  dvoid *elemind = (dvoid *)0;
  OCIInd *addr_tab_null=(OCIInd *)0;
  address *addr_coll[3]; 


  printf("Testing INSERT ... \n");   

  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, 
                          ctxptr->errhp, inst1, (ub4) strlen((char *)inst1),
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  
  checkerr(ctxptr->errhp,  OCITypeByName(ctxptr->envhp,
                          ctxptr->errhp, ctxptr->svchp,
                          (CONST text *)"SYS", (ub4) strlen("SYS"),
                          (CONST text *) "ANYDATA", (ub4) strlen("ANYDATA"),
                          (CONST text *) 0, (ub4) 0, OCI_DURATION_SESSION,
                          OCI_TYPEGET_ALL, &any_tdo));
  checkerr(ctxptr->errhp,  OCITypeByName(ctxptr->envhp,
                          ctxptr->errhp, ctxptr->svchp,
                          (CONST text *)"", (ub4) strlen(""),
                          (CONST text *) "ADDRESS_OBJECT", 
                          (ub4) strlen("ADDRESS_OBJECT"),
                          (CONST text *) 0, (ub4) 0, OCI_DURATION_SESSION,
                          OCI_TYPEGET_ALL, &addr_tdo));

  /* prepare data for insertion */
  switch (typecode)
  {
    case OCI_TYPECODE_NUMBER:
    any_number = 10;
    checkerr(ctxptr->errhp, OCINumberFromInt(ctxptr->errhp, 
                                    &any_number, sizeof(any_number), 
                                    OCI_NUMBER_UNSIGNED, &num));
 
    checkerr(ctxptr->errhp, OCIAnyDataConvert(ctxptr->svchp, 
                    ctxptr->errhp, (OCITypeCode)OCI_TYPECODE_NUMBER, 
                    (OCIType *)0, OCI_DURATION_SESSION, (dvoid *)&indp, 
                    (dvoid *)&num, 0, &oan_buffer));  
    break;
  
    case OCI_TYPECODE_VARCHAR2: 
    OCIStringAssignText(ctxptr->envhp, ctxptr->errhp, 
                        (text *)"TestString", 10, &str);
    checkerr(ctxptr->errhp, OCIAnyDataConvert(ctxptr->svchp, 
                     ctxptr->errhp,(OCITypeCode)OCI_TYPECODE_VARCHAR2, 
                     (OCIType *)0, OCI_DURATION_SESSION, (dvoid *)&indp, 
                     (dvoid *) str, 0, &oan_buffer));
    break;
  
    case OCI_TYPECODE_DATE:
    OCIDateSetDate((OCIDate *) &date, (sb2)2000, (ub1) 7, (ub1)31);
    checkerr(ctxptr->errhp, OCIAnyDataConvert(ctxptr->svchp,
                     ctxptr->errhp, (OCITypeCode)OCI_TYPECODE_DATE, 
                     (OCIType *)0, OCI_DURATION_SESSION, (dvoid *)&indp, 
                     (dvoid *)&date, 0, &oan_buffer));
    break;
  
    case OCI_TYPECODE_CHAR: 
    OCIStringAssignText(ctxptr->envhp, ctxptr->errhp, 
                        (text *)"CHARString", 10, &str);
    checkerr(ctxptr->errhp, OCIAnyDataConvert(ctxptr->svchp, 
                     ctxptr->errhp,(OCITypeCode)OCI_TYPECODE_CHAR, 
                     (OCIType *)0, OCI_DURATION_SESSION, (dvoid *)&indp, 
                     (dvoid *) str, 0, &oan_buffer));
    break;

    case OCI_TYPECODE_OBJECT:
    if ( OCIObjectNew(ctxptr->envhp, ctxptr->errhp,
                      ctxptr->svchp, OCI_TYPECODE_OBJECT,
                         addr_tdo, (dvoid *) 0,
                         OCI_DURATION_DEFAULT, TRUE,
                         (dvoid **) &addr_obj) != OCI_SUCCESS)
     {
      printf("object new error \n");  
      exit(0);  /*no point of proceeding */ 
      }
    
    OCIStringAssignText(ctxptr->envhp, ctxptr->errhp, 
                        (text *)"NV", 2, &addr_obj->state);
    OCIStringAssignText(ctxptr->envhp, ctxptr->errhp, 
                        (text *)"11111", 5, &addr_obj->zip);

    OCIObjectGetInd(ctxptr->envhp, ctxptr->errhp, (dvoid *)addr_obj, &nind);
    ADDR_SETNOTNULL(nind);

    checkerr(ctxptr->errhp, OCIAnyDataConvert(ctxptr->svchp,
                              ctxptr->errhp, OCI_TYPECODE_OBJECT,
                              addr_tdo, OCI_DURATION_SESSION, (dvoid *)nind,
                              addr_obj, 0, &oan_buffer));
    break;

    case OCI_TYPECODE_RAW:
    for (i = 0; i < BUFLEN; i++)
      lbuf1[i] = (ub1) 'R';  
    checkerr(ctxptr->errhp, OCIRawAssignBytes(ctxptr->envhp,
                     ctxptr->errhp, lbuf1, BUFLEN, &raw_col));
    checkerr(ctxptr->errhp, OCIAnyDataConvert(ctxptr->svchp,
                     ctxptr->errhp, (OCITypeCode)OCI_TYPECODE_RAW, 
                     (OCIType *)0, OCI_DURATION_SESSION, (dvoid *)&indp, 
                     (dvoid *) raw_col, 0, &oan_buffer));
    break;

    case OCI_TYPECODE_NAMEDCOLLECTION:
    checkerr(ctxptr->errhp, OCITypeByName(ctxptr->envhp,
                          ctxptr->errhp, ctxptr->svchp,
                          (CONST text *)"", (ub4) strlen(""),
                          (const text *) "ADDR_TAB",
                          (ub4) strlen((const char *) "ADDR_TAB"),
                          (CONST text *) 0, (ub4) 0, OCI_DURATION_SESSION,
                          OCI_TYPEGET_ALL, &addr_tab_tdo));

    checkerr(ctxptr->errhp, OCIObjectNew(ctxptr->envhp, ctxptr->errhp,
                         ctxptr->svchp, OCI_TYPECODE_NAMEDCOLLECTION,
                         addr_tab_tdo, (dvoid *) 0,
                         OCI_DURATION_DEFAULT, TRUE,
                         (dvoid **) &addr_tab));

    for(i=0;i<3;i++) {
      addr_coll[i] = (address *) 0;
      checkerr(ctxptr->errhp, OCIObjectNew(ctxptr->envhp, ctxptr->errhp,
                   ctxptr->svchp, OCI_TYPECODE_OBJECT, addr_tdo, (dvoid *) 0, 
                   OCI_DURATION_DEFAULT, TRUE, (dvoid **) &addr_coll[i]));
      OCIStringAssignText(ctxptr->envhp, ctxptr->errhp, 
                        (text *)"CA", 2, &addr_coll[i]->state);
      (void) sprintf((char *) zip, "%s%d", "9406", i);
      OCIStringAssignText(ctxptr->envhp, ctxptr->errhp, 
                        (text *) zip, 5, &addr_coll[i]->zip);
      checkerr(ctxptr->errhp, OCICollAppend(ctxptr->envhp, ctxptr->errhp,   
                              addr_coll[i], addr_tab_null, (OCIColl *)addr_tab));  
    }

    OCIObjectGetInd(ctxptr->envhp, ctxptr->errhp, (dvoid *)addr_tab, &nind);
    ADDR_SETNOTNULL(nind);

    checkerr(ctxptr->errhp, OCIAnyDataConvert(ctxptr->svchp,
                              ctxptr->errhp, OCI_TYPECODE_NAMEDCOLLECTION,
                              addr_tab_tdo, OCI_DURATION_SESSION, (dvoid *)nind,
                              addr_tab, 0, &oan_buffer));

    break;
  }
                          
      /*
       *  associate items in insert statement with output buffer variables
       */
  checkerr(ctxptr->errhp,OCIBindByName(ctxptr->stmthp,&bndp1,
                   ctxptr->errhp, (text *) ":first_col", (sb4) -1,
                   (dvoid*) &first_col, (sb4) sizeof(sword), (ub2)SQLT_INT,
                   (dvoid*) 0, (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
                   (ub4) OCI_DEFAULT));
  checkerr(ctxptr->errhp, OCIBindByName(ctxptr->stmthp, &bndp2,
                   ctxptr->errhp, (text *) ":oan_buffer", (sb4)-1,
                   (dvoid *)0, (sb4) 0, (ub2)SQLT_NTY, (dvoid *)0, (ub2 *)0,
                   (ub2 *)0, (ub4) 0, (ub4 *) 0, (ub4)OCI_DEFAULT));
  checkerr(ctxptr->errhp, OCIBindObject(bndp2, ctxptr->errhp,
                                  any_tdo, (dvoid **) &oan_buffer, (ub4 *) 0,  
                                  (dvoid **)&any_indp, (ub4 *) 0));   
    
  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp, 
                          ctxptr->stmthp, ctxptr->errhp,
                          (ub4)1, (ub4)0, (OCISnapshot *) NULL, 
                          (OCISnapshot *) NULL, (ub4)OCI_DEFAULT));
                     
  OCIAnyDataDestroy(ctxptr->svchp, ctxptr->errhp, oan_buffer);
  
} /* end sql_exe_insert() */
    

/*********************************************************************
 * execute select statement                                          *
 * This function selects OCIANYDATA from table anydatatab, retrieves *
 * the type information from resulting OCIANYDATA and accesses data  *
 * according to the type using OCIANYDATACCESS().
 *********************************************************************/
void sql_exec_select(ctxptr, typecode, recnum)
cdemoanctx *ctxptr;
OCITypeCode typecode;
ub4 recnum;
{
  sword  col1; 
  OCIDefine *defnp1 = (OCIDefine *) NULL;
  OCIDefine *defnp2 = (OCIDefine *) NULL;

  OCIType *any_tdo = (OCIType *)0;
  OCIType *addr_tdo = (OCIType *)0;
  address *addr_obj = (address *)0;
  OCIType *type = (OCIType *)0;
  OCITypeCode tc;
  OCIInd indp;
  OCIInd *any_indp = (OCIInd *) 0;

  OCIAnyData *oan_buffer = (OCIAnyData *)0;
  ub4 len, col2, i;
  OCINumber num;
  OCINumber *num_ptr = &num;
  OCIString *str = (OCIString *) 0;
  OCIDate date;
  OCIDate *date_ptr = &date;
  text sel_stmt[200];
  sb2   year1;
  ub1   month1, day1;
  OCIRaw *raw_col = (OCIRaw *)0;
  ub1 *raw_ptr = (ub1 *)0;

  OCITable *addr_tab = (OCITable *) 0;
  OCIType *addr_tab_tdo = (OCIType *)0;
  sb4 collsiz;
  address *addr = (address *)0, *addr2 = (address *)0;
  boolean exist;
  sb4 index;
  dvoid *elem = (dvoid *)0; 
  dvoid *elemind = (dvoid *)0;

  OCIInd ind2=OCI_IND_NOTNULL;
  OCIInd *ind2p = &ind2;


  checkerr(ctxptr->errhp, OCITypeByName(ctxptr->envhp,
                   ctxptr->errhp, ctxptr->svchp,
                   (const text *) "", (ub4) strlen((const char *) ""),
                   (const text *) "ADDRESS_OBJECT",
                   (ub4) strlen((const char *) "ADDRESS_OBJECT"),
                   (CONST text *) 0, (ub4) 0,
                   OCI_DURATION_SESSION, OCI_TYPEGET_ALL, &addr_tdo));

  (void) sprintf((char *) sel_stmt, "%s%d", sel1, recnum);
  
  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, 
                ctxptr->errhp, sel_stmt, (ub4) strlen((char *)sel_stmt), 
                (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("Testing SELECT ... \n");

  checkerr(ctxptr->errhp,  OCITypeByName(ctxptr->envhp, 
                          ctxptr->errhp, ctxptr->svchp, 
                          (CONST text *)"SYS", (ub4) strlen("SYS"), 
                          (CONST text *) "ANYDATA", (ub4) strlen("ANYDATA"),
                          (CONST text *) 0, (ub4) 0, OCI_DURATION_SESSION,
                          OCI_TYPEGET_ALL, &any_tdo));
      /*
       *  associate items in select statement with output buffer variables
       */
  checkerr(ctxptr->errhp,OCIDefineByPos(ctxptr->stmthp, &defnp1,
                                  ctxptr->errhp,(ub4) 1,(dvoid*) &col1, 
                                  (sb4) sizeof(col1), (ub2)SQLT_INT, (dvoid*) 0, 
                                  (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));
  checkerr(ctxptr->errhp, OCIDefineByPos(ctxptr->stmthp, &defnp2, 
                                    ctxptr->errhp, (ub4)2, (dvoid *)0, 
                                    (sb4) 0, (ub2)SQLT_NTY, (dvoid *)0, 
                                    (ub2 *)0, (ub2 *)0, (ub4)OCI_DEFAULT));
  checkerr(ctxptr->errhp, OCIDefineObject(defnp2,
                               ctxptr->errhp, any_tdo,
                               (dvoid **) &oan_buffer, (ub4 *) 0,
                               (dvoid **)&any_indp, (ub4 *) 0));

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp, ctxptr->stmthp,
                          ctxptr->errhp, (ub4)1, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          (ub4)OCI_DEFAULT));
  
  checkerr(ctxptr->errhp, OCIAnyDataGetType(ctxptr->svchp,
                                  ctxptr->errhp, (OCIAnyData *)oan_buffer,
                                  (OCITypeCode *)&tc, (OCIType **)&type));

  if(tc != typecode)
    printf("ERROR: OCIAnyDataGetType retrieved wrong type information\n");

  /* print out results */
    printf("Printing out select stmt results ... \n");
    printf("c1 is: %d\n",col1);

  switch (typecode)
  {
    case OCI_TYPECODE_NUMBER:
      /*checkerr(ctxptr->errhp,
                OCIAnyDataAccess(ctxptr->svchp, ctxptr->errhp, 
                       oan_buffer, (OCITypeCode)OCI_TYPECODE_NUMBER, 
                       (OCIType *)0, (dvoid *)&indp, (dvoid *)&num, &len));
    OCINumberToInt(ctxptr->errhp, &num, sizeof(col2), 0, &col2); */

    checkerr(ctxptr->errhp, OCIAnyDataAccess(ctxptr->svchp, ctxptr->errhp, 
                       oan_buffer, (OCITypeCode)OCI_TYPECODE_NUMBER, 
                       (OCIType *)0, (dvoid *)&indp, (dvoid **)&num_ptr, &len));
    OCINumberToInt(ctxptr->errhp, num_ptr, sizeof(col2), 0, &col2);

    printf("c2 is: %d\n",col2);
    break;

    case OCI_TYPECODE_VARCHAR2:
    checkerr(ctxptr->errhp, OCIAnyDataAccess(ctxptr->svchp, ctxptr->errhp, 
                         oan_buffer, (OCITypeCode)OCI_TYPECODE_VARCHAR2, 
                         (OCIType *)0, (dvoid *)&indp, (dvoid *)&str, &len));
    
    printf("c2 is %s \n", OCIStringPtr(ctxptr->envhp, str));
    break;

    case OCI_TYPECODE_DATE:
      /*checkerr(ctxptr->errhp, OCIAnyDataAccess(ctxptr->svchp,
                         ctxptr->errhp, oan_buffer,
                         (OCITypeCode)OCI_TYPECODE_DATE, 
                         (OCIType *)0, (dvoid *)&indp, (dvoid *)&date, &len));
    OCIDateGetDate( (CONST OCIDate *) &date, &year1, &month1, &day1 ); */

    checkerr(ctxptr->errhp, OCIAnyDataAccess(ctxptr->svchp,
                         ctxptr->errhp, oan_buffer,
                         (OCITypeCode)OCI_TYPECODE_DATE, 
                         (OCIType *)0, (dvoid *)&indp, 
                         (dvoid **)&date_ptr, &len));
    OCIDateGetDate( (CONST OCIDate *) date_ptr, &year1, &month1, &day1 );

    printf("c2 is %d/%d/%d\n", day1, month1, year1);

    break;

    case OCI_TYPECODE_OBJECT:
    checkerr(ctxptr->errhp, OCIAnyDataAccess(ctxptr->svchp, ctxptr->errhp,
                           oan_buffer, (OCITypeCode) OCI_TYPECODE_OBJECT,
                           (OCIType *)addr_tdo, (dvoid *) &ind2p, 
                           (dvoid **)&addr_obj, &len));
    printf("state is %s \n",
                  OCIStringPtr(ctxptr->envhp, addr_obj->state));
    printf("zip is %s \n",
                  OCIStringPtr(ctxptr->envhp, addr_obj->zip));
    break;
 
    case OCI_TYPECODE_CHAR:
    checkerr(ctxptr->errhp, OCIAnyDataAccess(ctxptr->svchp, 
                         ctxptr->errhp, oan_buffer, 
                         (OCITypeCode)OCI_TYPECODE_CHAR, 
                         (OCIType *)0, (dvoid *)&indp, (dvoid *)&str, &len));
    
      printf("c2 is %s \n", OCIStringPtr(ctxptr->envhp, str));
    break;

    case OCI_TYPECODE_RAW:
    checkerr(ctxptr->errhp, OCIAnyDataAccess(ctxptr->svchp, ctxptr->errhp, 
                       oan_buffer, (OCITypeCode)OCI_TYPECODE_RAW, 
                       (OCIType *)0, (dvoid *)&indp, (dvoid *)&raw_col, &len));
   
    raw_ptr = OCIRawPtr(ctxptr->envhp, raw_col);
    printf("RAW data is: ");
    for(i=0;i<BUFLEN;i++)
      printf("%c", (int) *(raw_ptr+i) & 0xFF);
    printf("\n");
    break;

    case OCI_TYPECODE_NAMEDCOLLECTION:
    checkerr(ctxptr->errhp,  OCITypeByName(ctxptr->envhp,
                          ctxptr->errhp, ctxptr->svchp,
                          (CONST text *)"", (ub4) strlen(""),
                          (const text *) "ADDR_TAB",
                          (ub4) strlen((const char *) "ADDR_TAB"),
                          (CONST text *) 0, (ub4) 0, OCI_DURATION_SESSION,
                          OCI_TYPEGET_ALL, &addr_tab_tdo));
 
    checkerr(ctxptr->errhp, OCIAnyDataAccess(ctxptr->svchp, 
                       ctxptr->errhp, oan_buffer, 
                       (OCITypeCode)OCI_TYPECODE_NAMEDCOLLECTION, 
                       (OCIType *) addr_tab_tdo, (dvoid *) &ind2p, 
                       (dvoid *)&addr_tab, &len));

    /* check how many elements in the typed table */
    checkerr(ctxptr->errhp, OCICollSize(ctxptr->envhp, ctxptr->errhp,
                          (CONST OCIColl *) addr_tab, &collsiz));
    printf("c2 is a typed table with %d elements:\n", collsiz);
    if (collsiz == 0)
      break;
                          
    /*Dump the table from the top to the bottom. */
    /* go to the first element and print out the index */
    checkerr(ctxptr->errhp, OCITableFirst(ctxptr->envhp, 
                             ctxptr->errhp, addr_tab, &index));
    checkerr(ctxptr->errhp, OCICollGetElem(ctxptr->envhp, 
                                ctxptr->errhp,
                                (CONST OCIColl *) addr_tab, index,
                                &exist, &elem, &elemind));
    addr = (address *)elem;
              
    printf("\tAddress 1 is: %s",
                           OCIStringPtr(ctxptr->envhp,addr->state));
    printf("\t%s\n", OCIStringPtr(ctxptr->envhp, addr->zip));
                   
    for(;!OCITableNext(ctxptr->envhp, ctxptr->errhp, index, 
                     addr_tab, &index, &exist) && exist;)
    {
      checkerr(ctxptr->errhp, OCICollGetElem(ctxptr->envhp, 
                                    ctxptr->errhp,
                                    (CONST OCIColl *) addr_tab,index,
                                    &exist, &elem, &elemind));
      addr = (address *)elem;
      printf("\tAddress %d is: %s", index+1,   
                         OCIStringPtr(ctxptr->envhp,addr->state));
      printf("\t%s\n", OCIStringPtr(ctxptr->envhp, addr->zip));
    } 
    break;

    DEFAULT:
      printf("TYPED DATA CAN'T BE DISPLAYED IN THIS PROGRAM\n");
    break;
  }
} /* end sql_exe_select() */


/*******************************************************                      
 * initialize envionment and handlers                  *
 *                                                     *
 *******************************************************/
                                     
void initialize(ctxptr)
cdemoanctx *ctxptr;
{
    
  if (OCIEnvCreate((OCIEnv **) &ctxptr->envhp,
                   (ub4)OCI_THREADED|OCI_OBJECT, (dvoid *)0,
                    (dvoid * (*)(dvoid *, size_t)) 0,
                   (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                   (void (*)(dvoid *, dvoid *)) 0,
                   (size_t) 0, (dvoid **) 0 ))
       printf("FAILED: OCIEnvCreate()\n");
                                                     
    
  printf("\n ######## Connect to server ############# \n");
                                    
  if (OCIHandleAlloc((dvoid *) ctxptr->envhp, (dvoid **) &ctxptr->errhp,
                     (ub4) OCI_HTYPE_ERROR, (size_t) 0, (dvoid **) 0))
    printf("FAILED: OCIHandleAlloc() on ctxptr->errhp\n");
    
  if (OCIHandleAlloc((dvoid *) ctxptr->envhp, (dvoid **) &ctxptr->srvhp,
                     (ub4) OCI_HTYPE_SERVER, (size_t) 0, (dvoid **) 0))
    printf("FAILED: OCIHandleAlloc() on ctxptr->srvhp\n");
       
  if (OCIHandleAlloc((dvoid *) ctxptr->envhp, (dvoid **) &ctxptr->svchp,
                     (ub4) OCI_HTYPE_SVCCTX, (size_t) 0, (dvoid **) 0))
    printf("FAILED: OCIHandleAlloc() on ctxptr->svchp\n");

  
  if (OCIHandleAlloc((dvoid *) ctxptr->envhp, (dvoid **) &ctxptr->authp,
                     (ub4) OCI_HTYPE_SESSION, (size_t) 0, (dvoid **) 0))
    printf("FAILED: OCIHandleAlloc() on ctxptr->authp\n"); 
  
  if (OCIServerAttach(ctxptr->srvhp, ctxptr->errhp, (text *) "",
                      (sb4) strlen((char *) ""), (ub4) OCI_DEFAULT))
    printf("FAILED: OCIServerAttach()\n");

  if (OCIAttrSet((dvoid *) ctxptr->svchp, (ub4) OCI_HTYPE_SVCCTX,
                 (dvoid *) ctxptr->srvhp, (ub4) 0, (ub4) OCI_ATTR_SERVER,
                 ctxptr->errhp))
    printf("FAILED: OCIAttrSet() server attribute\n");
    
  
  /*begin log_on part */
                    
  if (OCIAttrSet((dvoid *) ctxptr->authp, (ub4) OCI_HTYPE_SESSION,
                 (dvoid *) username, (ub4) strlen((char *) username),
                 (ub4) OCI_ATTR_USERNAME, ctxptr->errhp))
    printf("FAILED: OCIAttrSet() userid\n"); 
  
  if (OCIAttrSet((dvoid *) ctxptr->authp, (ub4) OCI_HTYPE_SESSION,
                 (dvoid *) password, (ub4) strlen((char *) password),
                 (ub4) OCI_ATTR_PASSWORD, ctxptr->errhp))
    printf("FAILED: OCIAttrSet() passwd\n");                                
                     
  printf("Logging on as %s  ....\n", username);
                     
  checkerr(ctxptr->errhp, OCISessionBegin((dvoid *)ctxptr->svchp, 
                                   ctxptr->errhp,ctxptr->authp, 
                                  (ub4) OCI_CRED_RDBMS,(ub4) OCI_DEFAULT ));
  
  printf("%s logged on.\n", username);
     
  if (OCIAttrSet((dvoid *) ctxptr->svchp, (ub4) OCI_HTYPE_SVCCTX,   
                 (dvoid *) ctxptr->authp, (ub4) 0, (ub4) OCI_ATTR_SESSION,
                 ctxptr->errhp))
    printf("FAILED: OCIAttrSet() session\n");
     
  /* end log_on part */
                                                  
   /* alocate stmt handle for sql queries */    
  if (OCIHandleAlloc((dvoid *)ctxptr->envhp, (dvoid **) &ctxptr->stmthp,
                     (ub4)OCI_HTYPE_STMT, (CONST size_t) 0, (dvoid **) 0))
    printf("FAILED: alloc statement handle\n");
      
   
} /* end initialize() */
   

/*******************************************************                      
 * clean up envionment and handlers                    *
 *                                                     *
 *******************************************************/     
void cleanup(ctxptr)
cdemoanctx *ctxptr;
{                   
  printf("\n ########## clean up ############ \n");
                 
    if (OCISessionEnd(ctxptr->svchp, ctxptr->errhp,
                      ctxptr->authp, (ub4) 0))
         printf("FAILED: OCISessionEnd()\n"); 
    
     printf("%s Logged off.\n", username);
  
    if (OCIServerDetach(ctxptr->srvhp, ctxptr->errhp, (ub4) OCI_DEFAULT))
         printf("FAILED: OCIServerDetach()\n");
     
     printf("Detached from server.\n");
                                    
   printf("Freeing handles ...\n");
  if (ctxptr->stmthp)
     OCIHandleFree((dvoid *) ctxptr->stmthp, (ub4) OCI_HTYPE_STMT);
  if (ctxptr->errhp)
     OCIHandleFree((dvoid *) ctxptr->errhp, (ub4) OCI_HTYPE_ERROR);
  if (ctxptr->srvhp)
     OCIHandleFree((dvoid *) ctxptr->srvhp, (ub4) OCI_HTYPE_SERVER);
  if (ctxptr->svchp)
     OCIHandleFree((dvoid *) ctxptr->svchp, (ub4) OCI_HTYPE_SVCCTX);
  if (ctxptr->authp)
     OCIHandleFree((dvoid *) ctxptr->authp, (ub4) OCI_HTYPE_SESSION);
  if (ctxptr->envhp)
     OCIHandleFree((dvoid *) ctxptr->envhp, (ub4) OCI_HTYPE_ENV);
                 
} /* end cleanup() */


/*******************************************************                      
 * check status and print error information            *
 *                                                     *
 *******************************************************/ 
void checkerr(errhp, status)
OCIError *errhp;
sword status;
{
  text errbuf[512];
  sb4 errcode = 0;

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
    (void) printf("Error - OCI_NODATA\n");
    break;
  case OCI_ERROR:
    (void) OCIErrorGet((dvoid *)errhp, (ub4) 1, (text *) NULL, &errcode,
                        errbuf, (ub4) sizeof(errbuf), OCI_HTYPE_ERROR);
    (void) printf("Error - %.*s\n", 512, errbuf);
    if(errcode == 22303){
        printf("Please run cdemoanydata1.sql before executing the demo! \n");
        exit(1);
    }
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
} /* end checkerr() */


/* end of file cdemoAnyData1.c */





















