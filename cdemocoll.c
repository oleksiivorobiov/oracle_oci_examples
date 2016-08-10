/* Copyright (c) 2004, 2006, Oracle. All rights reserved.  */

/*

   NAME
     cdemocoll.c - collection demo program in OCI

   DESCRIPTION - demo of insertion(bind) and selection(define) of collection 
                 columns (nested table and varray) as named datatype SQLT_NTY
                 using OCI interface.

         Note:   Before running this program, ensure that the database is
                 started up and tables of cdemocoll_customer_nt and
                 cdemocoll_customer_va and types of cdemocoll_int_nt and
                 cdemocoll_char_va do not exist in SCOTT/TIGER sample account.
      

   EXPORT FUNCTION(S)
     <external functions defined for use outside package - one-line descriptions>

   INTERNAL FUNCTION(S)
     <other external functions defined - one-line descriptions>

   STATIC FUNCTION(S)
     <static functions defined - one-line descriptions>

   NOTES
     <other useful comments, qualifications, etc.>

   MODIFIED   (MM/DD/YY)
   azhao       10/10/06 - case-senstive password change
   aliu        05/06/04 - aliu_add_coll_demo
   aliu        05/04/04 - Creation

*/

#ifndef CDEMOCOLL
#define CDEMOCOLL

/*------------------------------------------------------------------------
 * Include Files
 */

#ifndef ORATYPES
#include <oratypes.h>
#endif

#ifndef STDIO
#include <stdio.h>
#endif

#ifndef STDLIB
#include <stdlib.h>
#endif

#ifndef STRING
#include <string.h>
#endif

#ifndef OCI_ORACLE
#include <oci.h>
#endif

#ifndef CDEMOCOLL_ORACLE
#include "cdemocoll.h"
#endif

/*----------------- End of including files -----------------*/

/*--------------------- Public Constants and Variables ----------------------*/

/* constants */
#define MAXBUFLEN 200

/* database login information */
static text *user=(text *)"SCOTT";
static text *password=(text *)"tiger";

/* OCI Handles and Variables */
static OCIEnv        *envhp;
static OCIServer     *srvhp;
static OCISvcCtx     *svchp;
static OCIError      *errhp;
static OCISession    *authp;
static OCIStmt       *stmthp;

/* Misellaneous */
static sword          status;
static boolean        tab_exists = FALSE;

/*----------------- End of Constants and Variables -----------------*/

/*--------------------- Functions Declaration --------------------*/

static void checkerr(/*_ OCIError *errhp, sword status _*/);
static void cleanup(/*_ void _*/);
static sb4 connect_server(/*_ void _*/);
static void disconnect_server(/*_ void _*/);
static void drop_table(/*_ void _*/);
static sb4 init_env_handle(/*_ void _*/);
static sb4 init_table(/*_ void _*/);
static sb4 insert_coll(/*_ ub4 custno, ub4 flag _*/);
static sb4 select_coll(/*_ ub4 custno, ub4 flag _*/);

/*--------------------- End of Functions Declaration --------------------*/

#endif


/*---------------------------Main function -----------------------------*/
int main()
{
 ub4 custno;
 ub4 flag;

 /* Initialize the environment and allocate handles */
 if (init_env_handle()) 
 {
   printf("FAILED: init_env_handle()!\n");
   return OCI_ERROR;
 }

 /* Log on to the server and begin a session */
 if (connect_server()) 
 {
   printf("FAILED: connect_server()!\n");
   cleanup();
   return OCI_ERROR;
 }

 /* Initialize the types/tables */
 if (init_table())
 {
   printf("FAILED: init_table()\n");
   disconnect_server();
   return OCI_ERROR;
 }

 custno = 1;
  /* flag = 1: nested table column
   otherwise: varray column  */
 for (flag = 1; flag <= 2; flag++) {
   /* Insertion */
   if (insert_coll(custno, flag))
   {
     printf("FAILED: insert_coll()!\n");
     disconnect_server();
     return OCI_ERROR;
   }

   /* Selection */
   if (select_coll(custno, flag))
   {
     printf("FAILED: select_coll()!\n");
     disconnect_server();
     return OCI_ERROR;
   }
 }

 /* Drop types/tables, detach from a server, and clean up the environment */
 disconnect_server();

 return OCI_SUCCESS;
}


/*---------------------------Subfunctions -----------------------------*/

/*--------------------------------------------------------*/
/* Return corresponding messages in differrent cases */
/*--------------------------------------------------------*/
void checkerr(errhp, status)
OCIError *errhp;
sword     status;
{
  text  msgbuf[512];
  sb4   errcode = 0;

  memset((void *) msgbuf, (int)'\0', (size_t)512);

  switch (status)
  {
  case OCI_SUCCESS: break;
  case OCI_SUCCESS_WITH_INFO:
    printf("status = OCI_SUCCESS_WITH_INFO\n");
    OCIErrorGet((dvoid *) errhp, (ub4) 1, (text *) NULL, &errcode,
                       msgbuf, (ub4) sizeof(msgbuf), (ub4) OCI_HTYPE_ERROR);
    printf("ERROR CODE = %d\n", errcode);
    printf("%.*s\n", 512, msgbuf);
    if (errcode == 436 || errcode == 437 || errcode == 438 || errcode == 439)
      exit(1);
    break;
  case OCI_NEED_DATA:
    printf("status = OCI_NEED_DATA\n");
    break;
  case OCI_NO_DATA:
     printf("status = OCI_NO_DATA\n");
    break;
  case OCI_ERROR:
     printf("status = OCI_ERROR\n");
     OCIErrorGet((dvoid *) errhp, (ub4) 1, (text *) NULL, &errcode,
                       msgbuf, (ub4) sizeof(msgbuf), (ub4) OCI_HTYPE_ERROR);
     printf("ERROR CODE = %d\n", errcode);
     printf("%.*s\n", 512, msgbuf);
    if (errcode == 436 || errcode == 437 || errcode == 438 || errcode == 439)
      exit(1);
    break;
  case OCI_INVALID_HANDLE:
     printf("status = OCI_INVALID_HANDLE\n");
    break;
  case OCI_STILL_EXECUTING:
     printf("status = OCI_STILL_EXECUTE\n");
    break;
  case OCI_CONTINUE:
     printf("status = OCI_CONTINUE\n");
    break;
  default:
    break;
  }

  return;
}


/*--------------------------------------------------------*/
/* Free the envhp whenever there is an error  */
/*--------------------------------------------------------*/
void cleanup() 
{
 if (envhp) {
    OCIHandleFree((dvoid *)envhp, OCI_HTYPE_ENV);
 }

 return;
}


/*--------------------------------------------------------*/
/* attach to server, set attributes, and begin session */
/*--------------------------------------------------------*/
sb4 connect_server()
{
 /* attach to server */
 if (status = OCIServerAttach((OCIServer *) srvhp, (OCIError *) errhp, 
                        (text *) "", (sb4) strlen(""), (ub4) OCI_DEFAULT)) 
 {
    printf("FAILED: OCIServerAttach() on srvhp\n");
   checkerr(errhp, status);
   return OCI_ERROR;
 }

 /* set server attribute to service context */
 if (status = OCIAttrSet((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX, 
                         (dvoid *) srvhp, (ub4) 0, (ub4) OCI_ATTR_SERVER, 
                         (OCIError *) errhp)) 
 {
    printf("FAILED: OCIAttrSet() on svchp\n");
   checkerr(errhp, status);
   return OCI_ERROR;
 }

 /* set user attribute to session */
 if (status = OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION, 
                         (dvoid *) user, (ub4) strlen((char *)user), 
                         (ub4) OCI_ATTR_USERNAME, (OCIError *) errhp)) 
 { 
    printf("FAILED: OCIAttrSet() on authp for user\n");
   checkerr(errhp, status);
   return OCI_ERROR;
 }

 /* set password attribute to session */
 if (status = OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION, 
                         (dvoid *) password, (ub4) strlen((char *)password), 
                         (ub4) OCI_ATTR_PASSWORD, (OCIError *) errhp)) 
 { 
    printf("FAILED: OCIAttrSet() on authp for password\n");
   checkerr(errhp, status);
   return OCI_ERROR;
 }

 /* Begin a session  */
 if (status = OCISessionBegin((OCISvcCtx *) svchp, (OCIError *) errhp, 
                              (OCISession *) authp, (ub4) OCI_CRED_RDBMS, 
                              (ub4) OCI_DEFAULT)) 
 {
    printf("FAILED: OCISessionBegin()\n");
   checkerr(errhp, status);
   return OCI_ERROR;
 } 

 /* set session attribute to service context */
 if (status = OCIAttrSet((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX, 
                         (dvoid *) authp, (ub4) 0, (ub4) OCI_ATTR_SESSION, 
                         (OCIError *) errhp)) 
 {
    printf("FAILED: OCIAttrSet() on svchp\n");
   checkerr(errhp, status);
   return OCI_ERROR;
 }

 return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* End the session, detach server and free handles. */
/*--------------------------------------------------------*/
void disconnect_server()
{
  printf("\n\nLogged off and detached from server.\n");

   /* Drop the tables/types if any */
 if (tab_exists)
    drop_table();

   /* End a session */
 if (status = OCISessionEnd((OCISvcCtx *)svchp, (OCIError *)errhp, 
                           (OCISession *)authp, (ub4) OCI_DEFAULT)) { 
   checkerr(errhp, status);
   cleanup();
   return; 
 }

   /* Detach from the server */
 if (status = OCIServerDetach((OCIServer *)srvhp, (OCIError *)errhp, 
                             (ub4)OCI_DEFAULT)) {
   checkerr(errhp, status);
   cleanup();
   return;
 } 

   /* Free the handles */
 if (stmthp) {
    OCIHandleFree((dvoid *)stmthp, (ub4) OCI_HTYPE_STMT); 
 }
 if (authp) {
    OCIHandleFree((dvoid *)authp, (ub4) OCI_HTYPE_SESSION); 
 }
 if (svchp) {
    OCIHandleFree((dvoid *)svchp, (ub4) OCI_HTYPE_SVCCTX); 
 }
 if (srvhp) {
    OCIHandleFree((dvoid *)srvhp, (ub4) OCI_HTYPE_SERVER); 
 }
 if (errhp) {
    OCIHandleFree((dvoid *)errhp, (ub4) OCI_HTYPE_ERROR); 
 }
 if (envhp) {
    OCIHandleFree((dvoid *)envhp, (ub4) OCI_HTYPE_ENV); 
 }

  return;
}


/*--------------------------------------------------------*/
/* Drop tables/types */
/*--------------------------------------------------------*/
void drop_table()
{
  text *dropstmt[4] = { (text *) "drop table cdemocoll_customer_nt",
        (text *) "drop type cdemocoll_int_nt",
        (text *) "drop table cdemocoll_customer_va",
        (text *) "drop type cdemocoll_char_va" };
  int i=0;

  for (i=0; i<4; i++) {

  /* prepare drop statement */
  if (OCIStmtPrepare(stmthp, errhp, dropstmt[i], (ub4) strlen((char *) dropstmt[i]), (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
     printf("FAILED: OCIStmtPrepare() dropstmt\n");
    return;
  }
  /* execute drop statement */
  if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                    (CONST OCISnapshot *) 0, (OCISnapshot *) 0,
                    (ub4) OCI_DEFAULT))
  {
     printf("FAILED: OCIStmtExecute() dropstmt\n");
    return;
  }

  } /* end of for (i=0; ... ) */

  return;
}


/*--------------------------------------------------------*/
/* Initialize the environment and allocate handles */
/*--------------------------------------------------------*/
sb4 init_env_handle() 
{
 /* Environment initialization and creation */
 if (OCIEnvCreate((OCIEnv **) &envhp, (ub4) OCI_OBJECT, (dvoid *) 0, 
                  (dvoid * (*)(dvoid *,size_t)) 0, 
                  (dvoid * (*)(dvoid *, dvoid *, size_t)) 0, 
                  (void (*)(dvoid *, dvoid *)) 0, (size_t) 0, (dvoid **) 0)) 
 {
    printf("FAILED: OCIEnvCreate()\n");
   return OCI_ERROR;
 }
 
 /* allocate error handle */
 if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &errhp, 
                (ub4) OCI_HTYPE_ERROR, (size_t) 0, (dvoid **) 0)) 
 {
    printf("FAILED: OCIHandleAlloc() on errhp\n");
   return OCI_ERROR;
 }

 /* allocate server handle */
 if (status = OCIHandleAlloc((dvoid *) envhp, (dvoid **) &srvhp, 
                        (ub4) OCI_HTYPE_SERVER, (size_t) 0, (dvoid **) 0)) 
 {
    printf("FAILED: OCIHandleAlloc() on srvhp\n");
   checkerr(errhp, status);
   return OCI_ERROR;
 }
 
 /* allocate service context handle */
 if (status = OCIHandleAlloc((dvoid *) envhp, (dvoid **) &svchp, 
                        (ub4) OCI_HTYPE_SVCCTX, (size_t) 0, (dvoid **) 0)) 
 {
    printf("FAILED: OCIHandleAlloc() on svchp\n");
   checkerr(errhp, status);
   return OCI_ERROR;
 }

 /* allocate session handle */
 if (status = OCIHandleAlloc((dvoid *) envhp, (dvoid **) &authp, 
                      (ub4) OCI_HTYPE_SESSION, (size_t) 0, (dvoid **) 0)) 
 {
    printf("FAILED: OCIHandleAlloc() on authp\n");
   checkerr(errhp, status);
   return OCI_ERROR;
 }

 /* Allocate statement handle */
 if (status = OCIHandleAlloc((dvoid *) envhp, (dvoid **) &stmthp, 
                       (ub4) OCI_HTYPE_STMT, (size_t) 0, (dvoid **) 0)) 
 {
    printf("FAILED: OCIHandleAlloc() on stmthp\n");
   checkerr(errhp, status);
   return OCI_ERROR;
 }

 return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* create types and tables */
/*--------------------------------------------------------*/
sb4 init_table()
{
  int   colc;
  text *crtstmt[4] = { (text *) "create type cdemocoll_int_nt is table of integer", 
        (text *) "create table cdemocoll_customer_nt (custno number, income cdemocoll_int_nt)\
                  nested table income store as cdemocoll_income_table",
        (text *) "create type cdemocoll_char_va is varray(10) of char(20)",
        (text *) "create table cdemocoll_customer_va (custno number, names cdemocoll_char_va)" };
  int i=0;

  for (i=0; i<4; i++) {

  /* prepare create statement */
  if (status = OCIStmtPrepare(stmthp, errhp, crtstmt[i], (ub4) strlen((char *) crtstmt[i]), (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
     printf("FAILED: OCIStmtPrepare() crtstmt\n");
    checkerr(errhp, status);
    return OCI_ERROR;
  }
  /* execute create statement */
  if (status = OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                    (CONST OCISnapshot *) 0, (OCISnapshot *) 0,
                    (ub4) OCI_DEFAULT))
  {
     printf("FAILED: OCIStmtExecute() crtstmt\n");
    checkerr(errhp, status);
    return OCI_ERROR;
  }
 
  /* commit the Xn */
   OCITransCommit(svchp, errhp, (ub4)0);

  } /* end of for (i=0; ... ) */

  /* set flag to be used by disconnect_server() to drop the tables */
  tab_exists = TRUE;

  return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* insert collection column into the table */
/* flag = 1: nested table column
   otherwise: varray column  */
/*--------------------------------------------------------*/
sb4 insert_coll(custno, flag)
ub4 custno;
ub4 flag;
{
  text inst[MAXBUFLEN];
  OCIBind *bndp1 = (OCIBind *)0;
  OCIBind *bndp2 = (OCIBind *)0;
  OCIType *coll_tdo= (OCIType *)0;
  OCIColl *ptrcoll = (OCIColl *)0;
  ub4 i = 0, elem = 0;
  OCINumber ocinum;
  OCIString *ocistr = (OCIString *)0;
  text *str[5] = { (text *)"Andy", (text *)"Bill", (text *)"Cathy",
                   (text *)"David", (text *)"Eddy" };

  if (flag == 1) {
    strcpy((char *)inst, (char *)"INSERT INTO cdemocoll_customer_nt VALUES (:num, :coll)");
  }
  else {
    strcpy((char *)inst, (char *)"INSERT INTO cdemocoll_customer_va VALUES (:num, :coll)");
  }

  printf("\n=> %s for custno %d ......\n", inst, custno);

  /* get type information */
  OCITypeByName(envhp, errhp, svchp, (CONST text *)"", (ub4)0,
           (CONST text *) ((flag == 1) ? "CDEMOCOLL_INT_NT" : "CDEMOCOLL_CHAR_VA"),
           (ub4) ((flag == 1) ? strlen("CDEMOCOLL_INT_NT") : strlen("CDEMOCOLL_CHAR_VA")),
           (CONST text *) 0, (ub4) 0, OCI_DURATION_SESSION,
           OCI_TYPEGET_HEADER, &coll_tdo);

  /* instantiate the collection */
  OCIObjectNew(envhp, errhp, svchp, 
                   ((flag == 1) ? OCI_TYPECODE_TABLE : OCI_TYPECODE_VARRAY), 
                   (OCIType *) coll_tdo,
                   (dvoid *) 0, OCI_DURATION_SESSION, TRUE,
                   (dvoid **) &ptrcoll);

  if (flag == 1) {
    /* populate the nested table */
    for (i = 0; i < 5; i++)
    {
      elem = i;
      checkerr(errhp,OCINumberFromInt(errhp, &elem, sizeof(elem), OCI_NUMBER_UNSIGNED,
      &ocinum));
      checkerr(errhp,OCICollAppend(envhp, errhp, (CONST dvoid *)&ocinum,
                       (dvoid *) 0, (OCIColl *) ptrcoll));
    }
  }
  else {
    /* populate VARRAY */
    for (i = 0; i < 5; i++)
    {
      checkerr(errhp, OCIStringAssignText(envhp,
                       errhp, str[i], (ub4)strlen((char *) str[i]),
                       &ocistr));
      checkerr(errhp, OCICollAppend(envhp,
                       errhp, (CONST dvoid *)ocistr, (dvoid *) 0,
                       (OCIColl *) ptrcoll));
    }
  }

  /* insert the collection column into the table */
  printf ("\n ######## inserting the collection ############ \n");
  OCIStmtPrepare(stmthp, errhp, inst, (ub4) strlen((char *)inst),
           (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT);

  i = custno;
  OCIBindByName(stmthp, &bndp1, errhp,
           (text *)":num", (sb4)strlen(":num"), (dvoid*)&i, sizeof(i),
           SQLT_INT, (dvoid*)0, (ub2*)0, (ub2*)0, (ub4)0, (ub4*)0,
           OCI_DEFAULT);

  OCIBindByName(stmthp, &bndp2, errhp,
           (text *)":coll", (sb4)strlen (":coll"), (dvoid*)0, (sb4)0,
           SQLT_NTY, (dvoid*)0, (ub2*)0, (ub2*)0, (ub4)0, (ub4*)0,
           OCI_DEFAULT);
  if (status = OCIBindObject(bndp2, errhp, coll_tdo, (dvoid **) &ptrcoll, (ub4 *) 0,
           (dvoid **)0, (ub4 *) 0)) {
     checkerr(errhp, status);
  }

  if (status = OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0, (OCISnapshot *)NULL,
           (OCISnapshot *)NULL, OCI_COMMIT_ON_SUCCESS)) {
     checkerr(errhp, status);
  }

  /* free the collection */
  OCIObjectFree(envhp, errhp, ptrcoll, OCI_OBJECTFREE_FORCE);

  printf("\n=> Insertion done\n");

  return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* select collection column from the table */
/* flag = 1: nested table column
   otherwise: varray column  */
/*--------------------------------------------------------*/
sb4 select_coll(custno, flag)
ub4 custno;
ub4 flag;
{
  text sel[MAXBUFLEN];
  OCIDefine *defp1 = (OCIDefine *)0;
  OCIBind *bndp1 = (OCIBind *)0;
  OCIType *coll_tdo= (OCIType *)0;
  OCIColl *ptrcoll = (OCIColl *)0;
  ub4 i = 0;
  dvoid *elem = (dvoid *)0;
  ub4 size = 0;
  boolean exists = FALSE;
  int nt_value = 0;
  OCIString *va_value = (OCIString *)0;

  if (flag == 1) {
    strcpy((char *)sel, (char *)"SELECT income FROM cdemocoll_customer_nt WHERE custno = :num"); 
  }
  else {
    strcpy((char *)sel, (char *)"SELECT names FROM cdemocoll_customer_va WHERE custno = :num"); 
  }

  printf("\n=> %s for custno %d ......\n", sel, custno);

  /* get type information */
    /* get type information */
  OCITypeByName(envhp, errhp, svchp, (CONST text *)"", (ub4)0,
           (CONST text *) ((flag == 1) ? "CDEMOCOLL_INT_NT" : "CDEMOCOLL_CHAR_VA"),
           (ub4) ((flag == 1) ? strlen("CDEMOCOLL_INT_NT") : strlen("CDEMOCOLL_CHAR_VA")),
           (CONST text *) 0, (ub4) 0, OCI_DURATION_SESSION,
           OCI_TYPEGET_HEADER, &coll_tdo);

  /* instantiate the collection */
  OCIObjectNew(envhp, errhp, svchp,
                   ((flag == 1) ? OCI_TYPECODE_TABLE : OCI_TYPECODE_VARRAY),
                   (OCIType *) coll_tdo,
                   (dvoid *) 0, OCI_DURATION_SESSION, TRUE,
                   (dvoid **) &ptrcoll);

  /* select the collection column from the table */
  printf ("\n ######## selecting the collection ############ \n");
  OCIStmtPrepare(stmthp, errhp, sel, (ub4) strlen((char *)sel),
           (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT);

  OCIDefineByPos(stmthp, &defp1, errhp, (ub4) 1, (dvoid *)0, 
           (sb4) 0, SQLT_NTY, (dvoid *) 0,
           (ub2 *)0, (ub2 *)0, (ub4) OCI_DEFAULT);
  if (status = OCIDefineObject(defp1, errhp, coll_tdo, (dvoid **) &ptrcoll, &size, 
           (dvoid **)0, (ub4 *)0)) {
     checkerr(errhp, status);
  }

  i = custno;
  OCIBindByName(stmthp, &bndp1, errhp,
           (text *)":num", (sb4)strlen(":num"), (dvoid*)&i, sizeof(i),
           SQLT_INT, (dvoid*)0, (ub2*)0, (ub2*)0, (ub4)0, (ub4*)0,
           OCI_DEFAULT);

  if (status = OCIStmtExecute(svchp, stmthp, errhp, (ub4) 0, (ub4) 0, (OCISnapshot *)NULL,
           (OCISnapshot *)NULL, OCI_DEFAULT)) {
     checkerr(errhp, status);
  }

  if (status = OCIStmtFetch2(stmthp, errhp, (ub4)1, OCI_FETCH_NEXT, (sb4) 0, OCI_DEFAULT)) {
     checkerr(errhp, status);
  }

  /* print out the results */
  if (flag == 1) {
    /* print out the nested table */
    printf ("\n ######## printing out the nested table ############ \n");
    for (i=0; i < 5; i++)
    {
      OCICollGetElem(envhp, errhp, (CONST OCIColl *) ptrcoll, (sb4) i, &exists,
               (dvoid **)&elem, (dvoid **)0);
      if (!exists)
      {
        printf("Error: end of collection\n");
        break;
      }
      else {
        OCINumberToInt(errhp, elem, sizeof(nt_value), 0, &nt_value);
        printf ("%d\n", nt_value);
      }
    }
  }
  else {
    /* print out VARRAY */
    printf ("\n ######## printing out the VARRAY ############ \n");
    for (i =0; i < 5; i++)
    {
      checkerr(errhp, OCICollGetElem(envhp,
               errhp, (CONST OCIColl *) ptrcoll, (sb4) i, &exists,
               (dvoid **)&elem, (dvoid **)0));
      if (!exists)
      {
        printf("Error: end of collection\n");
        break;
      }
      else {
        va_value = *(OCIString **)elem;
        printf("%s\n", (text *)OCIStringPtr(envhp, (CONST OCIString *)va_value));
      }
    }
  }

  /* free the collection */
  OCIObjectFree(envhp, errhp, ptrcoll, OCI_OBJECTFREE_FORCE);

  printf("\n=> Selection done\n");

  return OCI_SUCCESS;
}


/* end of file cdemocoll.c */
