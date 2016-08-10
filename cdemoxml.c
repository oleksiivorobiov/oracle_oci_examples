/* Copyright (c) 2001, 2006, Oracle. All rights reserved.  
*/

/*

   NAME
     cdemoxml.c - XMLType demo program in OCI

   DESCRIPTION - demo of selection and insertion of XMLType column
                 using OCI interface.

         Note:   Before running this program, ensure that the database is
                 started up with compatible=9.0.0.0.0 and an table CDEMOXML_FOO does 
                 not exist in the SCOTT/TIGER sample account.
      


   MODIFIED   (MM/DD/YY)
   azhao      10/10/06 - case-senstive password change
   aliu       07/05/01 - update SQLCS_IMPLICIT to OCI_DEFAULT for OCILobCreateTemporary.
   aliu       06/27/01 - Change table name not to be conflict with other tests.
   aliu       04/24/01 - Merged aliu_dt_demo_oci
   aliu       04/24/01 - Modify variable names and main prototype.
   aliu       04/24/01 - Update lint warnings.
   aliu       04/23/01 - Add more comments.
   aliu       04/10/00 - Creation

*/

#ifndef CDEMOXML
#define CDEMOXML

/*------------------------------------------------------------------------
 * Include Files
 */

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

/*----------------- End of including files -----------------*/

/*--------------------- Public Constants and Variables ----------------------*/

/* constants */
#define MAXBUFLEN 2000

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
static OCIDefine     *defnp = (OCIDefine *) 0;
static OCIBind       *bndhp1 = (OCIBind *) 0, *bndhp2 = (OCIBind *) 0;

/* Misellaneous */
static sword          status;
static boolean        tab_exists = FALSE;

/*----------------- End of Constants and Variables -----------------*/

/*--------------------- Functions Declaration --------------------*/

int  main(/*_ void _*/);
static void checkerr(/*_ OCIError *errhp, sword status _*/);
static void cleanup(/*_ void _*/);
static sb4 connect_server(/*_ void _*/);
static void disconnect_server(/*_ void _*/);
static void drop_table(/*_ void _*/);
static sb4 init_env_handle(/*_ void _*/);
static sb4 init_table(/*_ void _*/);
static sb4 insert_xml(/*_ ub4 rownum _*/);
static sb4 select_xml(/*_ ub4 rownum _*/);

/*--------------------- End of Functions Declaration --------------------*/

#endif


/*---------------------------Main function -----------------------------*/
int main()
{
 ub4 rownum;

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

 /* Initialize the demo table */
 if (init_table())
 {
   printf("FAILED: init_table()\n");
   disconnect_server();
   return OCI_ERROR;
 }

 rownum = 1;
 /* Insertion */
 if (insert_xml(rownum))
 {
   printf("FAILED: insert_xml()!\n");
   disconnect_server();
   return OCI_ERROR;
 }

 /* Selection */
 if (select_xml(rownum))
 {
   printf("FAILED: select_xml()!\n");
   disconnect_server();
   return OCI_ERROR;
 }

 /* Detach from a server and clean up the environment */
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

   /* Drop the demo table if any */
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
/* Drop table CDEMOXML_FOO before logging off from the server */
/*--------------------------------------------------------*/
void drop_table()
{
  text *dropstmt = (text *) "DROP TABLE CDEMOXML_FOO";

  /* prepare drop statement */
  if (OCIStmtPrepare(stmthp, errhp, dropstmt, (ub4) strlen((char *) dropstmt), (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
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
/* create a table CDEMOXML_FOO with one XMLType column */
/*--------------------------------------------------------*/
sb4 init_table()
{
  int   colc;
  text *crtstmt = (text *) "CREATE TABLE CDEMOXML_FOO (xml_col sys.xmltype, int_col INTEGER)";

  /* prepare create statement */
  if (status = OCIStmtPrepare(stmthp, errhp, crtstmt, (ub4) strlen((char *) crtstmt), (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
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

  /* set flag to be used by disconnect_server() to drop the table */
  tab_exists = TRUE;

  return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* insert xml into the table */
/*--------------------------------------------------------*/
sb4 insert_xml(rownum)
ub4 rownum;
{
  ub4      colc = rownum;
  ub4      amtp, nbytes;
  ub1      bufp[MAXBUFLEN] = "<?xml version=\"1.0\"?>\n<name>Rachel</name>";
  text     *insstmt = (text *)"INSERT INTO CDEMOXML_FOO (xml_col, int_col) VALUES (sys.xmltype.createxml(:xmlval), :intval)";
  OCILobLocator *clob;

   printf("\n=> Inserting to row %d......\n", rownum);

   /* Allocate the lob descriptor */
  if (OCIDescriptorAlloc((dvoid *) envhp, (dvoid **) &clob,
                       (ub4)OCI_DTYPE_LOB, (size_t) 0, (dvoid **) 0))
  {
    printf("FAILED: OCIDescriptorAlloc()\n");
    checkerr(errhp, status);
    return OCI_ERROR;
  }

  /* initialize a lob */
  if (status = OCILobCreateTemporary(svchp,errhp,clob,
                 (ub2)OCI_DEFAULT,(ub1)OCI_DEFAULT,OCI_TEMP_CLOB,FALSE,
                 OCI_DURATION_SESSION))
  {
    printf("ERROR: OCILobCreateTemporary()\n");
    checkerr(errhp, status);
    return OCI_ERROR;
  }
  nbytes = MAXBUFLEN-1;
  amtp = (ub4)strlen((char *)bufp);
  if ( status = OCILobWrite(svchp, errhp, clob, &amtp, 1,
                    (dvoid *) bufp, (ub4) nbytes, OCI_ONE_PIECE, (dvoid *)0,
                    (sb4 (*)(dvoid *, dvoid *, ub4 *, ub1 *)) 0,
                    (ub2) 0, (ub1) SQLCS_IMPLICIT))
  {
    printf("ERROR: OCILobWrite()\n");
    checkerr(errhp, status);
    return OCI_ERROR;
  }

  /* prepare insert statement */
  if (status = OCIStmtPrepare(stmthp, errhp, (text *)insstmt,
                              (ub4) strlen((char *)insstmt),
                              (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
     printf("FAILED: OCIStmtPrepare() insstmt\n");
    checkerr(errhp, status);
    return OCI_ERROR;
  }

  /* associate variables with bind placeholders in the SQL statement */
  if ((status = OCIBindByName(stmthp, &bndhp1, errhp,
                   (CONST text *)":xmlval",(sb4) strlen((char *) ":xmlval"),
                   (dvoid *)&clob, (sb4) -1, SQLT_CLOB,
                   (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                   (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT))
     || (status = OCIBindByName(stmthp, &bndhp2, errhp,
                   (CONST text *)":intval",(sb4) strlen((char *) ":intval"),
                   (dvoid *)&colc, (sb4) sizeof(colc), SQLT_INT,
                   (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                   (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT)))
  {
    printf("FAILED: OCIBindByName()\n");
    checkerr(errhp, status);
    return OCI_ERROR;
  }

  /* execute the statement */
  if (status = OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                    (CONST OCISnapshot*) 0, (OCISnapshot*) 0,
                    (ub4) OCI_DEFAULT))
  {
    printf("FAILED: OCIStmtExecute() insstmt\n");
    checkerr(errhp, status);
    return OCI_ERROR;
  }

   printf("\n=> Insertion done\n");

  /* Free the lob locator */
  if (clob) {
     OCIDescriptorFree((dvoid *) clob, (ub4) OCI_DTYPE_LOB);
  }

  return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* select xml column from the table */
/*--------------------------------------------------------*/
sb4 select_xml(rownum)
ub4 rownum;
{
  ub4      colc = rownum;
  ub4      amtp, nbytes;
  ub1      bufp[MAXBUFLEN];
  text     *selstmt =(text *)"SELECT t.xml_col.getClobVal() FROM CDEMOXML_FOO t WHERE t.int_col = :1";
  OCILobLocator *clob;

  printf("\n=> Selecting row %d......\n", rownum);

   /* Allocate the lob descriptor */
  if (OCIDescriptorAlloc((dvoid *) envhp, (dvoid **) &clob,
                       (ub4)OCI_DTYPE_LOB, (size_t) 0, (dvoid **) 0))
  {
    printf("FAILED: OCIDescriptorAlloc()\n");
    checkerr(errhp, status);
    return OCI_ERROR;
  }

  /* prepare select statement */
  if (status = OCIStmtPrepare(stmthp, errhp, (text *)selstmt,
                              (ub4) strlen((char *)selstmt),
                              (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
     printf("FAILED: OCIStmtPrepare() selstmt\n");
    checkerr(errhp, status);
    return OCI_ERROR;
  }

  /* associate variable colc with bind placeholder #1 in the SQL statement */
  if (status = OCIBindByPos(stmthp, &bndhp1, errhp, (ub4) 1,
                      (dvoid *) &colc, (sb4) sizeof(colc), SQLT_INT,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT))
  {
     printf("FAILED: OCIBindByPos()\n");
    checkerr(errhp, status);
    return OCI_ERROR;
  }

  /* associate lob var with its define handle */
  if (status = OCIDefineByPos(stmthp, &defnp, errhp, (ub4) 1,
                   (dvoid *) &clob, (sb4) -1, (ub2) SQLT_CLOB,
                   (dvoid *) 0, (ub2 *) 0, (ub2 *) 0, (ub4) OCI_DEFAULT))
  {
    printf("FAILED: OCIDefineByPos() CLOB\n");
    checkerr(errhp, status);
    return OCI_ERROR;
  }

  /* execute the select statement */
  if (status = OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                    (CONST OCISnapshot*) 0, (OCISnapshot*) 0,
                    (ub4) OCI_DEFAULT))
  {
     printf("FAILED: OCIStmtExecute() selstmt\n");
    checkerr(errhp, status);
    return OCI_ERROR;
  }

  /* read the fetched value into a buffer */
  amtp = nbytes = MAXBUFLEN-1;
  if (status = OCILobRead(svchp, errhp, clob, &amtp, 
                (ub4) 1, (dvoid *) bufp, (ub4) nbytes, (dvoid *)0,
                (sb4 (*)(dvoid *, CONST dvoid *, ub4, ub1)) 0,
                (ub2) 0, (ub1) SQLCS_IMPLICIT))
  {
    printf("FAILED: OCILobRead() \n");
    checkerr(errhp, status);
    return OCI_ERROR;
  }
  bufp[amtp] = '\0';

  /* Print out the buffer */
  if (amtp > 0) {
     printf("\n=> Query result of %s on row %d: \n%s\n", selstmt, rownum, bufp);
  }
  else {
     printf("\nEmpty value\n");
  }

  /* Free the lob locator */
  if (clob) {
     OCIDescriptorFree((dvoid *) clob, (ub4) OCI_DTYPE_LOB);
  }

  return OCI_SUCCESS;
}


/* end of file cdemoxml.c */
