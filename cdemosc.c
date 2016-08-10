/* Copyright (c) 2001, Oracle Corporation.  All rights reserved.  */

/*

   NAME
     cdemosc.c - OCI demo program for scrollable cursor.

   DESCRIPTION
     An example program which reads employee records from table empo.
     SQL> describe empo;
     Name                      Null?    Type
     ------------------------- -------- ---------------
     EMPNO                              NUMBER
     ENAME                              CHAR(5)
     ADDR                               EMPADDR
     ECOLL                              EVARRAY
     SQL> describe empaddr;
     Name                      Null?    Type
     ------------------------- -------- ---------------
     STATE                              CHAR(2)
     ZIP                                NUMBER

   EXPORT FUNCTION(S)
     <external functions defined for use outside package - one-line descriptions>

   INTERNAL FUNCTION(S)
     <other external functions defined - one-line descriptions>

   STATIC FUNCTION(S)
     <static functions defined - one-line descriptions>

   NOTES
     Dependent Files:
       cdemosc.sql  - SQL script to be run before execution.

   MODIFIED   (MM/DD/YY)
   ani         04/30/01 - Merged ani_ocidemo
   ani         04/24/01 - Creation

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef OCI_ORACLE
#include <oci.h>
#endif

typedef struct address
{
  OCIString * state ;
  OCINumber zip;
} address ;

typedef struct null_address
{
  sb2 null_state ;
  sb2 null_zip;
} null_address ;


/*---------------------------------------------------------------------------
                     PRIVATE TYPES AND CONSTANTS
  ---------------------------------------------------------------------------*/

#define MAXROWS 3
#define MAX_ENAMELEN 20

static text *username = (text *) "scott";
static text *password = (text *) "tiger";

/* Define SQL statements to be used in program. */
static text *selemp = (text *)"SELECT empno, ename, addr, ecoll FROM empo";

static sword    empno[MAXROWS] ;
static text empname[MAXROWS][MAX_ENAMELEN];
static address * empaddr [MAXROWS] ;
static null_address * indaddr [MAXROWS] ;
static ub4 rc[MAXROWS] ;
static OCIColl * evarray[MAXROWS] ;
static OCIEnv *envhp;
static OCIError *errhp;
static OCISvcCtx *svchp;
static sword status;
static OCIStmt *stmthp;
static sword status;
                
/*---------------------------------------------------------------------------
                     STATIC FUNCTION DECLARATIONS 
  ---------------------------------------------------------------------------*/

static void checkerr(/*_ OCIError *errhp, sword status _*/);
static void checkprint(/*_ OCIError *errhp, sword status, ub4 nrows _*/);
static void cleanup(/*_ void _*/);
static void myprint (/*_ ub4 _*/);
int main(/*_ int argc, char *argv[] _*/);


int main(argc, argv)
int argc;
char *argv[];
{

  OCISession *authp = (OCISession *) 0;
  OCIServer *srvhp;
  OCISvcCtx *svchp;
  OCIType * addr_tdo ;  
  OCIType * addr_tdo2 ;  
  ub4 empaddrsz ;  
  OCIDefine *defn1p = (OCIDefine *) 0;
  OCIDefine *defn2p = (OCIDefine *) 0;
  OCIDefine *defn3p = (OCIDefine *) 0;
  OCIDefine *defn4p = (OCIDefine *) 0;
  OCIDefine *defn5p = (OCIDefine *) 0;
  int num ;  

  ub4 prefetch = 5 ;
   
  (void) OCIInitialize((ub4) OCI_DEFAULT | OCI_OBJECT, (dvoid *)0,
                       (dvoid * (*)(dvoid *, size_t)) 0,
                       (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                       (void (*)(dvoid *, dvoid *)) 0 );

  (void) OCIEnvInit( (OCIEnv **) &envhp, OCI_DEFAULT, (size_t) 0,
                     (dvoid **) 0 );

  (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &errhp, OCI_HTYPE_ERROR,
                   (size_t) 0, (dvoid **) 0);

  /* server contexts */
  (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &srvhp, OCI_HTYPE_SERVER,
                   (size_t) 0, (dvoid **) 0);

  (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &svchp, OCI_HTYPE_SVCCTX,
                   (size_t) 0, (dvoid **) 0);

  (void) OCIServerAttach( srvhp, errhp, (text *)"", (sb4) strlen(""), 0);

  /* set attribute server context in the service context */
  (void) OCIAttrSet( (dvoid *) svchp, OCI_HTYPE_SVCCTX, (dvoid *)srvhp,
                     (ub4) 0, OCI_ATTR_SERVER, (OCIError *) errhp);

  (void) OCIHandleAlloc((dvoid *) envhp, (dvoid **)&authp,
                        (ub4) OCI_HTYPE_SESSION, (size_t) 0, (dvoid **) 0);

  (void) OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION,
                 (dvoid *) username, (ub4) strlen((char *)username),
                 (ub4) OCI_ATTR_USERNAME, errhp);

  (void) OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION,
                 (dvoid *) password, (ub4) strlen((char *)password),
                 (ub4) OCI_ATTR_PASSWORD, errhp);

  checkerr(errhp, OCISessionBegin ( svchp,  errhp, authp, OCI_CRED_RDBMS,
                          (ub4) OCI_DEFAULT));

  (void) OCIAttrSet((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
                   (dvoid *) authp, (ub4) 0,
                   (ub4) OCI_ATTR_SESSION, errhp);

  checkerr(errhp, OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &stmthp,
           OCI_HTYPE_STMT, (size_t) 0, (dvoid **) 0));


  /**** SCROLLABLE RESULT SET *****/

  /* Retrieve the employee records */
  checkerr(errhp, OCIStmtPrepare(stmthp, errhp, selemp,
                                (ub4) strlen((char *) selemp),
                                (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  (void) OCIAttrSet((dvoid *) stmthp, (ub4) OCI_HTYPE_STMT,
                 (dvoid *) & prefetch, 0,
                 (ub4) OCI_ATTR_PREFETCH_ROWS, errhp);

  /* bind the input variable */
  checkerr(errhp, OCIDefineByPos(stmthp, &defn1p, errhp, 1, (dvoid *) empno,
                   (sword) sizeof(sword), SQLT_INT, (dvoid *) 0, (ub2 *)0,
                   (ub2 *)0, OCI_DEFAULT));
  checkerr(errhp, OCIDefineByPos(stmthp, &defn2p, errhp, 2, (dvoid **) empname,
                   (sword) MAX_ENAMELEN, SQLT_STR, (dvoid *) 0, (ub2 *)0,
                   (ub2 *)0, OCI_DEFAULT));

  checkerr(errhp, OCITypeByName(envhp, errhp, svchp, (const text *) 0,
                   (ub4) 0, (const text *) "EMPADDR",
                   (ub4) strlen((const char *) "EMPADDR"),
                   (CONST text *) 0, (ub4) 0,
                                OCI_DURATION_SESSION,  OCI_TYPEGET_ALL,
                   &addr_tdo)); 

  checkerr(errhp, OCIDefineByPos(stmthp, &defn3p, errhp, 3, (dvoid *) 0,
                   (sword) 0, SQLT_NTY, (dvoid *) 0, (ub2 *)0,
                   (ub2 *)0, OCI_DEFAULT));
  checkerr(errhp, OCIDefineObject(defn3p, errhp, addr_tdo, (dvoid **) empaddr,
                                  (ub4 *) & empaddrsz, (dvoid **) indaddr, (ub4 *) rc));  
  
  checkerr(errhp, OCITypeByName(envhp, errhp, svchp, (const text *) 0,
                   (ub4) 0, (const text *) "EVARRAY",
                   (ub4) strlen((const char *) "EVARRAY"),
                   (CONST text *) 0, (ub4) 0,
                                OCI_DURATION_SESSION,  OCI_TYPEGET_ALL,
                   &addr_tdo2)); 
  checkerr(errhp, OCIDefineByPos(stmthp, &defn4p, errhp, 4, (dvoid *) 0,
                   (sword) 0, SQLT_NTY, (dvoid *) 0, (ub2 *)0,
                   (ub2 *)0, OCI_DEFAULT));
  checkerr(errhp, OCIDefineObject(defn4p, errhp, addr_tdo2, (dvoid **) evarray,
                                  (ub4 *) 0, (dvoid **) 0, (ub4 *) 0));  
  
  checkprint(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 0, (ub4)
                                 0,  (CONST OCISnapshot *)
                                 NULL, (OCISnapshot *) NULL,
                                 OCI_STMT_SCROLLABLE_READONLY ),0);  

  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 3, 
                               OCI_FETCH_ABSOLUTE, (sb4) 6, OCI_DEFAULT),3);

  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 3, 
                               OCI_FETCH_RELATIVE, (sb4) -2, OCI_DEFAULT),3);

  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 2, 
                               OCI_FETCH_ABSOLUTE, (sb4) 9, OCI_DEFAULT),2);

  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 1, 
                               OCI_FETCH_LAST, (sb4) 0, OCI_DEFAULT),1);

  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 1, 
                               OCI_FETCH_FIRST, (sb4) 0, OCI_DEFAULT),1);

  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 1, 
                               OCI_FETCH_LAST, (sb4) 0, OCI_DEFAULT),1);

  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 1, 
                               OCI_FETCH_FIRST, (sb4) 0, OCI_DEFAULT),1);

  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 1, 
                               OCI_FETCH_ABSOLUTE, (sb4) 15, OCI_DEFAULT),1);

  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 2, 
                               OCI_FETCH_NEXT, (sb4) 0, OCI_DEFAULT),2);

  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 1, 
                               OCI_FETCH_PRIOR, (sb4) 0, OCI_DEFAULT),1);

  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 3, 
                               OCI_FETCH_RELATIVE, (sb4) -8, OCI_DEFAULT),3);
  
  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 2, 
                               OCI_FETCH_NEXT, (sb4) 0, OCI_DEFAULT),2);
  
  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 1, 
                               OCI_FETCH_FIRST, (sb4) 0, OCI_DEFAULT),1);

  /* cancel the statement handle - and free resources on client and server */
  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 0, 
                               OCI_FETCH_NEXT, (sb4) 0, OCI_DEFAULT),0);

  /* this should result in some error */
  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 1, 
                                OCI_FETCH_NEXT, (sb4) 0, OCI_DEFAULT),1); 

  /* re-execute in the non-scrollable mode */
  checkprint(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4) 3, (ub4)
                                 0,  (CONST OCISnapshot *)
                                 NULL, (OCISnapshot *) NULL,
                                 0),3);  

  /* this should result in error */
  checkprint(errhp, OCIStmtFetch2(stmthp, errhp, (ub4) 1, 
                                OCI_FETCH_ABSOLUTE, (sb4) 4, OCI_DEFAULT),1); 
  cleanup() ;   

  return 1;
}


/*check fetch status and print rows upon success*/
void checkprint(errhp, status,nrows)
OCIError *errhp;
sword status;
ub4 nrows;
{
  checkerr(errhp,status);
  if (status != OCI_ERROR && status != OCI_NO_DATA)
    myprint(nrows);
}


/*check status and print error information */
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


/*
 *  Exit program with an exit code.
 */
void cleanup()
{
  if (envhp)
    (void) OCIHandleFree((dvoid *) envhp, OCI_HTYPE_ENV);
  return;
}


/*void myfflush()
{
  eb1 buf[50];

  fgets((char *) buf, 50, stdin);
}*/


/*print rows*/
void myprint (ub4 nrows)
{
  int i, j, num, cp, rc, amount ;
  sb4 colsz;
  void * elem ;  
  ub4 sz = sizeof(cp) ;
  boolean exist = FALSE;  
  
  checkerr(errhp, OCIAttrGet((CONST void *) stmthp, OCI_HTYPE_STMT, 
                             (void *) & cp, (ub4 *)  & sz, 
                             OCI_ATTR_CURRENT_POSITION, errhp));
  checkerr(errhp, OCIAttrGet((CONST void *) stmthp, OCI_HTYPE_STMT, 
                             (void *) & rc, (ub4 *)  & sz, 
                             OCI_ATTR_ROW_COUNT, errhp));
  printf("******** Current position, Row Count = %d, %d ******** \n", cp, rc);  

  for (i =0 ; i < nrows ; i++ ) 
  {      
    OCINumberToInt(errhp, & empaddr[i]->zip, sizeof(num), 
                   OCI_NUMBER_SIGNED, (void *) & num);
    printf("\n %d %s %s %d ", empno[i], empname[i], 
            OCIStringPtr(envhp, empaddr[i]->state), num);  

    colsz = OCICollMax (envhp, (OCIColl *)  evarray[i]) ;
    for (j = 0; j < colsz ; j++) 
      {        
        OCICollGetElem (envhp, errhp, (OCIColl *) evarray[i], j, & exist, 
                        (void **) & elem, (void **) 0);
        if (!exist) 
          printf(" *** error - coll, row %d col-elem %d ", i, j);        
        else {          
          checkerr(errhp, OCINumberToInt(errhp, (OCINumber *) elem, 
                            sizeof(int), OCI_NUMBER_SIGNED, & num));
          printf("%d ", num);        
        }        
      }
      printf ("\n"); 
  }    
} 

/* end of file cdemosc.c */

