/* Copyright (c) 2001, 2005, Oracle. All rights reserved.  */

/*

   NAME
     cdemol2l.c - OCI demo program for accessing LOBs using LONG API.

   DESCRIPTION
     An example program which creates two tables 
     PERSON_1 (ssn NUMBER, resume LONG), 
     PERSON_2 (ssn NUMBER, photo LONG RAW).
     A series of operations using LONG API: 
     simple insert/select, piecewise insert/select with polling, 
     piecewise insert/select with callback, array insert/select
     are performed on the two tables respectively.
     Then the tables are altered to 
     PERSON_1 (ssn NUMBER, resume CLOB), 
     PERSON_2 (ssn NUMBER, photo BLOB).
     The same series of operations are performed on altered tables
     and the same results are obtained.


   EXPORT FUNCTION(S)
     <external functions defined for use outside package - one-line descriptions>

   INTERNAL FUNCTION(S)
     <other external functions defined - one-line descriptions>

   STATIC FUNCTION(S)
     <static functions defined - one-line descriptions>

   NOTES
     <other useful comments, qualifications, etc.>

   MODIFIED   (MM/DD/YY)
   aliu        02/01/05 - add order by in array select stmt
   jchai       07/27/04 - add order by in select stmt 
   ani         04/30/01 - Merged ani_ocidemo
   ani         04/24/01 - Creation

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <oci.h>

#define DATA_SIZE 5000
#define PIECE_SIZE 1000
#define MAXCOLS 2
#define MAXROWS 10
#define NPIECE DATA_SIZE/PIECE_SIZE
#define MAX_IN_ROWS 10

typedef struct cdemol2lctx
{
  OCIEnv *envhp;
  OCIServer *srvhp;
  OCISvcCtx *svchp;
  OCIError *errhp;
  OCISession *authp;
  OCIStmt *stmthp;
} cdemol2lctx;


/*---------------------------------------------------------------------------
                     PRIVATE TYPES AND CONSTANTS
  ---------------------------------------------------------------------------*/
static text *username = (text *) "scott";
static text *password = (text *) "tiger";

/*global variable for PIECEWISE operations with CALLBACK*/
static text nextpiece[DATA_SIZE];
static ub1 nextpiece2[DATA_SIZE];
static ub4  len = DATA_SIZE;
static sb2  ind = 0;
static ub2  rc = 0;
static boolean glGetInd = 0;         /* global var to control return of ind */
static boolean glGetRc = 0;           /* global var to control return of rc */
static int column = 1;
static char in1[DATA_SIZE];
static ub1 in2[DATA_SIZE];


/*---------------------------------------------------------------------------
                     STATIC FUNCTION DECLARATIONS
  ---------------------------------------------------------------------------*/
static void initialize(cdemol2lctx *ctxptr);
static void cleanup(cdemol2lctx *ctxptr);
static void checkprint(cdemol2lctx *ctxptr, sword status, ub4 nrows);
static void checkerr(/*_ OCIError *errhp, sword status _*/);
static void sql_stmt_execute(/*_ cdemol2lctx *ctxptx, text *sql_stmt _*/);
static void sql_exec_insert(/*_ cdemol2lctx *ctxptx _*/);
static void insert_piecewise_polling(/*_ cdemol2lctx *ctxptx _*/);
static void insert_piecewise_callback(/*_ cdemol2lctx *ctxptx _*/);
static sb4 cbf_in_data(/*_ dvoid *ctxp, OCIBind *bindp, ub4 iter, 
                            ub4 index, dvoid **bufpp, ub4 *alenpp, 
                            ub1 *piecep, dvoid **indpp _*/);
static void set_piece(/*_ ub1 *piece  _*/);
static void sql_array_insert(/*_ cdemol2lctx *ctxptx _*/);
static void sql_exec_select(/*_ cdemol2lctx *ctxptx _*/);
static void select_piecewise_polling(/*_ cdemol2lctx *ctxptx _*/);
static void select_piecewise_callback(/*_ cdemol2lctx *ctxptx _*/);
static void sql_array_select(/*_ cdemol2lctx *ctxptx _*/);
static sb4 cdf_fetch_buffer(/*_ dvoid *ctxp, OCIDefine *defnp, ub4 iter,
                                 dvoid **bufpp, ub4 **alen, ub1 *piecep,
                                 dvoid **indp, ub2 **rcpp _*/);
int main(/*_ int argc, char *argv[] _*/);


int main(argc, argv)
int argc;
char *argv[];
{
  cdemol2lctx ctx;
  text *cretab_stmt1 = (text *)"CREATE TABLE PERSON_1 (SSN NUMBER, \
                                                 RESUME LONG)";
  text *cretab_stmt2 = (text *)"CREATE TABLE PERSON_2 (SSN NUMBER, \
                                                 PHOTO LONG RAW)";
  text *alter_stmt1 = (text *)"ALTER TABLE person_1 modify resume CLOB";
  text *alter_stmt2 = (text *)"ALTER TABLE person_2 modify photo BLOB";
  text *drop_stmt1 = (text *)"DROP TABLE PERSON_1";
  text *drop_stmt2 = (text *)"DROP TABLE PERSON_2";

  printf("\n ######## start DEMO program ############ \n");

  initialize(&ctx);

  /* execute sql statement */
  printf("\nCREATING TABLE PERSON_1 ... \n");
  sql_stmt_execute(&ctx, cretab_stmt1);
  printf("\nCREATING TABLE PERSON_2 ... \n");
  sql_stmt_execute(&ctx, cretab_stmt2);

  /*perform a series of INSERT/SELECT operations */
  sql_exec_insert(&ctx); /*simple insert using LONG api*/
  sql_exec_select(&ctx); /*simple select using LONG api*/

  insert_piecewise_polling(&ctx);
  select_piecewise_polling(&ctx);

  insert_piecewise_callback(&ctx);
  select_piecewise_callback(&ctx);
 
  sql_array_insert(&ctx);
  sql_array_select(&ctx);

  printf("\nALTERING TABLE PERSON_1 ... \n");
  sql_stmt_execute(&ctx, alter_stmt1);
  printf("\nALTERING TABLE PERSON_2 ... \n");
  sql_stmt_execute(&ctx, alter_stmt2);

  /*perform the same series of operations again*/
  sql_exec_insert(&ctx); 
  sql_exec_select(&ctx); 

  insert_piecewise_polling(&ctx);
  select_piecewise_polling(&ctx);

  insert_piecewise_callback(&ctx);
  select_piecewise_callback(&ctx);
 
  sql_array_insert(&ctx);
  sql_array_select(&ctx);

  printf("\nDROPPING TABLE PERSON_1 ... \n");
  sql_stmt_execute(&ctx, drop_stmt1);
  printf("\nDROPPING TABLE PERSON_2 ... \n");
  sql_stmt_execute(&ctx, drop_stmt2);

  /* clean things up before exhit */
  cleanup(&ctx);

  return 1;

} /*end main*/


/*execute a single SQL statement */
void sql_stmt_execute(ctxptr, sql_stmt)
cdemol2lctx *ctxptr;
text *sql_stmt;
{ 
  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          sql_stmt, (ub4) strlen((char *)sql_stmt), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp,
                          ctxptr->stmthp,
                          ctxptr->errhp, (ub4) 1, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          OCI_DEFAULT)); 
} /* end of sql_stmt_execute() */


/*perform simple insert using LONG API*/
void sql_exec_insert(ctxptr)
cdemol2lctx *ctxptr;
{ 
  text *ins_stmt1 = (text *)"INSERT INTO PERSON_1 VALUES (:1, :2)";
  text *ins_stmt2 = (text *)"INSERT INTO PERSON_2 VALUES (:1, :2)";
  OCIBind *bndp1 = (OCIBind *) NULL;
  OCIBind *bndp2 = (OCIBind *) NULL;

  sword  ssn= 1, i;
  char col2[DATA_SIZE];
  ub1 col3[DATA_SIZE];
  ub2 col2len=DATA_SIZE, col3len=DATA_SIZE;

  for(i=0;i<DATA_SIZE;i++) {
    col2[i] = 'A';
    col3[i] = 'B';
  }
    
  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          ins_stmt1, (ub4) strlen((char *)ins_stmt1), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("\nBEGINING SIMPLE INSERT INTO PERSON_1... \n");

  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp1, 
                  ctxptr->errhp, (ub4) 1,
                  (dvoid *) &ssn, (sb4) sizeof(ssn), SQLT_INT,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp2, 
                  ctxptr->errhp, (ub4) 2,
                  (dvoid *) col2, (sb4) col2len, SQLT_CHR,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp,
                          ctxptr->stmthp,
                          ctxptr->errhp, (ub4) 1, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          OCI_DEFAULT)); 

  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          ins_stmt2, (ub4) strlen((char *)ins_stmt2), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("\nBEGINING SIMPLE INSERT INTO PERSON_2... \n");

  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp1, 
                  ctxptr->errhp, (ub4) 1,
                  (dvoid *) &ssn, (sb4) sizeof(ssn), SQLT_INT,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp2, 
                  ctxptr->errhp, (ub4) 2,
                  (dvoid *) col3, (sb4) col3len, SQLT_BIN,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp,
                          ctxptr->stmthp,
                          ctxptr->errhp, (ub4) 1, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          OCI_DEFAULT)); 

} /* end sql_exe_insert() */


/*perform simple select using LONG API */
void sql_exec_select(ctxptr)
cdemol2lctx *ctxptr;
{ 
  text *sel_stmt1 = (text *)"SELECT * FROM PERSON_1 ORDER BY SSN";
  text *sel_stmt2 = (text *)"SELECT * FROM PERSON_2 ORDER BY SSN";
  OCIDefine *defnp1 = (OCIDefine *) NULL;
  OCIDefine *defnp2 = (OCIDefine *) NULL;

  ub4 i;
  sword  ssn;
  char col2[DATA_SIZE];
  ub1 col3[DATA_SIZE];
  ub2 col2len, col3len;
  boolean bufok;
 
  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          sel_stmt1, (ub4) strlen((char *)sel_stmt1), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("\nBEGINING SIMPLE SELECT OF PERSON_1... \n");

  checkerr(ctxptr->errhp,OCIDefineByPos(ctxptr->stmthp, &defnp1,
                         ctxptr->errhp, (ub4) 1, (dvoid*) &ssn, 
                         (sb4) sizeof(ssn), (ub2)SQLT_INT, (dvoid*) 0, 
                         (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIDefineByPos(ctxptr->stmthp, &defnp2, 
                          ctxptr->errhp, (ub4) 2, (dvoid *) col2, 
                          (sb4) sizeof(col2), (ub2)SQLT_CHR, (dvoid *)0, 
                          (ub2 *)&col2len, (ub2 *)0, (ub4)OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp,
                          ctxptr->stmthp,
                          ctxptr->errhp, (ub4) 1, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          OCI_DEFAULT)); 
                
  printf("ssn = %d\n", ssn);
  printf("col2len = %d\n", col2len);

  bufok = TRUE;
  for (i = 0; i < col2len; i++)
    if (col2[i] != 'A') 
      bufok = FALSE;

  if (bufok)
    printf("SUCCESS: contents of col2 have expected value\n");
  else
    printf("FAILURE: contents of col2 do not have expected value\n");


  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          sel_stmt2, (ub4) strlen((char *)sel_stmt2), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("\nBEGINING SIMPLE SELECT OF PERSON_2... \n");

  checkerr(ctxptr->errhp,OCIDefineByPos(ctxptr->stmthp, &defnp1,
                         ctxptr->errhp, (ub4) 1, (dvoid*) &ssn, 
                         (sb4) sizeof(ssn), (ub2)SQLT_INT, (dvoid*) 0, 
                         (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIDefineByPos(ctxptr->stmthp, &defnp2, 
                          ctxptr->errhp, (ub4) 2, (dvoid *) col3, 
                          (sb4) sizeof(col3), (ub2)SQLT_BIN, (dvoid *)0, 
                          (ub2 *)&col3len, (ub2 *)0, (ub4)OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp,
                          ctxptr->stmthp,
                          ctxptr->errhp, (ub4) 1, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          OCI_DEFAULT)); 
                
  printf("ssn = %d\n", ssn);
  printf("col3len = %d\n", col3len);

  bufok = TRUE;
  for (i = 0; i < col3len; i++)
    if (col3[i] != 'B') 
      bufok = FALSE;

  if (bufok)
    printf("SUCCESS: contents of col3 have expected value\n");
  else
    printf("FAILURE: contents of col3 do not have expected value\n");
  
} /* end sql_exe_select() */


/*perform piecewise insert with polling*/
void insert_piecewise_polling(ctxptr)
cdemol2lctx *ctxptr;
{ 
  text *ins_stmt1 = (text *)"INSERT INTO PERSON_1 VALUES (:1, :2)";
  text *ins_stmt2 = (text *)"INSERT INTO PERSON_2 VALUES (:1, :2)";
  OCIBind *bndp1 = (OCIBind *) NULL;
  OCIBind *bndp2 = (OCIBind *) NULL;
  sword status, ssn= 2, i;
  char col2[PIECE_SIZE];
  ub1 col3[PIECE_SIZE];
  ub1    piece;
  ub4    alenp2 = PIECE_SIZE;
  ub2    rcode2;

  for(i=0;i<PIECE_SIZE;i++) {
    col2[i] = 'A';
    col3[i] = 'B';
  }

  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          ins_stmt1, (ub4) strlen((char *)ins_stmt1), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("\nBEGINING PIECEWISE INSERT INTO PERSON_1 WITH POLLING ... \n");

  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp1, 
                  ctxptr->errhp, (ub4) 1,
                  (dvoid *) &ssn, (sb4) sizeof(ssn), SQLT_INT,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp2, 
                  ctxptr->errhp, (ub4) 2,
                  (dvoid *) col2, (sb4) DATA_SIZE, SQLT_CHR,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DATA_AT_EXEC));

  while (1)
  {
    status = OCIStmtExecute(ctxptr->svchp, ctxptr->stmthp, ctxptr->errhp, 
                            (ub4) 1, (ub4) 0, (CONST OCISnapshot*) 0, 
                            (OCISnapshot*) 0, (ub4) OCI_DEFAULT);                  
    switch(status)
    {
      case OCI_NEED_DATA:    
        set_piece(&piece);                   
        if (OCIStmtSetPieceInfo((dvoid *)bndp2,   
                      (ub4)OCI_HTYPE_BIND,ctxptr->errhp, (dvoid *)col2,
                      &alenp2, piece, (dvoid *) 0, &rcode2))
        {
          printf("ERROR: OCIStmtSetPieceInfo returned %d \n", status);
          break;
        }
        status = OCI_NEED_DATA;
        break;
      case OCI_SUCCESS:
        break;
      default:
        printf( "oci exec returned %d \n", status);
        checkerr(ctxptr->errhp, status);
        status = 0;
    }
    if (!status) break;
  }

  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          ins_stmt2, (ub4) strlen((char *)ins_stmt2), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("\nBEGINING PIECEWISE INSERT INTO PERSON_2 WITH POLLING ... \n");

  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp1, 
                  ctxptr->errhp, (ub4) 1,
                  (dvoid *) &ssn, (sb4) sizeof(ssn), SQLT_INT,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp2, 
                  ctxptr->errhp, (ub4) 2,
                  (dvoid *) col3, (sb4) DATA_SIZE, SQLT_BIN,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DATA_AT_EXEC));

  while (1)
  {
    status = OCIStmtExecute(ctxptr->svchp, ctxptr->stmthp, ctxptr->errhp, 
                            (ub4) 1, (ub4) 0, (CONST OCISnapshot*) 0, 
                            (OCISnapshot*) 0, (ub4) OCI_DEFAULT);                  
    switch(status)
    {
      case OCI_NEED_DATA:    
        set_piece(&piece);                   
        if (OCIStmtSetPieceInfo((dvoid *)bndp2,   
                      (ub4)OCI_HTYPE_BIND,ctxptr->errhp, (dvoid *)col3,
                      &alenp2, piece, (dvoid *) 0, &rcode2))
        {
          printf("ERROR: OCIStmtSetPieceInfo returned %d \n", status);
          break;
        }
        status = OCI_NEED_DATA;
        break;
      case OCI_SUCCESS:
        break;
      default:
        printf( "oci exec returned %d \n", status);
        checkerr(ctxptr->errhp, status);
        status = 0;
    }
    if (!status) break;
  }

} /* end insert_piecewise_polling() */


/*set piece information for piecewise insert with polling*/
void set_piece(piecep)
ub1  *piecep;
{
  static sword piece_cnt = 0;

  switch (piece_cnt)
  {
    case 0:
      *piecep = OCI_FIRST_PIECE;
      break;
    case NPIECE - 1:
      *piecep = OCI_LAST_PIECE;
      piece_cnt = 0;
      return;
    default:
      *piecep = OCI_NEXT_PIECE;
  }
  piece_cnt++;
  return;
}


/*perform piecewise select with polling*/
void select_piecewise_polling(ctxptr)
cdemol2lctx *ctxptr;
{ 
  text *sel_stmt1 = (text *)"SELECT * FROM PERSON_1 where ssn=2";
  text *sel_stmt2 = (text *)"SELECT * FROM PERSON_2 where ssn=2";
  OCIDefine *defnp1 = (OCIDefine *) NULL;
  OCIDefine *defnp2 = (OCIDefine *) NULL;
  ub4 i;
  sword status, ssn;
  boolean bufok = TRUE;
  char buf1[PIECE_SIZE];
  ub1 buf2[PIECE_SIZE];
  ub4   alen  = PIECE_SIZE;
  ub1   piece = OCI_FIRST_PIECE;
  dvoid *hdlptr = (dvoid *) 0;
  ub4 hdltype = OCI_HTYPE_DEFINE, iter = 0, idx = 0;
  ub1   in_out = 0;
  sb2   indptr = 0;
  ub2   rcode = 0;
 
  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          sel_stmt1, (ub4) strlen((char *)sel_stmt1), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("\nBEGINING SELECT PIECEWISE WITH POLLING OF PERSON_1 ... \n");

  checkerr(ctxptr->errhp,OCIDefineByPos(ctxptr->stmthp, &defnp1,
                         ctxptr->errhp, (ub4) 1, (dvoid*) &ssn, 
                         (sb4) sizeof(ssn), (ub2)SQLT_INT, (dvoid*) 0, 
                         (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIDefineByPos(ctxptr->stmthp, &defnp2, 
                          ctxptr->errhp, (ub4) 2, (dvoid *) 0, 
                          (sb4) DATA_SIZE, (ub2)SQLT_CHR, (dvoid *)0, 
                          (ub2 *) 0, (ub2 *)0, (ub4)OCI_DYNAMIC_FETCH));

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp,
                          ctxptr->stmthp,
                          ctxptr->errhp, (ub4) 0, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          OCI_DEFAULT)); 

  status = OCIStmtFetch(ctxptr->stmthp, ctxptr->errhp,
                          (ub4) 1, (ub2) OCI_FETCH_NEXT, (ub4) OCI_DEFAULT);
  checkerr(ctxptr->errhp, status);

  printf("ssn = %d\n", ssn);
  printf("checking contents of RESUME piece by piece\n");
  while (status == OCI_NEED_DATA)
  {
    checkerr(ctxptr->errhp, OCIStmtGetPieceInfo(ctxptr->stmthp, 
                            ctxptr->errhp, &hdlptr, &hdltype,
                            &in_out, &iter, &idx, &piece));

    checkerr(ctxptr->errhp, OCIStmtSetPieceInfo((dvoid *)hdlptr, (ub4)hdltype,
                                  ctxptr->errhp, (dvoid *) &buf1, &alen, piece,
                                  (dvoid *)&indptr, &rcode));
                                
    status = OCIStmtFetch(ctxptr->stmthp,ctxptr->errhp, (ub4) 1, 
                          (ub2) OCI_FETCH_NEXT, (ub4) OCI_DEFAULT);

    /*verify if fetch results are correct */
    for (i = 0; i < alen; i++)
      if (buf1[i] != 'A')
        bufok = FALSE;

    if (bufok)
      printf("SUCCESS: contents of buf1 have expected value\n");
    else
      printf("FAILURE: contents of buf1 do not have expected value\n");
  }
  if(status == OCI_SUCCESS)
    printf("SUCCESS: fetched all pieces of RESUME CORRECTLY\n");


  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          sel_stmt2, (ub4) strlen((char *)sel_stmt2), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("\nBEGINING SELECT PIECEWISE WITH POLLING OF PERSON_2 ... \n");

  checkerr(ctxptr->errhp,OCIDefineByPos(ctxptr->stmthp, &defnp1,
                         ctxptr->errhp, (ub4) 1, (dvoid*) &ssn, 
                         (sb4) sizeof(ssn), (ub2)SQLT_INT, (dvoid*) 0, 
                         (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIDefineByPos(ctxptr->stmthp, &defnp2, 
                          ctxptr->errhp, (ub4) 2, (dvoid *) 0, 
                          (sb4) DATA_SIZE, (ub2)SQLT_BIN, (dvoid *)0, 
                          (ub2 *) 0, (ub2 *)0, (ub4)OCI_DYNAMIC_FETCH));

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp,
                          ctxptr->stmthp,
                          ctxptr->errhp, (ub4) 0, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          OCI_DEFAULT)); 

  status = OCIStmtFetch(ctxptr->stmthp, ctxptr->errhp,
                          (ub4) 1, (ub2) OCI_FETCH_NEXT, (ub4) OCI_DEFAULT);
  checkerr(ctxptr->errhp, status);

  printf("ssn = %d\n", ssn);
  printf("checking contents of PHOTO piece by piece\n");
  while (status == OCI_NEED_DATA)
  {
    checkerr(ctxptr->errhp, OCIStmtGetPieceInfo(ctxptr->stmthp, 
                            ctxptr->errhp, &hdlptr, &hdltype,
                            &in_out, &iter, &idx, &piece));

    checkerr(ctxptr->errhp, OCIStmtSetPieceInfo((dvoid *)hdlptr, (ub4)hdltype,
                                  ctxptr->errhp, (dvoid *) &buf2, &alen, piece,
                                  (dvoid *)&indptr, &rcode));
                                
    status = OCIStmtFetch(ctxptr->stmthp,ctxptr->errhp, (ub4) 1, 
                          (ub2) OCI_FETCH_NEXT, (ub4) OCI_DEFAULT);

    /*verify if fetch results are correct */
    bufok = TRUE;
    for (i = 0; i < alen; i++)
      if (buf2[i] != 'B')
        bufok = FALSE;

    if (bufok)
      printf("SUCCESS: contents of buf2 have expected value\n");
    else
      printf("FAILURE: contents of buf2 do not have expected value\n");
  }
  if(status == OCI_SUCCESS)
    printf("SUCCESS: fetched all pieces of PHOTO CORRECTLY\n");
} /* end of select_piecewise_polling() */


/*perform piecewise insert with callback*/
void insert_piecewise_callback(ctxptr)
cdemol2lctx *ctxptr;
{ 
  text *ins_stmt1 = (text *)"INSERT INTO PERSON_1 VALUES (:1, :2)";
  text *ins_stmt2 = (text *)"INSERT INTO PERSON_2 VALUES (:1, :2)";
  OCIBind *bndp1 = (OCIBind *) NULL;
  OCIBind *bndp2 = (OCIBind *) NULL;
  sword status, ssn= 3, i;
  ub4 pos[MAXCOLS];

  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          ins_stmt1, (ub4) strlen((char *)ins_stmt1), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("\nBEGINING PIECEWISE INSERT INTO PERSON_1 WITH CALLBACK ... \n");

  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp1, 
                  ctxptr->errhp, (ub4) 1,
                  (dvoid *) &ssn, (sb4) sizeof(ssn), SQLT_INT,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp2, 
                  ctxptr->errhp, (ub4) 2,
                  (dvoid *) 0, (sb4) DATA_SIZE, SQLT_LNG,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DATA_AT_EXEC));

  for (i = 0; i < MAXCOLS; i++)
    pos[i] = i+1;

  checkerr(ctxptr->errhp, OCIBindDynamic(bndp2, ctxptr->errhp,
                   (dvoid *) &pos[0], cbf_in_data,
                   (dvoid *) 0, (OCICallbackOutBind) 0));

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp,
                          ctxptr->stmthp,
                          ctxptr->errhp, (ub4) 1, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          OCI_DEFAULT)); 


  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          ins_stmt2, (ub4) strlen((char *)ins_stmt2), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("\nBEGINING PIECEWISE INSERT INTO PERSON_2 WITH CALLBACK ... \n");

  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp1, 
                  ctxptr->errhp, (ub4) 1,
                  (dvoid *) &ssn, (sb4) sizeof(ssn), SQLT_INT,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp2, 
                  ctxptr->errhp, (ub4) 2,
                  (dvoid *) 0, (sb4) DATA_SIZE, SQLT_BIN,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DATA_AT_EXEC));

  checkerr(ctxptr->errhp, OCIBindDynamic(bndp2, ctxptr->errhp,
                   (dvoid *) &pos[1], cbf_in_data,
                   (dvoid *) 0, (OCICallbackOutBind) 0));

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp,
                          ctxptr->stmthp,
                          ctxptr->errhp, (ub4) 1, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          OCI_DEFAULT)); 
} /* end insert_piecewise_callback() */


/* ----------------------------------------------------------------- */
/* Inbind callback to specify input data.                            */
/* ----------------------------------------------------------------- */
static sb4 cbf_in_data(ctxp, bindp, iter, index, bufpp, alenpp, piecep, indpp)
dvoid *ctxp;
OCIBind *bindp;
ub4 iter;
ub4 index;
dvoid **bufpp;
ub4 *alenpp;
ub1 *piecep;
dvoid **indpp;
{
  sword j;
  ub4  inpos = *((ub4 *)ctxp);

  switch(inpos)
  {
  case 1:
    memset((void *)in1, (int) 'A', (size_t) DATA_SIZE);
    *bufpp = (dvoid *) in1;
    *alenpp = sizeof(in1);
    break;
  case 2:
    memset((void *)in2, (int) 'B', (size_t) DATA_SIZE);
    *bufpp = (dvoid *) in2;
    *alenpp = sizeof(in2);
    break;
  default:
    *bufpp =  (dvoid *) 0;
    *alenpp =  0;
    printf("ERROR: invalid position number: %d\n", inpos);
  }

  *indpp = (dvoid *) 0;
  *piecep = OCI_ONE_PIECE;

  return OCI_CONTINUE;
}


/*perform piecewise select with callback*/
void select_piecewise_callback(ctxptr)
cdemol2lctx *ctxptr;
{ 
  text *sel_stmt1 = (text *)"SELECT * FROM PERSON_1 where ssn=3";
  text *sel_stmt2 = (text *)"SELECT * FROM PERSON_2 where ssn=3";
  OCIDefine *defnp1 = (OCIDefine *) NULL;
  OCIDefine *defnp2 = (OCIDefine *) NULL;
  sword status, ssn;
  char buf1[DATA_SIZE];
  ub1  buf2[DATA_SIZE];
  char *buf1p = buf1;
  ub1 *buf2p = buf2;
  boolean  bufok = TRUE;
  int  i;

  column = 1;
  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          sel_stmt1, (ub4) strlen((char *)sel_stmt1), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("\nBEGINING SELECT PIECEWISE WITH CALLBACK OF PERSON_1... \n");

  checkerr(ctxptr->errhp,OCIDefineByPos(ctxptr->stmthp, &defnp1,
                         ctxptr->errhp, (ub4) 1, (dvoid*) &ssn, 
                         (sb4) sizeof(ssn), (ub2)SQLT_INT, (dvoid*) 0, 
                         (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIDefineByPos(ctxptr->stmthp, &defnp2, 
                          ctxptr->errhp, (ub4) 2, (dvoid *) 0, 
                          (sb4) DATA_SIZE, (ub2)SQLT_CHR, (dvoid *)0, 
                          (ub2 *) 0, (ub2 *)0, (ub4)OCI_DYNAMIC_FETCH));

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp,
                          ctxptr->stmthp,
                          ctxptr->errhp, (ub4) 0, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          OCI_DEFAULT)); 

  checkerr(ctxptr->errhp, OCIDefineDynamic(defnp2, ctxptr->errhp, 
                          (dvoid *) &buf1p, (OCICallbackDefine) cdf_fetch_buffer));

  status = OCIStmtFetch(ctxptr->stmthp, ctxptr->errhp, (ub4) 1, 
                       (ub2) OCI_FETCH_NEXT, (ub4) OCI_DEFAULT);

  checkerr(ctxptr->errhp, status);

  /* check the contents of the last piece */
  for(i=0;i<PIECE_SIZE;i++)
    if( (char) nextpiece[i] != 'A')
      bufok = FALSE;
      
  if(bufok)
    printf("SUCCESS: Contents of last piece of RESUME are valid\n");
  else
    printf("FAILURE: Contents of last piece of RESUME are not valid\n");
  printf("ssn = %d\n", ssn);

  column = 2;
  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          sel_stmt2, (ub4) strlen((char *)sel_stmt2), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("\nBEGINING SELECT PIECEWISE WITH CALLBACK OF PERSON_2... \n");

  checkerr(ctxptr->errhp,OCIDefineByPos(ctxptr->stmthp, &defnp1,
                         ctxptr->errhp, (ub4) 1, (dvoid*) &ssn, 
                         (sb4) sizeof(ssn), (ub2)SQLT_INT, (dvoid*) 0, 
                         (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIDefineByPos(ctxptr->stmthp, &defnp2, 
                          ctxptr->errhp, (ub4) 2, (dvoid *) 0, 
                          (sb4) DATA_SIZE, (ub2)SQLT_BIN, (dvoid *)0, 
                          (ub2 *) 0, (ub2 *)0, (ub4)OCI_DYNAMIC_FETCH));

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp,
                          ctxptr->stmthp,
                          ctxptr->errhp, (ub4) 0, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          OCI_DEFAULT)); 

  checkerr(ctxptr->errhp, OCIDefineDynamic(defnp2, ctxptr->errhp, 
                          (dvoid *) &buf2p, (OCICallbackDefine) cdf_fetch_buffer));

  status = OCIStmtFetch(ctxptr->stmthp, ctxptr->errhp, (ub4) 1, 
                       (ub2) OCI_FETCH_NEXT, (ub4) OCI_DEFAULT);

  checkerr(ctxptr->errhp, status);

  /* check the contents of the last piece */
  bufok = TRUE;
  for(i=0;i<PIECE_SIZE;i++)
    if( (char) nextpiece2[i] != 'B')
      bufok = FALSE;
      
  if(bufok)
    printf("SUCCESS: Contents of last piece of PHOTO are valid\n");
  else
    printf("FAILURE: Contents of last piece of PHOTO are not valid\n");
  printf("ssn = %d\n", ssn);
} /* end select_piecewise_callback() */


/* ----------------------------------------------------------------- */
/* Callback to specify output data.                            */
/* ----------------------------------------------------------------- */
sb4 cdf_fetch_buffer(ctx, defnp, iter, bufpp, alenpp, piecep, indpp, rcpp)
dvoid *ctx;
OCIDefine *defnp;
ub4 iter;
dvoid **bufpp;
ub4 **alenpp;
ub1 *piecep;  
dvoid **indpp;
ub2 **rcpp;
{
  int i;
  boolean bufok = TRUE;

  if(column==1)
    *bufpp = (dvoid *) nextpiece;
  else if(column==2)
    *bufpp = (dvoid *) nextpiece2;

  /*verify the contents of fetched pieces*/
  if(piecep[0]==(ub1)OCI_FIRST_PIECE)
    ;
  else { 
    if(column ==1) {
      for(i=0;i<len;i++)
        if(( (char) *((ub1 *)*bufpp + i*sizeof(char))) != 'A')
          bufok = FALSE;
    }
    else if(column == 2) {
      for(i=0;i<len;i++)
        if(( (char) *((ub1 *)*bufpp + i)) != 'B')
          bufok = FALSE;
    }

    if(bufok)
      printf("SUCCESS: Contents are valid\n");
    else
      printf("FAILURE: Contents are not valid\n");
  }
  
  if ((*piecep) == (ub1)OCI_ONE_PIECE)
    *piecep = OCI_FIRST_PIECE;
  /*end verify*/

  printf("Getting OCI_%s_PIECE .... \n",
                 (piecep[0]==(ub1)OCI_ONE_PIECE) ? "ONE" :
                 (piecep[0]==(ub1)OCI_FIRST_PIECE) ? "FIRST" :
                 (piecep[0]==(ub1)OCI_NEXT_PIECE) ? "NEXT" :
                 (piecep[0]==(ub1)OCI_LAST_PIECE) ? "LAST" : "???");

  len = PIECE_SIZE;
  *alenpp = &len;
    
  ind = 0;
  if (glGetInd)
    *indpp  = (dvoid *) &ind;
  else
    *indpp = (dvoid *)0;

  rc = 0;
  if (glGetRc)
    *rcpp  = (ub2 *) &rc;
  else
    *rcpp = (ub2 *)0;
  
  return OCI_CONTINUE;
} /* end cdf_fetch_buffer() */


/*perform array insert*/
void sql_array_insert(ctxptr)
cdemol2lctx *ctxptr;
{ 
  text *ins_stmt1 = (text *)"INSERT INTO PERSON_1 VALUES (:1, :2)";
  text *ins_stmt2 = (text *)"INSERT INTO PERSON_2 VALUES (:1, :2)";
  OCIBind *bndp1 = (OCIBind *) NULL;
  OCIBind *bndp2 = (OCIBind *) NULL;

  sword  i, j;
  typedef struct {
    sword ssn;
    char buf1[DATA_SIZE];
  } person_1;

  typedef struct {
    sword ssn;
    ub1  buf2[DATA_SIZE]; 
  } person_2;

  person_1 data1[MAX_IN_ROWS];
  person_2 data2[MAX_IN_ROWS];
  ub4 valsk1 = (ub4) sizeof(person_1); /* value skip */
  ub4 valsk2 = (ub4) sizeof(person_2); /* value skip */
  ub4 indsk = 0; /*  indicator skips */
  ub4 rlsk = 0; /*  return length skips */
  ub4 rcsk = 0; /*  return code skips */

  for (i=0; i<MAX_IN_ROWS; i++)
  {
    data1[i].ssn = data2[i].ssn = 4+i;
    for(j=0; j< DATA_SIZE; j++)
    {
      data1[i].buf1[j] = 'A';
      data2[i].buf2[j] = 'B';
    }
  }

  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          ins_stmt1, (ub4) strlen((char *)ins_stmt1), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("\nBEGINING ARRAY INSERT INTO PERSON_1... \n");
  
  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp1, 
                  ctxptr->errhp, (ub4) 1, (dvoid *) &data1[0].ssn, 
                  (sb4) sizeof(data1[0].ssn), SQLT_INT,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp2, 
                  ctxptr->errhp, (ub4) 2, (dvoid *) data1[0].buf1, 
                  (sb4) sizeof(data1[0].buf1), SQLT_CHR,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT));

  if (OCIBindArrayOfStruct(bndp1,ctxptr->errhp,valsk1, indsk, rlsk, rcsk)
   || OCIBindArrayOfStruct(bndp2,ctxptr->errhp,valsk1, indsk, rlsk, rcsk))
  {
    printf("FAILED: OCIBindeArrayOfStruct()\n");
    return;
  }    

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp, ctxptr->stmthp, 
                          ctxptr->errhp, (ub4) MAX_IN_ROWS, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          OCI_DEFAULT)); 

  printf("\nBEGINING ARRAY INSERT INTO PERSON_2... \n");

  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          ins_stmt2, (ub4) strlen((char *)ins_stmt2), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));
  
  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp1, 
                  ctxptr->errhp, (ub4) 1, (dvoid *) &data2[0].ssn, 
                  (sb4) sizeof(data2[0].ssn), SQLT_INT,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIBindByPos(ctxptr->stmthp, &bndp2, 
                  ctxptr->errhp, (ub4) 2, (dvoid *) data2[0].buf2, 
                  (sb4) sizeof(data2[0].buf2), SQLT_BIN,
                  (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                  (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT));

  if (OCIBindArrayOfStruct(bndp1,ctxptr->errhp,valsk2, indsk, rlsk, rcsk)
   || OCIBindArrayOfStruct(bndp2,ctxptr->errhp,valsk2, indsk, rlsk, rcsk))
  {
    printf("FAILED: OCIBindeArrayOfStruct()\n");
    return;
  }    

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp, ctxptr->stmthp, 
                          ctxptr->errhp, (ub4) MAX_IN_ROWS, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          OCI_DEFAULT)); 
} /* end sql_array_insert() */


/*perform array select*/
void sql_array_select(ctxptr)
cdemol2lctx *ctxptr;
{ 
  text *sel_stmt1 = (text *)"SELECT * FROM PERSON_1 where ssn>3 ORDER BY SSN";
  text *sel_stmt2 = (text *)"SELECT * FROM PERSON_2 where ssn>3 ORDER BY SSN";
  OCIDefine *defnp1 = (OCIDefine *) NULL;
  OCIDefine *defnp2 = (OCIDefine *) NULL;
  ub2 buflen1, buflen2;
  boolean buf1ok = TRUE, buf2ok = TRUE;
  ub4 i,j;

  typedef struct {
    sword ssn;
    char buf1[DATA_SIZE];
  } person_1;

  typedef struct {
    sword ssn;
    ub1  buf2[DATA_SIZE]; 
  } person_2;

  person_1 data1[MAX_IN_ROWS];
  person_2 data2[MAX_IN_ROWS];
  ub4 valsk1 = (ub4) sizeof(person_1); /* value skip */
  ub4 valsk2 = (ub4) sizeof(person_2); /* value skip */
  ub4 indsk = 0; /*  indicator skips */
  ub4 rlsk = 0; /*  return length skips */
  ub4 rcsk = 0; /*  return code skips */

  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          sel_stmt1, (ub4) strlen((char *)sel_stmt1), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("\nBEGINING ARRAY SELECT OF PERSON_1... \n");

  checkerr(ctxptr->errhp,OCIDefineByPos(ctxptr->stmthp, &defnp1,
                         ctxptr->errhp, (ub4) 1, (dvoid*) &data1[0].ssn, 
                         (sb4) sizeof(data1[0].ssn), (ub2)SQLT_INT, (dvoid*) 0, 
                         (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIDefineByPos(ctxptr->stmthp, &defnp2, 
                          ctxptr->errhp, (ub4) 2, (dvoid *) data1[0].buf1, 
                          (sb4) sizeof(data1[0].buf1), (ub2)SQLT_CHR, (dvoid *)0, 
                          (ub2 *) &buflen1, (ub2 *)0, (ub4)OCI_DEFAULT));

  if (OCIDefineArrayOfStruct(defnp1,ctxptr->errhp,valsk1, indsk, rlsk, rcsk)
   || OCIDefineArrayOfStruct(defnp2,ctxptr->errhp,valsk1, indsk, rlsk, rcsk))
  {
    printf("FAILED: OCIDefineArrayOfStruct()\n");
    return;
  }    

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp, ctxptr->stmthp, 
                          ctxptr->errhp, (ub4) MAXROWS, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          OCI_DEFAULT)); 


  checkerr(ctxptr->errhp, OCIStmtPrepare(ctxptr->stmthp, ctxptr->errhp, 
                          sel_stmt2, (ub4) strlen((char *)sel_stmt2), 
                          (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  printf("\nBEGINING ARRAY SELECT OF PERSON_2... \n");

  checkerr(ctxptr->errhp,OCIDefineByPos(ctxptr->stmthp, &defnp1,
                         ctxptr->errhp, (ub4) 1, (dvoid*) &data2[0].ssn, 
                         (sb4) sizeof(data2[0].ssn), (ub2)SQLT_INT, (dvoid*) 0, 
                         (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(ctxptr->errhp, OCIDefineByPos(ctxptr->stmthp, &defnp2, 
                          ctxptr->errhp, (ub4) 2, (dvoid *) data2[0].buf2, 
                          (sb4) sizeof(data2[0].buf2), (ub2)SQLT_BIN, (dvoid *)0, 
                          (ub2 *) &buflen2, (ub2 *)0, (ub4)OCI_DEFAULT));

  if (OCIDefineArrayOfStruct(defnp1,ctxptr->errhp,valsk2, indsk, rlsk, rcsk)
   || OCIDefineArrayOfStruct(defnp2,ctxptr->errhp,valsk2, indsk, rlsk, rcsk))
  {
    printf("FAILED: OCIDefineArrayOfStruct()\n");
    return;
  }    

  checkerr(ctxptr->errhp, OCIStmtExecute(ctxptr->svchp, ctxptr->stmthp, 
                          ctxptr->errhp, (ub4) MAXROWS, (ub4)0,
                          (OCISnapshot *) NULL, (OCISnapshot *) NULL,
                          OCI_DEFAULT)); 

  /*check output */
  for(i=0;i<MAXROWS;i++) {
    buf1ok = buf2ok = TRUE;
    printf("ssn in person_1 = %d\n", data1[i].ssn);
    printf("ssn in person_2 = %d\n", data2[i].ssn);
    for(j=0;j<DATA_SIZE;j++)
    {
      if(data1[i].buf1[j] != 'A')
        buf1ok = FALSE;
      if((char) data2[i].buf2[j] != 'B')
        buf2ok = FALSE;
    }
    if(buf1ok)
      printf("SUCCESS: contents of RESUME have expected value\n");
    else
      printf("FAILURE: contents of RESUME have unexpected value\n");
    if(buf2ok)
      printf("SUCCESS: contents of PHOTO have expected value\n");
    else
      printf("FAILURE: contents of PHOTO have unexpected value\n");
  }
} /* end sql_array_select() */


/*initialize envionment and handler*/
void initialize(ctxptr)
cdemol2lctx *ctxptr;
{

  if (OCIEnvCreate((OCIEnv **) &ctxptr->envhp,
                   (ub4)OCI_THREADED|OCI_OBJECT, (dvoid *)0,
                    (dvoid * (*)(dvoid *, size_t)) 0,
                   (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                   (void (*)(dvoid *, dvoid *)) 0,
                   (size_t) 0, (dvoid **) 0 ))
    printf("FAILED: OCIEnvCreate()\n");


  printf("\n ######## Connect to server ############# \n");

  if (OCIHandleAlloc((dvoid *) ctxptr->envhp,
                     (dvoid **) &ctxptr->errhp,
                     (ub4) OCI_HTYPE_ERROR, (size_t) 0, (dvoid **) 0))
    printf("FAILED: OCIHandleAlloc() on ctxptr->errhp\n");

  if (OCIHandleAlloc((dvoid *) ctxptr->envhp,
                     (dvoid **) &ctxptr->srvhp,
                     (ub4) OCI_HTYPE_SERVER, (size_t) 0, (dvoid **) 0))
    printf("FAILED: OCIHandleAlloc() on ctxptr->srvhp\n");

  if (OCIHandleAlloc((dvoid *) ctxptr->envhp,
                     (dvoid **) &ctxptr->svchp,
                     (ub4) OCI_HTYPE_SVCCTX, (size_t) 0, (dvoid **) 0))
    printf("FAILED: OCIHandleAlloc() on ctxptr->svchp\n");

  if (OCIHandleAlloc((dvoid *) ctxptr->envhp,
                     (dvoid **) &ctxptr->authp,
                     (ub4) OCI_HTYPE_SESSION, (size_t) 0, (dvoid **) 0))
    printf("FAILED: OCIHandleAlloc() on ctxptr->authp\n");

  if (OCIServerAttach(ctxptr->srvhp, ctxptr->errhp,
                      (text *) "", (sb4) strlen((char *) ""),
                      (ub4) OCI_DEFAULT))
    printf("FAILED: OCIServerAttach()\n");

  if (OCIAttrSet((dvoid *) ctxptr->svchp, (ub4) OCI_HTYPE_SVCCTX,
                 (dvoid *) ctxptr->srvhp, (ub4) 0,
                 (ub4) OCI_ATTR_SERVER, ctxptr->errhp))
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
                        ctxptr->errhp, ctxptr->authp,
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


/*check status and print error information*/
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
} /* end checkerr() */


/*clean up envionment*/
void cleanup(ctxptr)
cdemol2lctx *ctxptr;
{
  printf("\n ########## clean up ############ \n");

  if (OCISessionEnd(ctxptr->svchp, ctxptr->errhp, 
                      ctxptr->authp, (ub4) 0))
    printf("FAILED: OCISessionEnd()\n");

  printf("%s Logged off.\n", username);

  if (OCIServerDetach(ctxptr->srvhp, ctxptr->errhp,
                   (ub4) OCI_DEFAULT))
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


/* end of file cdemol2l.c */

