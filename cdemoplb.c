#ifdef RCSID
static char *RCSid =
   "$Header: cdemoplb.c 14-jul-99.13:14:39 mjaeger Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1998, 1999, Oracle Corporation.  All rights reserved.
*/

/*

   NAME
     cdemoplb.c - C Demo program - using LOBs in partitioned tables.

   DESCRIPTION
     This program demonstrates using LOBs in partitioned tables.
     A partitioned table is created with one BLOB and two CLOB columns
     and LOB storage clause is specified to store lobs in different
     tablespaces based on value of column A and if the LOB in question
     is column B, C or D.

     Two data files, a text file and a binary file are expected as
     arguments.

     The program creates six log files:
          binfile1.log
          txtfile2.log
          txtfile3.log
          binfile11.log
          txtfile12.log
          txtfile13.log

   MODIFIED   (MM/DD/YY)
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   svedala     07/22/98 - Creation

*/

#include <stdio.h>
#include <oci.h>

static sb4 init_handles(/*_ void _*/);
static sb4 log_on(/*_ void _*/);
static sb4 create_table(/*_ void _*/);
static sb4 select_locator(/*_ int rowind _*/);
static ub4 file_length(/*_ FILE *fp _*/);
static sb4 test_file_to_lob(/*_ int rowind, char *tfname, char *bfname _*/);
static void test_lob_to_file(/*_ int rowind _*/);
static void stream_write_lob(/*_ int rowind, OCILobLocator *lobl,
                                 FILE *fp, ub4 filelen _*/);
static void stream_read_lob(/*_ int rowind, OCILobLocator *lobl, FILE *fp _*/);
static sb4 cbk_fill_buffer(/*_ dvoid *ctxp, dvoid *bufxp, ub4 *lenp,
                                ub1 *piece _*/);
static sb4 cbk_write_buffer(/*_ dvoid *ctxp, CONST dvoid *bufxp, ub4 lenp,
                                 ub1 piece _*/);

static void logout(/*_ void _*/);
static void report_error(/*_ void _*/);

int main(/*_ int argc, char *argv[] _*/);

#define TRUE       1
#define FALSE      0

#define MAXBUFLEN  5000

static OCIEnv        *envhp;
static OCIServer     *srvhp;
static OCISvcCtx     *svchp;
static OCIError      *errhp;
static OCISession    *authp;
static OCIStmt       *stmthp;
static OCILobLocator *lob;
static OCIDefine     *defnp1 = (OCIDefine *) 0, *defnp2 = (OCIDefine *) 0;
static OCIBind       *bndhp = (OCIBind *) 0;

static FILE *fp1;

static ub4  filelen = 0;

static boolean  istxtfile;

/*------------------------end of Inclusions-----------------------------*/

int main(argc, argv)
int argc;
char *argv[];
{
  int rowind;

  if (argc != 3)
  {
    (void) printf("Usage: %s txtfilename binfilename\n", argv[0]);
    return 0;
  }

  if (init_handles())
  {
    (void) printf("FAILED: init_handles()\n");
    return OCI_ERROR;
  }

  if (log_on())
  {
    (void) printf("FAILED: log_on()\n");
    return OCI_ERROR;
  }

  if (create_table())
  {
    (void) printf("FAILED: create_table()\n");
    logout();
    return OCI_ERROR;
  }

  /************************************************
  ** First three inserts will go into partition P1
  ** column A = 1, column B into tablespace tbs_2
  ** column A = 2, column C into tablespace tbs_4
  ** column A = 3, column D into tablesapce tbs_2
  */
  for (rowind = 1; rowind <= 3; rowind++)
  {
    if (insert_record(rowind))
    {
      (void) printf("FAILED: insert_record()\n");
      logout();
      return OCI_ERROR;
    }

    if (select_locator(rowind))
    {
      (void) printf("FAILED: select_locator()\n");
      logout();
      return OCI_ERROR;
    }

    if (test_file_to_lob(rowind, argv[1], argv[2]))
    {
      (void) printf("FAILED: load files to lobs\n");
      logout();
      return OCI_ERROR;
    }

    test_lob_to_file(rowind);
  }

  /************************************************
  ** First three inserts will go into partition P1
  ** column A = 11, column B into tablespace tbs_3
  ** column A = 12, column C into tablespace tbs_3
  ** column A = 13, column D into tablesapce tbs_3
  */
  for (rowind = 11; rowind <= 13; rowind++)
  {
    if (insert_record(rowind))
    {
      (void) printf("FAILED: insert_record()\n");
      logout();
      return OCI_ERROR;
    }

    if (select_locator(rowind))
    {
      (void) printf("FAILED: select_locator()\n");
      logout();
      return OCI_ERROR;
    }

    if (test_file_to_lob(rowind, argv[1], argv[2]))
    {
      (void) printf("FAILED: load files to lobs\n");
      logout();
      return OCI_ERROR;
    }

    test_lob_to_file(rowind);
  }

  logout();

  return OCI_SUCCESS;
}




/* ----------------------------------------------------------------- */
/* initialize environment, allocate handles, etc.                    */
/* ----------------------------------------------------------------- */

sb4 init_handles()
{
  if (OCIInitialize((ub4) OCI_DEFAULT, (dvoid *)0,
                    (dvoid * (*)(dvoid *, size_t)) 0,
                    (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                    (void (*)(dvoid *, dvoid *)) 0 ))
  {
    (void) printf("FAILED: OCIInitialize()\n");
    return OCI_ERROR;
  }

  /* initialize environment handle */
  if (OCIEnvInit((OCIEnv **) &envhp, (ub4) OCI_DEFAULT,
                 (size_t) 0, (dvoid **) 0 ))
  {
    (void) printf("FAILED: OCIEnvInit()\n");
    return OCI_ERROR;
  }

  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &svchp,
                     (ub4) OCI_HTYPE_SVCCTX, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc()\n");
    return OCI_ERROR;
  }

  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &errhp,
                     (ub4) OCI_HTYPE_ERROR, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc()\n");
    return OCI_ERROR;
  }

  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &stmthp,
                     (ub4) OCI_HTYPE_STMT, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc()\n");
    return OCI_ERROR;
  }

  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &srvhp,
                     (ub4) OCI_HTYPE_SERVER, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc()\n");
    return OCI_ERROR;
  }

  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &authp,
                     (ub4) OCI_HTYPE_SESSION, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc()\n");
    return OCI_ERROR;
  }

  if (OCIDescriptorAlloc((dvoid *) envhp, (dvoid **) &lob,
                         (ub4)OCI_DTYPE_LOB, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIDescriptorAlloc()\n");
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}



/* ----------------------------------------------------------------- */
/* attach to the server and log on as SCOTT/TIGER                    */
/* ----------------------------------------------------------------- */

sb4 log_on()
{
  text *uid = (text *)"CDEMOPLB";
  text *pwd = (text *)"CDEMOPLB";
  text *cstring = (text *) "";

  /* attach to the server */
  if (OCIServerAttach(srvhp, errhp, (text *) cstring,
                     (sb4) strlen((char *)cstring), (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIServerAttach()\n");
    return OCI_ERROR;
  }

  if (OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION,
                 (dvoid *) uid, (ub4) strlen((char *)uid),
                 (ub4) OCI_ATTR_USERNAME, errhp))
  {
    (void) printf("FAILED: OCIAttrSet()\n");
    return OCI_ERROR;
  }

  if (OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION,
                 (dvoid *) pwd, (ub4) strlen((char *)pwd),
                 (ub4) OCI_ATTR_PASSWORD, errhp))
  {
    (void) printf("FAILED: OCIAttrSet()\n");
    return OCI_ERROR;
  }

  /* set the server attribute in the service context */
  if (OCIAttrSet((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
                 (dvoid *) srvhp, (ub4) 0, (ub4) OCI_ATTR_SERVER, errhp))
  {
    (void) printf("FAILED: OCIAttrSet()\n");
    return OCI_ERROR;
  }

  /* log on */
  if (OCISessionBegin(svchp, errhp, authp, (ub4) OCI_CRED_RDBMS,
                     (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCISessionBegin()\n");
    return OCI_ERROR;
  }

  /* set the session attribute in the service context */
  if (OCIAttrSet((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX, (dvoid *) authp,
                 (ub4) 0, (ub4) OCI_ATTR_SESSION, errhp))
  {
    (void) printf("FAILED: OCIAttrSet()\n");
    return OCI_ERROR;
  }

  return OCI_SUCCESS;

}


/* ----------------------------------------------------------------- */
/* Create table FOO with CLOB, BLOB columns and insert one row.      */
/* Both columns are empty lobs, not null lobs.                       */
/* ----------------------------------------------------------------- */

sb4 create_table()
{
  int colc;

  text *crtstmt = (text *)
     "CREATE TABLE PT1 (A NUMBER, B BLOB, C CLOB, D CLOB) "
     "PARTITION BY RANGE (A)"
     "  (PARTITION P1 VALUES LESS THAN (10) TABLESPACE TBS_1"
     "     LOB (B,D) STORE AS (TABLESPACE TBS_2),"
     "   PARTITION P2 VALUES LESS THAN (MAXVALUE)"
     "     LOB (B,C,D) STORE AS (TABLESPACE TBS_3)"
     "  ) TABLESPACE TBS_4";

  if (OCIStmtPrepare(stmthp, errhp, crtstmt, (ub4) strlen((char *) crtstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() crtstmt\n");
    return OCI_ERROR;
  }

  if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                    (CONST OCISnapshot *) 0, (OCISnapshot *) 0,
                    (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtExecute() crtstmt\n");
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}


/* ----------------------------------------------------------------- */
/* Insert record into table PT1 with empty_blob() and empty_clob()   */
/* as values for columns b, c or d                                   */
/* ----------------------------------------------------------------- */

sb4 insert_record(int rowind)
{
  int colc;

  text  insstmt[100];

  switch (rowind)
  {
    case 1:
    case 11:
       sprintf(insstmt, "INSERT INTO PT1 (A, B) VALUES (:1, EMPTY_BLOB())");
       break;
    case 2:
    case 12:
       sprintf(insstmt, "INSERT INTO PT1 (A, C) VALUES (:1, EMPTY_CLOB())");
       break;
    case 3:
    case 13:
       sprintf(insstmt, "INSERT INTO PT1 (A, D) VALUES (:1, EMPTY_CLOB())");
       break;
    default:
       (void) printf("ERROR: Invalid row indicator.\n");
       break;
  }

  if (OCIStmtPrepare(stmthp, errhp, insstmt, (ub4) strlen((char *) insstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() insstmt\n");
    return OCI_ERROR;
  }

  if (OCIBindByPos(stmthp, &bndhp, errhp, (ub4) 1,
                      (dvoid *) &colc, (sb4) sizeof(colc), SQLT_INT,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIBindByPos()\n");
    return OCI_ERROR;
  }

  colc = rowind;
  if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                    (CONST OCISnapshot *) 0, (OCISnapshot *) 0,
                    (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtExecute() insstmt\n");
    return OCI_ERROR;
  }

  (void) OCITransCommit(svchp, errhp, (ub4)0);

  return OCI_SUCCESS;
}

/*---------------------------------------------------------------------*/
/* Select lob locators from the CLOB, BLOB columns.                    */
/* We need the 'FOR UPDATE' clause since we need to write to the lobs. */
/*---------------------------------------------------------------------*/

sb4 select_locator(int rowind)
{
  int colc = rowind;
  text  sqlstmt[100];

  switch(rowind)
  {
    case 1:
    case 11:
       sprintf(sqlstmt, "SELECT B FROM PT1 WHERE A = :1 FOR UPDATE");
       break;
    case 2:
    case 12:
       sprintf(sqlstmt, "SELECT C FROM PT1 WHERE A = :1 FOR UPDATE");
       break;
    case 3:
    case 13:
       sprintf(sqlstmt, "SELECT D FROM PT1 WHERE A = :1 FOR UPDATE");
       break;
    default:
       (void) printf("ERROR: Invalid row indicator.\n");
       break;
  }

  if (OCIStmtPrepare(stmthp, errhp, sqlstmt, (ub4) strlen((char *)sqlstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() sqlstmt\n");
    return OCI_ERROR;
  }

  if (OCIBindByPos(stmthp, &bndhp, errhp, (ub4) 1,
                      (dvoid *) &colc, (sb4) sizeof(colc), SQLT_INT,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIBindByPos()\n");
    return OCI_ERROR;
  }

  switch (rowind)
  {
    case 1:
    case 11:
       if (OCIDefineByPos(stmthp, &defnp2, errhp, (ub4) 1,
                      (dvoid *) &lob, (sb4) -1, (ub2) SQLT_BLOB,
                      (dvoid *) 0, (ub2 *) 0, (ub2 *) 0, (ub4) OCI_DEFAULT))
       {
         (void) printf("FAILED: OCIDefineByPos() - BLOB\n");
         return OCI_ERROR;
       }
       break;
    case 2:
    case 3:
    case 12:
    case 13:
       if (OCIDefineByPos(stmthp, &defnp1, errhp, (ub4) 1,
                      (dvoid *) &lob, (sb4) -1, (ub2) SQLT_CLOB,
                      (dvoid *) 0, (ub2 *) 0, (ub2 *) 0, (ub4) OCI_DEFAULT))
       {
         (void) printf("FAILED: OCIDefineByPos() - CLOB\n");
         return OCI_ERROR;
       }
       break;
    default:
       (void) printf("ERROR: Invalid row indicator.\n");
       break;
  }

  /* execute the select and fetch one row */
  if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                    (CONST OCISnapshot*) 0, (OCISnapshot*) 0,
                    (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtExecute() sqlstmt\n");
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}



/* ----------------------------------------------------------------- */
/* Read operating system files into local buffers and then write the */
/* buffers to lobs.                                                  */
/* ----------------------------------------------------------------- */

sb4 test_file_to_lob(int rowind, char *txtfile, char *binfile)
{
  (void) printf("\n===> Testing loading files into lobs .....\n\n");

  if (rowind == 1 || rowind == 11)
  {
    fp1 = fopen((const char *)binfile, (const char *) "rb");
  }
  else
  {
    fp1 = fopen((const char *)txtfile, (const char *) "r");
  }

  if (!fp1)
  {
    (void) printf("ERROR: Failed to open file(s).\n");
    return -1;
  }

  filelen = file_length(fp1);

  stream_write_lob(rowind, lob, fp1, filelen);

  (void) fclose(fp1);

  return 0;
}


/* ----------------------------------------------------------------- */
/* get the length of the input file.                                 */
/* ----------------------------------------------------------------- */

ub4 file_length(FILE *fp)
{
  fseek(fp, 0, SEEK_END);
  return (ub4) (ftell(fp));
}


/* ----------------------------------------------------------------- */
/* Read operating system files into local buffers and then write the */
/* buffers to lobs using stream mode.                                */
/* ----------------------------------------------------------------- */

void stream_write_lob(int rowind, OCILobLocator *lobl, FILE *fp, ub4 filelen)
{
  ub4   offset = 1;
  ub4   loblen = 0;
  ub1   bufp[MAXBUFLEN];
  ub4   amtp = filelen;
  ub1   piece;
  sword retval;
  int   readval;
  ub4   len = 0;
  ub4   nbytes;
  ub4   remainder = filelen;

  (void) printf("--> To do streamed write lob, amount = %d\n", filelen);

  (void) OCILobGetLength(svchp, errhp, lobl, &loblen);
  (void) printf("Before stream write, LOB length = %d\n\n", loblen);

  (void) fseek(fp, 0, 0);

  if (filelen > MAXBUFLEN)
    nbytes = MAXBUFLEN;
  else
    nbytes = filelen;

  if (fread((void *)bufp, (size_t)nbytes, 1, fp) != 1)
  {
    (void) printf("ERROR: read file.\n");
    return;
  }

  remainder -= nbytes;

  if (remainder == 0)       /* exactly one piece in the file */
  {
    (void) printf("Only one piece, no need for stream write.\n");
    if (retval = OCILobWrite(svchp, errhp, lobl, &amtp, offset, (dvoid *) bufp,
                            (ub4) nbytes, OCI_ONE_PIECE, (dvoid *)0,
                            (sb4 (*)(dvoid *, dvoid *, ub4 *, ub1 *)) 0,
                            (ub2) 0, (ub1) SQLCS_IMPLICIT) != OCI_SUCCESS)
    {
      (void) printf("ERROR: OCILobWrite(), retval = %d\n", retval);
      return;
    }
  }
  else                     /* more than one piece */
  {
    if (OCILobWrite(svchp, errhp, lobl, &amtp, offset, (dvoid *) bufp,
                    (ub4) MAXBUFLEN, OCI_FIRST_PIECE, (dvoid *)0,
                    (sb4 (*)(dvoid *, dvoid *, ub4 *, ub1 *)) 0,
                    (ub2) 0, (ub1) SQLCS_IMPLICIT) != OCI_NEED_DATA)
    {
      (void) printf("ERROR: OCILobWrite().\n");
      return;
    }

    piece = OCI_NEXT_PIECE;

    do
    {
      if (remainder > MAXBUFLEN)
        nbytes = MAXBUFLEN;
      else
      {
        nbytes = remainder;
        piece = OCI_LAST_PIECE;
      }

      if (fread((void *)bufp, (size_t)nbytes, 1, fp) != 1)
      {
        (void) printf("ERROR: read file.\n");
        piece = OCI_LAST_PIECE;
      }

      retval = OCILobWrite(svchp, errhp, lobl, &amtp, offset, (dvoid *) bufp,
                          (ub4) nbytes, piece, (dvoid *)0,
                          (sb4 (*)(dvoid *, dvoid *, ub4 *, ub1 *)) 0,
                          (ub2) 0, (ub1) SQLCS_IMPLICIT);
      remainder -= nbytes;

    } while (retval == OCI_NEED_DATA && !feof(fp));
  }

  if (retval != OCI_SUCCESS)
  {
    (void) printf("Error: stream writing LOB.\n");
    return;
  }

  (void) OCILobGetLength(svchp, errhp, lobl, &loblen);
  (void) printf("After stream write, LOB length = %d\n\n", loblen);

  return;
}



/* ----------------------------------------------------------------- */
/* Read lobs into local buffers and then write them to operating     */
/* system files.                                                     */
/* ----------------------------------------------------------------- */

void test_lob_to_file(int rowind)
{
  ub4   offset = 1;
  ub4   loblen = 0;
  ub1   bufp[MAXBUFLEN];
  ub4   amtp = MAXBUFLEN;
  text  filename[20];

  if (rowind == 1 || rowind == 11)
  {
    (void) sprintf((char *) filename, (char *)"binfile%d.log", rowind);
    fp1 = fopen((char *)filename, (const char *) "wb");
  }
  else
  {
    (void) sprintf((char *) filename, (char *)"txtfile%d.log", rowind);
    fp1 = fopen((char *)filename, (const char *) "w");
  }

  (void) printf("\n===> Testing writing lobs to files .....\n\n");

  if (!fp1)
  {
    (void) printf("ERROR: Failed to open file(s).\n");
    return;
  }

  stream_read_lob(rowind, lob, fp1);

  (void) fclose(fp1);

  return;
}


/* ----------------------------------------------------------------- */
/* Read lobs using stream mode into local buffers and then write     */
/* them to operating system files.                                   */
/* ----------------------------------------------------------------- */

void stream_read_lob(int rowind, OCILobLocator *lobl, FILE *fp)
{
  ub4   offset = 1;
  ub4   loblen = 0;
  ub1   bufp[MAXBUFLEN];
  ub4   amtp = 0;
  sword retval;
  ub4   piece = 0;
  ub4   remainder;            /* the number of bytes for the last piece */

  (void) OCILobGetLength(svchp, errhp, lobl, &loblen);
  amtp = loblen;

  (void) printf("--> To stream read LOB, loblen = %d.\n", loblen);

  memset(bufp, '\0', MAXBUFLEN);

  retval = OCILobRead(svchp, errhp, lobl, &amtp, offset, (dvoid *) bufp,
                     (loblen < MAXBUFLEN ? loblen : MAXBUFLEN), (dvoid *)0,
                     (sb4 (*)(dvoid *, const dvoid *, ub4, ub1)) 0,
                     (ub2) 0, (ub1) SQLCS_IMPLICIT);

  switch (retval)
  {
    case OCI_SUCCESS:             /* only one piece */
      (void) printf("stream read %d th piece\n", ++piece);
      (void) fwrite((void *)bufp, (size_t)loblen, 1, fp);
      break;
    case OCI_ERROR:
      report_error();
      break;
    case OCI_NEED_DATA:           /* there are 2 or more pieces */

      remainder = loblen;

      /* a full buffer to write */
      (void) fwrite((void *)bufp, MAXBUFLEN, 1, fp);

      do
      {
        memset(bufp, '\0', MAXBUFLEN);
        amtp = 0;

        remainder -= MAXBUFLEN;

        retval = OCILobRead(svchp, errhp, lobl, &amtp, offset, (dvoid *) bufp,
                           (ub4) MAXBUFLEN, (dvoid *)0,
                           (sb4 (*)(dvoid *, const dvoid *, ub4, ub1)) 0,
                           (ub2) 0, (ub1) SQLCS_IMPLICIT);

        /* the amount read returned is undefined for FIRST, NEXT pieces */
        (void) printf("stream read %d th piece, amtp = %d\n", ++piece, amtp);

        if (remainder < MAXBUFLEN)     /* last piece not a full buffer piece */
           (void) fwrite((void *)bufp, (size_t)remainder, 1, fp);
        else
           (void) fwrite((void *)bufp, MAXBUFLEN, 1, fp);

      } while (retval == OCI_NEED_DATA);
      break;
    default:
      (void) printf("Unexpected ERROR: OCILobRead() LOB.\n");
      break;
  }

  return;
}


/*-------------------------------------------------------------------*/
/* Logoff and disconnect from the server.  Free handles.             */
/*-------------------------------------------------------------------*/

void logout()
{

  (void) OCISessionEnd(svchp, errhp, authp, (ub4) 0);
  (void) OCIServerDetach(srvhp, errhp, (ub4) OCI_DEFAULT);

  (void) printf("Logged off and detached from server.\n");

  (void) OCIHandleFree((dvoid *) srvhp, (ub4) OCI_HTYPE_SERVER);
  (void) OCIHandleFree((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX);
  (void) OCIHandleFree((dvoid *) errhp, (ub4) OCI_HTYPE_ERROR);
  (void) OCIHandleFree((dvoid *) authp, (ub4) OCI_HTYPE_SESSION);
  (void) OCIDescriptorFree((dvoid *) lob, (ub4) OCI_DTYPE_LOB);
  (void) OCIHandleFree((dvoid *) stmthp, (ub4) OCI_HTYPE_STMT);

  (void) printf("All handles freed\n");
  return;
}


/* ----------------------------------------------------------------- */
/* retrieve error message and print it out.                          */
/* ----------------------------------------------------------------- */
void report_error()
{
  text  msgbuf[512];
  sb4   errcode = 0;

  (void) OCIErrorGet((dvoid *) errhp, (ub4) 1, (text *) NULL, &errcode,
                       msgbuf, (ub4) sizeof(msgbuf), (ub4) OCI_HTYPE_ERROR);
  (void) printf("ERROR CODE = %d\n", errcode);
  (void) printf("%.*s\n", 512, msgbuf);
  return;
}


/* end of file cdemolb2.c */

