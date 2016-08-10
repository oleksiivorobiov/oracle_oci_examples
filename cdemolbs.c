#ifdef RCSID
static char *RCSid =
   "$Header: cdemolbs.c 10-oct-2006.14:39:58 azhao Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1997, 2006, Oracle. All rights reserved.  
*/

/*

   NAME
     cdemolbs.c - Demonstrates reading and writing to LOBs through
                  the LOB Buffering Subsystem.

   DESCRIPTION
     This program reads from an input binary/text file, writing into an
     initialized B/CLOB column in buffered mode. It then reads in buffered
     mode from the B/CLOB column and populates an output file. After building
     the executable (assume it is called cdemolbs), run the program as follows:
         cdemolbs src.txt src.bin dst.txt dst.bin
     where src.txt and src.bin are text and binary files of size <= 512Kbytes.
     IMPORTANT: . This program works only for single-byte CLOBs.
                . Before running this program, ensure that the database is
                  started up and a table FOO does not exist in the SCOTT/
                  TIGER sample account.

   PUBLIC FUNCTION(S)
     <list of external functions declared/defined - with one-line descriptions>

   PRIVATE FUNCTION(S)
     <list of static functions defined in .c file - with one-line descriptions>

   RETURNS
     <function return values, for .c file with single function>

   NOTES
     <other useful comments, qualifications, etc.>

   MODIFIED   (MM/DD/YY)
   azhao       10/10/06 - case-senstive password change
   gtarora     10/18/99 - Correct parameters to printf
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   ramkrish    03/06/98 - 546944: add/modify output statements for debug mode
   skotsovo    08/07/97 - modified lbs buffer size so max read allowed changed
   azhao       07/15/97 - fix reading files
   azhao       05/30/97 - Use OCIBindByPos
   azhao       05/27/97 - add savepoint code
   ramkrish    05/01/97 - Creation

*/

#include <stdio.h>
#include <string.h>
#include <oci.h>

/*--------------------- Public Constants and Variables ----------------------*/

/* Constants */
#define TRUE       1
#define FALSE      0
#define MAXBUFLEN  32768
/* the size of the lob buffer depends on the usable chunk size.  For
 * this example, the maximum amount of lob data that can be read in
 * one call is 508928.
 */
#define MAXLBSLEN  508928

/* OCI Handles */
static OCIEnv        *envhp;
static OCIServer     *srvhp;
static OCISvcCtx     *svchp;
static OCIError      *errhp;
static OCISession    *authp;
static OCIStmt       *stmthp;
static OCILobLocator *clob, *blob;
static OCIDefine     *defnp1 = (OCIDefine *) 0, *defnp2 = (OCIDefine *) 0;
static OCIBind       *bndhp = (OCIBind *) 0;

/* Misellaneous */
static FILE          *fp1, *fp2;
static ub4            txtfilelen = 0;
static ub4            binfilelen = 0;
static boolean        istxtfile;
static boolean        tab_exists = FALSE;

/*--------------------- Public functions - Specification --------------------*/

int  main           (/*_ int argc, char *argv[] _*/);

static sb4  init_handles   (/*_ void _*/);
static sb4  init_table     (/*_ void _*/);
static sb4  log_on         (/*_ void _*/);
static void log_off        (/*_ void _*/);
static sb4  write_lobs     (/*_ int rowind, char *txtfile, char *binfile _*/);
static sb4  read_lobs      (/*_ int rowind, char *txtfile, char *binfile _*/);

/*--------------------- Private functions - Specification -------------------*/

static sb4  select_clob   (/*_ int rowind _*/);
static sb4  select_blob   (/*_ int rowind _*/);
static sb4  select_lobs   (/*_ int rowind _*/);
static sb4  buf_write_lob (/*_ int rowind, OCILobLocator *locator, FILE *fp,
                               ub4 filelen _*/);
static sb4  buf_read_lob  (/*_ int rowind, OCILobLocator *locator,
                                FILE *fp _*/);
static void drop_table    (/*_ void _*/);
static void report_error  (/*_ void _*/);
static ub4  file_length   (/*_ FILE *fp _*/);

/*----------------------------- Public functions ----------------------------*/

/*---------------------------------- main -----------------------------------*/

/* main driver */
int main(argc, argv)
int argc;
char *argv[];
{
  int rowind;

  /* validate input arguments */
  if (argc != 5)
  {
    (void) printf("Usage: %s srctxtfile srcbinfile desttxtfile destbinfile\n",
                  argv[0]);
    return 0;
  }
  /* initialize OCI handles */
  if (init_handles())
  {
    (void) printf("FAILED: init_handles()\n");
    return OCI_ERROR;
  }
  /* log on to server */
  if (log_on())
  {
    (void) printf("FAILED: log_on()\n");
    return OCI_ERROR;
  }
  /* init demo table */
  if (init_table())
  {
    (void) printf("FAILED: init_table()\n");
    log_off();
    return OCI_ERROR;
  }
  /* write to LOBs in row 1 thro buffering subsystem, reading from src files */
  rowind = 1;
  if (write_lobs(rowind, argv[1], argv[2]))
  {
    (void) printf("FAILED: write files to lobs\n");
    log_off();
    return OCI_ERROR;
  }
  /* read from LOBs in row 1 thro buffering subsystem, writing to dest files */
  rowind = 1;
  if (read_lobs(rowind, argv[3], argv[4]))
  {
    (void) printf("FAILED: write lobs to files\n");
    log_off();
    return OCI_ERROR;
  }
  /*  clean up and log off from server */
  log_off();

  return OCI_SUCCESS;
}

/*----------------------------- init_handles --------------------------------*/

/* initialize environment, and allocate all handles */
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
  /* initialize service context */
  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &svchp,
                     (ub4) OCI_HTYPE_SVCCTX, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc()\n");
    return OCI_ERROR;
  }
  /* initialize error handle */
  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &errhp,
                     (ub4) OCI_HTYPE_ERROR, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc()\n");
    return OCI_ERROR;
  }
  /* initialize statement handle */
  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &stmthp,
                     (ub4) OCI_HTYPE_STMT, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc()\n");
    return OCI_ERROR;
  }
  /* initialize server handle */
  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &srvhp,
                     (ub4) OCI_HTYPE_SERVER, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc()\n");
    return OCI_ERROR;
  }
  /* initialize session/authentication handle */
  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &authp,
                     (ub4) OCI_HTYPE_SESSION, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIHandleAlloc()\n");
    return OCI_ERROR;
  }

  /* allocate the lob locator variables */
  if (OCIDescriptorAlloc((dvoid *) envhp, (dvoid **) &clob,
                         (ub4)OCI_DTYPE_LOB, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIDescriptorAlloc()\n");
    return OCI_ERROR;
  }
  if (OCIDescriptorAlloc((dvoid *) envhp, (dvoid **) &blob,
                         (ub4)OCI_DTYPE_LOB, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIDescriptorAlloc()\n");
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}

/*------------------------------- init_table --------------------------------*/

/* create table FOO with initialized CLOB, BLOB columns, and insert two rows */
sb4 init_table()
{
  int   colc;
  text *crtstmt = (text *) "CREATE TABLE FOO (C1 CLOB, C2 BLOB, C3 INTEGER)";
  text *insstmt =
            (text *) "INSERT INTO FOO VALUES (EMPTY_CLOB(), EMPTY_BLOB(), :1)";

  /* prepare create statement */
  if (OCIStmtPrepare(stmthp, errhp, crtstmt, (ub4) strlen((char *) crtstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() crtstmt\n");
    return OCI_ERROR;
  }
  /* execute create statement */
  if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                    (CONST OCISnapshot *) 0, (OCISnapshot *) 0,
                    (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtExecute() crtstmt\n");
    return OCI_ERROR;
  }
  /* prepare insert statement */
  if (OCIStmtPrepare(stmthp, errhp, insstmt, (ub4) strlen((char *) insstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() insstmt\n");
    return OCI_ERROR;
  }
  /* associate variable colc with bind placeholder #1 in the SQL statement */
  if (OCIBindByPos(stmthp, &bndhp, errhp, (ub4) 1,
                      (dvoid *) &colc, (sb4) sizeof(colc), SQLT_INT,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIBindByPos()\n");
    return OCI_ERROR;
  }
  /* insert two rows */
  for (colc = 1; colc <= 2; colc++)
  {
    if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                       (CONST OCISnapshot *) 0, (OCISnapshot *) 0,
                       (ub4) OCI_DEFAULT))
    {
      (void) printf("FAILED: OCIStmtExecute() insstmt\n");
      return OCI_ERROR;
    }
  }

  /* commit the Xn */
  (void) OCITransCommit(svchp, errhp, (ub4)0);

  /* set flag to be used by log_off() to drop the table */
  tab_exists = TRUE;

  return OCI_SUCCESS;
}

/*---------------------------------- log_on ---------------------------------*/

/* attach to the server and log on as SCOTT/TIGER */
sb4 log_on()
{
  text *uid     = (text *)"SCOTT";
  text *pwd     = (text *)"tiger";
  text *cstring = (text *)"";

  /* attach to the server */
  if (OCIServerAttach(srvhp, errhp, (text *) cstring,
                     (sb4) strlen((char *)cstring), (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIServerAttach()\n");
    return OCI_ERROR;
  }

  /* set username and password attributes of the server handle */
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

/*------------------------------------ logoff -------------------------------*/

/* Logoff and disconnect from the server.  Free handles */
void log_off()
{
  if (tab_exists)
    drop_table();

  (void) OCISessionEnd(svchp, errhp, authp, (ub4) 0);
  (void) OCIServerDetach(srvhp, errhp, (ub4) OCI_DEFAULT);

  (void) printf("\n\nLogged off and detached from server.\n");

  (void) OCIHandleFree((dvoid *) srvhp, (ub4) OCI_HTYPE_SERVER);
  (void) OCIHandleFree((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX);
  (void) OCIHandleFree((dvoid *) errhp, (ub4) OCI_HTYPE_ERROR);
  (void) OCIHandleFree((dvoid *) authp, (ub4) OCI_HTYPE_SESSION);
  (void) OCIDescriptorFree((dvoid *) clob, (ub4) OCI_DTYPE_LOB);
  (void) OCIDescriptorFree((dvoid *) blob, (ub4) OCI_DTYPE_LOB);
  (void) OCIHandleFree((dvoid *) stmthp, (ub4) OCI_HTYPE_STMT);

  (void) printf("\nAll handles freed\n");
  return;
}

/*-------------------------------- write_lobs -------------------------------*/

/* write from files to LOBs */
sb4 write_lobs (rowind, txtfile, binfile)
int   rowind;
char *txtfile;
char *binfile;
{
  ub4   loblen = 0;
  text *svptstmt = (text *)"SAVEPOINT cdemolbs_svpt";
  text *rlbkstmt = (text *)"ROLLBACK TO SAVEPOINT cdemolbs_svpt";
  ub4   txtfilelen = 0;
  ub4   binfilelen = 0;

  /* validate row indicator */
  if (!rowind || (rowind > 2))
  {
    (void) printf("ERROR: Invalid row indicator.\n");
    return OCI_ERROR;
  }
  /* open source files */
  fp1 = fopen((CONST char *)txtfile, (CONST char *) "r");
  fp2 = fopen((CONST char *)binfile, (CONST char *) "rb");
  if (!(fp1 && fp2))
  {
    (void) printf("ERROR: Failed to open file(s).\n");
    return -1;
  }
  if ((txtfilelen = file_length(fp1)) > MAXLBSLEN)
  {
    (void) printf("ERROR: %s - length > 512Kbytes", txtfile);
    return -1;
  }
  if ((binfilelen = file_length(fp2)) > MAXLBSLEN)
  {
    (void) printf("ERROR: %s - length > 512Kbytes", binfile);
    return -1;
  }

  /* reset file pointers to start of file */
  (void) fseek(fp1, 0, 0);
  (void) fseek(fp2, 0, 0);

  /* set savepoint for Xn before commencing buffered mode operations */
  if (OCIStmtPrepare(stmthp, errhp, svptstmt, (ub4) strlen((char *)svptstmt),
                     (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() svptstmt\n");
    return OCI_ERROR;
  }
  if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                     (CONST OCISnapshot*) 0, (OCISnapshot*) 0,
                     (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtExecute() svptstmt\n");
    report_error();
    return OCI_ERROR;
  }

  (void) printf("\n===> Writing CLOB from txtfile in buffered mode.....\n\n");

  /* fetch the CLOB's locator from the table for update */
  if (select_clob(rowind))
  {
    (void) printf("FAILED: select_clob()\n");
    log_off();
    return OCI_ERROR;
  }

  /* report CLOB length before buffered write begins */
  (void) OCILobGetLength(svchp, errhp, clob, &loblen);
  (void) printf("\nBefore buffered write, CLOB length = %d\n\n", loblen);

  /* enable the CLOB locator for buffering operations */
  if (OCILobEnableBuffering(svchp, errhp, clob))
  {
    (void) printf("FAILED: OCILobEnableBuffering() CLOB\n");
    return OCI_ERROR;
  }

  /* write the text file contents into CLOB through the buffering subsystem */
  if (buf_write_lob(rowind, clob, fp1, txtfilelen) == OCI_ERROR)
  {
    /* if buffered write operation failed, rollback Xn to savepoint & exit */
    if (OCIStmtPrepare(stmthp, errhp, rlbkstmt, (ub4) strlen((char *)rlbkstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
    {
      (void) printf("FAILED: OCIStmtPrepare() rlbkstmt\n");
      return OCI_ERROR;
    }
    if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                    (CONST OCISnapshot*) 0, (OCISnapshot*) 0,
                    (ub4) OCI_DEFAULT))
    {
      (void) printf("FAILED: OCIStmtExecute() rlbkstmt\n");
      report_error();
      return OCI_ERROR;
    }

    (void) printf("FAILED: buf_write_lob() CLOB\n");
    return OCI_ERROR;
  }

  /* commit the Xn if the CLOB's buffer was flushed successfully */
  (void) OCITransCommit(svchp, errhp, (ub4)0);

  /* disable CLOB locator from buffering */
  if (OCILobDisableBuffering(svchp, errhp, clob))
  {
    (void) printf("FAILED: OCILobDisableBuffering() CLOB\n");
    return OCI_ERROR;
  }

  (void) printf("\n===> Writing BLOB from binfile in buffered mode.....\n\n");

  /* fetch the BLOB's locator from the table for update */
  if (select_blob(rowind))
  {
    (void) printf("FAILED: select_blob()\n");
    log_off();
    return OCI_ERROR;
  }

  /* report LOB length before buffered write begins */
  (void) OCILobGetLength(svchp, errhp, blob, &loblen);
  (void) printf("\nBefore buffered write, BLOB length = %d\n\n", loblen);

  /* enable the BLOB locator for buffering operations */
  if (OCILobEnableBuffering(svchp, errhp, blob))
  {
    (void) printf("FAILED: OCILobEnableBuffering() BLOB\n");
    return OCI_ERROR;
  }

  /* write the bin file contents into BLOB through the buffering subsystem */
  if (buf_write_lob(rowind, blob, fp2, binfilelen) > 0)
  {
    /* if buffered write operation failed, rollback Xn to savepoint & exit */
    if (OCIStmtPrepare(stmthp, errhp, rlbkstmt, (ub4) strlen((char *)rlbkstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
    {
      (void) printf("FAILED: OCIStmtPrepare() rlbkstmt\n");
      return OCI_ERROR;
    }
    if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                    (CONST OCISnapshot*) 0, (OCISnapshot*) 0,
                    (ub4) OCI_DEFAULT))
    {
      (void) printf("FAILED: OCIStmtExecute() rlbkstmt\n");
      report_error();
      return OCI_ERROR;
    }

    (void) printf("FAILED: buf_write_lob() BLOB\n");
    return OCI_ERROR;
  }

  /* commit the Xn if the BLOB's buffer was flushed successfully */
  (void) OCITransCommit(svchp, errhp, (ub4)0);

  /* disable BLOB locator from buffering */
  if (OCILobDisableBuffering(svchp, errhp, blob))
  {
    (void) printf("FAILED: OCILobDisableBuffering() BLOB\n");
    return OCI_ERROR;
  }

  /* close input files */
  (void) fclose(fp1);
  (void) fclose(fp2);

  return OCI_SUCCESS;
}

/*-------------------------------- read_lobs --------------------------------*/

/* read from LOBs into files */
sb4 read_lobs (rowind, txtfile, binfile)
int   rowind;
char *txtfile;
char *binfile;
{
  ub4 loblen = 0;
  text *svptstmt = (text *)"SAVEPOINT cdemolbs_svpt";
  text *rlbkstmt = (text *)"ROLLBACK TO SAVEPOINT cdemolbs_svpt";

  if (!rowind || (rowind > 2))
  {
    (void) printf("ERROR: Invalid row indicator.\n");
    return -1;
  }

  /* open destination files */
  fp1 = fopen((CONST char *)txtfile, (CONST char *) "w");
  fp2 = fopen((CONST char *)binfile, (CONST char *) "wb");
  if (!(fp1 && fp2))
  {
    (void) printf("ERROR: Failed to open file(s).\n");
    return -1;
  }

  /* reset file pointers to start of file */
  (void) fseek(fp1, 0, 0);
  (void) fseek(fp2, 0, 0);

  /* fetch both the LOB locators from the table for reads */
  if (select_lobs(rowind))
  {
    (void) printf("FAILED: select_lobs()\n");
    log_off();
    return OCI_ERROR;
  }

  /* report CLOB length before buffered read begins */
  (void) OCILobGetLength(svchp, errhp, clob, &loblen);
  (void) printf("\n\nBefore buffered read, CLOB length = %d\n\n", loblen);

  /* report BLOB length before buffered read begins */
  (void) OCILobGetLength(svchp, errhp, blob, &loblen);
  (void) printf("\nBefore buffered read, BLOB length = %d\n\n", loblen);

  /* set savepoint for Xn before commencing buffered mode operations */
  if (OCIStmtPrepare(stmthp, errhp, svptstmt, (ub4) strlen((char *)svptstmt),
                     (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() svptstmt\n");
    return OCI_ERROR;
  }
  if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                     (CONST OCISnapshot*) 0, (OCISnapshot*) 0,
                     (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtExecute() svptstmt\n");
    report_error();
    return OCI_ERROR;
  }

  /* enable the locators for buffering operations */
  if (OCILobEnableBuffering(svchp, errhp, clob))
  {
    (void) printf("FAILED: OCILobEnableBuffering() CLOB\n");
    return OCI_ERROR;
  }
  if (OCILobEnableBuffering(svchp, errhp, blob))
  {
    (void) printf("FAILED: OCILobEnableBuffering() BLOB\n");
    return OCI_ERROR;
  }

  (void) printf("\n===> Reading CLOB into dst.txt in buffered mode...\n\n");

  /* read the CLOB into buffer and write the contents to a text file */
  if (buf_read_lob(rowind, clob, fp1) == OCI_ERROR)
  {
    /* if buffered read operation failed, rollback Xn to savepoint & exit */
    if (OCIStmtPrepare(stmthp, errhp, rlbkstmt, (ub4) strlen((char *)rlbkstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
    {
      (void) printf("FAILED: OCIStmtPrepare() rlbkstmt\n");
      return OCI_ERROR;
    }
    if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                    (CONST OCISnapshot*) 0, (OCISnapshot*) 0,
                    (ub4) OCI_DEFAULT))
    {
      (void) printf("FAILED: OCIStmtExecute() rlbkstmt\n");
      report_error();
      return OCI_ERROR;
    }

    (void) printf("FAILED: buf_read_lob() CLOB\n");
    return OCI_ERROR;
  }

  (void) printf("\n===> Reading BLOB into dst.bin in buffered mode...\n\n");

  /* read the BLOB into buffer and write the contents to a binary file */
  if (buf_read_lob(rowind, blob, fp2) > 0)
  {
    /* if buffered read operation failed, rollback Xn to savepoint & exit */
    if (OCIStmtPrepare(stmthp, errhp, rlbkstmt, (ub4) strlen((char *)rlbkstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
    {
      (void) printf("FAILED: OCIStmtPrepare() rlbkstmt\n");
      return OCI_ERROR;
    }
    if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                    (CONST OCISnapshot*) 0, (OCISnapshot*) 0,
                    (ub4) OCI_DEFAULT))
    {
      (void) printf("FAILED: OCIStmtExecute() rlbkstmt\n");
      report_error();
      return OCI_ERROR;
    }

    (void) printf("FAILED: buf_read_clob()\n");
    return OCI_ERROR;
  }

  /* commit the Xn if buffered reads went off successfully */
  (void) OCITransCommit(svchp, errhp, (ub4)0);

  /* disable locator for buffering */
  if (OCILobDisableBuffering(svchp, errhp, clob))
  {
    (void) printf("FAILED: OCILobDisableBuffering() \n");
    return OCI_ERROR;
  }
  if (OCILobDisableBuffering(svchp, errhp, blob))
  {
    (void) printf("FAILED: OCILobDisableBuffering() \n");
    return OCI_ERROR;
  }

  /* close output files */
  (void) fclose(fp1);
  (void) fclose(fp2);

  /* report output file sizes */
  printf("\n\nInput and Output file sizes should be the same.\n");
  printf("Please verify using OS commands\n");

  return OCI_SUCCESS;
}

/*----------------------------- Public functions ----------------------------*/

/*------------------------------- select_clob -------------------------------*/

/* select locator from the CLOB column */
sb4 select_clob(rowind)
int rowind;
{
  int   colc    = rowind;
  text *sqlstmt = (text *)"SELECT C1 FROM FOO WHERE C3 = :1 FOR UPDATE";
  /* we need the 'FOR UPDATE' clause since we need to write to the lobs */

  /* prepare select statement */
  if (OCIStmtPrepare(stmthp, errhp, sqlstmt, (ub4) strlen((char *)sqlstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() sqlstmt\n");
    return OCI_ERROR;
  }
  /* associate variable colc with bind placeholder #1 in the SQL statement */
  if (OCIBindByPos(stmthp, &bndhp, errhp, (ub4) 1,
                      (dvoid *) &colc, (sb4) sizeof(colc), SQLT_INT,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIBindByPos()\n");
    return OCI_ERROR;
  }
  /* associate clob var with its define handle */
  if (OCIDefineByPos(stmthp, &defnp1, errhp, (ub4) 1,
                     (dvoid *) &clob, (sb4) -1, (ub2) SQLT_CLOB,
                     (dvoid *) 0, (ub2 *) 0, (ub2 *) 0, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIDefineByPos() CLOB\n");
    return OCI_ERROR;
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

/*------------------------------- select_blob -------------------------------*/

/* select locator from the BLOB column */
sb4 select_blob(rowind)
int rowind;
{
  int   colc    = rowind;
  text *sqlstmt = (text *)"SELECT C2 FROM FOO WHERE C3 = :1 FOR UPDATE";
  /* we need the 'FOR UPDATE' clause since we need to write to the lobs */

  /* prepare select statement */
  if (OCIStmtPrepare(stmthp, errhp, sqlstmt, (ub4) strlen((char *)sqlstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() sqlstmt\n");
    return OCI_ERROR;
  }
  /* associate variable colc with bind placeholder #1 in the SQL statement */
  if (OCIBindByPos(stmthp, &bndhp, errhp, (ub4) 1,
                      (dvoid *) &colc, (sb4) sizeof(colc), SQLT_INT,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIBindByPos()\n");
    return OCI_ERROR;
  }
  /* associate blob var with its define handle */
  if (OCIDefineByPos(stmthp, &defnp2, errhp, (ub4) 1,
                     (dvoid *) &blob, (sb4) -1, (ub2) SQLT_BLOB,
                     (dvoid *) 0, (ub2 *) 0, (ub2 *) 0, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIDefineByPos()\n");
    return OCI_ERROR;
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

/*------------------------------- select_lobs -------------------------------*/

/* select lob locators from the CLOB, BLOB columns */
sb4 select_lobs(rowind)
int rowind;
{
  int   colc    = rowind;
  text *sqlstmt = (text *)"SELECT C1, C2 FROM FOO WHERE C3 = :1";
  /* we don't need the 'FOR UPDATE' clause since we are just reading the LOBs*/

  /* prepare select statement */
  if (OCIStmtPrepare(stmthp, errhp, sqlstmt, (ub4) strlen((char *)sqlstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() sqlstmt\n");
    return OCI_ERROR;
  }
  /* associate variable colc with bind placeholder #1 in the SQL statement */
  if (OCIBindByPos(stmthp, &bndhp, errhp, (ub4) 1,
                      (dvoid *) &colc, (sb4) sizeof(colc), SQLT_INT,
                      (dvoid *) 0, (ub2 *)0, (ub2 *)0,
                      (ub4) 0, (ub4 *) 0, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIBindByPos()\n");
    return OCI_ERROR;
  }
  /* associate clob and blob vars with their define handles */
  if (OCIDefineByPos(stmthp, &defnp1, errhp, (ub4) 1,
                     (dvoid *) &clob, (sb4) -1, (ub2) SQLT_CLOB,
                     (dvoid *) 0, (ub2 *) 0, (ub2 *) 0, (ub4) OCI_DEFAULT)
  ||  OCIDefineByPos(stmthp, &defnp2, errhp, (ub4) 2,
                     (dvoid *) &blob, (sb4) -1, (ub2) SQLT_BLOB,
                     (dvoid *) 0, (ub2 *) 0, (ub2 *) 0, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIDefineByPos()\n");
    return OCI_ERROR;
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

/*------------------------------- buf_write_lob -----------------------------*/

/*
 * Read operating system files into local buffers and then write these local
 * buffers to LOBs using buffering system.
 */
sb4 buf_write_lob(rowind, locator, fp, filelen)
int rowind;
OCILobLocator *locator;
FILE *fp;
ub4 filelen;
{
  ub4 offset = 1;
  ub1 bufp[MAXBUFLEN];
  ub4 amtp, nbytes;
  ub4 remainder = filelen;

  while (remainder > 0 && !feof(fp))
  {
    amtp = nbytes = (remainder > MAXBUFLEN) ? MAXBUFLEN : remainder;

    if (fread((void *)bufp, (size_t)nbytes, (size_t)1, fp) != 1)
    {
      (void) printf("ERROR: read file.\n");
      return OCI_ERROR;
    }

    if (OCILobWrite(svchp, errhp, locator, &amtp, offset, (dvoid *) bufp,
                   (ub4) nbytes, OCI_ONE_PIECE, (dvoid *)0,
                   (sb4 (*)(dvoid *, dvoid *, ub4 *, ub1 *)) 0,
                   (ub2) 0, (ub1) SQLCS_IMPLICIT) != OCI_SUCCESS)
    {
      (void) printf("ERROR: OCILobWrite()\n");
      report_error();
      return OCI_ERROR;
    }

    (void) printf("Wrote %d of %d bytes starting at offset %d;; ",
                  amtp, filelen, offset);

    remainder -= nbytes;
    offset += nbytes;
    (void) printf("%d remaining\n\n", remainder);
  }

  /* flush the buffers back to the server */
  (void) printf("\nFlush LOB's buffer to server\n");
  if (OCILobFlushBuffer(svchp, errhp, locator, OCI_LOB_BUFFER_NOFREE))
  {
    (void) printf("FAILED: OCILobFlushBuffer() \n");
    return OCI_ERROR;
  }
  (void) printf("\n\nBuffered Write done\n\n");
  return OCI_SUCCESS;
}

/*--------------------------------- buf_read_lob ----------------------------*/

/*
 * Read LOBs using buffered mode into local buffers and writes them into
 * operating system files.
 */
sb4 buf_read_lob(rowind, locator, fp)
int rowind;
OCILobLocator *locator;
FILE *fp;
{
  ub4 offset = 1;
  ub1 bufp[MAXBUFLEN];
  ub4 amtp = 0;
  ub4 nbytes = 0;
  ub4 filelen = 0;
  ub4 total = 0;

  /*
   * read from CLOB and write to text file (in the process, populating upto
   * 16 pages in the buffering subsystem).
   */

  /* set amount to be read per iteration */
  amtp = nbytes = MAXBUFLEN;

  while (amtp)
  {
    if (OCILobRead(svchp, errhp, locator, &amtp, (ub4) offset, (dvoid *) bufp,
                  (ub4) nbytes, (dvoid *)0,
                  (sb4 (*)(dvoid *, CONST dvoid *, ub4, ub1)) 0,
                  (ub2) 0, (ub1) SQLCS_IMPLICIT))
    {
      (void) printf("FAILED: OCILobRead() \n");
      report_error();
      return OCI_ERROR;
    }

    if (amtp > 0)
    {
      total += amtp;
      (void) printf("Read %d bytes starting at offset %d; Total %d\n\n",
                     amtp, offset, total);
      (void) fwrite((void *)bufp, (size_t)amtp, (size_t)1, fp);
       offset += nbytes;
    }
    else
       break;
  }
  printf("\n\nBuffered Read Done\n\n");
  return OCI_SUCCESS;
}

/*--------------------------------- drop_table ------------------------------*/

/* Drop table FOO before logging off from the server */
void drop_table()
{
  text *dropstmt = (text *) "DROP TABLE FOO";

  /* prepare drop statement */
  if (OCIStmtPrepare(stmthp, errhp, dropstmt, (ub4) strlen((char *) dropstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() dropstmt\n");
    return;
  }
  /* execute drop statement */
  if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                    (CONST OCISnapshot *) 0, (OCISnapshot *) 0,
                    (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtExecute() dropstmt\n");
    return;
  }
  return;
}

/*-------------------------------- report_error -----------------------------*/

/* retrieve error message and print it out */
void report_error()
{
  text msgbuf[512];
  sb4  errcode = 0;

  (void) OCIErrorGet((dvoid *) errhp, (ub4) 1, (text *) NULL, &errcode,
                       msgbuf, (ub4) sizeof(msgbuf), (ub4) OCI_HTYPE_ERROR);
  (void) printf("ERROR CODE = %d\n", errcode);
  (void) printf("%.*s\n", 512, msgbuf);
  return;
}

/*-------------------------------- file_length ------------------------------*/

/* get the length of the input file */
ub4 file_length(fp)
FILE *fp;
{
  fseek(fp, 0, SEEK_END);
  return (ub4) (ftell(fp));
}

/* end of file cdemolbs.c */
