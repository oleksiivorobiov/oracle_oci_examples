#ifdef RCSID
static char *RCSid =
   "$Header: cdemolb2.c 10-oct-2006.14:39:58 azhao Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1996, 2006, Oracle. All rights reserved.  
*/

/*

   NAME
     cdemolb2.c - Demonstrates writing and reading of CLOB/BLOB columns
               with stream mode and with callback functions.

   DESCRIPTION
     This program takes 2 input files (the first a text file and the
     second a binary file) and stores the files into CLOB, BLOB columns.

     On output, the program reads the newly populated CLOB/BLOB columns
     and writes them to the output files (txtfile1.log, binfile1.log,
     txtfile2.log, binfile2.log), where

     txtfile1.log  --  created for stream reading CLOB contents to it
     binfile1.log  --  created for stream reading BLOB contents to it

     txtfile2.log  --  created for callback reading CLOB contents to it
     binfile2.log  --  created for callback reading BLOB contents to it


     Sample usage: cdemolb2 cdemolb.dat giffile.dat

     cdemolb.dat   --  a text file in the demo directory
     giffile.dat   --  a gif file in the demo directory

     After successful execution of the program, the files, cdemolb.dat,
     txtfile1.log, and txtfile2.log should be identical.  giffile.dat,
     binfile1.log, and binfile2.log should be identical.

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
   aliu        02/17/05 - fix lint issues 
   aliu        02/16/05 - fix bug 4184372 
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   svedala     09/09/98 - lines longer than 79 chars reformatted - bug 722491
   azhao       08/19/97 - replace OCIStmtBindByPos with OCIBindByPos
   azhao       06/25/97 - use rb, wb when opening bin file
   cchau       03/03/97 - change functions to long names
   azhao       01/31/97 - return OCI_ERROR if OCIStmtBindByPos fails
   azhao       01/30/97 - fix lint error
   azhao       12/31/96 - correct typo
   azhao       12/31/96 - Creation

*/

#include <stdio.h>
#include <oci.h>
#include <string.h>

static sb4 init_handles(/*_ void _*/);
static sb4 log_on(/*_ void _*/);
static sb4 setup_table(/*_ void _*/);
static sb4 select_locator(/*_ int rowind _*/);
static ub4 file_length(/*_ FILE *fp _*/);
static sb4 test_file_to_lob(/*_ int rowind, char *tfname, char *bfname _*/);
static void test_lob_to_file(/*_ int rowind _*/);
static void stream_write_lob(/*_ int rowind, OCILobLocator *lobl,
                                 FILE *fp, ub4 filelen _*/);
static void callback_write_lob(/*_ int rowind, OCILobLocator *lobl,
                                 FILE *fp, ub4 filelen _*/);
static void stream_read_lob(/*_ int rowind, OCILobLocator *lobl, FILE *fp _*/);
static void callback_read_lob(/*_ int rowind, OCILobLocator *lobl,
                                                               FILE *fp _*/);
static sb4 cbk_fill_buffer(/*_ dvoid *ctxp, dvoid *bufxp, ub4 *lenp,
                                ub1 *piece _*/);
static sb4 cbk_write_buffer(/*_ dvoid *ctxp, CONST dvoid *bufxp, ub4 lenp,
                                 ub1 piece _*/);

static void logout(/*_ void _*/);
static void drop_table(/*_ void _*/);
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
static OCILobLocator *clob, *blob;
static OCIDefine     *defnp1 = (OCIDefine *) 0, *defnp2 = (OCIDefine *) 0;
static OCIBind       *bndhp = (OCIBind *) 0;

static FILE *fp1, *fp2;

static ub4  txtfilelen = 0;
static ub4  binfilelen = 0;

static boolean  istxtfile;
static boolean  tab_exists = FALSE;

/*------------------------end of Inclusions-----------------------------*/

int main(argc, argv)
int argc;
char *argv[];
{
  int  rowind;

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

  if (setup_table())
  {
    (void) printf("FAILED: setup_table()\n");
    logout();
    return OCI_ERROR;
  }

  tab_exists = TRUE;

  for (rowind = 1; rowind <= 2; rowind++)
  {
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

  if (OCIDescriptorAlloc((dvoid *) envhp, (dvoid **) &clob,
                         (ub4)OCI_DTYPE_LOB, (size_t) 0, (dvoid **) 0))
  {
    (void) printf("FAILED: OCIDescriptorAlloc()\n");
    return OCI_ERROR;
  }

  /* allocate the lob locator variables */
  if (OCIDescriptorAlloc((dvoid *) envhp, (dvoid **) &blob,
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
  text *uid = (text *)"SCOTT";
  text *pwd = (text *)"tiger";
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

sb4 setup_table()
{
  int colc;

  text  *crtstmt = (text *) "CREATE TABLE FOO (A CLOB, B BLOB, C INTEGER)";
  text  *insstmt =
          (text *) "INSERT INTO FOO VALUES (EMPTY_CLOB(), EMPTY_BLOB(), :1)";

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
  text  *sqlstmt = (text *)"SELECT A, B FROM FOO WHERE C = :1 FOR UPDATE";

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



/* ----------------------------------------------------------------- */
/* Read operating system files into local buffers and then write the */
/* buffers to lobs.                                                  */
/* ----------------------------------------------------------------- */

sb4 test_file_to_lob(int rowind, char *txtfile, char *binfile)
{
  (void) printf("\n===> Testing loading files into lobs .....\n\n");

  fp1 = fopen((const char *)txtfile, (const char *) "r");
  fp2 = fopen((const char *)binfile, (const char *) "rb");

  if ( !(fp1 && fp2))
  {
    (void) printf("ERROR: Failed to open file(s).\n");
    return -1;
  }

  txtfilelen = file_length(fp1);
  binfilelen = file_length(fp2);

  switch (rowind)
  {
    case 1:
       stream_write_lob(rowind, clob, fp1, txtfilelen);
       stream_write_lob(rowind, blob, fp2, binfilelen);
       break;
    case 2:
       istxtfile = TRUE;
       callback_write_lob(rowind, clob, fp1, txtfilelen);
       istxtfile = FALSE;
       callback_write_lob(rowind, blob, fp2, binfilelen);
       break;
    default:
       (void) printf("ERROR: Invalid row indicator.\n");
       break;
  }

  (void) fclose(fp1);
  (void) fclose(fp2);

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
                            (OCICallbackLobWrite) 0,
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
                    (OCICallbackLobWrite) 0,
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
                          (OCICallbackLobWrite) 0,
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
/* Read operating system files into local buffers and then write the */
/* buffers to lobs using callback function.                          */
/* ----------------------------------------------------------------- */

void callback_write_lob(int rowind, OCILobLocator *lobl, FILE *fp, ub4 filelen)
{
  ub4   offset = 1;
  ub4   loblen = 0;
  ub1   bufp[MAXBUFLEN];
  ub4   amtp = filelen;
  ub4   nbytes;
  sword retval;

  (void) printf("--> To do callback write lob, amount = %d\n", filelen);

  (void) OCILobGetLength(svchp, errhp, lobl, &loblen);
  (void) printf("Before callback write, LOB length = %d\n\n", loblen);

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

  if (filelen < MAXBUFLEN)       /* exactly one piece in the file */
  {
    (void) printf("Only one piece, no need for callback write.\n");
    if (retval = OCILobWrite(svchp, errhp, lobl, &amtp, offset, (dvoid *) bufp,
                            (ub4) nbytes, OCI_ONE_PIECE, (dvoid *)0,
                            (OCICallbackLobWrite) 0,
                            (ub2) 0, (ub1) SQLCS_IMPLICIT) != OCI_SUCCESS)
    {
      (void) printf("ERROR: OCILobWrite().\n");
      return;
    }
  }
  else                     /* more than one piece */
  {
    if (retval = OCILobWrite(svchp, errhp, lobl, &amtp, offset, (dvoid *)bufp,
                            (ub4)nbytes, OCI_FIRST_PIECE, (dvoid *)0,
                            cbk_fill_buffer, (ub2) 0, (ub1) SQLCS_IMPLICIT))
    {
      (void) printf("ERROR: OCILobWrite().\n");
      report_error();
      return;
    }
  }

  (void) OCILobGetLength(svchp, errhp, lobl, &loblen);
  (void) printf("After callback write, LOB length = %d\n\n", loblen);

  return;
}



/* ----------------------------------------------------------------- */
/* callback function to read the file into buffer.                   */
/* ----------------------------------------------------------------- */

sb4 cbk_fill_buffer(ctxp, bufxp, lenp, piece)
  dvoid *ctxp;
  dvoid *bufxp;
  ub4 *lenp;
  ub1 *piece;
{
  FILE   *fp = (istxtfile ? fp1 : fp2);
  ub4    filelen = (istxtfile ? txtfilelen : binfilelen);
  ub4    nbytes;
  static ub4 len = MAXBUFLEN;     /* because 1st piece has been written */


  if ((filelen - len) > MAXBUFLEN)
    nbytes = MAXBUFLEN;
  else
    nbytes = filelen - len;

  *lenp = nbytes;

  if (fread((void *)bufxp, (size_t)nbytes, 1, fp) != 1)
  {
    (void) printf("ERROR: read file. Abort callback fill buffer\n");
    *piece = OCI_LAST_PIECE;
    len = MAXBUFLEN;         /* reset it for the next callback_write_lob() */
    return OCI_CONTINUE;
  }

  len += nbytes;

  if (len == filelen)         /* entire file has been read */
  {
    *piece = OCI_LAST_PIECE;
    len = MAXBUFLEN;          /* reset it for the next callback_write_lob() */
  }
  else
    *piece = OCI_NEXT_PIECE;

  return OCI_CONTINUE;
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
  text  txtfilename[20], binfilename[20];

  (void) sprintf((char *) txtfilename, (char *)"txtfile%d.log", rowind);
  (void) sprintf((char *) binfilename, (char *)"binfile%d.log", rowind);

  (void) printf("\n===> Testing writing lobs to files .....\n\n");

  fp1 = fopen((char *)txtfilename, (const char *) "w");
  fp2 = fopen((char *)binfilename, (const char *) "wb");

  if ( !(fp1 && fp2))
  {
    (void) printf("ERROR: Failed to open file(s).\n");
    return;
  }

  switch (rowind)
  {
    case 1:
       stream_read_lob(rowind, clob, fp1);
       stream_read_lob(rowind, blob, fp2);
       break;
    case 2:
       istxtfile = TRUE;
       callback_read_lob(rowind, clob, fp1);

       istxtfile = FALSE;
       callback_read_lob(rowind, blob, fp2);
       break;
    default:
       (void) printf("ERROR: Invalid row indicator.\n");
       break;
  }

  (void) fclose(fp1);
  (void) fclose(fp2);

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

  memset((void *)bufp, '\0', MAXBUFLEN);

  retval = OCILobRead(svchp, errhp, lobl, &amtp, offset, (dvoid *) bufp,
                     (loblen < MAXBUFLEN ? loblen : MAXBUFLEN), (dvoid *)0,
                     (OCICallbackLobRead) 0,
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

      (void) fwrite((void *)bufp, MAXBUFLEN, 1, fp); /* full buffer to write */

      do
      {
        memset((void *)bufp, '\0', MAXBUFLEN);
        amtp = 0;

        remainder -= MAXBUFLEN;

        retval = OCILobRead(svchp, errhp, lobl, &amtp, offset, (dvoid *) bufp,
                           (ub4) MAXBUFLEN, (dvoid *)0,
                           (OCICallbackLobRead) 0,
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


/* ----------------------------------------------------------------- */
/* Read lobs using callback function into local buffers and          */
/* then write them to operating system files.                        */
/* ----------------------------------------------------------------- */

void callback_read_lob(int rowind, OCILobLocator *lobl, FILE *fp)
{
  ub4   offset = 1;
  ub4   loblen = 0;
  ub1   bufp[MAXBUFLEN];
  ub4   amtp = 0;
  sword retval;

  (void) OCILobGetLength(svchp, errhp, lobl, &loblen);
  amtp = loblen;

  (void) printf("--> To callback read LOB, loblen = %d.\n", loblen);

  if (retval = OCILobRead(svchp, errhp, lobl, &amtp, offset, (dvoid *) bufp,
                         (ub4) MAXBUFLEN, (dvoid *) bufp, 
                         (OCICallbackLobRead) cbk_write_buffer,
                         (ub2) 0, (ub1) SQLCS_IMPLICIT))
  {
    (void) printf("ERROR: OCILobRead() LOB.\n");
    report_error();
  }

  return;
}


/* ----------------------------------------------------------------- */
/* callback function to write buffer to the file.                    */
/* ----------------------------------------------------------------- */

sb4 cbk_write_buffer(ctxp, bufxp, lenp, piece)
  dvoid *ctxp;
  CONST dvoid *bufxp;
  ub4 lenp;
  ub1 piece;
{
  static ub4 piece_count = 0;
  FILE *fp = (istxtfile ? fp1 : fp2);

  piece_count++;

  switch (piece)
  {
    case OCI_LAST_PIECE:
      (void) fwrite((void *)bufxp, (size_t)lenp, 1, fp);
      (void) printf("callback read the %d th piece\n\n", piece_count);
      piece_count = 0;
      return OCI_CONTINUE;

    case OCI_FIRST_PIECE:
    case OCI_NEXT_PIECE:
      (void) fwrite((void *)bufxp, (size_t)lenp, 1, fp);
      break;
    default:
      (void) printf("callback read error: unkown piece = %d.\n", piece);
      return OCI_ERROR;
  }

  (void) printf("callback read the %d th piece\n", piece_count);

  return OCI_CONTINUE;
}


/*-------------------------------------------------------------------*/
/* Drop table FOO before logging off from the server.                */
/*-------------------------------------------------------------------*/

void drop_table()
{
  text  *sqlstmt = (text *) "DROP TABLE FOO";

  if (OCIStmtPrepare(stmthp, errhp, sqlstmt, (ub4) strlen((char *) sqlstmt),
                    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    (void) printf("FAILED: OCIStmtPrepare() sqlstmt\n");
    return;
  }

  if (OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                    (CONST OCISnapshot *) 0, (OCISnapshot *) 0,
                    (ub4) OCI_DEFAULT))
    (void) printf("FAILED: OCIStmtExecute() sqlstmt\n");

  return;
}

/*-------------------------------------------------------------------*/
/* Logoff and disconnect from the server.  Free handles.             */
/*-------------------------------------------------------------------*/

void logout()
{
  if (tab_exists)
    drop_table();

  (void) OCISessionEnd(svchp, errhp, authp, (ub4) 0);
  (void) OCIServerDetach(srvhp, errhp, (ub4) OCI_DEFAULT);

  (void) printf("Logged off and detached from server.\n");

  (void) OCIHandleFree((dvoid *) srvhp, (ub4) OCI_HTYPE_SERVER);
  (void) OCIHandleFree((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX);
  (void) OCIHandleFree((dvoid *) errhp, (ub4) OCI_HTYPE_ERROR);
  (void) OCIHandleFree((dvoid *) authp, (ub4) OCI_HTYPE_SESSION);
  (void) OCIDescriptorFree((dvoid *) clob, (ub4) OCI_DTYPE_LOB);
  (void) OCIDescriptorFree((dvoid *) blob, (ub4) OCI_DTYPE_LOB);
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

