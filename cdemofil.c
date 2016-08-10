#ifdef RCSID
static char *RCSid =
   "$Header: cdemofil.c 27-jul-2004.08:40:29 jchai Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1997, 2004, Oracle. All rights reserved.  
*/

/*

   NAME
     cdemofil.c - C Demo for ociFILe

   DESCRIPTION
     This file demonstrates an example of using the OCIFile package

   PUBLIC FUNCTION(S)
     <list of external functions declared/defined - with one-line descriptions>

   PRIVATE FUNCTION(S)
     <list of static functions defined in .c file - with one-line descriptions>

   RETURNS
     <function return values, for .c file with single function>

   NOTES
     <other useful comments, qualifications, etc.>

   MODIFIED   (MM/DD/YY)
   jchai       07/27/04 - 
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   nramakri    12/17/97 - Creation

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef OCI_ORACLE
#include <oci.h>
#endif

static sword checkerr(/*_ OCIError *errhp, sword status _*/);
int main(/*_ int argc, char *argv[] _*/);

int main(argc, argv)
int argc;
char *argv[];
{
  OCIEnv *envhp;
  OCIError *errhp;

  OCIFileObject *of1o, *of2o;
  OraText *f1name, *f1path, *f2name, *f2path;
  OraText buf1[500], buf2[50], buf3[500], buf4[500];
  ub4 bytes1, bytes2, i;
  ubig_ora f1len, f2len;
  ub1 flag; /* boolean */

  f1name = (OraText *)"test_file1.dat";
  f1path = (OraText *)NULL;

  f2name = (OraText *)"test_file2.dat";
  f2path = (OraText *)NULL;


  (void) OCIInitialize((ub4) OCI_DEFAULT, (dvoid *)0,
                       (dvoid * (*)(dvoid *, size_t)) 0,
                       (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                       (void (*)(dvoid *, dvoid *)) 0 );

  (void) OCIEnvInit( (OCIEnv **) &envhp, OCI_DEFAULT, (size_t) 0,
                     (dvoid **) 0 );

  (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &errhp, OCI_HTYPE_ERROR,
                         (size_t) 0, (dvoid **) 0);

  /* Initialize OCIFile */
  if (checkerr(errhp, OCIFileInit(envhp, errhp)) != OCI_SUCCESS)
    goto exit0;
  else
    printf("OCIFile initialized successfully\n");


  /* Create the file and open it for read & write: file1 */
  if (checkerr(errhp, OCIFileOpen(envhp, errhp, &of1o, f1name, f1path,
                                  OCI_FILE_READ_WRITE, OCI_FILE_CREATE,
                                  OCI_FILE_TEXT)) != OCI_SUCCESS)
    goto exit0;
  else
    printf("File %s opened successfully\n", f1name);

  /* fill buf4 with data to be written */
   for (i=0; i<500; i++)
     buf4[i] = 'a'+i%26;

  /* write buf2 */
  if (checkerr(errhp, OCIFileWrite(envhp, errhp, of1o, (void *)buf4,
                                   500, &bytes1))
              == OCI_SUCCESS)
  {
    printf("No of bytes written to file %s is %d\n", f1name, bytes1);
  }

  /* flush the buffers */
  checkerr(errhp, OCIFileFlush(envhp, errhp, of1o));

  /* Print out the length of file1 */
  if (checkerr(errhp, OCIFileGetLength(envhp, errhp, f1name, f1path, &f1len))
              == OCI_SUCCESS)
  {
    printf("The length of the file %s is %d\n",f1name, f1len);
  }

  /* Seek to the beginning of the file */
  if (checkerr(errhp, OCIFileSeek(envhp, errhp, of1o, OCI_FILE_SEEK_BEGINNING,
               0, OCI_FILE_FORWARD)) != OCI_SUCCESS)
    goto exit0;

  /* Read upto 400 bytes from test_file1.txt */
  if (checkerr(errhp, OCIFileRead(envhp, errhp, of1o, (void *)buf1,
                                  400, &bytes1)) == OCI_SUCCESS)
  {
    printf("No of bytes read from file %s is %d\n", f1name, bytes1);
  }

  flag = FALSE;

  /* Check whether file2 exists */
  checkerr(errhp, OCIFileExists(envhp, errhp, f2name, f2path, &flag));

  if (flag == FALSE)
   {
    /* File doesn't exist, Open for write in exclusive mode */
    /* skip printf, causes dif in tkpgoci2.tsc */ 
    /* printf("File %s does not exist\n", f2name); */
    if (checkerr(errhp, OCIFileOpen(envhp, errhp, &of2o, f2name, f2path,
                                    OCI_FILE_READ_WRITE, OCI_FILE_EXCL,
                                    OCI_FILE_TEXT)) != OCI_SUCCESS)
      goto exit0;
    else
      printf("File %s opened successfully\n", f2name);
  }
  else /* File exists, open in truncate mode */
  {
    /* skip printf, causes dif in tkpgoci2.tsc */ 
    /*printf("File %s exists \n", f2name); */
    if (checkerr(errhp, OCIFileOpen(envhp, errhp, &of2o, f2name, f2path,
                                    OCI_FILE_READ_WRITE, OCI_FILE_TRUNCATE,
                                    OCI_FILE_TEXT)) != OCI_SUCCESS)
      goto exit0;
    else
      printf("File %s opened successfully\n", f2name);
  }

  /* write from buf1 */
  if (checkerr(errhp, OCIFileWrite(envhp, errhp, of2o, buf1, bytes1, &bytes2))
              == OCI_SUCCESS)
  {
    printf("No of bytes written to file %s is %d\n", f2name, bytes2);
  }

  /* Seek to the end of the file */
  if (checkerr(errhp, OCIFileSeek(envhp, errhp, of2o, OCI_FILE_SEEK_END, 0,
                              OCI_FILE_FORWARD)) != OCI_SUCCESS)
    goto exit0;

  strcpy((char *)buf2, "This is the end of this file!");

  /* write buf2 */
  if (checkerr(errhp, OCIFileWrite(envhp, errhp, of2o, (void *)buf2,
                                   strlen((CONST char *)buf2), &bytes2))
              == OCI_SUCCESS)
    printf("No of bytes written to file %s is %d\n", f2name, bytes2);

  /* flush the buffers */
  checkerr(errhp, OCIFileFlush(envhp, errhp, of2o));

  /* Print out the length of the file */
  if (checkerr(errhp, OCIFileGetLength(envhp, errhp, f2name, f2path, &f2len))
              == OCI_SUCCESS)
    printf("The length of file %s is %d\n", f2name, f2len);

  /* Seek to the beginning of file2 */
  if (checkerr(errhp, OCIFileSeek(envhp, errhp, of2o, OCI_FILE_SEEK_BEGINNING,
               0, OCI_FILE_FORWARD)) != OCI_SUCCESS)
    goto exit0;

  /* Read all the data from test_file2.txt */
  if (checkerr(errhp, OCIFileRead(envhp, errhp, of2o, (void *)buf3,
                                  f2len, &bytes1)) == OCI_SUCCESS)
    printf("No of bytes read from file %s is %d\n", f2name, bytes1);

  /* append buf2 contents to buf1 */
  memcpy(buf1+400, buf2, bytes2);

  /* compare buf1 & buf3 */
  if (memcmp(buf1, buf3, bytes1) == 0)
    printf("Reads & Writes are successful\n");
  else
    printf("Error in data read/written\n");

  /* close the files and terminate OCIFile */
  checkerr(errhp, OCIFileClose(envhp, errhp, of2o));
  checkerr(errhp, OCIFileClose(envhp, errhp, of1o));
  checkerr(errhp, OCIFileTerm(envhp, errhp));

 exit0:
  if (errhp)
    (void) OCIHandleFree((dvoid *) errhp, OCI_HTYPE_ERROR);

  return 0;
}

sword checkerr(errhp, status)
OCIError *errhp;
sword status;
{
  OraText errbuf[512];
  sb4 errcode = 0;

  switch (status)
  {
  case OCI_SUCCESS:
    break;
  case OCI_ERROR:
    (void) OCIErrorGet((dvoid *)errhp, (ub4) 1, (OraText *) NULL, &errcode,
                        errbuf, (ub4) sizeof(errbuf), OCI_HTYPE_ERROR);
    (void) printf("Error - %.*s", 512, errbuf);
    break;
  case OCI_INVALID_HANDLE:
    (void) printf("Error - OCI_INVALID_HANDLE\n");
    break;
  default:
    break;
  }
  return status;
}

/* end of file cdemofil.c */

