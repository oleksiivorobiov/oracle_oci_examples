#ifdef RCSID
static char *RCSid =
   "$Header: cdemofor.c 14-jul-99.13:12:32 mjaeger Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1997, 1999, Oracle Corporation.  All rights reserved.
*/

/*

   NAME
     cdemofor.c - C Demo for ociFORmat

   DESCRIPTION
     This file demonstrates an example of using the OCIFormat package.

   PUBLIC FUNCTION(S)
     <list of external functions declared/defined - with one-line descriptions>

   PRIVATE FUNCTION(S)
     <list of static functions defined in .c file - with one-line descriptions>

   RETURNS
     <function return values, for .c file with single function>

   NOTES
     <other useful comments, qualifications, etc.>

   MODIFIED   (MM/DD/YY)
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   ewaugh      05/06/98 - OCIString renamed to OCIFormat.
   nramakri    12/17/97 - Creation

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <oci.h>

#define BUFLEN    100
#define FORMATLEN 100

struct
{
  text     buffer[BUFLEN];
  sbig_ora bytesWritten;
  text     formatString[FORMATLEN];
  text     style[3];
} MyDate[2];

sword checkerr(/*_ OCIError *errhp, sword status _*/);
int   main    (/*_ int argc, char *argv[] _*/);

int main(argc, argv)
int   argc;
char *argv[];
{
  OCIEnv   *envhp;
  OCIError *errhp;

  ub1 month = 12;
  ub1 day   = 31;
  ub1 year  = 97;

  uword i;

  strcpy((char *)MyDate[0].formatString, "%(2)02u/%(1)02u/%(3)02u");
  strcpy((char *)MyDate[0].style, "US");
  strcpy((char *)MyDate[1].formatString, "%(1)02u/%(2)02u/%(3)02u");
  strcpy((char *)MyDate[1].style, "NZ");

  (void) OCIInitialize(OCI_DEFAULT, (dvoid *)0,
                        (dvoid * (*)(dvoid *, size_t))0,
                        (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                        (void (*)(dvoid *, dvoid *))0);

  (void) OCIEnvInit((OCIEnv **)&envhp, OCI_DEFAULT, (size_t)0, (dvoid **)0);

  (void) OCIHandleAlloc((dvoid *)envhp, (dvoid **)&errhp, OCI_HTYPE_ERROR,
                        (size_t)0, (dvoid **)0);

  if (checkerr(errhp, OCIFormatInit(envhp, errhp)) != OCI_SUCCESS)
  {
    goto error;
  }

  for (i = 0 ; i < 2 ; i++)
  {
    if (checkerr(errhp, OCIFormatString(envhp, errhp, MyDate[i].buffer,
                                        BUFLEN, &MyDate[i].bytesWritten,
                                       (CONST text *)MyDate[i].formatString,
                                        OCIFormatUb1(day),
                                        OCIFormatUb1(month),
                                        OCIFormatUb1(year),
                                        OCIFormatEnd)) == OCI_SUCCESS)
    {
      (void) printf("New Year's eve in %s style is %s\n",
                   (char *)MyDate[i].style, (char *)MyDate[i].buffer);
    }
  }

  (void) checkerr(errhp, OCIFormatTerm(envhp, errhp));

error:
  if (errhp)
  {
    (void) OCIHandleFree((dvoid *)errhp, OCI_HTYPE_ERROR);
  }

  return 0;
}

sword checkerr(errhp, status)
OCIError *errhp;
sword     status;
{
  text errbuf[512];
  sb4  errcode = 0;

  switch (status)
  {
  case OCI_SUCCESS:
    break;
  case OCI_ERROR:
    (void) OCIErrorGet((dvoid *)errhp, (ub4)1, (text *)NULL,
                        &errcode,
                        errbuf, (ub4)sizeof(errbuf),
                        OCI_HTYPE_ERROR);
    (void) printf("Error - %.*s\n", (int)512, (char *)errbuf);
    break;
  case OCI_INVALID_HANDLE:
    (void) printf("Error - OCI_INVALID_HANDLE\n");
    break;
  default:
    break;
  }

  return status;
}

/* end of file cdemofor.c */

