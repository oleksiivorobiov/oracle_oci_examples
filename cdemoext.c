#ifdef RCSID
static char *RCSid =
   "$Header: cdemoext.c 14-jul-99.13:10:58 mjaeger Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1997, 1999,, 2000 Oracle Corporation.  All rights reserved.
*/

/*

   NAME
     cdemoext.c - C Demo for ociEXTract

   DESCRIPTION
     This file demonstrates an example of using the OCIExtract package

   PUBLIC FUNCTION(S)
     <list of external functions declared/defined - with one-line descriptions>

   PRIVATE FUNCTION(S)
     <list of static functions defined in .c file - with one-line descriptions>

   RETURNS
     <function return values, for .c file with single function>

   NOTES
     <other useful comments, qualifications, etc.>

   MODIFIED   (MM/DD/YY)
   tsaulys     01/05/00 - #(1078628) do not use strlist
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   nramakri    01/16/98 - add demos for OCINUM and List support
   nramakri    12/17/97 - Creation

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <oci.h>

#define NUMKEYS 9 /* Number of parameter keys */

/* Default values and ranges for various parameter keys */
static const sb4 dflt_intval  = 1234;
static const ub1 dflt_boolval = TRUE;

static const sb4 intrange[]   = {1234, 5678};
static const text *strlst[]   = {(const text *)"abcd",
                                 (const text *)"efghi",
                                 (const text *)"ijlkmn",
                                 (const text *)"mnopqrs",
                                 (const text *)"tuvwxyz",
                                 (const text *)0
                                };

/* strings containing parameters to be parsed */
static text *par_str[] = {(text *)"param2=hello param3=30",
                          (text *)"Param4=hi Param5=5555 Param6=tuvwxyz",
                          (text *)"param1=45 param7=f param9=3.1415"
                         };

/* file containing parameters to be parsed */
static text *par_file = (text *)"cdemoext.dat";

/* behavior flags to use while parsing parameter string */
static const ub4 par_str_flags[] = {0,
                                    OCI_EXTRACT_CASE_SENSITIVE,
                                    OCI_EXTRACT_APPEND_VALUES
                                   };

/* behavior flags to use while parsing parameter file */
static const ub4 par_file_flags = OCI_EXTRACT_APPEND_VALUES;

static sword checkerr(/*_ OCIError *errhp, sword status _*/);
int main(/*_ void _*/);

int main()
{
  OCIEnv *envhp;
  OCIError *errhp;
  sword status;
  sb4 outi;
  ub1 outb;
  text outs[100];
  OCINumber outn;
  uword scount;
  uword numkeys;
  text *keyname;
  ub1 keytype;
  uword keynumvals;
  dvoid **keyvalues;
  double dnum;

  /* Set up OCI */
  (void) OCIInitialize((ub4)OCI_OBJECT, (dvoid *)0,
                       (dvoid * (*)(dvoid *, size_t))0,
                       (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                       (void (*)(dvoid *, dvoid *))0 );

  (void) OCIEnvInit((OCIEnv **)&envhp, OCI_DEFAULT, (size_t)0, (dvoid **)0);

  (void) OCIHandleAlloc((dvoid *)envhp, (dvoid **)&errhp, OCI_HTYPE_ERROR,
                        (size_t)0, (dvoid **)0);

  /* Initialize OCI extraction package */
  if (checkerr(errhp, OCIExtractInit(envhp, errhp)) != OCI_SUCCESS)
    goto exit0;

  /* Set definitions for parameters to be extracted */
  if (checkerr(errhp, OCIExtractSetNumKeys(envhp, errhp, NUMKEYS+1))
              != OCI_SUCCESS)
    goto exit0;

  /* Reset parameter space, cancels previous setting */
  (void) checkerr(errhp, OCIExtractReset(envhp, errhp));

  if (checkerr(errhp, OCIExtractSetNumKeys(envhp, errhp, NUMKEYS))
              != OCI_SUCCESS)
    goto exit0;

  /* 1: integer parameter, with no default value */
  (void) checkerr(errhp, OCIExtractSetKey(envhp, errhp,
                                         (CONST text *)"Param1",
                                         (ub1)OCI_EXTRACT_TYPE_INTEGER,
                                         (ub4)OCI_EXTRACT_MULTIPLE,
                                         (dvoid *)0,
                                         (sb4 *)0,
                                         (CONST text *CONST *)0));

  /* 2: string parameter, with no default value */
  (void) checkerr(errhp, OCIExtractSetKey(envhp, errhp,
                                         (CONST text *)"Param2",
                                         (ub1)OCI_EXTRACT_TYPE_STRING,
                                         (ub4)0,
                                         (dvoid *)0,
                                         (sb4 *)0,
                                         (CONST text *CONST *)0));

  /* 3: integer parameter, with a default value */
  (void) checkerr(errhp, OCIExtractSetKey(envhp, errhp,
                                         (CONST text *)"Param3",
                                         (ub1)OCI_EXTRACT_TYPE_INTEGER,
                                         (ub4)0,
                                         (dvoid *)&dflt_intval,
                                         (sb4 *)0,
                                         (CONST text *CONST *)0));

  /* 4: string parameter, with a default value */
  (void) checkerr(errhp, OCIExtractSetKey(envhp, errhp,
                                         (CONST text *)"Param4",
                                         (ub1)OCI_EXTRACT_TYPE_STRING,
                                         (ub4)0,
                                         (dvoid *)"abcd",
                                         (sb4 *)0,
                                         (CONST text *CONST *)0));

  /* 5: integer parameter, with no default value, and allowable range */
  (void) checkerr(errhp, OCIExtractSetKey(envhp, errhp,
                                         (CONST text *)"Param5",
                                         (ub1)OCI_EXTRACT_TYPE_INTEGER,
                                         (ub4)0,
                                         (dvoid *)0,
                                         (sb4 *)intrange,
                                         (CONST text *CONST *)0));

  /* 6: string parameter, with a default value, and allowable list */
  (void) checkerr(errhp, OCIExtractSetKey(envhp, errhp,
                                         (CONST text *)"Param6",
                                         (ub1)OCI_EXTRACT_TYPE_STRING,
                                         (ub4)0,
                                         (dvoid *)"efghi",
                                         (sb4 *)0,
                                         (CONST text *CONST *)strlst));

  /* 7: boolean parameter, with no default value */
  (void) checkerr(errhp, OCIExtractSetKey(envhp, errhp,
                                         (CONST text *)"Param7",
                                         (ub1)OCI_EXTRACT_TYPE_BOOLEAN,
                                         (ub4)OCI_EXTRACT_MULTIPLE,
                                         (dvoid *)0,
                                         (sb4 *)0,
                                         (CONST text *CONST *)0));

  /* 8: boolean parameter, with a default value */
  (void) checkerr(errhp, OCIExtractSetKey(envhp, errhp,
                                         (CONST text *)"Param8",
                                         (ub1)OCI_EXTRACT_TYPE_BOOLEAN,
                                         (ub4)0,
                                         (dvoid *)&dflt_boolval,
                                         (sb4 *)0,
                                         (CONST text *CONST *)0));

  /* 9: ocinum parameter with no default value */
  (void) checkerr(errhp, OCIExtractSetKey(envhp, errhp,
                                  (CONST text *)"Param9",
                                   OCI_EXTRACT_TYPE_OCINUM,
                                   OCI_EXTRACT_MULTIPLE,
                                  (CONST dvoid *)0,
                                  (CONST sb4 *)0,
                                  (CONST text *CONST *)0));

  /* Extract parameters from a file */
  (void) checkerr(errhp, OCIExtractFromFile(envhp, errhp,
                                            par_file_flags, par_file));

  /* Extract parameters from strings */
  for (scount = 0; scount < 3; scount++)
    (void) checkerr(errhp, OCIExtractFromStr(envhp, errhp,
                                             par_str_flags[scount],
                                             par_str[scount]));

  /* Output some parameters */
  if (checkerr(errhp, OCIExtractToInt(envhp, errhp, (text *)"Param1",
                                      0, &outi)) == OCI_SUCCESS)
    printf("Param1 is %d\n", outi);

  if (checkerr(errhp, OCIExtractToStr(envhp, errhp, (text *)"Param6",
                                      0, outs, 100)) == OCI_SUCCESS)
    printf("Param6 is %s\n", outs);

  if (checkerr(errhp, OCIExtractToBool(envhp, errhp, (text *)"Param8",
                                       0, &outb)) == OCI_SUCCESS)
    printf("Param8 is %s\n", (outb == TRUE)?"true":"false");

  if (checkerr(errhp, OCIExtractToList(envhp, errhp, &numkeys))
              == OCI_SUCCESS)
  {
    /* Extract some key */
    (void) checkerr(errhp, OCIExtractFromList(envhp, errhp, 1,
                                              &keyname, &keytype,
                                              &keynumvals, &keyvalues));

    /* Print details about the extracted key */
    printf("The key extracted was \'%s\'\n", keyname);
    printf("%s contains %d ", keyname, keynumvals);
    switch(keytype)
    {
    case OCI_EXTRACT_TYPE_INTEGER:
      printf("integer values which are:\n");
      for (scount=0; scount < keynumvals; scount++)
        printf("%d\n", *(sb4 *)keyvalues[scount]);
      break;
    case OCI_EXTRACT_TYPE_BOOLEAN:
      printf("boolean values which are:\n");
      for (scount=0; scount < keynumvals; scount++)
        printf("%s\n", (*(ub1 *)keyvalues[scount]==TRUE)?"true":"false");
      break;
    case OCI_EXTRACT_TYPE_STRING:
      printf("string values which are:\n");
      for (scount=0; scount < keynumvals; scount++)
        printf("%s\n", (text *)keyvalues[scount]);
      break;
    case OCI_EXTRACT_TYPE_OCINUM:
      printf("OCINumber values which are:\n");
      for (scount=0; scount < keynumvals; scount++)
      {
        (void) checkerr(errhp,
                        OCINumberToReal(errhp,
                                       (CONST OCINumber *)keyvalues[scount],
                                       (uword)sizeof(dnum), (dvoid *)&dnum));
        (void) printf("%11.4f\n", dnum);
      }
     break;
    default:
      printf("unknown values\n");
      break;
    }
  }

  /* Terminate OCI extraction package */
  (void) checkerr(errhp, OCIExtractTerm(envhp, errhp));

 exit0:
  if (errhp)
    (void) OCIHandleFree((dvoid *)errhp, OCI_HTYPE_ERROR);

  return 0;
}

static sword checkerr(errhp, status)
OCIError *errhp;
sword status;
{
  text errbuf[512];
  sb4 errcode = 0;

  switch (status)
  {
  case OCI_SUCCESS:
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
  default:
    break;
  }
  return status;
}

/* end of file cdemoext.c */

