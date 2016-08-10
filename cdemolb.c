#ifdef RCSID
static char *RCSid =
   "$Header: cdemolb.c 05-apr-2005.11:49:19 lzhao Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1996, 2005, Oracle. All rights reserved.  
*/

/*

   NAME
     cdemolb.c - C Demo program to illustrate the OCI Lob interface.

   DESCRIPTION
     This C file contains code to demonstrate the use of the OCI LOB
     (Large OBject) interface.  It provides a typical example of how
     OCI programs can be used to create/insert lob data, and then
     access and manipulate (read, write,copy, append, trim) the data.

   PUBLIC FUNCTION(S)
     <list of external functions declared/defined - with one-line descriptions>

   PRIVATE FUNCTION(S)
     <list of static functions defined in .c file - with one-line descriptions>

   RETURNS
     EX_SUCCESS - if the program terminates succesfully.
     EX_FAILURE - if it encounters any error.

   NOTES
     In the beta-1 release only Lob reads and writes (OCILobRead(),
     OCILobWrite()) with no callbacks have been demonstrated.
     More examples will be added in future releases.

     Before executing this program execute the following sql program:
        cdemolb.sql
     Executing this sql program will delete any earlier tables created
     by this demo and create new tables.  Each time the demo program runs
     successfully it adds row to tables. So it might be a good idea to run
     the sql program after several runs of the demo program.

   MODIFIED   (MM/DD/YY)
   lzhao       04/04/05 - bug4184359
   ani         02/19/02 - initialize OCIInd variables.
   ani         01/16/02 - add indicator variable to DefineByPos using SQLT_CHR
   dchatter    05/09/00 - lob read as char
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   svedala     10/01/98 - include stdlib.h - bug 714175
   svedala     09/09/98 - lines longer than 79 chars reformatted - bug 722491
   svedala     02/17/98 - OCI obsoletion changes
   cchau       03/03/97 - change functions to long names
   azhao       01/31/97 - removed isnull
   azhao       01/30/97 - fix lint error
   echen       01/03/97 - OCI beautification
   pshah       10/11/96 -
   kosinski    09/24/96 - Merge Win32 changes into base development
   aroy        07/22/96 - Demonstrate the OCI Lob interface.
   aroy        07/22/96 - Creation

*/

/*---------------------------------------------------------------------*/
#include <cdemolb.h>
#include <oci.h>
#include <ocidfn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main ()
{
  ldemodef *ctx;
  sword retval = 0;
  ub2  i;
  OCILobLocator *lobp;

  /* allocate and initialise ctx structure */
  ctx = alloc_handles();

  /* allocate lob descriptor */
  alloc_lob_desc(ctx, &lobp);

  /* Allocate statement/bind/define handles */
  alloc_stmt_handles (ctx, ctx->s1, 0, 0);       /* for the insert statement */
  alloc_stmt_handles (ctx, ctx->s2, 0, 2);       /* Selecting lob descriptor */

  /* Authenticate the user (connect to server) */
  authenticate_user(ctx);

  /* Insert/Select Lob locator */
  insert_select_loc(ctx, lobp);

  /* Write Data into Lob Locator */
  Write_to_loc(ctx, lobp);


  /* Read data from Lob Locator using OCILobRead() */
  Read_from_loc(ctx, lobp);

  /* free lob locator */
  (void) OCIDescriptorFree((dvoid *) lobp, (ub4) OCI_DTYPE_LOB);

  /* Read data from Lob Column using Char type */
  select_loc_data(ctx);

  /* Deauthenticate (logout) user */
  deauthenticate(ctx);

  /* Clean up */
  cleanup(ctx);
  return(EX_SUCCESS);
}

/* --------------------------------------------------------------------- */
/* Write data into the selected lob locator
*/
void Write_to_loc(ctx, lobp)
ldemodef *ctx;
OCILobLocator *lobp;
{
  FILE *fp;
  ub1  buf[BUFSIZE+1];
  ub4  offset;
  ub4  amtp;
  ub4 lenp;
  sb4 err;

  if ((fp = fopen (CDEMOLB_TEXT_FILE, "r"))==NULL)
  {
    COMMENT ("Cannot open file for reading");
    exit (EX_FAILURE);
  }

  offset = 1;                                  /* Offset for Lobs start at 1 */

  while (!feof(fp))
  {
    /* Read the data from file */
    memset ((void *)buf, '\0', BUFSIZE);
    fread((void *)buf, BUFSIZE, 1, fp);
    buf[BUFSIZE]='\0';
    /* printf("%s",buf); */

    /*Write it into the locator */
    amtp = BUFSIZE;                 /* IN/OUT : IN - amount if data to write */
    err = OCILobWrite (ctx->svchp, ctx->errhp, lobp, &amtp, offset,
                   (dvoid *) buf, (ub4) BUFSIZE, OCI_ONE_PIECE,
                   (dvoid *)0, (sb4 (*)()) 0,
                   (ub2) 0, (ub1) SQLCS_IMPLICIT);
    if (err == OCI_SUCCESS)
    {
      printf("Written some data...\n");
      offset += amtp;
    }
    else
    {
      fclose(fp);
      errrpt (ctx, (text *) "Write_to_loc : OCILobWrite");
    }
  }

  COMMENT("Write Successful");
  /* Length of the Lob */
  err = OCILobGetLength(ctx->svchp, ctx->errhp, lobp, &lenp);
  if (err != OCI_SUCCESS)
    printf ("   get lob length fails. err = %d\n\n", err);
  else
    printf ("   Written %d bytes into Locator Successfully.\n\n", lenp);

  fclose (fp);
}

/* --------------------------------------------------------------------- */
/* Read data from the lob locator
*/
void Read_from_loc(ctx, lobp)
ldemodef *ctx;
OCILobLocator *lobp;
{
  ub1  buf[BUFSIZE+1];
  ub4  offset;
  ub4  amtp;
  ub4 lenp=0;
  sb4 err;

  COMMENT("Read from the locator.");
  /* Length of the Lob */
  if (OCILobGetLength(ctx->svchp, ctx->errhp, lobp, &lenp) != OCI_SUCCESS)
    errrpt(ctx, "Read_from_loc: OCILobGetLength");
  else
  {
    printf("  Length of Locator is %d\n", lenp);
  }

  printf ("Enter the offset to read from (starting at 1): ");
  scanf("%d", &offset);
  printf ("Enter amount to read: ");
  scanf("%d", &amtp);

  if ((offset + amtp - 1) > lenp)
  {
    printf ("Error: Trying to get more data than available!\n");
    exit (EX_FAILURE);
  }
  printf(
    "\n------------------------------------------------------------------\n");

  /* Read the locator */
  do
  {
    memset ((dvoid *)buf, '\0', BUFSIZE);
    err = OCILobRead(ctx->svchp, ctx->errhp, lobp, &amtp, offset,
               (dvoid *) buf, (ub4)BUFSIZE , (dvoid *) 0,
               (OCICallbackLobRead) 0, (ub2) 0, (ub1) SQLCS_IMPLICIT);
    if (err == OCI_SUCCESS || err == OCI_NEED_DATA)
    {
      buf[BUFSIZE] = '\0';
      printf ("%s", buf);
      offset +=amtp;
    }
    else
      errrpt(ctx, "Read_from_loc : OCILobRead");
  }
  while (err == OCI_NEED_DATA);
  printf(
    "\n------------------------------------------------------------------\n");
  printf("\n");
}

/* --------------------------------------------------------------------- */
/* insert one row into table and select the lob descriptor.
*/
static
void alloc_stmt_handles (ctx, sptr, nbnd, ndfn)
ldemodef *ctx;
stmtdef *sptr;
sb2     nbnd;
sb2     ndfn;
{
  sb2 i;

  /* Get statement handles */
  if (OCIHandleAlloc( (dvoid *)(ctx->envhp), (dvoid **) &(sptr->stmhp),
                   (ub4) OCI_HTYPE_STMT,
                   (size_t) 0, (dvoid **) 0) != OCI_SUCCESS )
  {
    COMMENT("Fail to OCIHandleAlloc for statement handle");
    cleanup(ctx);
    exit(EX_FAILURE);
  }

  /* Get bind handles */

/*
  for (i=0; i<nbnd; i++)
  {
    if (OCIHandleAlloc( (dvoid *)(sptr->stmhp), (dvoid **) &(sptr->bndhp[i]),
                   (ub4) OCI_HTYPE_BIND,
                   (size_t) 0, (dvoid **) 0) != OCI_SUCCESS )
    {
      COMMENT("Fail to OCIHandleAlloc for bind handle");
      cleanup(ctx);
      exit(EX_FAILURE);
    }
  }
*/

  /* Get define handles */

/*
  for (i=0; i<ndfn; i++)
  {
    if (OCIHandleAlloc( (dvoid *)(sptr->stmhp), (dvoid **) &(sptr->dfnhp[i]),
                   (ub4) OCI_HTYPE_DEFINE,
                   (size_t) 0, (dvoid **) 0) != OCI_SUCCESS )
    {
      COMMENT("Fail to OCIHandleAlloc for define handle");
      cleanup(ctx);
      exit(EX_FAILURE);
    }
  }
*/

}

/* --------------------------------------------------------------------- */
/* insert one row into table and select the lob descriptor.
*/
static
void insert_select_loc(ctx, lobsrc)
ldemodef *ctx;
dvoid    *lobsrc;
{

  /* Insert an empty locator */
  if (OCIStmtPrepare (ctx->s1->stmhp, ctx->errhp,
                insstmt[0], (ub4)strlen ((char *)insstmt[0])+1,
                OCI_NTV_SYNTAX, OCI_DEFAULT) != OCI_SUCCESS)
    errrpt (ctx, "insert_select_loc: OCIStmtPrepare");

  if (OCIStmtExecute (ctx->svchp, ctx->s1->stmhp, ctx->errhp, 1, 0, 0, 0,
                  OCI_DEFAULT))
    errrpt (ctx, "insert_select_loc: OCIStmtExecute") ;

  printf("Inserted...\n");

  /* Now select the locator */
  if (OCIStmtPrepare (ctx->s2->stmhp, ctx->errhp,
                selstmt[0], (ub4)strlen ((char *)selstmt[0])+1,
                OCI_NTV_SYNTAX, OCI_DEFAULT))
    errrpt (ctx, "insert_select_loc: OCIStmtPrepare");


  /* Call define for each column of interest */
  if (OCIDefineByPos (ctx->s2->stmhp, &(ctx->s2->dfnhp[1]), ctx->errhp, 1,
                 (dvoid *)&lobsrc, 0 , SQLT_CLOB,
                 (dvoid *)0, (ub2 *)0, (ub2 *)0, OCI_DEFAULT))
    errrpt (ctx, "insert_select_loc: ocidefn");

  printf("About to select locator...\n");

  if (OCIStmtExecute (ctx->svchp, ctx->s2->stmhp, ctx->errhp, 1, 0, 0, 0,
                  OCI_DEFAULT))
    errrpt (ctx, "insert_select_loc: OCIStmtExecute") ;

}

/* --------------------------------------------------------------------- */
/* select the lob data.
*/

static void select_loc_data(ctx)
ldemodef *ctx;
{

  ub1 essay[1024];
  ub4 status = 0;
  sb2 outind = 0; 

  printf ("\nSelect Locator Data as SQLT_CHR\n");
  printf ("Reading first 1000 bytes\n\n");

  memset ((void*)essay, 0, 1024);

  /* Now select the locator */
  if (OCIStmtPrepare (ctx->s2->stmhp, ctx->errhp,
                selstmt[1], (ub4)strlen ((char *)selstmt[1])+1,
                OCI_NTV_SYNTAX, OCI_DEFAULT))
    errrpt (ctx, "select_loc_data: OCIStmtPrepare");

  /* Call define for each column of interest */
  if (OCIDefineByPos (ctx->s2->stmhp, &(ctx->s2->dfnhp[1]), ctx->errhp, 1,
                 (dvoid *)essay, 1000, SQLT_CHR,
                 (dvoid *) &outind, (ub2 *)0, (ub2 *)0, OCI_DEFAULT))
    errrpt (ctx, "select_loc_data: OCIDefineByPos");


  if (OCIStmtExecute (ctx->svchp, ctx->s2->stmhp, ctx->errhp, 0, 0, 0, 0,
                      OCI_DEFAULT))
    errrpt (ctx, "select_loc_data: OCIStmtExecute") ;

  do 
  {
    status = OCIStmtFetch(ctx->s2->stmhp, ctx->errhp, (ub4) 1, 
                        (ub4) OCI_FETCH_NEXT, (ub4) OCI_DEFAULT);
  
    if(outind !=0)      
      printf("Normal data not fetched, Output Indicator Value is %d\n",outind);

    if(status == OCI_SUCCESS || status == OCI_SUCCESS_WITH_INFO)
      printf ("Essay %s\n", essay);
    else
    {
      if (status != OCI_NO_DATA)
        errrpt (ctx, "on fetching the lob as char");
      break;
    }
  }while(0);

  printf ("End of Reading Locator as SQLT_CHR\n\n");
}


/*-------------------------------------------------------------------------*/
/* Authenticate users and connect to server
*/
static void authenticate_user(ctx)
ldemodef *ctx;
{
   COMMENT ("Authentication for scott is progress...");

   if (OCIServerAttach(ctx->srvhp, ctx->errhp, (CONST OraText *)"",
                 0, 0) != OCI_SUCCESS )
     errrpt(ctx, "conn2serv - OCIServerAttach");

   /* Set the server handle in service handle */
   if (OCIAttrSet (ctx->svchp, OCI_HTYPE_SVCCTX, ctx->srvhp, 0,
                   OCI_ATTR_SERVER, ctx->errhp) != OCI_SUCCESS)
     errrpt(ctx, "conn2serv - OCIAttrSet");

   /* set the username/password in user handle */
   if (OCIAttrSet(ctx->authp, OCI_HTYPE_SESSION, "scott", 5,
                  OCI_ATTR_USERNAME, ctx->errhp) != OCI_SUCCESS)
      errrpt(ctx, "conn2serv - OCIAttrSet");

   if (OCIAttrSet(ctx->authp, OCI_HTYPE_SESSION, "tiger", 5,
                  OCI_ATTR_PASSWORD, ctx->errhp) != OCI_SUCCESS)
      errrpt(ctx, "conn2serv - OCIAttrSet");

   /* Authenticate */
   if (OCISessionBegin (ctx->svchp, ctx->errhp, ctx->authp,
                  OCI_CRED_RDBMS, OCI_DEFAULT) != OCI_SUCCESS)
      errrpt(ctx, "conn2serv - ocisauth");

   COMMENT ("Authentication for scott successful.");

   /* Set the Authentication handle in the service handle */
   if (OCIAttrSet(ctx->svchp, OCI_HTYPE_SVCCTX, ctx->authp, 0,
                  OCI_ATTR_SESSION, ctx->errhp) != OCI_SUCCESS)
      errrpt(ctx, "conn2serv - OCIAttrSet");
}

/*-------------------------------------------------------------------------*/
/* create all tables.
*/
static void deauthenticate(ctx)
ldemodef *ctx;
{
  COMMENT ("Logging off...\n");
  if (OCISessionEnd(ctx->svchp, ctx->errhp, ctx->authp, (ub4) 0))
  {
    errrpt(ctx, (CONST text *)"logout: ologof");
  }
  COMMENT("Logged off.\n");
}

/*---------------------------------------------------------------*/
/*
** Allocate and initialise global and local context structures.
*/

static ldemodef *alloc_handles()
{
  ldemodef * csptr;
  int i;

  csptr = (ldemodef *) malloc(sizeof(ldemodef));
  if (csptr ==  (ldemodef *)  0)
  {
    COMMENT("Unable To Allocate Memory for ldemodef ...");
    exit(EX_FAILURE);
  }
  memset ((void *) csptr, '\0', sizeof(ldemodef));

  if (OCIInitialize((ub4) OCI_DEFAULT, (dvoid *)0, (dvoid * (*)()) 0,
            (dvoid * (*)())0, (void (*)()) 0 ) != OCI_SUCCESS)
  {
    COMMENT("Fail to OCIInitialize...");
    /* cleanup(csptr); */
    exit(EX_FAILURE);
  }

  if (OCIEnvInit( (OCIEnv **) &csptr->envhp, (ub4) OCI_DEFAULT,
                   (size_t) 0, (dvoid **) 0 ) != OCI_SUCCESS)
  {
    COMMENT("Fail to OCIEnvInit for service handle...");
    cleanup(csptr);
    exit(EX_FAILURE);
  }

  /* Get Error Handle */
  if (OCIHandleAlloc( (dvoid *) csptr->envhp, (dvoid **) &csptr->errhp,
                   (ub4) OCI_HTYPE_ERROR,
                   (size_t) 0, (dvoid **) 0) != OCI_SUCCESS)
  {
    COMMENT("Fail to OCIHandleAlloc for error handle...");
    cleanup(csptr);
    exit(EX_FAILURE);
  }

  /* server context */
  if (OCIHandleAlloc( (dvoid *) csptr->envhp, (dvoid **) &csptr->srvhp,
                   (ub4) OCI_HTYPE_SERVER,
                   (size_t) 0, (dvoid **) 0) != OCI_SUCCESS)
  {
    COMMENT("Fail to OCIHandleAlloc for server handle...");
    cleanup(csptr);
    exit(EX_FAILURE);
  }

  /* Service Context */
  if (OCIHandleAlloc( (dvoid *) csptr->envhp, (dvoid **) &csptr->svchp,
                   (ub4) OCI_HTYPE_SVCCTX,
                   (size_t) 0, (dvoid **) 0) != OCI_SUCCESS )
  {
    COMMENT("Fail to OCIHandleAlloc for service handle...");
    cleanup(csptr);
    exit(EX_FAILURE);
  }

  /* Auth Context */
  if (OCIHandleAlloc( (dvoid *) csptr->envhp, (dvoid **) &csptr->authp,
                   (ub4) OCI_HTYPE_SESSION,
                   (size_t) 0, (dvoid **) 0) != OCI_SUCCESS )
  {
    COMMENT("Fail to OCIHandleAlloc for user handle...");
    cleanup(csptr);
    exit(EX_FAILURE);
  }

  /*Stmtdef structures */
  csptr->s1 = (stmtdef *) malloc(sizeof(stmtdef));
  if (csptr->s1 ==  (stmtdef *)  0)
  {
    COMMENT("Unable To Allocate Memory for stmtdef ...");
    exit(EX_FAILURE);
  }
  memset ((void *) csptr->s1, '\0', sizeof(stmtdef));

  csptr->s2 = (stmtdef *) malloc(sizeof(stmtdef));
  if (csptr->s2 ==  (stmtdef *)  0)
  {
    COMMENT("Unable To Allocate Memory for stmtdef ...");
    exit(EX_FAILURE);
  }
  memset ((void *) csptr->s2, '\0', sizeof(stmtdef));
  return (csptr);

}

/* -------------------------------------------------------------- */
/*  Allocate lob descriptors.
*/
static void alloc_lob_desc(ctx, lobsrc)
  ldemodef *ctx;
  OCILobLocator     **lobsrc;
{
    if (OCIDescriptorAlloc((dvoid *) ctx->envhp, (dvoid **) lobsrc,
                 (ub4) OCI_DTYPE_LOB,
                 (size_t) 0, (dvoid **) 0) != OCI_SUCCESS )
    {
      errrpt(ctx, (CONST text *) "OCIDescriptorAlloc");
      cleanup(ctx);
      exit(EX_FAILURE);
    }
}

/* -------------------------------------------------------------- */
/*  Clean up all structures used.
*/

static void cleanup(ctx)
ldemodef *ctx;
{
  (void) OCIHandleFree((dvoid *) ctx->srvhp, (ub4) OCI_HTYPE_SERVER);
  (void) OCIHandleFree((dvoid *) ctx->svchp, (ub4) OCI_HTYPE_SVCCTX);
  (void) OCIHandleFree((dvoid *) ctx->errhp, (ub4) OCI_HTYPE_ERROR);
  (void) OCIHandleFree((dvoid *) ctx->authp, (ub4) OCI_HTYPE_SESSION);
  (void) OCIHandleFree((dvoid *) ctx->s1, (ub4) OCI_HTYPE_STMT);
  (void) OCIHandleFree((dvoid *) ctx->s2, (ub4) OCI_HTYPE_STMT);
}

/* ------------------------------------------------------------------------- */

/*
** Format the output error message and obtain error string from Oracle
** given the error code
*/
void errrpt(ctx, op)
     ldemodef *ctx;
     CONST text *op;
{
  text  msgbuf[LONGTEXTLENGTH];
  sb4   errcode = 0;

  fprintf(stdout,"ORACLE error during %s\n", op);
  OCIErrorGet ((dvoid *) ctx->errhp, (ub4) 1, (text *) NULL, &errcode,
                    msgbuf, (ub4) sizeof(msgbuf), (ub4) OCI_HTYPE_ERROR);
  fprintf(stdout,"ERROR CODE = %d\n", errcode);
  fprintf(stdout,"%s\n", msgbuf);
  exit(EX_FAILURE);
}

/* end of file cdemolb.c */

