#ifdef RCSID
static char *RCSid =
   "$Header: cdemorid.c 14-jul-99.13:15:16 mjaeger Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1997, 1999, Oracle Corporation.  All rights reserved.
*/

/*

   NAME
     cdemorid - example of array DML and fetches using prefetch
                example of getting multiple rowids in one roundtrip


   DESCRIPTION
     test retrieves ROWIDs from table and use it for a subsequent
     UPDATE. 2 roundtrips, one to get all the rowids, and another to update it
     Does not require use of ROWID psuedo column in the text of the SQL.
     Uses OCI_COMMIT_ON_SUCCESS to cut down a roundtrip to commit the update.

   NOTES
     The ROWID used for INSERT, UPDATE, or DELETE must be obtained
     through a SELECT ... FOR UPDATE ... statement, not through a
     simple SELECT statement, using OCIGetAttr. Prefetching must be used.
     Not compatible with pre 8i servers.

     Please run cdemorid.sql before this.

   MODIFIED   (MM/DD/YY)
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   dchatter    10/16/98 - Creating new demo for select for update and update
                          using rowids in two roundtrips.
   dchatter    10/16/98 - Creation



*/

/*------------------------Inclusions-------------------------------*/

#ifndef CDEMORID
#include <cdemorid.h>
#endif

static boolean logged_on = FALSE;


static ub4  ridlen = 0;           /* for the one obtained from RDD */
static ub4  i;

/*------------------------end of Inclusions-----------------------------*/

int main(argc, argv)
int argc;
char *argv[];
{
  text *username = (text *)"CDEMORID";
  text *password = (text *)"CDEMORID";

  OCIEnv    *envhp;
  OCISvcCtx *svchp;
  OCIError  *errhp;
  OCIRowid *Rowid[MAXROWS];
  OCIStmt *select_p, *update_p;

  /*
   * Initialize the handles for the program
   */

  if (init_handles(&envhp, &errhp, (ub4)OCI_DEFAULT))
  {
    printf("FAILED: init_handles()\n");
    return cleanup(logged_on, envhp, svchp, errhp);
  }

  /*
   * Connect and create an user session
   */

  if (OCILogon(envhp, errhp, &svchp, username, strlen (username),
               password, strlen (password), "", 0))
  {
    printf("FAILED: log_on()\n");
    return cleanup(logged_on, envhp, svchp, errhp);
  }

  logged_on = TRUE;

  /* allocate the statement handles for Select and Update */

  if (OCIHandleAlloc((dvoid *)envhp, (dvoid **) &select_p,
                     (ub4)OCI_HTYPE_STMT, (CONST size_t) 0, (dvoid **) 0)
  ||  OCIHandleAlloc((dvoid *)envhp, (dvoid **) &update_p,
                     (ub4)OCI_HTYPE_STMT, (CONST size_t) 0, (dvoid **) 0))
  {
    printf("FAILED: alloc statement handles\n");
    return cleanup(logged_on, envhp, svchp, errhp);
  }

  for (i = 0; i < MAXROWS; i++)
    if (OCIDescriptorAlloc((dvoid *) envhp, (dvoid **) &(Rowid[i]),
                           (ub4) OCI_DTYPE_ROWID, (size_t) 0, (dvoid **) 0))
    {
      printf("FAILED: OCIDescriptorAlloc()\n");
      return cleanup(logged_on, envhp, svchp, errhp);
    }


  /*
   * First we select all the rows that are to be updated.
   * There are five rows that satisfy the example query and we get all the five
   * rows in one roundtrip since prefetch count is 10.
   */
  if (get_all_rows(svchp, errhp, select_p, Rowid))
  {
    printf("Fetch of all rowids failed\n");
    return cleanup(logged_on, envhp, svchp, errhp);
  }

  /*
   * We update five rows in one roundtrip by binding an array of rowids fetched
   * in the previous functions.
   * We also commit in the same roundtrip.
   * We do another roundtrip to fetch the updated rows for display although we
   * could have used the returning clause to even cut that roundtrip. See
   * cdemodr.c for details on how to use DML returning.
   */

  if (update_all_rows(svchp, errhp, update_p, select_p, Rowid))
  {
    printf("Update of all rows failed\n");
    return cleanup(logged_on, envhp, svchp, errhp);
  }

  return cleanup(logged_on, envhp, svchp, errhp);
}


/* ----------------------------------------------------------------- */
/* initialize environment, allocate handles                          */
/* ----------------------------------------------------------------- */
sword init_handles(envhp, errhp, init_mode)
OCIEnv **envhp;
OCIError **errhp;
ub4 init_mode;
{
  printf("Environment setup ....\n");

  if (OCIInitialize(init_mode, (dvoid *)0,
                    (dvoid * (*)(dvoid *, size_t)) 0,
                    (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                    (void (*)(dvoid *, dvoid *)) 0 ))
  {
    printf("FAILED: OCIInitialize()\n");
    return OCI_ERROR;
  }

  if (OCIEnvInit((OCIEnv **) envhp, (ub4) OCI_DEFAULT,
                 (size_t) 0, (dvoid **) 0 ))
  {
    printf("FAILED: OCIEnvInit()\n");
    return OCI_ERROR;
  }

  if (OCIHandleAlloc((dvoid *) *envhp, (dvoid **) errhp,
                     (ub4) OCI_HTYPE_ERROR, (size_t) 0, (dvoid **) 0))
  {
    printf("FAILED: OCIHandleAlloc() on errhp\n");
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}

/*---------------------------------------------------------------------*
 * cleanup()
 * disconnects and cleans up the allocated memory
 *---------------------------------------------------------------------*/

sword cleanup(loggedon, envhp, svchp, errhp)
boolean loggedon;
OCIEnv    *envhp;
OCISvcCtx *svchp;
OCIError  *errhp;
{

  report_error(errhp);

  if (loggedon)
    OCILogoff (svchp, errhp);

   printf("Freeing handles ...\n");

  if (envhp)
     OCIHandleFree((dvoid *) envhp, (ub4) OCI_HTYPE_ENV);

  return OCI_SUCCESS;
}


/* ----------------------------------------------------------------- */
void report_error(errhp)
OCIError *errhp;
{
  text  msgbuf[512];
  sb4   errcode = 0;

  memset((void *) msgbuf, (int)'\0', (size_t)512);

  OCIErrorGet((dvoid *) errhp, (ub4) 1, (text *) NULL, &errcode,
                       msgbuf, (ub4) sizeof(msgbuf), (ub4) OCI_HTYPE_ERROR);
  if (errcode)
  {
    printf("ERROR CODE = %d\n", errcode);
    printf("%.*s\n", 512, msgbuf);
    exit(1);
  }

}

/* ----------------------------------------------------------------- */
void checkerr(errhp, status)
OCIError *errhp;
sword     status;
{
  switch (status)
  {
  case OCI_SUCCESS: break;
  case OCI_SUCCESS_WITH_INFO:
    printf("status = OCI_SUCCESS_WITH_INFO\n");
    report_error(errhp);
    break;
  case OCI_NEED_DATA:
    printf("status = OCI_NEED_DATA\n");
    break;
  case OCI_NO_DATA:
    printf("status = OCI_NO_DATA\n");
    break;
  case OCI_ERROR:
    printf("status = OCI_ERROR\n");
    report_error(errhp);
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
}

sword get_all_rows(svchp, errhp, select_p, Rowid)
OCISvcCtx *svchp;
OCIError  *errhp;
OCIStmt   *select_p;
OCIRowid  **Rowid;

{


  text *mySql = (text *) "SELECT C1, C2 from FOO where C1 > 15 for UPDATE";
  ub4   prefetch = 10;
  sword errr;
  ub4   c1;
  text  c2[30];
  OCIDefine *defnp1, *defnp2;

  /* prepare the select statement for fetching all rows */

  if (OCIStmtPrepare (select_p, errhp,
                      mySql, strlen(mySql), OCI_NTV_SYNTAX, OCI_DEFAULT))
  {
    printf ("Prepare failed \n");
    return (OCI_ERROR);
  }

  /*
   * since we are interested in all the rows lets set prefetch to 10
   * before execute
   */


  if (OCIAttrSet (select_p,
                  OCI_HTYPE_STMT,
                  &prefetch,                        /* prefetch upto 10 rows */
                  0,
                  OCI_ATTR_PREFETCH_ROWS,
                  errhp))
  {
    printf ("Setting the prefetch count failed \n");
    return (OCI_ERROR);
  }


  if (errr = OCIStmtExecute(svchp,
                            select_p,
                            errhp,
                            (ub4) 0,
                            (ub4) 0,
                            (OCISnapshot *) NULL,
                            (OCISnapshot *) NULL,
                            (ub4) OCI_DEFAULT))
  {
    if (errr != OCI_NO_DATA) return errr;

  }


  /*
   * now fetch all the rows.
   * we will just define one piece of storage each for the columns and get the
   * data into it since we really do not care for the column data.
   * after each fetch we will retrieve the rowid of the fetched row.
   */

  if (OCIDefineByPos ( select_p,
                       &defnp1,
                       errhp,
                       1,
                       &c1,
                       sizeof (c1),
                       SQLT_INT,
                       (dvoid *) 0,
                       (dvoid *) 0,
                       (dvoid *) 0,
                       OCI_DEFAULT) ||
      OCIDefineByPos ( select_p,
                       &defnp2,
                       errhp,
                       2,
                       &c2,
                       30,
                       SQLT_STR,
                       (dvoid *) 0,
                       (dvoid *) 0,
                       (dvoid *) 0,
                       OCI_DEFAULT))
  {
    printf ("Failed to define\n");
    return (OCI_ERROR);
  }

  printf ("Column C1     Column C2\n");
  printf ("_______________________\n");

  for (i = 0; i < MAXROWS; i++)
  {
    if (OCIStmtFetch (select_p,
                      errhp,
                      1,
                      OCI_FETCH_NEXT,
                      OCI_DEFAULT))
    {
      printf ("Fetch failed \n");
      return (OCI_ERROR);
    }

    printf ("%d            %s\n", c1, c2);

    if (OCIAttrGet (select_p,
                    OCI_HTYPE_STMT,
                    Rowid[i],                      /* get the current rowid */
                    0,
                    OCI_ATTR_ROWID,
                    errhp))
    {
      printf ("Getting the Rowid failed \n");
      return (OCI_ERROR);
    }

  }

  return OCI_SUCCESS;
}


sword update_all_rows(svchp, errhp, update_p, select_p, Rowid)
OCISvcCtx *svchp;
OCIError  *errhp;
OCIStmt   *update_p;
OCIStmt   *select_p;
OCIRowid  **Rowid;

{


  text *mySql = (text *) "UPDATE FOO set c1 = c1 - 1 where rowid = :a";
  ub4   prefetch = 10;
  sword errr;
  ub4   c1;
  text  c2[30];
  OCIBind *bndhp;
  OCIDefine *defnp1,
            *defnp2;

  /* prepare the select statement for fetching all rows */

  if (OCIStmtPrepare (update_p, errhp,
                      mySql, strlen(mySql), OCI_NTV_SYNTAX, OCI_DEFAULT))
  {
    printf ("Prepare failed \n");
    return (OCI_ERROR);
  }

  if (OCIBindByPos ( update_p,
                     &bndhp,
                     errhp,
                     1,
                     &Rowid[0],
                     sizeof(OCIRowid *),
                     SQLT_RDD,
                     (ub2 *) 0,
                     (ub2 *) 0,
                     (ub4)   0,
                     (ub4)   0,
                     (ub4 *) 0,
                     (ub4)   OCI_DEFAULT))
  {
    printf ("Bind failed \n");
    return (OCI_ERROR);
  }

  if (errr = OCIStmtExecute(svchp,
                            update_p,
                            errhp,
                            (ub4) MAXROWS,
                            (ub4) 0,
                            (OCISnapshot *) NULL,
                            (OCISnapshot *) NULL,
                            (ub4) OCI_COMMIT_ON_SUCCESS))
  {
    printf ("Update failed \n");
    return (OCI_ERROR);
  }


  /*
   * now fetch all the rows for display.
   * we will just define one piece of storage each for the columns and get the
   * data into it since we really do not care for the column data.
   */

  if (errr = OCIStmtExecute(svchp,
                            select_p,
                            errhp,
                            (ub4) 0,
                            (ub4) 0,
                            (OCISnapshot *) NULL,
                            (OCISnapshot *) NULL,
                            (ub4) OCI_DEFAULT))
  {
    if (errr != OCI_NO_DATA) return errr;

  }

  if (OCIDefineByPos ( select_p,
                       &defnp1,
                       errhp,
                       1,
                       &c1,
                       sizeof (c1),
                       SQLT_INT,
                       (dvoid *) 0,
                       (dvoid *) 0,
                       (dvoid *) 0,
                       OCI_DEFAULT) ||
      OCIDefineByPos ( select_p,
                       &defnp2,
                       errhp,
                       2,
                       &c2,
                       30,
                       SQLT_STR,
                       (dvoid *) 0,
                       (dvoid *) 0,
                       (dvoid *) 0,
                       OCI_DEFAULT))
  {
    printf ("Failed to define\n");
    return (OCI_ERROR);
  }


  printf ("after UPDATE\n");
  printf ("Column C1     Column C2\n");
  printf ("_______________________\n");


  for (i = 0; i < MAXROWS; i++)
  {
    if (OCIStmtFetch (select_p,
                      errhp,
                      1,
                      OCI_FETCH_NEXT,
                      OCI_DEFAULT))
    {
      printf ("Fetch failed \n");
      return (OCI_ERROR);
    }

    printf ("%d            %s\n", c1, c2);

  }

  return OCI_SUCCESS;
}


/* end of file cdemorid.c */


