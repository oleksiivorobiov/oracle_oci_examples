#ifdef RCSID
static char *RCSid =
   "$Header: cdemoses.c 08-dec-2000.12:35:31 lchidamb Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1998, 1999, Oracle Corporation.  All rights reserved.
*/

/*

   NAME
     cdemoses.c - C Demo program to illustrate Session Management

   DESCRIPTION
     This program Illustrates the following functionality:
     (1) Session Switching
      Session Switching allows applications to multiplex several users
      over the same connection. This allows apps to multiplex several
      sessions over one connection without losing the database privilege
      and security features.

      a) Create One Connection
      b) Create 10 sessions
      c) Switch one session after another followed by SELECT USER FROM DUAL

     (2) Session Migration
     Session Migration lets applications move sessions across connections.
     With this feature, the application can move sessions around
     dynamically based on system load and the application could implement
     its own application level user priority scheme.

      a) Create Two Connections
      b) Create 10 migratable sessions on first connection
      c) Switch one session after another followed by SELECT USER FROM DUAL
         On first connection.
      b) Migrate sessions to second connection and SEKECT USER FROM DUAL

   NOTES

     IMPORTANT!!!!
     TO RUN THIS DEMOS, YOU NEED TO RUN cdemoses.sql
     IMPORTANT!!!!


   MODIFIED   (MM/DD/YY)
   lchidamb    12/08/00 - lint
   lchidamb    12/05/00 - remove DISCARD
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   lchidamb    10/14/98 - Add Migration Demo
   lchidamb    10/13/98 - Session Management Demo
   lchidamb    10/13/98 - Creation

*/

/*---------------------------------------------------------------------*/
#include <oci.h>
#include <ocidfn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cdemoses.h>

#define EX_FAILURE -1
#define NUM_USERS 10
char *user[NUM_USERS] =
{  "user0",
   "user1",
   "user2",
   "user3",
   "user4",
   "user5",
   "user6",
   "user7",
   "user8",
   "user9"};

/* this statement is executed by all sessions */
char stmt[] = "SELECT USER FROM DUAL";

/* global username array: keeps track of current user session */
char username[100];

/* length of username */
ub2 userlen;

/* indicator for username */
sb2 userind;

int main ()
{
  sword retval = 0;
  ub2  i;
  OCIEnv    *envhp;
  OCIError  *errhp;
  OCIServer *srvhp;
  OCIServer *srvhp1;
  OCISession *userhp[10];
  OCISession *primary, *primary1;
  OCIStmt    *stmhp;

  /* Initialize OCI Process and Initialize Environment and Error handles */
  initialize_main(&envhp, &errhp);

  /* Initialize Server handle */
  initialize_server(envhp, errhp, &srvhp);


  /************************* SESSION SWITCHING DEMO **************************/
  fprintf(stdout, "----------SESSION SWITCHING DEMO-----------------------\n");
  /* Initialize Users */
  for(i = 0; i < NUM_USERS; i++)
    initialize_user(envhp, errhp, srvhp, &userhp[i], user[i]);

  /* Initialize Statement */
  initialize_statement(envhp, errhp, &stmhp);

  /* Switch Sessions and Execute Statements */
  for(i = 0; i < NUM_USERS; i++)
  {
    fprintf(stdout, "SWITCHING SESSION to %s\n", user[i]);
    execute_statement(envhp, errhp, userhp[i], srvhp, stmhp);
  }

  /* Switch Sessions and Terminate Users */
  for(i = 0; i < NUM_USERS; i++)
  {
    fprintf(stdout, "SWITCHING SESSION to %s\n", user[i]);
    terminate_user(envhp, errhp, srvhp, userhp[i]);
  }

  fprintf(stdout, "\n\n\n");
  /************************* SESSION MIGRATION DEMO **************************/
  fprintf(stdout, "----------SESSION MIGRATION DEMO-----------------------\n");
  /* Initialize Another Server handle */
  initialize_server(envhp, errhp, &srvhp1);

  /* Initialize non-migratable primary session on both servers */
  initialize_user(envhp, errhp, srvhp, &primary,  "primary");
  initialize_user(envhp, errhp, srvhp1,&primary1, "primary");


  /* Initialize Migratable Users on First Server */
  for(i = 0; i < NUM_USERS; i++)
    initialize_migratable_user(envhp, errhp, srvhp, primary,
                               &userhp[i], user[i]);

  /* Switch Sessions and Execute Statements on First Server */
  for(i = 0; i < NUM_USERS; i++)
  {
    fprintf(stdout, "SWITCHING SESSION to %s\n", user[i]);
    execute_statement(envhp, errhp, userhp[i], srvhp, stmhp);
  }


  /*
   * Oops, Imagine first connection is too loaded, lets move all
   * users to the second connection and leave the first connection
   * for doing some long operation.
   */

  fprintf(stdout, "\n--------MIGRATING ALL SESSIONS to SERVER2------------\n");
  /* Switch Sessions and Execute Statements on Second Server */
  for(i = 0; i < NUM_USERS; i++)
  {
    fprintf(stdout, "MIGRATING SESSION %s to SERVER2\n", user[i]);
    execute_statement(envhp, errhp, userhp[i], srvhp1, stmhp);
  }

  /* Terminate Statement */
  terminate_statement(stmhp);

  /* Migrate Sessions Back to Server1 and Terminate Users */
  fprintf(stdout, "\n-------MIGRATING ALL SESSIONS BACK to SERVER1--------\n");
  for(i = 0; i < NUM_USERS; i++)
  {
    fprintf(stdout, "MIGRATING SESSION %s BACK TO SERVER1\n", user[i]);
    terminate_user(envhp, errhp, srvhp, userhp[i]);
  }

  fprintf(stdout, "\n------TERMINATING PRIMARY SESSIONS-------------------\n");
  terminate_user(envhp, errhp, srvhp, primary);
  terminate_user(envhp, errhp, srvhp1, primary1);

  /* Terminate Servers */
  terminate_server(errhp, srvhp);
  terminate_server(errhp, srvhp1);

  /* Terminate Main */
  terminate_main(envhp, errhp);
  return 0;
}


/* initialize_user */
void initialize_user(envhp, errhp, srvhp, userhpp, name)
OCIEnv *envhp;
OCIError *errhp;
OCIServer *srvhp;
OCISession **userhpp;
char *name;
{
  OCISvcCtx *svchp;

  fprintf (stdout, "Authentication for %s is progress...0\n", name);

  /* Temporary Service Context */
  if (OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &svchp,
                   (ub4) OCI_HTYPE_SVCCTX,
                   (size_t) 0, (dvoid **) 0) != OCI_SUCCESS )
  {
    fprintf(stdout, "Fail to OCIHandleAlloc for service handle...");
    terminate_main(envhp, errhp);
    exit(EX_FAILURE);
  }

  if (OCIHandleAlloc( (dvoid *) envhp, (dvoid **) userhpp,
                     (ub4) OCI_HTYPE_SESSION,
                     (size_t) 0, (dvoid **) 0) != OCI_SUCCESS )
  {
    fprintf(stdout, "Fail to OCIHandleAlloc for user handle...");
    terminate_main(envhp, errhp);
    exit(EX_FAILURE);
  }



  /* Set the server handle in service handle */
  if (OCIAttrSet (svchp, OCI_HTYPE_SVCCTX, srvhp, 0,
                  OCI_ATTR_SERVER, errhp) != OCI_SUCCESS)
    error_report(errhp, "initialize_user - OCIAttrSet");

  /* set the username/password in user handle */
  if (OCIAttrSet(*userhpp, OCI_HTYPE_SESSION, name, strlen(name),
                 OCI_ATTR_USERNAME, errhp) != OCI_SUCCESS)
    error_report(errhp, "initialize_user - OCIAttrSet");

  if (OCIAttrSet(*userhpp, OCI_HTYPE_SESSION, name, strlen(name),
                 OCI_ATTR_PASSWORD, errhp) != OCI_SUCCESS)
    error_report(errhp, "initialize_user - OCIAttrSet");

  /* Authenticate */
  if (OCISessionBegin (svchp, errhp, *userhpp,
                       OCI_CRED_RDBMS, OCI_DEFAULT) != OCI_SUCCESS)
    error_report(errhp, "initialize_user - ocisauth");

  /* Free Temporary Service Context */
  (void) OCIHandleFree((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX);

  fprintf (stdout, "Authentication for %s successful.\n", name);
}


/* initialize_migratable_user */
void initialize_migratable_user(envhp, errhp, srvhp, primary, userhpp, name)
OCIEnv *envhp;
OCIError *errhp;
OCIServer *srvhp;
OCISession *primary;
OCISession **userhpp;
char *name;
{
  OCISvcCtx *svchp;

  fprintf (stdout, "Authentication for %s is progress...0\n", name);

  /* Temporary Service Context */
  if (OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &svchp,
                   (ub4) OCI_HTYPE_SVCCTX,
                   (size_t) 0, (dvoid **) 0) != OCI_SUCCESS )
  {
    fprintf(stdout, "Fail to OCIHandleAlloc for service handle...");
    terminate_main(envhp, errhp);
    exit(EX_FAILURE);
  }

  /* Set the Primary Session  handle in the service handle */
  if (OCIAttrSet(svchp, OCI_HTYPE_SVCCTX, primary, 0,
                 OCI_ATTR_SESSION, errhp) != OCI_SUCCESS)
    error_report(errhp, "conn2serv - OCIAttrSet");

  /* Set the server handle in service handle */
  if (OCIAttrSet (svchp, OCI_HTYPE_SVCCTX, srvhp, 0,
                  OCI_ATTR_SERVER, errhp) != OCI_SUCCESS)
    error_report(errhp, "conn2serv - OCIAttrSet");

  if (OCIHandleAlloc( (dvoid *) envhp, (dvoid **) userhpp,
                     (ub4) OCI_HTYPE_SESSION,
                     (size_t) 0, (dvoid **) 0) != OCI_SUCCESS )
  {
    fprintf(stdout, "Fail to OCIHandleAlloc for user handle...");
    terminate_main(envhp, errhp);
    exit(EX_FAILURE);
  }

  /* set the username/password in user handle */
  if (OCIAttrSet(*userhpp, OCI_HTYPE_SESSION, name, strlen(name),
                 OCI_ATTR_USERNAME, errhp) != OCI_SUCCESS)
    error_report(errhp, "conn2serv - OCIAttrSet");

  if (OCIAttrSet(*userhpp, OCI_HTYPE_SESSION, name, strlen(name),
                 OCI_ATTR_PASSWORD, errhp) != OCI_SUCCESS)
    error_report(errhp, "conn2serv - OCIAttrSet");

  /* Authenticate */
  if (OCISessionBegin (svchp, errhp, *userhpp,
                       OCI_CRED_RDBMS, OCI_MIGRATE) != OCI_SUCCESS)
    error_report(errhp, "conn2serv - ocisauth");

  /* Free Temporary Service Context */
  (void) OCIHandleFree((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX);

  fprintf (stdout, "Authentication for %s successful.\n", name);
}

/* terminate_user */
void terminate_user(envhp, errhp, srvhp, userhp)
OCIEnv *envhp;
OCIError *errhp;
OCIServer *srvhp;
OCISession *userhp;
{
  OCISvcCtx *svchp;
  /* Temporary Service Context */
  if (OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &svchp,
                   (ub4) OCI_HTYPE_SVCCTX,
                   (size_t) 0, (dvoid **) 0) != OCI_SUCCESS )
  {
    fprintf(stdout, "Fail to OCIHandleAlloc for service handle...");
    terminate_main(envhp, errhp);
    exit(EX_FAILURE);
  }

  fprintf(stdout, "Logging off...\n");

  /*****************************************************************/
  /**************** SWITCH SESSIONS BEGIN **************************/
  /* Set the server handle in service handle */
  if (OCIAttrSet (svchp, OCI_HTYPE_SVCCTX, srvhp, 0,
                  OCI_ATTR_SERVER, errhp) != OCI_SUCCESS)
    error_report(errhp, "conn2serv - OCIAttrSet");

  /* Set the Authentication handle in the service handle */
  if (OCIAttrSet(svchp, OCI_HTYPE_SVCCTX, userhp, 0,
                 OCI_ATTR_SESSION, errhp) != OCI_SUCCESS)
    error_report(errhp, "conn2serv - OCIAttrSet");
  /**************** SWITCH SESSIONS END ****************************/
  /*****************************************************************/
  if (OCISessionEnd(svchp, errhp, userhp, (ub4) 0))
  {
    error_report(errhp, (CONST text *)"logout: ologof");
  }

  /* Free Temporary Service Context */
  (void) OCIHandleFree((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX);
  fprintf(stdout, "Logged off.\n");
}

/* initialize_main */
void initialize_main(envhpp, errhpp)
OCIEnv **envhpp;
OCIError **errhpp;
{
  int i;

  if (OCIInitialize((ub4) OCI_DEFAULT, (dvoid *)0, (dvoid * (*)()) 0,
            (dvoid * (*)())0, (void (*)()) 0 ) != OCI_SUCCESS)
  {
    fprintf(stdout, "Fail to OCIInitialize...");
    exit(EX_FAILURE);
  }

  if (OCIEnvInit( (OCIEnv **) envhpp, (ub4) OCI_DEFAULT,
                   (size_t) 0, (dvoid **) 0 ) != OCI_SUCCESS)
  {
    fprintf(stdout, "Fail to OCIEnvInit for service handle...");
    exit(EX_FAILURE);
  }

  /* Get Error Handle */
  if (OCIHandleAlloc( (dvoid *) *envhpp, (dvoid **) errhpp,
                   (ub4) OCI_HTYPE_ERROR,
                   (size_t) 0, (dvoid **) 0) != OCI_SUCCESS)
  {
    fprintf(stdout, "Fail to OCIHandleAlloc for error handle...");
    exit(EX_FAILURE);
  }
  return;
}

/* terminate_main*/
void terminate_main(envhp, errhp)
OCIEnv    *envhp;
OCIError  *errhp;
{
  (void) OCIHandleFree((dvoid *) errhp, (ub4) OCI_HTYPE_ERROR);
  (void) OCIHandleFree((dvoid *) envhp, (ub4) OCI_HTYPE_ENV);
}


/* initialize_server */
void initialize_server(envhp, errhp, srvhpp)
OCIEnv *envhp;
OCIError *errhp;
OCIServer **srvhpp;
{
  if (OCIHandleAlloc( (dvoid *) envhp, (dvoid **) srvhpp,
                     (ub4) OCI_HTYPE_SERVER,
                     (size_t) 0, (dvoid **) 0) != OCI_SUCCESS )
  {
    fprintf(stdout, "Fail to OCIHandleAlloc for server handle...");
    exit(EX_FAILURE);
  }

  /* Initialize Server Handle */
  if (OCIServerAttach(*srvhpp, errhp, (text *)"", 0, 0) != OCI_SUCCESS )
    error_report(errhp, "conn2serv - OCIServerAttach");
}

/* terminate_server */
void terminate_server(errhp, srvhp)
OCIError  *errhp;
OCIServer *srvhp;
{
  (void) OCIServerDetach(srvhp, errhp, OCI_DEFAULT );
  (void) OCIHandleFree((dvoid *) srvhp, (ub4) OCI_HTYPE_SERVER);
}


/* initialize_statement */
void initialize_statement(envhp, errhp, stmhpp)
OCIEnv *envhp;
OCIError *errhp;
OCIStmt **stmhpp;
{
  OCIDefine *dfnhp = (OCIDefine *)NULL;

  /* Get statement handles */
  if (OCIHandleAlloc( (dvoid *)envhp, (dvoid **) stmhpp,
                   (ub4) OCI_HTYPE_STMT,
                   (size_t) 0, (dvoid **) 0) != OCI_SUCCESS )
  {
    fprintf(stdout, "Fail to OCIHandleAlloc for statement handle");
    terminate_main(envhp, errhp);
    exit(EX_FAILURE);
  }

  if (OCIStmtPrepare(*stmhpp, errhp, (text *)stmt,
                     (ub4)strlen(stmt),
                     (ub4)OCI_NTV_SYNTAX, (ub4)OCI_DEFAULT))
  {
    fprintf(stdout, "Fail to OCIHandleAlloc for statement handle");
    terminate_main(envhp, errhp);
    exit(EX_FAILURE);
  }

  /* OCIDefineByPos */
  if (OCIDefineByPos(*stmhpp, &dfnhp, errhp, (ub4)1,
                     (dvoid *)username, (sb4)sizeof(username), SQLT_CHR,
                    (   dvoid *)&userind, &userlen, (ub2 *)NULL,
                     (ub4)OCI_DEFAULT))
  {
    fprintf(stdout, "Fail to OCIHandleAlloc for statement handle");
    terminate_main(envhp, errhp);
    exit(EX_FAILURE);
  }

  fprintf(stdout, "");
}

/* terminate_statement */
void terminate_statement(stmhp)
OCIStmt *stmhp;
{
  (void) OCIHandleFree((dvoid *) stmhp, (ub4) OCI_HTYPE_STMT);
}

/* error_report */
void error_report(errhp, op)
OCIError *errhp;
CONST text *op;
{
  text  msgbuf[2000];
  sb4   errcode = 0;

  fprintf(stdout,"ORACLE error during %s\n", op);
  OCIErrorGet ((dvoid *) errhp, (ub4) 1, (text *) NULL, &errcode,
                    msgbuf, (ub4) sizeof(msgbuf), (ub4) OCI_HTYPE_ERROR);
  fprintf(stdout,"ERROR CODE = %d\n", errcode);
  fprintf(stdout,"%s\n", msgbuf);
  exit(EX_FAILURE);
}


void execute_statement(envhp, errhp, userhp, srvhp, stmhp)
OCIEnv *envhp;
OCIError *errhp;
OCISession *userhp;
OCIServer *srvhp;
OCIStmt *stmhp;
{
  OCISvcCtx *svchp;
  sword retval;

  /* Temporary Service Context */
  if (OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &svchp,
                   (ub4) OCI_HTYPE_SVCCTX,
                   (size_t) 0, (dvoid **) 0) != OCI_SUCCESS )
  {
    fprintf(stdout, "Fail to OCIHandleAlloc for service handle...");
    terminate_main(envhp, errhp);
    exit(EX_FAILURE);
  }

  /*****************************************************************/
  /**************** SWITCH SESSIONS BEGIN **************************/
  /* Set the server handle in service handle */
  if (OCIAttrSet (svchp, OCI_HTYPE_SVCCTX, srvhp, 0,
                  OCI_ATTR_SERVER, errhp) != OCI_SUCCESS)
    error_report(errhp, "conn2serv - OCIAttrSet");

  /* Set the Authentication handle in the service handle */
  if (OCIAttrSet(svchp, OCI_HTYPE_SVCCTX, userhp, 0,
                 OCI_ATTR_SESSION, errhp) != OCI_SUCCESS)
    error_report(errhp, "conn2serv - OCIAttrSet");
  /**************** SWITCH SESSIONS END ****************************/
  /*****************************************************************/


  /* Now attempt to select user from dual!!!!! */
  fprintf(stdout, "SELECT USER FROM DUAL\n");

  retval = OCIStmtExecute(svchp, stmhp, errhp, (ub4)1, (ub4)0,
                          (OCISnapshot*)0, (OCISnapshot*)0,
                          (ub4)OCI_DEFAULT);

  if (retval == OCI_SUCCESS)
  {
    fprintf(stdout, "User name of current session: %.*s\n",
                    userlen, username);
    memset((void *)username, 0,  sizeof(username));
  }
  else
  {
    error_report(errhp, (text *)"OCIStmtExecute");
    return;
  }

  /* Free Temporary Service Context */
  (void) OCIHandleFree((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX);
}

/* end of file cdemoses.c */

