#ifdef RCSID
static char *RCSid =
   "$Header: cdemodt.c 10-oct-2006.14:39:59 azhao Exp $ ";
#endif /* RCSID */

/* Copyright (c) 2001, 2006, Oracle. All rights reserved.  
*/

/*

   NAME
     cdemodt.c - Datetime/Interval type demo program in OCI

   DESCRIPTION - Demonstrate insertion and selection of timestamp, timestamp
                 with time zone, timestamp with local time zone, interval year
                 to month, and interval day to seconds via Bind and Define with 
                 SQL statements.
                 Demonstrate IN/OUT bind with PL/SQL procedure.
                 Demonstrate how time zone names work and daylight saving features
                 using OCIDateTimeConvert() on different values of timestamp with 
                 time zone region.
                 Demonstrate how daylight saving works with time zone names when
                 time changes from daylight saving time to standard time or vice
                 verse using OCIDateTimeIntervalAdd().

         Note:   Before running this program, ensure that the database is started
                 up with compatible=9.0.0.0.0 and a table CDEMODT_FOO or a procedure 
                 CDEMODT_INS_SEL does not exist in the SCOTT/TIGER sample account.


   MODIFIED   (MM/DD/YY)
   azhao      10/10/06 - case-senstive password change
   aliu       06/27/01 - Change table and procedure names to be free of conflict with other tests.
   aliu       06/12/01 - Fix porting bug 1827600.
   aliu       04/24/01 - Merged aliu_dt_demo_oci
   aliu       04/24/01 - Modify variable names and main prototype.
   aliu       04/23/01 - Update lint warnings.
   aliu       04/12/00 - Creation

*/

#ifndef CDEMOXML
#define CDEMOXML

/*------------------------------------------------------------------------
 * Include Files
 */

#ifndef STDIO
#include <stdio.h>
#endif

#ifndef STDLIB
#include <stdlib.h>
#endif

#ifndef STRING
#include <string.h>
#endif

#ifndef OCI_ORACLE
#include <oci.h>
#endif

/*----------------- End of including files -----------------*/

/*--------------------- Public Constants and Variables ----------------------*/

/* constants */
#define MAXDATELEN 64
#define COLNUM 6

/* database login information */
static OraText *user=(OraText *)"SCOTT";
static OraText *password=(OraText *)"tiger";

/* OCI Handles and Variables */
static OCIEnv        *envhp;
static OCIServer     *srvhp;
static OCISvcCtx     *svchp;
static OCIError      *errhp;
static OCISession    *authp;
static OCIBind       *bndhp[COLNUM] = { (OCIBind *)0, (OCIBind *)0, (OCIBind *)0,
                                        (OCIBind *)0, (OCIBind *)0, (OCIBind *)0 }; 
static OCIDefine     *defhp[COLNUM] = { (OCIDefine *)0, (OCIDefine *)0, (OCIDefine *)0,
                                        (OCIDefine *)0, (OCIDefine *)0, (OCIDefine *)0 }; 
static OCIStmt       *stmthp = (OCIStmt *) 0;

/* Misellaneous */
static boolean        ret = FALSE;
static boolean        tab_exists = FALSE;
static boolean        proc_exists = FALSE;
static ub2            sqlt[COLNUM] = {SQLT_TIMESTAMP, SQLT_TIMESTAMP_TZ, SQLT_TIMESTAMP_LTZ,
                                      SQLT_INTERVAL_YM, SQLT_INTERVAL_DS, SQLT_INT};
static ub4            ocit[COLNUM-1] = { OCI_DTYPE_TIMESTAMP, OCI_DTYPE_TIMESTAMP_TZ,
                                         OCI_DTYPE_TIMESTAMP_LTZ, OCI_DTYPE_INTERVAL_YM, 
                                         OCI_DTYPE_INTERVAL_DS };

/*----------------- End of Constants and Variables -----------------*/

/*--------------------- Functions Declaration --------------------*/

int main(/*_ int argc, char *argv[] _*/);
static sb4 add_interval(/* OraText *dt, OraText *interval, OraText *fmt, ub4 type1, ub4 type2 _*/);
static sb4 alloc_desc(/*_ OCIDateTime *dvar[], OCIInterval *ivar[], sword size1, sword size2 _*/);
static void chk_err(/*_ OCIError *errhp, boolean ec, sb2 linenum, OraText *comment _*/);
static void cleanup(/*_ void _*/);
static sb4 connect_server(/*_ OraText *cstring _*/);
static sb4 conv_dt(/*_ OraText *str1, ub4 type1, ub4 type2, OraText *fmt1,
   OraText *fmt2, OraText *name1, OraText *name2 _*/);
static void disconnect_server(/*_ void _*/);
static sb4 exe_insert(/*_ OraText *insstmt, OraText *strdate[COLNUM-1], 
   OraText *fmtdate[3], OCIDateTime *dvar[], OCIInterval *ivar[], size_t rownum_*/);
static sb4 exe_sql(/*_ OraText *stmt _*/);
static sb4 free_desc(/*_ OCIDateTime *dvar[], OCIInterval *ivar[], sword size1, sword size2 _*/);
static sb4 init_env_handle(/*_ void _*/);
static sb4 init_sql(/*_ void _*/);
static sb4 insert_dt(/*_ size_t rownum _*/);
static sb4 io_proc(/*_ _*/);
static sb4 select_dt(/*_ size_t rownum _*/);
static sb4 set_sdtz();
static sb4 tzds_1();
static sb4 tzds_2();

/*--------------------- End of Functions Declaration --------------------*/

#endif


/*---------------------------Main function -----------------------------*/
int main(argc, argv)
int argc;
char *argv[];
{
 OraText cstring[50];
 size_t rownum = 1;

 memset((void *)cstring, '\0', 50);

 if (argc > 1 && strncmp((char *) "remote", (char *)argv[1], 6) == 0)
 {
   strcpy((char *)cstring, "inst1_alias");
 }

 /* Initialize the environment and allocate handles */
 if (init_env_handle())
 {
   printf("FAILED: init_env_handle()!\n");
   return OCI_ERROR;
 }

 /* Log on to the server and begin a session */
 if (connect_server(cstring))
 {
   printf("FAILED: connect_server()!\n");
   cleanup();
   return OCI_ERROR;
 }

 /* Initialize the demo table and PL/SQL procedure */
 if (init_sql())
 {
   printf("FAILED: init_sql()\n");
   disconnect_server();
   return OCI_ERROR;
 }

 /* Set the session datetime zone */
 if (set_sdtz())
 {
   printf("FAILED: set_sdtz()\n");
   disconnect_server();
   return OCI_ERROR;
 }

 rownum = 1;
 /* Insertion via SQL statement */
 if (insert_dt(rownum))
 {
   printf("FAILED: insert_dt()!\n");
   disconnect_server();
   return OCI_ERROR;
 }

 /* Selection via SQL statement */
 if (select_dt(rownum))
 {
   printf("FAILED: select_dt()!\n");
   disconnect_server();
   return OCI_ERROR;
 }

 /* An example on IN/OUT BIND in PL/SQL procedure */
 if (io_proc())
 {
   printf("FAILED: io_proc()!\n");
   disconnect_server();
   return OCI_ERROR;
 }

 /* An example on time zone name and daylight saving using OCIDateTimeConvert() */
 if (tzds_1())
 {
   printf("FAILED: tzds_1()!\n");
   disconnect_server();
   return OCI_ERROR;
 }

 /* An example on time zone name and daylight saving using OCIDateTimeIntervalAdd() */
 if (tzds_2())
 {
   printf("FAILED: tzds_2()!\n");
   disconnect_server();
   return OCI_ERROR;
 }

  /* Detach from a server and clean up the environment */
 disconnect_server();

 return OCI_SUCCESS;
}


/*---------------------------Subfunctions -----------------------------*/

/*--------------------------------------------------------*/
/* Add an interval to a datetime */
/*--------------------------------------------------------*/
sb4 add_interval(dt, interval, fmt, type1, type2)
OraText *dt, *interval, *fmt;
ub4 type1, type2;
{
 OCIDateTime *var1, *result;
 OCIInterval *var2;
 OraText *str, *fmt2;
 ub4 buflen;

   /* Initialize the output string */
 str = (OraText *)malloc(MAXDATELEN);
 memset ((void *) str, '\0', MAXDATELEN);
 fmt2 = (OraText *)malloc(MAXDATELEN);
 memset ((void *) fmt2, '\0', MAXDATELEN);

   /* Allocate descriptors */
 if (ret = OCIDescriptorAlloc((dvoid *)envhp,(dvoid **)&var1, type1,
                           0, (dvoid **) 0) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIDescriptorAlloc");
   return OCI_ERROR;
 }
 if (ret = OCIDescriptorAlloc((dvoid *)envhp,(dvoid **)&result, type1,
                           0, (dvoid **) 0) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIDescriptorAlloc");
   return OCI_ERROR;
 }
 if (ret = OCIDescriptorAlloc((dvoid *)envhp,(dvoid **)&var2, type2,
                           0,(dvoid **)0) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIDescriptorAlloc");
   return OCI_ERROR;
 }

   /* Change to datetime type format */
 if (ret = OCIDateTimeFromText((dvoid *)authp,errhp,(CONST OraText *)dt,
             (ub4) strlen((char *)dt), (CONST OraText *)fmt,
             (ub1) strlen((char *)fmt),(CONST OraText *)0,0, var1) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIDateTimeFromText");
   return OCI_ERROR;
 }
   /* Change to interval type format */
 if ((ret = OCIIntervalFromText((dvoid *)authp, errhp,(CONST OraText *)interval,
              (ub4) strlen((char *)interval),
              (OCIInterval *)var2)) != OCI_SUCCESS) {
   chk_err(errhp, ret, __LINE__,(OraText *)"OCIIntervalFromText");
   return OCI_ERROR;
 }

   /* Add the interval to datetime */
 if (ret = OCIDateTimeIntervalAdd(envhp, errhp, var1, var2, result) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIDateTimeIntervalAdd");
   return OCI_ERROR;
 }

 buflen = MAXDATELEN;

   /* Change the result to text format */
 strcpy((char *)fmt2,"DD-MON-RR HH.MI.SSXFF AM TZR TZD");
 if (ret = OCIDateTimeToText((dvoid *)authp,errhp,(CONST OCIDateTime *)result,
         (CONST OraText *)fmt2,(ub1)strlen((char *)fmt2),(ub1) 2,
         (CONST OraText *) NULL,
         (ub4)0, (ub4 *) &buflen, (OraText *) str) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIDateTimeToText");
   return OCI_ERROR;
 }

 printf("( %s ) + ( %s ) --->\n\t%s\n\n", dt, interval, str);

   /* Free the descriptors */
 if (var1) {
   OCIDescriptorFree((dvoid *) var1, (ub4) type1);
 }
 if (result) {
   OCIDescriptorFree((dvoid *) result, (ub4) type1);
 }
 if (var2) {
   OCIDescriptorFree((dvoid *) var2, (ub4) type2);
 }
 
 return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* Allocate descriptors */
/*--------------------------------------------------------*/
sb4 alloc_desc(dvar, ivar, size1, size2)
OCIDateTime *dvar[];
OCIInterval *ivar[];
sword size1, size2;
{
 sword i;

 /* allocate datetime and interval descriptors */
 for (i=0; i<size1; i++) {
   if ((ret = OCIDescriptorAlloc(envhp,(dvoid **)&dvar[i], ocit[i],
                               0,(dvoid **)0)) != OCI_SUCCESS) {
     printf("Error With Descriptor Alloc!\n");
     return OCI_ERROR;
   }
 }
 for (i=0; i<size2; i++) {
   if ((ret = OCIDescriptorAlloc(envhp,(dvoid **)&ivar[i], ocit[i+3],
                               0,(dvoid **)0)) != OCI_SUCCESS) {
     printf("Error With Descriptor Alloc!\n");
     return OCI_ERROR;
   }
 }

 return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* Error handling  */
/*--------------------------------------------------------*/
void chk_err(errhp,ec,linenum,comment)
OCIError  *errhp;
boolean ec;
sb2 linenum;
OraText *comment;
{
  OraText  msgbuf[1024];
  sb4 errcode;

  switch(ec) {
    case OCI_SUCCESS:
      break;

    case OCI_SUCCESS_WITH_INFO:
      printf("------------------------------------------------------------\n");
      printf("Error:Line # %d: OCI_SUCCESS_WITH_INFO - %s\n",linenum,comment);
      break;

    case OCI_NEED_DATA:
      printf("------------------------------------------------------------\n");
      printf("Error:Line # %d: OCI_NEED_DATA - %s\n",linenum,comment);
      break;

    case OCI_NO_DATA:
      printf("------------------------------------------------------------\n");
      printf("Error:Line # %d: OCI_NO_DATA - %s\n",linenum,comment);
      break;

    case OCI_ERROR:
      printf("------------------------------------------------------------\n");
      printf("Error:Line # %d: OCI_ERROR - %s\n",linenum,comment);
      OCIErrorGet ((dvoid *) errhp,(ub4) 1, (OraText *) NULL, &errcode,
              msgbuf, (ub4) sizeof(msgbuf), (ub4) OCI_HTYPE_ERROR);
      printf("%s\n",msgbuf);
      break;

    case OCI_INVALID_HANDLE:
      printf("------------------------------------------------------------\n");
      printf("Error:Line # %d: OCI_INVALID_HANDLE - %s\n",linenum,comment);
      exit(1);

    case OCI_STILL_EXECUTING:
      printf("------------------------------------------------------------\n");
      printf("Error:Line # %d: OCI_STILL_EXECUTING - %s\n",linenum,comment);

      break;

    case OCI_CONTINUE:
      printf("Error:Line # %d: OCI_CONTINUE - %s\n",linenum,comment);
      break;

    default:
      printf("------------------------------------------------------------\n");
      printf("Error:Line # %d: Error Code %d :%s\n",linenum,ec,comment);
      break;
  }
}


/*--------------------------------------------------------*/
/* Free the envhp whenever there is an error  */
/*--------------------------------------------------------*/
void cleanup()
{
 if (envhp) {
    OCIHandleFree((dvoid *)envhp, OCI_HTYPE_ENV);
 }

 return;
}


/*--------------------------------------------------------*/
/* attach to server, set attributes, and begin session */
/*--------------------------------------------------------*/
sb4 connect_server(cstring)
OraText *cstring;
{
 /* attach to server */
 if ((ret = OCIServerAttach( srvhp,errhp,(OraText *) cstring,
                          (sb4) strlen((char *)cstring), OCI_DEFAULT)) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIServerAttach");
   return OCI_ERROR;
 }

 /* set server attribute to service context */
 if ((ret = OCIAttrSet((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
                       (dvoid *) srvhp, (ub4) 0, (ub4) OCI_ATTR_SERVER, errhp))
       != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIAttrSet: OCI_HTYPE_SVCCTX");
   return OCI_ERROR;
 }

 /* set user attribute to session */
 if ((ret = OCIAttrSet ( (dvoid *) authp,OCI_HTYPE_SESSION,
                         (dvoid *) user, (ub4) strlen((char *)user),
                   (ub4) OCI_ATTR_USERNAME, (OCIError *) errhp)) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIAttrSet: OCI_ATTR_USERNAME");
   return OCI_ERROR;
 }

 /* set password attribute to session */
 if ((ret = OCIAttrSet ( (dvoid *) authp,OCI_HTYPE_SESSION,
                         (dvoid *) password, (ub4) strlen((char *)password),
                   (ub4) OCI_ATTR_PASSWORD,(OCIError *) errhp)) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIAttrSet: OCI_ATTR_PASSWORD");
   return OCI_ERROR;
 }

 /* Begin a session  */
 if ( (ret = OCISessionBegin (svchp,errhp,authp,OCI_CRED_RDBMS,
                  OCI_DEFAULT)) == OCI_SUCCESS)
   printf("Logged in!\n\n");
 else {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCISessionBegin");
   return OCI_ERROR;
 }

 /* set session attribute to service context */
 if ((ret = OCIAttrSet((dvoid *)svchp, (ub4)OCI_HTYPE_SVCCTX,
                       (dvoid *)authp, (ub4)0,(ub4)OCI_ATTR_SESSION, errhp))
       != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIAttrSet: OCI_ATTR_SESSION");
   return OCI_ERROR;
 }

 return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* Convert a datetime type to another */
/*--------------------------------------------------------*/
sb4 conv_dt(str1, type1, type2, fmt1, fmt2, name1, name2)
OraText *str1;
ub4 type1, type2;
OraText *fmt1, *fmt2, *name1, *name2;
{
 OCIDateTime *var1, *var2;
 OraText *str2;
 ub4 buflen;

   /* Initialize the output string */
 str2 = (OraText *)malloc(MAXDATELEN);
 memset ((void *) str2, '\0', MAXDATELEN);

   /* Allocate descriptors */
 if (ret = OCIDescriptorAlloc((dvoid *)envhp,(dvoid **)&var1, type1,
                           0, (dvoid **) 0) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIDescriptorAlloc--var1");
   return OCI_ERROR;
 }
 if (ret = OCIDescriptorAlloc((dvoid *)envhp,(dvoid **)&var2, type2,
                           0,(dvoid **)0) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIDescriptorAlloc--var2");
   return OCI_ERROR;
 }

   /* Change to datetime type format */
 if (ret = OCIDateTimeFromText((dvoid *)authp,errhp,(CONST OraText *)str1,
             (ub4) strlen((char *)str1), (CONST OraText *)fmt1,
             (ub1) strlen((char *)fmt1),(CONST OraText *)0,0, var1) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIDateTimeFromText--var1");
   return OCI_ERROR;
 }

   /* Convert one datetime type to another */
 if (ret = OCIDateTimeConvert((dvoid *)authp,errhp,var1,var2) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIDateTimeConvert");
   return OCI_ERROR;
 }

 buflen = MAXDATELEN;

   /* Change to text format */
 if (ret = OCIDateTimeToText((dvoid *)authp,errhp,(CONST OCIDateTime *)var2,
         (CONST OraText *)fmt2,(ub1)strlen((char *)fmt2),(ub1) 2,
         (CONST OraText *) NULL,
         (ub4)0, (ub4 *) &buflen, (OraText *) str2) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIDateTimeToText");
   return OCI_ERROR;
 }

 printf("%s ---> %s:\n", name1, name2);
 printf("\t%s ---> %s\n\n", str1, str2);

   /* Free the descriptors */
 if (var1) {
   OCIDescriptorFree((dvoid *) var1, (ub4) type1);
 }
 if (var2) {
   OCIDescriptorFree((dvoid *) var2, (ub4) type2);
 }
 
 return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* End the session, detach server and free environmental handles. */
/*--------------------------------------------------------*/
void disconnect_server()
{
  printf("\n\nLogged off and detached from server.\n");

   /* Drop the demo table and procedure if any */
 if (tab_exists)
   exe_sql((OraText *)"drop table cdemodt_foo");
 if (proc_exists)
   exe_sql((OraText *)"drop procedure cdemodt_ins_sel");

   /* End a session */
 if (ret = OCISessionEnd((OCISvcCtx *)svchp, (OCIError *)errhp,
                           (OCISession *)authp, (ub4) OCI_DEFAULT)) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCISessionEnd");
   cleanup();
   return;
 }

   /* Detach from the server */
 if (ret = OCIServerDetach((OCIServer *)srvhp, (OCIError *)errhp,
                             (ub4)OCI_DEFAULT)) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIServerDetach");
   cleanup();
   return;
 }

   /* Free the handles */
 if (stmthp) {
    OCIHandleFree((dvoid *)stmthp, (ub4) OCI_HTYPE_STMT);
 }
 if (authp) {
    OCIHandleFree((dvoid *)authp, (ub4) OCI_HTYPE_SESSION);
 }
 if (svchp) {
    OCIHandleFree((dvoid *)svchp, (ub4) OCI_HTYPE_SVCCTX);
 }
 if (srvhp) {
    OCIHandleFree((dvoid *)srvhp, (ub4) OCI_HTYPE_SERVER);
 }
 if (errhp) {
    OCIHandleFree((dvoid *)errhp, (ub4) OCI_HTYPE_ERROR);
 }
 if (envhp) {
    OCIHandleFree((dvoid *)envhp, (ub4) OCI_HTYPE_ENV);
 }

  return;
}


/*--------------------------------------------------------*/
/* Execute the insertion */
/*--------------------------------------------------------*/
sb4 exe_insert(insstmt, strdate, fmtdate, dvar, ivar, rownum)
OraText *insstmt, *strdate[COLNUM-1], *fmtdate[3];
OCIDateTime *dvar[3];
OCIInterval *ivar[2];
size_t rownum;
{
 sword i;
 size_t colc = rownum;

   /* Converts the text strings to the datetime types in the table */
 for (i=0; i<3; i++) {
   if (ret = OCIDateTimeFromText(authp,errhp,(CONST OraText *)strdate[i],
           (ub4) strlen((char *)strdate[i]),(CONST OraText *) fmtdate[i],
           (ub1) strlen((char *)fmtdate[i]),(OraText *) NULL,(ub4) 0,dvar[i])) {
     chk_err(errhp,ret,__LINE__,(OraText *)"OCIDateFromText");
     return OCI_ERROR;
   }
 }

   /* Converts the text strings to the interval types in the table */
 for (i=0; i<2; i++) {
   if ((ret = OCIIntervalFromText((dvoid *)authp, errhp,(CONST OraText *)
                strdate[i+3], (ub4) strlen((char *)strdate[i+3]),
                (OCIInterval *)ivar[i])) != OCI_SUCCESS) {
     chk_err(errhp, ret, __LINE__,(OraText *)"OCIIntervalFromText");
     return OCI_ERROR;
   }
 }

   /* Preparation of statment handle for the SQL statement */
 if ((ret = OCIStmtPrepare(stmthp, errhp, insstmt, (ub4)strlen((char *)insstmt), OCI_NTV_SYNTAX,
                      OCI_DEFAULT)) != OCI_SUCCESS) {
    chk_err(errhp,ret,__LINE__,(OraText *)"OCIStmtPrepare");
    return OCI_ERROR;
 }

   /* Binds the input variable */
 for (i=0; i<3; i++) {
   if ((ret = OCIBindByPos(stmthp,&bndhp[i],errhp,i+1,(dvoid *)&dvar[i],
              (sb4) sizeof(OCIDateTime *), sqlt[i], (dvoid *)0,
              (ub2 *) 0,(ub2 *) 0,(ub4) 0,(ub4 *)0,OCI_DEFAULT)) != OCI_SUCCESS) {
     chk_err(errhp,ret,__LINE__,(OraText *)"OCIBindByPos");
     return OCI_ERROR;
   }
 }
 for (i=0; i<2; i++) {
   if ((ret = OCIBindByPos(stmthp,&bndhp[i+3],errhp,i+4,(dvoid *)&ivar[i],
              (sb4) sizeof(OCIInterval *), sqlt[i+3], (dvoid *)0,
              (ub2 *) 0,(ub2 *) 0,(ub4) 0,(ub4 *)0,OCI_DEFAULT)) != OCI_SUCCESS) {
     chk_err(errhp,ret,__LINE__,(OraText *)"OCIBindByPos");
     return OCI_ERROR;
   }
 }
 if ((ret = OCIBindByPos(stmthp,&bndhp[5],errhp,6,(dvoid *)&colc,
            (sb4) sizeof(colc), sqlt[5], (dvoid *)0,
            (ub2 *) 0,(ub2 *) 0,(ub4) 0,(ub4 *)0,OCI_DEFAULT)) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIBindByPos");
   return OCI_ERROR;
 }

   /* Executing the SQL statement */
 if ((ret = OCIStmtExecute(svchp, stmthp, errhp, 1, 0, (OCISnapshot *) NULL,
                           (OCISnapshot *) NULL, OCI_DEFAULT)) != OCI_SUCCESS) {
    chk_err(errhp,ret,__LINE__,(OraText *)"OCIStmtExecute");
    return OCI_ERROR;
 }
 
 printf("Values inserted\n\n");

 return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* Execute a sql statement */
/*--------------------------------------------------------*/
sb4 exe_sql(stmt)
OraText *stmt;
{
  /* prepare statement */
  if (ret = OCIStmtPrepare(stmthp, errhp, stmt, (ub4) strlen((char *) stmt), (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT))
  {
    chk_err(errhp,ret,__LINE__,(OraText *)"OCIStmtPrepare");
    return OCI_ERROR;
  }
  /* execute statement */
  if (ret = OCIStmtExecute(svchp, stmthp, errhp, (ub4) 1, (ub4) 0,
                    (CONST OCISnapshot *) 0, (OCISnapshot *) 0,
                    (ub4) OCI_DEFAULT))
  {
    chk_err(errhp,ret,__LINE__,(OraText *)"OCIStmtExecute");
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* Free the descriptors */
/*--------------------------------------------------------*/
sb4 free_desc(dvar, ivar, size1, size2)
OCIDateTime *dvar[];
OCIInterval *ivar[];
sword size1, size2;
{
 sword i;

   /* Free the descriptors */
 for (i=0; i<size1; i++) {
   if (dvar[i]) {
     OCIDescriptorFree((dvoid *) dvar[i], (ub4) ocit[i]);
   }
 }
 for (i=0; i<size2; i++) {
   if (dvar[i]) {
     OCIDescriptorFree((dvoid *) dvar[i], (ub4) ocit[i+3]);
   }
 }

 return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* Initialize the environment and allocate handles */
/*--------------------------------------------------------*/
sb4 init_env_handle()
{
 /* Environment initialization and creation */
 if ((ret = OCIEnvCreate((OCIEnv **) &envhp, (ub4) OCI_DEFAULT | OCI_OBJECT,
                   (dvoid *) 0, (dvoid * (*)(dvoid *,size_t)) 0,
                   (dvoid * (*)(dvoid *, dvoid *, size_t)) 0,
                   (void (*)(dvoid *, dvoid *)) 0, (size_t) 0, (dvoid **) 0))
       != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIEnvCreate");
   return OCI_ERROR;
 }

 /* allocate error handle */
 if ((ret = OCIHandleAlloc((dvoid *) envhp,(dvoid **) &errhp,OCI_HTYPE_ERROR,
                           (size_t) 0, (dvoid **) 0)) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIHandleAlloc: errhp");
   return OCI_ERROR;
 }

 /* allocate server handle */
 if ((ret = OCIHandleAlloc ( (dvoid *) envhp,(dvoid **) &srvhp,OCI_HTYPE_SERVER,
                             (size_t) 0, (dvoid **) 0)) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIHandleAlloc: srvhp");
   return OCI_ERROR;
 }

 /* allocate service context handle */
 if ((ret = OCIHandleAlloc ( (dvoid *) envhp,(dvoid **) &svchp,OCI_HTYPE_SVCCTX,
                             (size_t) 0, (dvoid **) 0)) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIHandleAlloc: svchp");
   return OCI_ERROR;
 }

 /* allocate session handle */
 if ((ret = OCIHandleAlloc((dvoid *) envhp,(dvoid **) &authp,OCI_HTYPE_SESSION,
                             (size_t) 0,(dvoid **) 0)) != OCI_SUCCESS) {
   chk_err(errhp,ret,__LINE__,(OraText *)"OCIHandleAlloc: authp");
   return OCI_ERROR;
 }

 /* allocate statement handle */
 if ((ret = OCIHandleAlloc(envhp,(dvoid*)  &stmthp,
                                  OCI_HTYPE_STMT, 0, (dvoid **)0)) != OCI_SUCCESS) {
   printf("Error in getting stmt handle\n");
   return OCI_ERROR;
 }

 return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* Create table and procedure for demo */
/*--------------------------------------------------------*/
sb4 init_sql()
{
  sword i;
  OraText *crtstmt[2] = { (OraText *) "CREATE TABLE CDEMODT_FOO (\
                                 ts TIMESTAMP(2), \
                                 tstz TIMESTAMP(2) WITH TIME ZONE, \
                                 tsltz TIMESTAMP(2) WITH LOCAL TIME ZONE, \
                                 iytm INTERVAL YEAR TO MONTH, \
                                 idts INTERVAL DAY(3) TO SECOND(2), \
                                 int INTEGER)",
                       (OraText *) "create or replace procedure cdemodt_ins_sel( \
                                 c1 IN OUT timestamp, \
                                 c2 IN OUT timestamp with time zone, \
                                 c3 IN OUT timestamp with local time zone, \
                                 c4 IN OUT interval year to month, \
                                 c5 IN OUT interval day to second, \
                                 y IN integer) is \
                                 begin \
                                   insert into cdemodt_foo values (c1, c2, c3, c4, c5, y+1); \
                                   select ts, tstz, tsltz, iytm, idts into c1, c2, c3, c4, c5 from cdemodt_foo where int = y; \
                                 end cdemodt_ins_sel;" };

    /* execute the sql statements */
  for (i=0; i<2; i++) {
    if (exe_sql(crtstmt[i])) {
      printf("FAILED: exe_sql()!\n");
      return OCI_ERROR;
    }
  }

  /* commit the Xn */
   OCITransCommit(svchp, errhp, (ub4)0);

  /* set flags to be used by disconnect_server() to drop the table and the procedure */
  tab_exists = TRUE;
  proc_exists = TRUE;

  return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* Insert datetime/interval values into the table */
/*--------------------------------------------------------*/
sb4 insert_dt(rownum)
size_t rownum;
{
 sword i;
 OraText *strdate[COLNUM-1];
 OraText *fmtdate[3];
 OraText *insstmt = (OraText *)"INSERT INTO CDEMODT_FOO VALUES (:1, :2, :3, :4, :5, :6)";
 OCIDateTime *dvar[3];
 OCIInterval *ivar[2];

   /* Initialize strings */
 for (i=0; i<COLNUM-1; i++) {
   strdate[i] = (OraText *)malloc(MAXDATELEN);
   memset ((void *) strdate[i], '\0', MAXDATELEN);
 }
 for (i=0; i<3; i++) {
   fmtdate[i] = (OraText *)malloc(MAXDATELEN);
   memset ((void *) fmtdate[i], '\0', MAXDATELEN);
 }

 strcpy((char *)strdate[0],"1999-10-01 01:30:10");
 strcpy((char *)strdate[1],"1999-10-01 01:30:10 US/Pacific PDT");
 strcpy((char *)strdate[2],"1999-10-01 01:30:10");
 strcpy((char *)strdate[3],"01-02");
 strcpy((char *)strdate[4],"1 02:02:02.98");
 strcpy((char *)fmtdate[0],"YYYY-MM-DD HH24:MI:SS");
 strcpy((char *)fmtdate[1],"YYYY-MM-DD HH24:MI:SS TZR TZD");
 strcpy((char *)fmtdate[2],"YYYY-MM-DD HH24:MI:SS");

 /* allocate datetime and interval descriptors */
 if (alloc_desc(dvar, ivar, 3, 2)) {
   printf("FAILED: alloc_desc()!\n");
   return OCI_ERROR;
 }

 printf("-->Insertion via SQL statement\n\n");
   /* execute the insertion */
 if (exe_insert(insstmt, strdate, fmtdate, dvar, ivar, rownum)) {
   printf("FAILED: exe_insert()!\n");
   return OCI_ERROR;
 }

   /* Free the descriptors */
 if (free_desc(dvar, ivar, 3, 2)) {
   printf("FAILED: free_desc()!\n");
   return OCI_ERROR;
 }

 return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* Insert/Select via In/Out Bind */
/*--------------------------------------------------------*/
sb4 io_proc()
{
 sword  i;
 size_t rownum = 1;
 OCIDateTime *dvar[3];
 OCIInterval *ivar[2];
 ub4 str_size[COLNUM-1];
 size_t resultlen;
 OraText *strdate[COLNUM-1];
 OraText *fmtdate[3];
 OraText *sqlstmt = (OraText *)"BEGIN CDEMODT_INS_SEL(:1, :2, :3, :4, :5, :6); END; ";

   /* Initialize strings */
 for (i=0; i<COLNUM-1; i++) {
   strdate[i] = (OraText *)malloc(MAXDATELEN);
   memset ((void *) strdate[i], '\0', MAXDATELEN);
 }
 for (i=0; i<3; i++) {
   fmtdate[i] = (OraText *)malloc(MAXDATELEN);
   memset ((void *) fmtdate[i], '\0', MAXDATELEN);
 }

 printf("-->Insertion and Selection via PL/SQL procedure\n\n");

 strcpy((char *)strdate[0],"1999-12-01 01:30:10");
 strcpy((char *)strdate[1],"1999-12-01 01:30:10 US/Pacific PST");
 strcpy((char *)strdate[2],"1999-12-01 01:30:10");
 strcpy((char *)strdate[3],"02-03");
 strcpy((char *)strdate[4],"2 03:03:03.09");
 strcpy((char *)fmtdate[0],"YYYY-MM-DD HH24:MI:SS");
 strcpy((char *)fmtdate[1],"YYYY-MM-DD HH24:MI:SS TZR TZD");
 strcpy((char *)fmtdate[2],"YYYY-MM-DD HH24:MI:SS");

 /* allocate datetime and interval descriptors */
 if (alloc_desc(dvar, ivar, 3, 2)) {
   printf("FAILED: alloc_desc()!\n");
   return OCI_ERROR;
 }

 /* execute the statement */
 if (exe_insert(sqlstmt, strdate, fmtdate, dvar, ivar, rownum)) {
   printf("FAILED: exe_insert()!\n");
   return OCI_ERROR;
 }

   /* Convert to text format */
 printf("Values selected:\n");
 for (i=0; i<3; i++) {
   str_size[i] = MAXDATELEN;
   if ((ret = OCIDateTimeToText(authp, errhp, dvar[i], (OraText*)fmtdate[i], 
                    (ub1) strlen((char *)fmtdate[i]), (ub1) 2, (OraText*)0, 0, 
                     &str_size[i], (OraText*)strdate[i])) != OCI_SUCCESS) {
      chk_err(errhp,ret,__LINE__,(OraText *)"OCIDateTimeToText");
      return OCI_ERROR;
   }
   printf("Column %d: %s\n", i+1, strdate[i]);
 }
 for (i=0; i<2; i++) {
   str_size[i+3] = MAXDATELEN;
   if ((ret = OCIIntervalToText(envhp, errhp, ivar[i], (ub1) 3, (ub1)2,
                     strdate[i+3],str_size[i+3],(size_t *)&resultlen)) != 0) {
      chk_err(errhp, ret, __LINE__,(OraText *)"Error in OCIIntervalToText");
      return OCI_ERROR;
   }
   printf("Column %d: %s\n", i+4, strdate[i+3]);
 }
 printf("Column 6: %d\n\n", rownum);

   /* Free the descriptors */
 if (free_desc(dvar, ivar, 3, 2)) {
   printf("FAILED: free_desc()!\n");
   return OCI_ERROR;
 }

  return OCI_SUCCESS;
}         


/*--------------------------------------------------------*/
/* Select datetime/interval values from the table */
/*--------------------------------------------------------*/
sb4 select_dt(rownum)
size_t rownum;
{
 sword i, var6;
 size_t colc = rownum;
 OraText *strdate[COLNUM-1];
 OraText *fmtdate[3];
 ub4 str_size[COLNUM-1];
 size_t resultlen; 
 OCIDateTime *dvar[3];
 OCIInterval *ivar[2];
 OraText *selstmt = (OraText *) "SELECT * FROM CDEMODT_FOO WHERE int = :1";

   /* Initialize strings */
 for (i=0; i<COLNUM-1; i++) {
   strdate[i] = (OraText *)malloc(MAXDATELEN);
   memset ((void *) strdate[i], '\0', MAXDATELEN);
 }
 for (i=0; i<3; i++) {
   fmtdate[i] = (OraText *)malloc(MAXDATELEN);
   memset ((void *) fmtdate[i], '\0', MAXDATELEN);
 }

 strcpy((char *)fmtdate[0],"YYYY-MM-DD HH24:MI:SS");
 strcpy((char *)fmtdate[1],"YYYY-MM-DD HH24:MI:SS TZR TZD");
 strcpy((char *)fmtdate[2],"YYYY-MM-DD HH24:MI:SS");

 /* allocate datetime and interval descriptors */
 if (alloc_desc(dvar, ivar, 3, 2)) {
   printf("FAILED: alloc_desc()!\n");
   return OCI_ERROR;
 }

 printf("-->Selection via SQL statement\n\n");
   /* Preparation of statment handle for the SQL statement */
 if ((ret = OCIStmtPrepare(stmthp, errhp, selstmt, (ub4)strlen((char *)selstmt), OCI_NTV_SYNTAX,
                      OCI_DEFAULT)) != OCI_SUCCESS) {
    chk_err(errhp,ret,__LINE__,(OraText *)"OCIStmtPrepare");
    return OCI_ERROR;
 }

   /* associate variable colc with bind placeholder in the SQL statement */
 if ((ret = OCIBindByPos(stmthp, &bndhp[0], errhp, 1, &colc, sizeof(colc),
                      SQLT_INT,(void *)0, (ub2 *)0,(ub2 *)0, 0, (ub4 *)0, OCI_DEFAULT)) != OCI_SUCCESS) {
    chk_err(errhp,ret,__LINE__,(OraText *)"OCIBindByPos");
    return OCI_ERROR;
 }

   /* Defining the positions for the variables */
 for (i=0; i<3; i++) {
   if ((ret = OCIDefineByPos(stmthp, &defhp[i], errhp, i+1, &dvar[i], sizeof(dvar[i]),
                      sqlt[i], (void *)0, (ub2 *)0, (ub2 *)0, OCI_DEFAULT)) != OCI_SUCCESS) {
      chk_err(errhp,ret,__LINE__,(OraText *)"OCIDefineByPos");
      return OCI_ERROR;
   }
 }
 for (i=0; i<2; i++) {
   if ((ret = OCIDefineByPos(stmthp, &defhp[i+3], errhp, i+4, &ivar[i], sizeof(ivar[i]),
                      sqlt[i+3], (void *)0,(ub2 *)0, (ub2 *)0, OCI_DEFAULT)) != OCI_SUCCESS) {
      chk_err(errhp,ret,__LINE__,(OraText *)"OCIDefineByPos");
      return OCI_ERROR;
   }
 }
 if ((ret = OCIDefineByPos(stmthp, &defhp[5], errhp, 6, &var6, sizeof(var6),
                      sqlt[5], (void *)0, (ub2 *)0,(ub2 *)0, OCI_DEFAULT)) != OCI_SUCCESS) {
    chk_err(errhp,ret,__LINE__,(OraText *)"OCIDefineByPos");
    return OCI_ERROR;
 }
 
   /* Executing the SQL statement */
 if ((ret = OCIStmtExecute(svchp, stmthp, errhp, 1, 0, (OCISnapshot *) NULL,
                           (OCISnapshot *) NULL, OCI_DEFAULT)) != OCI_SUCCESS) {
    chk_err(errhp,ret,__LINE__,(OraText *)"OCIStmtExecute");
    return OCI_ERROR;
 }
 
   /* Convert to text format */
 printf("Values selected:\n");
 for (i=0; i<3; i++) {
   str_size[i] = MAXDATELEN;
   if ((ret = OCIDateTimeToText(authp, errhp, dvar[i], (OraText*)fmtdate[i], 
                    (ub1) strlen((char *)fmtdate[i]), (ub1) 2, (OraText*)0,(size_t)0, 
                     &str_size[i], (OraText*)strdate[i])) != OCI_SUCCESS) {
      chk_err(errhp,ret,__LINE__,(OraText *)"OCIDateTimeToText");
      return OCI_ERROR;
   }
   printf("Column %d: %s\n", i+1, strdate[i]);
 }
 for (i=0; i<2; i++) {
   str_size[i+3] = MAXDATELEN;
   if ((ret = OCIIntervalToText(envhp, errhp, ivar[i], (ub1) 3, (ub1)2,
                     strdate[i+3],str_size[i+3],(size_t *)&resultlen)) != 0) {
      chk_err(errhp, ret, __LINE__, (OraText *)"Error in OCIIntervalToText");
      return OCI_ERROR;
   }
   printf("Column %d: %s\n", i+4, strdate[i+3]);
 }
 printf("Column 6: %d\n\n", var6);

   /* Free the descriptors */
 if (free_desc(dvar, ivar, 3, 2)) {
   printf("FAILED: free_desc()!\n");
   return OCI_ERROR;
 }

 return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* Set session datetime zone */
/*--------------------------------------------------------*/
sb4 set_sdtz()
{
 OraText *altstmt=(OraText *)"ALTER SESSION SET TIME_ZONE='US/Central'";
 OraText *selstmt=(OraText *)"SELECT SESSIONTIMEZONE FROM DUAL";
 OraText *str;

   /* Initialize strings */
 str = (OraText *)malloc(MAXDATELEN);
 memset ((void *) str, '\0', MAXDATELEN);

   /* Alter session time zone */
 if (exe_sql(altstmt)) {
   printf("FAILED: exe_sql()!\n");
   return OCI_ERROR;
 }

    /* Preparation of statment handle for the select SQL statement */
 if ((ret = OCIStmtPrepare(stmthp, errhp, selstmt, (ub4) strlen((char *)selstmt),
                OCI_NTV_SYNTAX, OCI_DEFAULT)) != OCI_SUCCESS) {
    chk_err(errhp,ret,__LINE__,(OraText *)"OCIStmtPrepare");
    return OCI_ERROR;
 }

   /* Defining the position for the variable */
 if ((ret = OCIDefineByPos(stmthp, &defhp[0], errhp, 1, str, MAXDATELEN,
                      SQLT_STR, (void *)0, (ub2 *)0, (ub2 *)0, OCI_DEFAULT)) != OCI_SUCCESS) {
    chk_err(errhp,ret,__LINE__,(OraText *)"OCIDefineByPos");
    return OCI_ERROR;
 }

    /* Executing the SQL statement */
 if ((ret = OCIStmtExecute(svchp, stmthp, errhp, 1, 0, (OCISnapshot *) NULL,
                           (OCISnapshot *) NULL, OCI_DEFAULT)) != OCI_SUCCESS) {
    chk_err(errhp,ret,__LINE__,(OraText *)"OCIStmtExecute");
    return OCI_ERROR;
 }

 printf("The session time zone is set to %s\n\n", str);

 return OCI_SUCCESS;
}


/*--------------------------------------------------------*/
/* An example on time zone name and daylight saving using OCIDateTimeConvert */
/*--------------------------------------------------------*/
sb4 tzds_1()
{
 ub4 type1, type2;
 OraText *str1, *fmt1, *fmt2, *name1, *name2;

   /* Initialize strings */
 str1 = (OraText *)malloc(MAXDATELEN);
 memset ((void *) str1, '\0', MAXDATELEN);
 fmt1 = (OraText *)malloc(MAXDATELEN);
 memset ((void *) fmt1, '\0', MAXDATELEN);
 fmt2 = (OraText *)malloc(MAXDATELEN);
 memset ((void *) fmt2, '\0', MAXDATELEN);
 name1 = (OraText *)malloc(MAXDATELEN);
 memset ((void *) name1, '\0', MAXDATELEN);
 name2 = (OraText *)malloc(MAXDATELEN);
 memset ((void *) name2, '\0', MAXDATELEN);

 printf("-->Conversion of datetime types\n\n");
   /* Convert from timestamp with time zone region to timestamp with time zone offset */
   /* The same time zone name will return different time zone offsets for standard time and daylight saving time */
   /* daylight saving time, expected time zone offset is -07:00 */
 type1 = OCI_DTYPE_TIMESTAMP_TZ;
 type2 = OCI_DTYPE_TIMESTAMP_TZ;
 strcpy((char *)str1,"1999-10-01 01:30:10 US/Pacific");
 strcpy((char *)fmt1,"YYYY-MM-DD HH:MI:SS TZR");
 strcpy((char *)fmt2,"YYYY-MM-DD HH:MI:SS TZH:TZM");
 strcpy((char *)name1, "TIMESTAMP WITH TIME ZONE REGION");
 strcpy((char *)name2, "TIMESTAMP WITH TIME ZONE OFFSET");
 if (conv_dt(str1, type1, type2, fmt1, fmt2, name1, name2)) {
   printf("FAILED: conv_dt()!\n");
   return OCI_ERROR;
 }

   /* standard time, expected time zone offset is -08:00 */
 strcpy((char *)str1,"1999-11-01 01:30:10 US/Pacific");
 if (conv_dt(str1, type1, type2, fmt1, fmt2, name1, name2)) {
   printf("FAILED: conv_dt()!\n");
   return OCI_ERROR;
 }

   /* Convert from timestamp with time zone region to timestamp with local time zone,
      the session time zone was set to US/Central at the begining of the program  */ 
   /* daylight saving time */     
 type1 = OCI_DTYPE_TIMESTAMP_TZ;
 type2 = OCI_DTYPE_TIMESTAMP_LTZ;
 strcpy((char *)str1,"1999-10-31 01:30:10 US/Pacific PDT");
 strcpy((char *)fmt1,"YYYY-MM-DD HH24:MI:SS TZR TZD");
 strcpy((char *)fmt2,"YYYY-MM-DD HH24:MI:SS");
 strcpy((char *)name1, "TIMESTAMP WITH TIME ZONE REGION");
 strcpy((char *)name2, "TIMESTAMP WITH LOCAL TIME ZONE");
 if (conv_dt(str1, type1, type2, fmt1, fmt2, name1, name2)) {
   printf("FAILED: conv_dt()!\n");
   return OCI_ERROR;
 }

  /* standard time */
 strcpy((char *)str1,"1999-10-31 01:30:10 US/Pacific PST");
 if (conv_dt(str1, type1, type2, fmt1, fmt2, name1, name2)) {
   printf("FAILED: conv_dt()!\n");
   return OCI_ERROR;
 }

 return OCI_SUCCESS;  
}


/*--------------------------------------------------------*/
/* An example on time zone name and daylight saving using OCIDateTimeIntervalAdd */
/*--------------------------------------------------------*/
sb4 tzds_2()
{
 ub4 type1, type2;
 OraText *dt, *interval, *fmt;

   /* Initialize strings */
 dt = (OraText *)malloc(MAXDATELEN);
 memset ((void *) dt, '\0', MAXDATELEN);
 interval = (OraText *)malloc(MAXDATELEN);
 memset ((void *) interval, '\0', MAXDATELEN);
 fmt = (OraText *)malloc(MAXDATELEN);
 memset ((void *) fmt, '\0', MAXDATELEN);

 printf("-->Addition of timestamp with time zone and interval day to seconds\n\n");
   /* The following two cases show that daylight saving is automatically
      reflected with a time zone name */
 type1 = OCI_DTYPE_TIMESTAMP_TZ;
 type2 = OCI_DTYPE_INTERVAL_DS;
 strcpy((char *)interval,"0 08:00:00.00");
 strcpy((char *)fmt,"YYYY-MM-DD HH24:MI:SS TZR");

   /* Standard time + an interval = daylight saving time */
 strcpy((char *)dt,"2000-4-1 23:24:54 AMERICA/LOS_ANGELES");
 if (add_interval(dt, interval, fmt, type1, type2)) {
   printf("FAILED: add_interval()!\n");
   return OCI_ERROR;
 }
 
   /* Daylight saving time + an interval = standard time */
 strcpy((char *)dt,"2000-10-28 22:00:00 AMERICA/LOS_ANGELES");
 if (add_interval(dt, interval, fmt, type1, type2)) {
   printf("FAILED: add_interval()!\n");
   return OCI_ERROR;
 }
 
 return OCI_SUCCESS;
}


/* end of file cdemodt.c */
