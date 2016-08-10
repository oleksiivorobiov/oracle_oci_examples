/* Copyright (c) 2001, 2009, Oracle and/or its affiliates. 
All rights reserved. */

/*

   NAME
     nchdemo1.c - show nchar implicit conversion feature and codepoint feature

   DESCRIPTION
 * Demo description:
 * 1.Implicit conversion:
 *    the data being inserted into nchar column of this table is in
 *    zhs16gbk encoding, you don't need specify charsetfm to
 *    SQLCS_NCHAR in bind/define. implicit conversion will happend
 *    and automatic convert zhs16gbk data into al16utf16 while
 *    insert and convert it back while doing select.
 * 2.Benefit from codepoint semantics: Client side application
 *    independent of server character set.
 *    Since Nchar is always in codepoint semantics, via implict
 *    describe, you can get the char size of nchar column, the number
 *    is always in characters, so the client side buffer is allocated
 *    as number of characters times max char-width of client side
 *    charset. even server side ncharset is changed from al16utf16
 *    to utf8, you don't need re-allocate the client side buffer.
 *
 * Setup:  to show the implicit conversion feature, you need create your
 *    database with database character set is utf8, otherwise, data loss will
 *    happened when doing implicit conversion.
 *
 * Note: the data is in zhs16gbk encoding, other platform can't display the 
 *       chinese character correctly. 
 *


   EXPORT FUNCTION(S)
     <external functions defined for use outside package - one-line descriptions>

   INTERNAL FUNCTION(S)
     <other external functions defined - one-line descriptions>

   STATIC FUNCTION(S)
     <static functions defined - one-line descriptions>

   NOTES
     <other useful comments, qualifications, etc.>

   MODIFIED   (MM/DD/YY)
   wezhu       05/28/09 - remove hard tabs
   wezhu       05/21/09 - provide errhp to checkerr function call.
   chli        11/04/02 - fix bug2840920
   chli        07/20/01 - fix bug1891086
   chli        06/20/01 - fix compile warning
   chli        03/15/01 - Merged chli_fixdiff_0313
   chli        03/15/01 - Creation

*/


#ifndef STDIO
#include <stdio.h>
#endif

#ifndef  CTYPE
#include <ctype.h>
#endif

#ifndef OCI_ORACLE
#include <oci.h>
#endif

#define OCI_UNICODE_U OCI_UNICODE
#define OCI_UNICODE_D OCI_DEFAULT
#define OCI_UNICODE_N OCI_NON_UNICODE

OCIEnv *envhp;
OCIServer *srvhp;
OCIError *errhp;
OCISvcCtx *svchp;
OCISession *authp;


void main();
static void checkerr(/*errhp, status*/);
sword init_env(/*_envhp, svchp, errhp, srvhp, authp, env_mode_*/);
sword attach_server(/*_server_mode, srvhp, errhp, svchp_*/);
sword log_on(/*_authp, errhp, svchp, uid, pwd, credt, session_mode_*/);
sword free_handles();


void main()
{

ub4 env_mode = OCI_UNICODE_D;
ub4 server_mode = OCI_UNICODE_D;
ub4 session_mode = OCI_UNICODE_D;
ub4 stmt_mode = OCI_UNICODE_D;
ub4 bind_mode = OCI_UNICODE_D;
ub4 def_mode  = OCI_UNICODE_D;

text *uid = (text *)"oe";
text *pwd = (text *)"oe";

static text *stmt=(text *)"INSERT INTO product_descriptions VALUES (:product_id, :language_id, :translated_name, :translated_description)";
static text *sel_stmt=(text *)"SELECT * FROM product_descriptions where product_id= :id";
static text *del_stmt=(text *)"DELETE FROM product_descriptions where product_id= :id";

OCIStmt *stmt1p;
OCIStmt *stmt2p;
OCIStmt *stmt3p;

OCIParam *mypard;

static OCIBind *bnd1p = (OCIBind *)0;
static OCIBind *bnd2p = (OCIBind *)0;
static OCIBind *bnd3p = (OCIBind *)0;
static OCIBind *bnd4p = (OCIBind *)0;
static OCIBind *bnd5p = (OCIBind *)0;

static OCIDefine *dfn1p,*dfn4p,*dfn2p,*dfn3p;

static text *colname1 = (text *)":product_id";
static text *colname2 = (text *)":language_id";
static text *colname3 = (text *)":translated_name";
static text *colname4 = (text *)":translated_description";
static text *colname5 = (text *)":id";

ub2 sqltype1 = SQLT_INT;
ub2 sqltype2 = SQLT_DAT;
ub2 sqltype3 = SQLT_STR;
ub2 sqltype4 = SQLT_STR;
ub2 sqltype5 = SQLT_BIN;


ub4 credt = OCI_CRED_RDBMS;
char char1_in[4],char2_in[50],char3_in[100];
sb2 ind=0;
dvoid *tmp;
sword err, status, i;
sword in_id, out_id;

/*char *char1_outp, *char2_outp, *char3_outp;*/
char char1_outp[40], char2_outp[40], char3_outp[40];

ub4 bmaxsiz = 20;
ub4 cmaxsiz = 20;
ub1 char_semantics;
ub2 col_width2, col_width3, col_width4;
ub2 chrsetid = 852;
ub1 flag2, flag3, flag4;


   in_id=100081;
   strcpy(char1_in,"zhs");
   char1_in[3]='\0';
   strcpy(char2_in,"是干");
   char2_in[8]='\0';
   strcpy(char3_in,"银柳是干花的一种，放于室内可经年不谢");
   char3_in[36]='\0';
   
/*
   char1_outp=(char *)malloc(40);
   char2_outp=(char *)malloc(40);
   char3_outp=(char *)malloc(40);
*/

   for (i=0; i<40; i++)
     {
     char1_outp[i]='\0';
     char2_outp[i]='\0';
     char3_outp[i]='\0';
     } 

   init_env(env_mode);
   attach_server(server_mode);
   log_on(uid, pwd, credt, session_mode);
   
/*---- Insert operation -------*/
   checkerr(errhp, OCIHandleAlloc((dvoid *) envhp, (dvoid **)&stmt1p, 
                           (ub4)OCI_HTYPE_STMT,100, (dvoid **) &tmp));
   checkerr(errhp, OCIStmtPrepare(stmt1p, errhp, stmt, (ub4)strlen((char *)stmt),
                           (ub4)OCI_NTV_SYNTAX, (ub4)stmt_mode));
   
   checkerr(errhp, OCIBindByName(stmt1p, &bnd1p, errhp, (text *)colname1,
                   (sb4)strlen((char*)colname1),
                   (dvoid *)&in_id, (sb4)sizeof(sword), sqltype1, (dvoid *)&ind,
                   (ub2 *) 0, (ub2 *) 0, (ub4) 0,(ub4 *)0,bind_mode));

  checkerr(errhp, OCIBindByName(stmt1p, &bnd2p, errhp, (text *)colname2,
                  (sb4)strlen((char*)colname2),
                  (dvoid *)char1_in, (sb4)sizeof(char1_in), sqltype3, 
                  (dvoid *)&ind,
                  (ub2 *) 0, (ub2 *) 0, (ub4) 0,(ub4 *)0,bind_mode));
  checkerr(errhp, OCIBindByName(stmt1p, &bnd3p, errhp, (text *)colname3,
                  (sb4)strlen((char*)colname3),
                  (dvoid *)char2_in, (sb4)sizeof(char2_in), sqltype3,
                  (dvoid *)&ind,(ub2 *) 0, (ub2 *) 0, 
                  (ub4) 0,(ub4 *)0,bind_mode));
  checkerr(errhp, OCIBindByName(stmt1p, &bnd4p, errhp, (text *)colname4,
                  (sb4)strlen((char*)colname4),
                  (dvoid *)char3_in, (sb4)sizeof(char3_in), sqltype3,
                  (dvoid *)&ind,(ub2 *) 0, (ub2 *) 0,
                  (ub4) 0,(ub4 *)0,bind_mode));

/* set character id 852(zhs16gbk) for varchar2 and nvarchar2 column 
   you don't need specify charset_form here, since oracle will do the 
   implicit conversion from char to nchar for you.
*/


  checkerr(errhp,OCIAttrSet((dvoid *)bnd2p, (ub4) OCI_HTYPE_BIND,
                (dvoid *)&chrsetid, (ub4) 0,
                (ub4) OCI_ATTR_CHARSET_ID, errhp));
  checkerr(errhp,OCIAttrSet((dvoid *)bnd3p, (ub4) OCI_HTYPE_BIND,
                (dvoid *)&chrsetid, (ub4) 0,
                (ub4) OCI_ATTR_CHARSET_ID, errhp));
  checkerr(errhp,OCIAttrSet((dvoid *)bnd4p, (ub4) OCI_HTYPE_BIND,
                (dvoid *)&chrsetid, (ub4) 0,
                (ub4) OCI_ATTR_CHARSET_ID, errhp));


   err=OCIStmtExecute (svchp, stmt1p, errhp, (ub4)1, (ub4)0,
                   (OCISnapshot *)NULL,(OCISnapshot *)NULL, (ub4)OCI_DEFAULT);

   if (err == OCI_SUCCESS )
        printf("Insert successful\n");
   else
      {
        checkerr(errhp,err);
        return;
      }

   OCITransCommit(svchp, errhp, 0);
   
/* end: ------  Insert complete -----------------*/


/* begin: ------ get char counts of each column by call OCIParamGet ---- */ 

   checkerr(errhp, OCIHandleAlloc((dvoid *) envhp, (dvoid **)&stmt2p,
                           (ub4)OCI_HTYPE_STMT,100, (dvoid **) &tmp));
   checkerr(errhp, OCIStmtPrepare(stmt2p, errhp, sel_stmt, (ub4)strlen((char *)sel_stmt),
                           (ub4)OCI_NTV_SYNTAX, (ub4)stmt_mode));

   checkerr(errhp, OCIStmtExecute(svchp,stmt2p,errhp,(ub4)1, 0,
                           (OCISnapshot *)NULL, (OCISnapshot *)NULL, (ub4)OCI_DESCRIBE_ONLY));
   err = OCIParamGet(stmt2p,OCI_HTYPE_STMT,errhp,&mypard, (ub4)2);
   if (err == OCI_SUCCESS)
      {
      checkerr(errhp, OCIAttrGet((dvoid *)mypard,(ub4)OCI_DTYPE_PARAM,
        (dvoid *)&char_semantics, (ub4)0, (ub4)OCI_ATTR_CHAR_USED,
        (OCIError *)errhp));
     if (char_semantics) {
        checkerr(errhp,OCIAttrGet((dvoid *)mypard,(ub4)OCI_DTYPE_PARAM,
        (dvoid *)&col_width2,(ub4)0,(ub4)OCI_ATTR_CHAR_SIZE,(OCIError *)errhp));
        flag2=1;
        }
      else {
        checkerr(errhp,OCIAttrGet((dvoid *)mypard,(ub4)OCI_DTYPE_PARAM,
        (dvoid *)&col_width2,(ub4)0,(ub4)OCI_ATTR_DATA_SIZE,(OCIError *)errhp));
        flag2=0;
        }
      }
    else
       checkerr(errhp,err);
      
   err = OCIParamGet(stmt2p,OCI_HTYPE_STMT,errhp,&mypard, (ub4)3);
   if (err == OCI_SUCCESS)
      {
      checkerr(errhp, OCIAttrGet((dvoid *)mypard,(ub4)OCI_DTYPE_PARAM,
        (dvoid *)&char_semantics, (ub4)0, (ub4)OCI_ATTR_CHAR_USED,
        (OCIError *)errhp));
      if (char_semantics) {
        checkerr(errhp,OCIAttrGet((dvoid *)mypard,(ub4)OCI_DTYPE_PARAM,
        (dvoid *)&col_width3,(ub4)0,(ub4)OCI_ATTR_CHAR_SIZE,(OCIError *)errhp));
        flag3=1;
      }
      else {
        checkerr(errhp,OCIAttrGet((dvoid *)mypard,(ub4)OCI_DTYPE_PARAM,
        (dvoid *)&col_width3,(ub4)0,(ub4)OCI_ATTR_DATA_SIZE,(OCIError *)errhp));
        flag3=0;
      }

        /* since in codepoint semantics mode, the col_width is always return in
         number of characters, the client side buffer size should be the number 
         of characters tims maximun char-width of client side charset. the max 
         charwidth for zhs16gbk is 2.  */
   /*   char2_outp=(char *)malloc(sizeof(char)*col_width3*2);  */
      }
   else
      checkerr(errhp,err); 

  /* ---- get char count of second column. ---- */
   err = OCIParamGet(stmt2p,OCI_HTYPE_STMT,errhp,&mypard, (ub4)4);
   if (err == OCI_SUCCESS)
      {
      checkerr(errhp, OCIAttrGet((dvoid *)mypard,(ub4)OCI_DTYPE_PARAM,
        (dvoid *)&char_semantics, (ub4)0, (ub4)OCI_ATTR_CHAR_USED,
        (OCIError *)errhp));
      if (char_semantics) {
        checkerr(errhp,OCIAttrGet((dvoid *)mypard,(ub4)OCI_DTYPE_PARAM,
        (dvoid *)&col_width4,(ub4)0,(ub4)OCI_ATTR_CHAR_SIZE,(OCIError *)errhp));
        flag4=1;
        }
      else {
        checkerr(errhp,OCIAttrGet((dvoid *)mypard,(ub4)OCI_DTYPE_PARAM,
        (dvoid *)&col_width4,(ub4)0,(ub4)OCI_ATTR_DATA_SIZE,(OCIError *)errhp));
        flag4=0;
        }
/*
      char3_outp=(char *)malloc(sizeof(char)*col_width4*2); 
*/
      }
   else
      checkerr(errhp,err);
   

   /* ---- define the output buffer and do the query ---- */
   checkerr(errhp,OCIDefineByPos(stmt2p, &dfn1p, errhp, 1, (dvoid *)&out_id, 
              sizeof(out_id), sqltype1, (dvoid *)0, (ub2 *)0, 
              (ub2 *)0, def_mode));
   checkerr(errhp,OCIDefineByPos(stmt2p, &dfn2p, errhp, 2, (dvoid *)char1_outp,
    9, sqltype3, (dvoid *)0, (ub2 *)0, (ub2 *)0, def_mode));
   checkerr(errhp,OCIDefineByPos(stmt2p, &dfn3p, errhp, 3, (dvoid *)char2_outp,
    col_width3, sqltype3, (dvoid *)0, (ub2 *)0, (ub2 *)0, def_mode));
   checkerr(errhp,OCIDefineByPos(stmt2p, &dfn4p, errhp, 4, (dvoid *)char3_outp,
    col_width4, sqltype3, (dvoid *)0, (ub2 *)0, (ub2 *)0, def_mode));

/* --- set the charset-id to 852 for output buffer ---- */

  checkerr(errhp,OCIAttrSet((dvoid *)dfn2p, (ub4) OCI_HTYPE_DEFINE,
                (dvoid *)&chrsetid, (ub4) 0,
                (ub4) OCI_ATTR_CHARSET_ID, errhp));

  checkerr(errhp,OCIAttrSet((dvoid *)dfn3p, (ub4) OCI_HTYPE_DEFINE,
                (dvoid *)&chrsetid, (ub4) 0,
                (ub4) OCI_ATTR_CHARSET_ID, errhp));

  checkerr(errhp,OCIAttrSet((dvoid *)dfn4p, (ub4) OCI_HTYPE_DEFINE,
                (dvoid *)&chrsetid, (ub4) 0,
                (ub4) OCI_ATTR_CHARSET_ID, errhp));

   checkerr(errhp, OCIBindByName(stmt2p, &bnd5p, errhp, (text *)colname5,
                   (sb4)strlen((char*)colname5),
                   (dvoid *)&in_id, (sb4)sizeof(sword), sqltype1, (dvoid *)&ind,
                   (ub2 *) 0, (ub2 *) 0, (ub4) 0,(ub4 *)0,bind_mode));

   err = OCIStmtExecute (svchp, stmt2p, errhp, (ub4)1, (ub4)0,
            (OCISnapshot *)NULL,(OCISnapshot *)NULL, (ub4)OCI_DEFAULT);

   checkerr(errhp, err);
   if (err == OCI_SUCCESS || err == OCI_SUCCESS_WITH_INFO)
   {
      do
      {
        printf("product_id is : %d\n", out_id);
         if (flag2==1)
             printf("column language id is char semantics and the width is: %d chars\n",col_width2);
         else
             printf("column language id is byte semantics and the width is: %d bytes\n",col_width2);
         printf("language_id is : %s\n\n", char1_outp);
         
         if (flag3==1)
             printf("column translated_name is char semantics and the width is: %d chars\n",col_width3);
         else
             printf("column translated_name is byte semantics and the width is: %d bytes\n",col_width3);
         printf("translated_name is : %s\n\n", char2_outp);

         if (flag4==1)
             printf("column product_description is char semantics and the width is: %d chars\n",col_width4);
         else
             printf("column product_description is byte semantics and the width is: %d bytes\n",col_width4);
         printf("product_description is : %s\n\n", char3_outp);

         status = OCIStmtFetch(stmt2p, errhp, (ub4) 1,
                                (ub4) OCI_FETCH_NEXT,
                                (ub4) OCI_DEFAULT);
      }while (status == OCI_SUCCESS || status == OCI_SUCCESS_WITH_INFO);
    }
  
/****** delete temporary data *******/

   checkerr(errhp, OCIHandleAlloc((dvoid *) envhp, (dvoid **)&stmt3p,
                           (ub4)OCI_HTYPE_STMT,100, (dvoid **) &tmp));
   checkerr(errhp, OCIStmtPrepare(stmt3p, errhp, del_stmt, 
                   (ub4)strlen((char *)del_stmt),
                           (ub4)OCI_NTV_SYNTAX, (ub4)stmt_mode));

   checkerr(errhp, OCIBindByName(stmt3p, &bnd5p, errhp, (text *)colname5,
                   (sb4)strlen((char*)colname5),
                   (dvoid *)&in_id, (sb4)sizeof(sword), sqltype1, (dvoid *)&ind,
                   (ub2 *) 0, (ub2 *) 0, (ub4) 0,(ub4 *)0,bind_mode));

   checkerr(errhp, OCIStmtExecute(svchp,stmt3p,errhp,(ub4)1, 0,
          (OCISnapshot *)NULL, (OCISnapshot *)NULL, (ub4)OCI_DEFAULT));

 /*free(char1_outp);
 free(char2_outp);
 free(char3_outp); */
 OCIHandleFree((dvoid *) stmt1p, (ub4) OCI_HTYPE_STMT);
 OCIHandleFree((dvoid *) stmt2p, (ub4) OCI_HTYPE_STMT);
 OCIHandleFree((dvoid *) stmt3p, (ub4) OCI_HTYPE_STMT);
 free_handles();

}

static void checkerr(errhp, status)
OCIError *errhp;
sword status;
{
  text errbuf[512];
  ub4 buflen;
  sb4 errcode;

  switch (status)
  {
  case OCI_SUCCESS:
    break;
  case OCI_SUCCESS_WITH_INFO:
    fprintf(stdout, "Error - OCI_SUCCESS_WITH_INFO\n");
    break;
  case OCI_NEED_DATA:
    fprintf(stdout, "Error - OCI_NEED_DATA\n");
    break;
  case OCI_NO_DATA:
    fprintf(stdout, "Error - OCI_NO_DATA\n"); 
    break;
  case OCI_ERROR:
    OCIErrorGet ((dvoid *) errhp, (ub4) 1, (text *) NULL, &errcode,
            errbuf, (ub4) sizeof(errbuf), (ub4) OCI_HTYPE_ERROR);
    fprintf(stdout, "Error - %s\n", errbuf);
    break;
  case OCI_INVALID_HANDLE:
    fprintf(stdout, "Error - OCI_INVALID_HANDLE\n");
    break;
  case OCI_STILL_EXECUTING:
    fprintf(stdout, "Error - OCI_STILL_EXECUTE\n");
    break;
  case OCI_CONTINUE:
    fprintf(stdout, "Error - OCI_CONTINUE\n");
    break;
  default:
    break;
  }
}


/* ----------------------------------------------------------------- */
/* initialize environment, allocate handles                          */
/* ----------------------------------------------------------------- */

sword init_env(env_mode)
ub4 env_mode;
{

  if (OCIEnvCreate((OCIEnv **) &envhp, env_mode, (dvoid *)0,
                    (dvoid * (*)(dvoid *, size_t)) 0,
                    (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                    (void (*)(dvoid *, dvoid *)) 0,
                    (size_t) 0, (dvoid **) 0 ))
  {
    printf("FAILED: OCIEnvCreate()\n");
    return OCI_ERROR;
  }

  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &svchp,
                     (ub4) OCI_HTYPE_SVCCTX, (size_t) 0, (dvoid **) 0))
  {
    printf("FAILED: OCIHandleAlloc() on svchp\n");
    return OCI_ERROR;
  }

  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &errhp,
                     (ub4) OCI_HTYPE_ERROR, (size_t) 0, (dvoid **) 0))
  {
    printf("FAILED: OCIHandleAlloc() on errhp\n");
    return OCI_ERROR;
  }

  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &srvhp,
                     (ub4) OCI_HTYPE_SERVER, (size_t) 0, (dvoid **) 0))
  {
    printf("FAILED: OCIHandleAlloc() on srvhp\n");
    return OCI_ERROR;
  }

  if (OCIHandleAlloc((dvoid *) envhp, (dvoid **) &authp,
                     (ub4) OCI_HTYPE_SESSION, (size_t) 0, (dvoid **) 0))
  {
    printf("FAILED: OCIHandleAlloc() on authp\n");
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}

/* ----------------------------------------------------------------- */
/* attach to server with a given mode.                               */
/* ----------------------------------------------------------------- */

sword attach_server(server_mode)
ub4 server_mode;
{

  if (OCIServerAttach(srvhp, errhp, (text *) "",
                     (sb4) strlen(""), (ub4) server_mode))
  {
    printf("FAILED: OCIServerAttach()\n");
    return OCI_ERROR;
  }

  if (OCIAttrSet((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
                 (dvoid *) srvhp, (ub4) 0, (ub4) OCI_ATTR_SERVER, errhp))
  {
    printf("FAILED: OCIAttrSet() server attribute\n");
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}
sword log_on(uid, pwd, credt, session_mode)
text *uid;
text *pwd;
ub4 credt;
ub4 session_mode;
{
  if (OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION,
                 (dvoid *) uid, (ub4) strlen((char *) uid),
                 (ub4) OCI_ATTR_USERNAME, errhp))
  {
    printf("FAILED: OCIAttrSet() userid\n");
    return OCI_ERROR;
  }
  if (OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION,
                 (dvoid *) pwd, (ub4) strlen((char *) pwd),
                 (ub4) OCI_ATTR_PASSWORD, errhp))
  {
    printf("FAILED: OCIAttrSet() passwd\n");
    return OCI_ERROR;
  }


  printf("Logging on as %s  ....\n", uid);

  if (OCISessionBegin(svchp, errhp, authp, credt, session_mode))
  {
    printf("FAILED: OCIAttrSet() passwd\n");
    return OCI_ERROR;
  }

  printf("%s logged on.\n", uid);

  if (OCIAttrSet((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
                 (dvoid *) authp, (ub4) 0, (ub4) OCI_ATTR_SESSION, errhp))
  {
    printf("FAILED: OCIAttrSet() session\n");
    return OCI_ERROR;
  }

  return OCI_SUCCESS;
}


sword free_handles()
{
   OCISessionEnd(svchp, errhp, authp, (ub4)OCI_DEFAULT);
   OCIServerDetach( srvhp, errhp, (ub4) OCI_DEFAULT );
   OCIHandleFree((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX);
   OCIHandleFree((dvoid *) errhp, (ub4) OCI_HTYPE_ERROR);
   OCIHandleFree((dvoid *) authp, (ub4) OCI_HTYPE_SESSION);
   OCIHandleFree((dvoid *) srvhp, (ub4) OCI_HTYPE_SERVER);
   return OCI_SUCCESS;

}


/*---------------------------------------------------------------------------
                     PRIVATE TYPES AND CONSTANTS
  ---------------------------------------------------------------------------*/
                
/*---------------------------------------------------------------------------
                     STATIC FUNCTION DECLARATIONS 
  ---------------------------------------------------------------------------*/


/* end of file nchdemo1.c */

