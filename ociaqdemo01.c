#ifdef RCSID
static char *RCSid =
   "$Header: ociaqdemo01.c 27-mar-2006.23:11:40 azhao Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1998, 2006, Oracle. All rights reserved.  
*/

/*

   NAME
     ociaqdemo01.c - <one-line expansion of the name>

   DESCRIPTION
     <short description of component this file declares/defines>
    This program dequeues messages by blocking on prop_queue for
    approximately  2 minutes on behalf of agent "prog3". The contents
    of the messages are inserted into prog3_processed_data table.

   PUBLIC FUNCTION(S)
     <list of external functions declared/defined - with one-line descriptions>

   PRIVATE FUNCTION(S)
     <list of static functions defined in .c file - with one-line descriptions>

   RETURNS
     <function return values, for .c file with single function>

   NOTES
     <other useful comments, qualifications, etc.>

   MODIFIED   (MM/DD/YY)
   azhao       03/27/06 - fix 4993505 
   rbhyrava    04/05/04 - linux porting 
   ekarichk    12/18/03 - bug3328852: missing prototypes
   bnainani    09/23/99 - Bug 996772: change order of freeing handles
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   kmeiyyap    06/30/99 - free handles
   kmeiyyap    06/29/99 - free descriptor
   kmeiyyap    10/07/98 - added wait time for dequeue
   kmeiyyap    10/06/98 - oci enqueue
   kmeiyyap    10/06/98 - Creation

*/

#ifndef OCI_ORACLE
#include <oci.h>
#endif

#include <oratypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static void checkerr(OCIError *errhp, sword status);

static void checkerr(OCIError *errhp, sword status)
{
    text errbuf[512];
    ub4 buflen;
    sb4 errcode;

    switch (status)
    {
        case OCI_SUCCESS:
            break;
        case OCI_SUCCESS_WITH_INFO:
            printf("Error - OCI_SUCCESS_WITH_INFO\n");
            break;
        case OCI_NEED_DATA:
            printf("Error - OCI_NEED_DATA\n");
            break;
        case OCI_NO_DATA:
            printf("Error - OCI_NO_DATA\n");
            break;
        case OCI_ERROR:
            OCIErrorGet ((dvoid *) errhp, (ub4) 1, (text *) NULL, &errcode,
            errbuf, (ub4) sizeof(errbuf), (ub4) OCI_HTYPE_ERROR);
            printf("Error - %s\n", errbuf);
            exit(1); 
        case OCI_INVALID_HANDLE:
            printf("Error - OCI_INVALID_HANDLE\n");
            break;
        case OCI_STILL_EXECUTING:
            printf("Error - OCI_STILL_EXECUTE\n");
            break;
        case OCI_CONTINUE:
            printf("Error - OCI_CONTINUE\n");
            break;
        default:
        break;
    }
}

struct message
{
    OCINumber    id;
    OCIString   *city;
    OCINumber    priority;
};
typedef struct message message;

struct null_message
{
    OCIInd    null_adt;
    OCIInd    null_id;
    OCIInd    null_city;
    OCIInd    null_priority;
};
typedef struct null_message null_message;

static void clean_up( OCIEnv *envhp,
               OCISvcCtx *svchp,
               OCIServer *srvhp,
               OCIError *errhp,
               OCIStmt *stmthp,
               OCISession *usrhp,
               OCIAQDeqOptions *deqopt,
               message *mesg) ;

static void clean_up( OCIEnv *envhp,
               OCISvcCtx *svchp,
               OCIServer *srvhp,
               OCIError *errhp,
               OCIStmt *stmthp,
               OCISession *usrhp,
               OCIAQDeqOptions *deqopt,
               message *mesg) 
{
  /* free dequeue options descriptor */
  checkerr(errhp, OCIDescriptorFree((dvoid *)deqopt,
                                    OCI_DTYPE_AQDEQ_OPTIONS));
  /* free message buffer */
  if (mesg)
    checkerr(errhp, OCIObjectFree(envhp, errhp, (dvoid *)mesg,
                                    OCI_OBJECTFREE_FORCE));

  /* detach from the server */
  checkerr(errhp, OCISessionEnd(svchp, errhp, usrhp, OCI_DEFAULT));
  checkerr(errhp, OCIServerDetach(srvhp, errhp, (ub4)OCI_DEFAULT));

  if (usrhp)
    (void) OCIHandleFree((dvoid *) usrhp, (ub4) OCI_HTYPE_SESSION);
  if (svchp)
    (void) OCIHandleFree((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX);
  if (srvhp)
    (void) OCIHandleFree((dvoid *) srvhp, (ub4) OCI_HTYPE_SERVER);
  if (errhp)
    (void) OCIHandleFree((dvoid *) errhp, (ub4) OCI_HTYPE_ERROR);
  if (stmthp)
    (void) OCIHandleFree((dvoid *) stmthp, (ub4) OCI_HTYPE_STMT);
  if (envhp)
    (void) OCIHandleFree((dvoid *) envhp, (ub4) OCI_HTYPE_ENV);

  return;
}

int  main()
{

    OCIEnv            *envhp;
    OCIServer         *srvhp;
    OCIError          *errhp;
    OCISvcCtx         *svchp;
    OCIStmt           *stmthp;
    OCISession        *usrhp;

    dvoid             *tmp;
    OCIType           *mesg_tdo   = (OCIType *) 0;
    OCIAQDeqOptions   *deqopt     = (OCIAQDeqOptions *)0;
    message           *mesg       = (message *)0;
    null_message      *nmesg      = (null_message *)0;
    int                i;
    sb4                navigation = OCI_DEQ_FIRST_MSG;
    ub4                wait       = 120;
    OCIBind           *bndhp[3];
    sword              status;
    sword              retval;
    text              *sqlstmt01;
    int                id;
    int                priority;
    text              *city;
    text               errbuf[512];
    sb4                errcode;



    /* Standard OCI Initialization */

   OCIInitialize((ub4) OCI_OBJECT, (dvoid *)0,  (dvoid * (*)(dvoid *,size_t)) 0,
   (dvoid * (*)(dvoid *,dvoid * ,size_t)) 0,  (dvoid (*)(dvoid *,dvoid *)) 0 );

    OCIHandleAlloc( (dvoid *) NULL, (dvoid **) &envhp,
                    (ub4) OCI_HTYPE_ENV, 52, (dvoid **) &tmp);

    OCIEnvInit( &envhp, (ub4) OCI_DEFAULT, 21, (dvoid **) &tmp  );

    /* allocate a error report handle */
    OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &errhp,
                    (ub4) OCI_HTYPE_ERROR, 52, (dvoid **) &tmp);

    /* allocate a server context handle */
    OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &srvhp,
                    (ub4) OCI_HTYPE_SERVER, 52, (dvoid **) &tmp);

    /* Create an assocaition between the server and the oci application */
    OCIServerAttach( srvhp, errhp, (text *) 0, (sb4) 0, (ub4) OCI_DEFAULT);

    /* allocate a service context handle */
    OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &svchp,
                    (ub4) OCI_HTYPE_SVCCTX, 52, (dvoid **) &tmp);


    /* set attribute server context in the service context */
    OCIAttrSet( (dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX, (dvoid *)srvhp,
                (ub4) 0, (ub4) OCI_ATTR_SERVER, (OCIError *) errhp);

    /* allocate a user session handle */
    OCIHandleAlloc((dvoid *)envhp, (dvoid **)&usrhp, (ub4) OCI_HTYPE_SESSION,
                   (size_t) 0, (dvoid **) 0);

    OCIAttrSet((dvoid *)usrhp, (ub4)OCI_HTYPE_SESSION,
               (dvoid *)"aquser", (ub4)strlen("aquser"),
               OCI_ATTR_USERNAME, errhp);

    OCIAttrSet((dvoid *)usrhp, (ub4)OCI_HTYPE_SESSION,
               (dvoid *)"aquser", (ub4)strlen("aquser"),
               OCI_ATTR_PASSWORD, errhp);

    checkerr(errhp, OCISessionBegin (svchp, errhp, usrhp, OCI_CRED_RDBMS,
               OCI_DEFAULT));

    OCIAttrSet((dvoid *)svchp, (ub4)OCI_HTYPE_SVCCTX,
               (dvoid *)usrhp, (ub4)0, OCI_ATTR_SESSION, errhp);

    /* Allocate a statement handle */
    OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &stmthp,
                    (ub4) OCI_HTYPE_STMT, 52, (dvoid **) &tmp);

    checkerr(errhp, OCITypeByName(envhp, errhp, svchp,
            (CONST text *)"AQUSER", (ub4)strlen("AQUSER"),
            (CONST text *)"MESSAGE", (ub4)strlen("MESSAGE"), (text *)0, 0,
            OCI_DURATION_SESSION, OCI_TYPEGET_ALL, &mesg_tdo));


    checkerr(errhp, OCIDescriptorAlloc(envhp, (dvoid **)&deqopt,
                                 OCI_DTYPE_AQDEQ_OPTIONS, 0, (dvoid **)0));

    /* set wait time of 120 seconds in dequeue option */
    checkerr(errhp, OCIAttrSet(deqopt, OCI_DTYPE_AQDEQ_OPTIONS,
                            (dvoid *) &wait, 0, OCI_ATTR_WAIT, errhp));

    /* set navigation to OCI_DEQ_FIRST_MSG in dequeue option */
    checkerr(errhp, OCIAttrSet(deqopt, OCI_DTYPE_AQDEQ_OPTIONS,
                             (dvoid *)&navigation, sizeof(sb4),
                             OCI_ATTR_NAVIGATION, errhp));


    /* set consumer name to prog3 in dequeue option*/
    checkerr(errhp, OCIAttrSet(deqopt, OCI_DTYPE_AQDEQ_OPTIONS,
                             (dvoid *)"PROG3", (ub4)strlen("prog3"),
                             OCI_ATTR_CONSUMER_NAME, errhp));

    printf("Dequeuing messages ...\n");

    while(1)
    {
        printf("Waiting for messages ...\n");
        retval = OCIAQDeq(svchp, errhp,
                        (text *)"aquser.prop_queue",
                        deqopt, (OCIAQMsgProperties *)0, mesg_tdo, 
                        (dvoid **)&mesg,
                        (dvoid **)&nmesg,
                        (OCIRaw **)0, 0);
        if (retval == OCI_ERROR)
        {
            OCIErrorGet ((dvoid *) errhp, (ub4) 1, (text *) NULL,
                        &errcode, errbuf, (ub4) sizeof(errbuf),
                        (ub4) OCI_HTYPE_ERROR);

            /* if no more messages to dequeue exit */
            if (errcode == 25228)
            {
              clean_up(envhp, svchp, srvhp, errhp, stmthp, usrhp, deqopt,
                       mesg);
              printf("No more messages\n");
              exit(1);
            }
            else
              {
                printf("PROB HERE?\n");
                checkerr(errhp, retval);
              }
        }
        else
        {
          checkerr(errhp, retval);
        }


        /* update the tables */
        checkerr(errhp, OCINumberToInt(errhp, &mesg->id, sizeof(id),
                OCI_NUMBER_SIGNED, (dvoid *)&id));
        city = OCIStringPtr(envhp, mesg->city);
        checkerr(errhp, OCINumberToInt(errhp, &mesg->priority,
                sizeof(priority), OCI_NUMBER_SIGNED, (dvoid *)&priority));

        printf("\tDequeueing message - id: %d\n", id);
        printf("\tInserted message info into table PROG3_PROCESSED_DATA\n");
        sqlstmt01 = (text *)"INSERT INTO PROG3_PROCESSED_DATA \
                                VALUES (:id, :city, :priority)";

        /* prepare the statement */
        checkerr(errhp, OCIStmtPrepare(stmthp, errhp, sqlstmt01,
            (ub4)strlen((char *)sqlstmt01), (ub4)OCI_NTV_SYNTAX,
            (ub4)OCI_DEFAULT));

        /* binding placeholders in the insert statement */

        checkerr(errhp, OCIBindByName(stmthp, &bndhp[0], errhp,
            (text *) ":id", -1, (dvoid *) &id, sizeof(id),
            SQLT_INT, (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4) 0, (ub4 *)0,
            OCI_DEFAULT));

    checkerr(errhp, OCIBindByName(stmthp, &bndhp[1], errhp,
   (text *)":city", -1, (dvoid *)city, (sb4)strlen((const signed char *)city)+1,
            SQLT_STR, (dvoid *)0, (ub2 *)0, (ub2 *) 0, (ub4) 0, (ub4 *)0,
            OCI_DEFAULT));

        checkerr(errhp, OCIBindByName(stmthp, &bndhp[2], errhp,
   (text *) ":priority", -1, (dvoid *) &priority, sizeof(priority),
            SQLT_INT, (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)0, (ub4 *)0,
            OCI_DEFAULT));

        checkerr(errhp, OCIStmtExecute(svchp, stmthp, errhp, (ub4)1, (ub4) 0,
            (CONST OCISnapshot *) NULL, (OCISnapshot *) NULL , OCI_DEFAULT));

        checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));
    }
    exit(0);
}

/* end of file ociaqdemo01.c */

