#ifdef RCSID
static char *RCSid =
   "$Header: ociaqdemo02.c 05-apr-2007.05:15:14 dprasann Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1998, 2007, Oracle. All rights reserved.  
*/

/*

   NAME
     ociaqdemo02.c - <one-line expansion of the name>

   DESCRIPTION
     <short description of component this file declares/defines>
    This program listens on input_queue for approxmately 2 minutes on behalf
    of two agents, "prog1" and "prog2". When there are messages for the agents
    in input_queue, the messages are dequeued and message contents are
    inserted into appropriate tables (messages for "prog1" is inserted into
    prog1_processed_data. messages for "prog2" is inserted into
    prog2_processed_data.)

   PUBLIC FUNCTION(S)
     <list of external functions declared/defined - with one-line descriptions>

   PRIVATE FUNCTION(S)
     <list of static functions defined in .c file - with one-line descriptions>

   RETURNS
     <function return values, for .c file with single function>

   NOTES
     <other useful comments, qualifications, etc.>

   MODIFIED   (MM/DD/YY)
   dprasann    04/05/07 - lowercase username/password
   azhao       03/27/06 - fix 4993505 
   rbhyrava    04/05/04 - linux porting 
   ekarichk    12/18/03 - bug3328852: missing prototypes
   lzhao       06/25/01 - comp warning fix
   bnainani    09/23/99 - Bug 996772: change order of freeing handles
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   kmeiyyap    06/30/99 - free memory buffer
   kmeiyyap    06/29/99 - remove time.h include file
   kmeiyyap    11/14/98 - fix gettimeofday parameters
   kmeiyyap    10/07/98 - handle timeout error
   kmeiyyap    10/06/98 - listen on queue
   kmeiyyap    10/06/98 - Creation

*/

#ifndef OCI_ORACLE
#include <oci.h>
#endif

#include <oratypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* general OCI context */
struct oci_ctx
{
    OCIEnv         *envhp;         /* Environment handle               */
    OCISvcCtx      *svchp;         /* Service context handle           */
    OCIServer      *srvhp;         /* Server handles                   */
    OCIError       *errhp;         /* Error handle                     */
    OCITrans       *txnhp;         /* Transaction handle               */
    OCISession     *usrhp;         /* User handle                      */
    char           server[40];     /* Server system id                 */
    OCIStmt        *stmthp;        /* Statement handle                 */
};
typedef struct oci_ctx oci_ctx;


/* dequeue context */
struct deq_ctx
{
    OCIAQDeqOptions         *dopt;
    OCIAQMsgProperties      *msgprop;
    OCIType                 *mesg_tdo;
};
typedef struct deq_ctx deq_ctx;

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

static void checkerr( 
        OCIError *errhp, 
        sword status) ;

static void update_table( oci_ctx  *ctx,
                   text *appname,
                   message *mesg);

static void clean_up( oci_ctx ctx,
               deq_ctx dctx,
               message *mesg,
               OCIAQAgent **agent_list);

static void SetAgent( OCIAQAgent *agent,
               text       *appname,
               text       *queue,
               OCIError   *errhp);

static text       *GetAgent( OCIAQAgent *agent,
                      OCIError   *errhp,
                      text       *qname);

static void      init_ctx( oci_ctx  *ctx,
                    text     *username,
                    text     *password );

static void            setup_dequeue(
                oci_ctx       *octx,
                deq_ctx       *dqctx,
                text          *message_type);

static void update_table( oci_ctx  *ctx,
                   text *appname,
                   message *mesg);

static void checkerr( OCIError *errhp,
                      sword status) 
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


static void clean_up( oci_ctx ctx,
               deq_ctx dctx,
               message *mesg,
               OCIAQAgent **agent_list)
{
  int i;
  for(i = 0; i < 2; i++)
  {
    checkerr(ctx.errhp, OCIDescriptorFree(agent_list[i],
                        OCI_DTYPE_AQAGENT));
  }

  /* free dequeue options descriptor */
  checkerr(ctx.errhp, OCIDescriptorFree((dvoid *)dctx.dopt,
                                        OCI_DTYPE_AQDEQ_OPTIONS));
  checkerr(ctx.errhp, OCIDescriptorFree((dvoid *)dctx.msgprop,
                                        OCI_DTYPE_AQMSG_PROPERTIES));
  /* free message buffer */
  if (mesg)
    checkerr(ctx.errhp, OCIObjectFree(ctx.envhp, ctx.errhp,
                                    (dvoid *)mesg, OCI_OBJECTFREE_FORCE));

  /* detach from the server */
  checkerr(ctx.errhp, OCISessionEnd(ctx.svchp, ctx.errhp,
                                    ctx.usrhp, OCI_DEFAULT));
  checkerr(ctx.errhp, OCIServerDetach(ctx.srvhp, ctx.errhp, (ub4)OCI_DEFAULT));

  /* free handles */
  if (ctx.usrhp)
    (void) OCIHandleFree((dvoid *) ctx.usrhp, (ub4) OCI_HTYPE_SESSION);
  if (ctx.svchp)
    (void) OCIHandleFree((dvoid *) ctx.svchp, (ub4) OCI_HTYPE_SVCCTX);
  if (ctx.srvhp)
    (void) OCIHandleFree((dvoid *) ctx.srvhp, (ub4) OCI_HTYPE_SERVER);
  if (ctx.errhp)
    (void) OCIHandleFree((dvoid *) ctx.errhp, (ub4) OCI_HTYPE_ERROR);
  if (ctx.stmthp)
    (void) OCIHandleFree((dvoid *) ctx.stmthp, (ub4) OCI_HTYPE_STMT);
  if (ctx.envhp)
    (void) OCIHandleFree((dvoid *) ctx.envhp, (ub4) OCI_HTYPE_ENV);
  return;
}


/* set agent into descriptor */
static void SetAgent( OCIAQAgent *agent,
               text       *appname,
               text       *queue,
               OCIError   *errhp)
{
    /* Set agent name */
    checkerr(errhp, OCIAttrSet(agent, OCI_DTYPE_AQAGENT,
                appname ? (dvoid *)appname : (dvoid *)"",
                appname ? (ub4)strlen((const char *)appname) : 0,
                OCI_ATTR_AGENT_NAME, errhp));

    /* Set agent address */
    checkerr(errhp, OCIAttrSet(agent, OCI_DTYPE_AQAGENT,
                queue ? (dvoid *)queue : (dvoid *)"",
                queue ? (ub4)strlen((const char *)queue) : 0,
                OCI_ATTR_AGENT_ADDRESS, errhp));
}


/* get agent from descriptor */
static text       *GetAgent( OCIAQAgent *agent,
                      OCIError   *errhp,
                      text       *qname)
{
    text       *appname;
    text       *queue;
    ub4         appsz;
    ub4         queuesz;

    if (!agent )
    {
        printf("agent was NULL \n");
        return (text *)"agent was NULL";
    }
    checkerr(errhp, OCIAttrGet(agent, OCI_DTYPE_AQAGENT,
                (dvoid *)&appname, &appsz, OCI_ATTR_AGENT_NAME, errhp));
    checkerr(errhp, OCIAttrGet(agent, OCI_DTYPE_AQAGENT,
                (dvoid *)&queue, &queuesz, OCI_ATTR_AGENT_ADDRESS, errhp));
    if (!appsz)
        printf("agent name: NULL\n");

    if (!queuesz)
        printf("agent address: NULL\n");

    memcpy((dvoid *)qname, (dvoid *)queue, (size_t)queuesz);

    qname[queuesz] = '\0';

    return qname;
}


/* Initialize the OCI context */

static void      init_ctx( oci_ctx  *ctx,
                    text     *username,
                    text     *password )
{
    dvoid *tmp;

    OCIHandleAlloc( (dvoid *) NULL, (dvoid **) &ctx->envhp,
                (ub4) OCI_HTYPE_ENV, 52, (dvoid **) &tmp);

    OCIEnvInit( &ctx->envhp, (ub4) OCI_DEFAULT, 21, (dvoid **) &tmp  );

    /* allocate a error report handle */
    OCIHandleAlloc((dvoid *) ctx->envhp, (dvoid **) &ctx->errhp,
                 (ub4) OCI_HTYPE_ERROR, 52, (dvoid **) &tmp);

    /* allocate a server context handle */
    OCIHandleAlloc( (dvoid *) ctx->envhp, (dvoid **) &ctx->srvhp,
                 (ub4) OCI_HTYPE_SERVER, 52, (dvoid **) &tmp);

    /* Create an assocaition between the server and the oci application */
    OCIServerAttach(ctx->srvhp, ctx->errhp, (text *) 0, (sb4) 0,
                  (ub4) OCI_DEFAULT);

    /* allocate a service context handle */
    OCIHandleAlloc( (dvoid *) ctx->envhp, (dvoid **) &ctx->svchp,
                 (ub4) OCI_HTYPE_SVCCTX, 52, (dvoid **) &tmp);

    /* set attribute server context in the service context */
    OCIAttrSet( (dvoid *) ctx->svchp, (ub4) OCI_HTYPE_SVCCTX,
             (dvoid *)ctx->srvhp, (ub4) 0,
             (ub4) OCI_ATTR_SERVER, (OCIError *) ctx->errhp);

    /* allocate a user session handle */
    OCIHandleAlloc((dvoid *)ctx->envhp, (dvoid **)&ctx->usrhp,
                 (ub4) OCI_HTYPE_SESSION, (size_t) 0, (dvoid **) 0);

    OCIAttrSet(ctx->usrhp, (ub4)OCI_HTYPE_SESSION,
           (dvoid *)username, (ub4)strlen((const signed char *)username),
            OCI_ATTR_USERNAME, ctx->errhp);


    OCIAttrSet(ctx->usrhp, (ub4)OCI_HTYPE_SESSION,
           (dvoid *)password, (ub4)strlen((const signed char *)password),
           OCI_ATTR_PASSWORD, ctx->errhp);

    checkerr(ctx->errhp, OCISessionBegin(ctx->svchp, ctx->errhp, ctx->usrhp,
                                       OCI_CRED_RDBMS, OCI_DEFAULT));

    OCIAttrSet(ctx->svchp, (ub4)OCI_HTYPE_SVCCTX,
           (dvoid *)ctx->usrhp, (ub4)0, OCI_ATTR_SESSION, ctx->errhp);

    /* Allocate a statement handle */
    OCIHandleAlloc( (dvoid *) ctx->envhp, (dvoid **) &ctx->stmthp,
    (ub4) OCI_HTYPE_STMT, (size_t)0, (dvoid **) 0);

}


/* set up the context for dequeue */
static void            setup_dequeue(
                oci_ctx       *octx,
                deq_ctx       *dqctx,
                text          *message_type)
{
    ub4             navigation = OCI_DEQ_FIRST_MSG;
    OCIDescribe   *dschp = (OCIDescribe *)0;
    OCIParam      *type_param = (OCIParam *)0;

    dqctx->dopt = (OCIAQDeqOptions *)0;
    dqctx->msgprop = (OCIAQMsgProperties *)0;
    dqctx->mesg_tdo = (OCIType *)0;

    /* message properties descriptor */
    checkerr(octx->errhp,
           OCIDescriptorAlloc(octx->envhp, (dvoid **) &dqctx->msgprop,
                              OCI_DTYPE_AQMSG_PROPERTIES, 0, (dvoid **)0));

    /* dequeue options descriptor */
    checkerr(octx->errhp,
           OCIDescriptorAlloc(octx->envhp, (dvoid **) &dqctx->dopt,
                              OCI_DTYPE_AQDEQ_OPTIONS, 0, (dvoid **)0));

    /* set navigation in dequeue options */
    checkerr(octx->errhp, OCIAttrSet(dqctx->dopt, OCI_DTYPE_AQDEQ_OPTIONS,
                             (dvoid *)&navigation, 0,
                             OCI_ATTR_NAVIGATION, octx->errhp));


    /* get TOID */
    checkerr(octx->errhp, OCIHandleAlloc((dvoid *) octx->envhp,
       (dvoid **)&dschp, (ub4) OCI_HTYPE_DESCRIBE, (size_t) 0, (dvoid **) 0));


    checkerr(octx->errhp, OCIDescribeAny(octx->svchp, octx->errhp,
       (dvoid *)message_type, (ub4)strlen((const signed char *)message_type),
                 OCI_OTYPE_NAME, 0, OCI_PTYPE_TYPE, dschp));

    /* get the type descriptor */
    checkerr(octx->errhp, OCIAttrGet((dvoid *)dschp,
                (ub4)OCI_HTYPE_DESCRIBE, (dvoid *)&type_param,
                (ub4 *)0, (ub4)OCI_ATTR_PARAM, (OCIError *)octx->errhp));


    /* get the tdo of the type */
    checkerr(octx->errhp, OCIAttrGet((dvoid *)type_param,
                (ub4)OCI_DTYPE_PARAM, (dvoid *)&dqctx->mesg_tdo,
                (ub4 *)0, (ub4)OCI_ATTR_TDO, (OCIError *)octx->errhp));
}

static void update_table( oci_ctx  *ctx,
                   text *appname,
                   message *mesg)
{

    text *sqlstmt01;
    OCIBind *bndhp[3];
    int id;
    int priority;
    text *city;

    if (strcmp((const signed char *)appname, "PROG1") == 0 )
    {
        sqlstmt01 = (text *)"INSERT INTO PROG1_PROCESSED_DATA \
                            VALUES (:id, :city, :priority)";
    }
    else if (strcmp((const signed char *)appname, "PROG2") == 0)
    {
        sqlstmt01 = (text *)"INSERT INTO PROG2_PROCESSED_DATA \
                            VALUES (:id, :city, :priority)";
    }

    checkerr(ctx->errhp, OCINumberToInt(ctx->errhp, &mesg->id, sizeof(id),
                OCI_NUMBER_SIGNED, (dvoid *)&id));
    city = OCIStringPtr(ctx->envhp, mesg->city);
    checkerr(ctx->errhp, OCINumberToInt(ctx->errhp, &mesg->priority,
                sizeof(priority), OCI_NUMBER_SIGNED, (dvoid *)&priority));

    printf("\tDequeueing message - id: %d for %s\n", id, appname);
    if (strcmp((const signed char *)appname, "PROG1") == 0)
      printf("\tInserting message info into table PROG1_PROCESSED_DATA\n");
    else if (strcmp((const signed char *)appname, "PROG2") == 0)
      printf("\tInserting message info into table PROG2_PROCESSED_DATA\n");

    /* prepare the statement */
    checkerr(ctx->errhp, OCIStmtPrepare(ctx->stmthp, ctx->errhp, sqlstmt01,
        (ub4)strlen((char *)sqlstmt01), (ub4)OCI_NTV_SYNTAX,
        (ub4)OCI_DEFAULT));

    /* binding placeholders in the insert statement */

    checkerr(ctx->errhp, OCIBindByName(ctx->stmthp, &bndhp[0], ctx->errhp,
        (text *) ":id", -1, (dvoid *) &id, sizeof(id), SQLT_INT,
        (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4) 0, (ub4 *)0, OCI_DEFAULT));

    checkerr(ctx->errhp, OCIBindByName(ctx->stmthp, &bndhp[1], ctx->errhp,
        (text *)":city",-1,(dvoid *)city, 
        (sb4)strlen((const signed char *)city)+1, SQLT_STR,
        (dvoid *)0, (ub2 *)0, (ub2 *) 0, (ub4) 0, (ub4 *)0, OCI_DEFAULT));

    checkerr(ctx->errhp, OCIBindByName(ctx->stmthp, &bndhp[2], ctx->errhp,
        (text *)":priority", -1, (dvoid *) &priority, sizeof(priority),
        SQLT_INT, (dvoid *)0, (ub2 *)0, (ub2 *)0, (ub4)0, (ub4 *)0,
        OCI_DEFAULT));

    checkerr(ctx->errhp, OCIStmtExecute(ctx->svchp, ctx->stmthp, ctx->errhp,
        (ub4)1 , (ub4) 0, (CONST OCISnapshot *) NULL, (OCISnapshot *) NULL ,
        OCI_DEFAULT));

}

int main()
{
    oci_ctx        ctx;
    deq_ctx        dctx;
    OCIAQAgent    *agent_list[2];
    OCIAQAgent    *agent = (OCIAQAgent *)0;
    OCIRaw        *msgID = (OCIRaw *)0;
    text           type_name[61] = "AQUSER.MESSAGE";
    OCIDescribe   *dschp = (OCIDescribe *)0;
    OCIParam      *type_param = (OCIParam *)0;
    OCIType       *mesg_tdo = (OCIType *)0;
    message       *mesg = (message *)0;
    null_message   nmsg;
    null_message  *nmesg = &nmsg;
    text           qname[61];
    ub4            i;
    sword          retval;
    text          *appname;
    ub4            appsz;
    text           errbuf[512];
    sb4            errcode;

   OCIInitialize((ub4) OCI_OBJECT, (dvoid *)0,  (dvoid * (*)(dvoid *,size_t)) 0,
   (dvoid * (*)(dvoid *,dvoid * ,size_t)) 0,  (dvoid (*)(dvoid *,dvoid *)) 0 );

    init_ctx(&ctx, (oratext *)"aquser",(oratext *) "aquser");

    /* allocate agent handles */
    for (i = 0; i < 2; i++)
    {
        checkerr(ctx.errhp, OCIDescriptorAlloc(ctx.envhp,
        (dvoid **)&agent_list[i], OCI_DTYPE_AQAGENT, 0, (dvoid **)0));
    }


    SetAgent(agent_list[0], (oratext *)"prog1",(oratext *) "aquser.input_queue", ctx.errhp);
    SetAgent(agent_list[1],(oratext *) "prog2", (oratext *)"aquser.input_queue", ctx.errhp);

    setup_dequeue(&ctx, &dctx, (oratext *)"AQUSER.MESSAGE");

    printf("Listening and dequeuing messages ...\n");

    while(1)
    {
        printf("Listening for messages for PROG1 and PROG2...\n");

        retval = OCIAQListen(ctx.svchp, ctx.errhp, agent_list, 2, 120, &agent,
                             0);
        if (retval == OCI_ERROR)
        {
            OCIErrorGet ((dvoid *) ctx.errhp, (ub4) 1, (text *) NULL,
                        &errcode, errbuf, (ub4) sizeof(errbuf),
                        (ub4) OCI_HTYPE_ERROR);

            /* if listen call timed out, continue listen */
            if (errcode == 25254)
            {
                printf("No more messages\n");
                clean_up(ctx, dctx, mesg, agent_list);
                exit(1);
            }
            else
                checkerr(ctx.errhp, retval);
        }
        else
        {
          checkerr(ctx.errhp, retval);
        }

        GetAgent(agent, ctx.errhp, qname);

        checkerr(ctx.errhp, OCIAttrGet(agent, OCI_DTYPE_AQAGENT,
            (dvoid *)&appname, &appsz, OCI_ATTR_AGENT_NAME, ctx.errhp));

        if (strcmp((const signed char *)appname, "PROG1") == 0)
        {
            /* set consumer name to prog1 */
            checkerr(ctx.errhp, OCIAttrSet(dctx.dopt, OCI_DTYPE_AQDEQ_OPTIONS,
                                     (dvoid *)"PROG1", (ub4)strlen("PROG1"),
                                     OCI_ATTR_CONSUMER_NAME, ctx.errhp));
        }
        else if (strcmp((const signed char *)appname, "PROG2") == 0)
        {
            /* set consumer name to prog1 */
            checkerr(ctx.errhp, OCIAttrSet(dctx.dopt, OCI_DTYPE_AQDEQ_OPTIONS,
                                     (dvoid *)"PROG2", (ub4)strlen("PROG2"),
                                     OCI_ATTR_CONSUMER_NAME, ctx.errhp));
        }

        checkerr(ctx.errhp, OCIAQDeq(ctx.svchp, ctx.errhp,
                        (text *)qname, dctx.dopt,
                        dctx.msgprop, dctx.mesg_tdo,
                        (dvoid **)&mesg, (dvoid **)&nmesg, &msgID, 0));

        update_table(&ctx, appname, mesg);

        checkerr(ctx.errhp, OCITransCommit(ctx.svchp, ctx.errhp, (ub4) 0));

    }
    exit(0);
}


/* end of file ociaqdemo02.c */

