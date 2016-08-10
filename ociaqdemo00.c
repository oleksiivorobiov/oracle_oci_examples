#ifdef RCSID
static char *RCSid =
   "$Header: ociaqdemo00.c 27-mar-2006.22:31:21 azhao Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1998, 2006, Oracle. All rights reserved.  
*/

/*

   NAME
     ociaqdemo00.c - <one-line expansion of the name>

   DESCRIPTION
    ociaqdemo00.c enqueues 100 messages into the input_queue. (10 messages are
    enqueued every 3 seconds to a maximum of 100 messages)

   PUBLIC FUNCTION(S)
     <list of external functions declared/defined - with one-line descriptions>

   PRIVATE FUNCTION(S)
     <list of static functions defined in .c file - with one-line descriptions>

   RETURNS
     <function return values, for .c file with single function>

   NOTES

    aqdemo00.sql (present in the $ORACLE_HOME/rdbms/demo directory) has 
    to be run first as user SYS to set up the required queue tables, 
    queues, subscribers etc.

    ociaqdemo00.c enqueues 100 messages into the input_queue.
    ociaqdemo01.c dequeues messages by blocking on prop_queue for agent "prog3"
    ociaqdemo02.c listens on input_queue and dequeues messages for agents
                  "prog1" and "prog2".


   MODIFIED   (MM/DD/YY)
   azhao       03/27/06 - fix 4993505 
   lzhao       04/16/04 - nt lrg 
   rbhyrava    04/05/04 - linux porting 
   ekarichk    12/18/03 - bug3328852: missing prototypes
   rbhyrava    11/14/00 - rename newaqdemo00
   mrhodes     01/25/00 - make sleep call nt compatible
   bnainani    09/23/99 - Bug 996772: change order of freeing handles
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   kmeiyyap    06/30/99 - free handles
   kmeiyyap    06/29/99 - free descriptor
   kmeiyyap    10/06/98 - oci enqueue
   kmeiyyap    10/06/98 - Creation

*/
/* for WINDOWS compatibility of 'sleep' call */
#if defined(WIN32COMMON) || defined(WIN32) || defined(_WIN32) 
#include <windows.h>
#define sleep(x) Sleep(1000*(x))
#endif

#include <oratypes.h>

#ifndef OCI_ORACLE
#include <oci.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static void checkerr( OCIError *errhp, sword status);

static void checkerr( OCIError *errhp, sword status)
{
    text errbuf[512];
    ub4 buflen;
    sb4 errcode;

    if (status == OCI_SUCCESS) return;

    switch (status)
    {
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
            printf("Error - %d\n", status);
            break;
    }
}

void clean_up(OCIEnv *envhp, 
              OCISvcCtx *svchp, 
              OCIServer *srvhp, 
              OCIError  *errhp, 
              OCISession *usrhp, 
              OCIAQMsgProperties *msgprop)
{
  /* free the message properties decriptor */
  checkerr(errhp, OCIDescriptorFree((dvoid *)msgprop,
                                      OCI_DTYPE_AQMSG_PROPERTIES));

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
  if (envhp)
    (void) OCIHandleFree((dvoid *) envhp, (ub4) OCI_HTYPE_ENV);

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



int main()
{
    OCIEnv             *envhp;
    OCIServer          *srvhp;
    OCIError           *errhp;
    OCISvcCtx          *svchp;
    OCISession         *usrhp;

    dvoid              *tmp;
    OCIType            *mesg_tdo = (OCIType *) 0;
    OCIAQMsgProperties *msgprop  = (OCIAQMsgProperties *)0;
    message             msg;
    null_message        nmsg;
    message            *mesg = &msg;
    null_message       *nmesg = &nmsg;
    int                 i;
    int                 priority;

    /* Standard OCI initialization */

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
                (ub4) 0 , (ub4) OCI_ATTR_SERVER, (OCIError *) errhp);

    /* allocate a user session handle */
    OCIHandleAlloc((dvoid *)envhp, (dvoid **)&usrhp, (ub4) OCI_HTYPE_SESSION,
                    (size_t) 0, (dvoid **) 0);

    OCIAttrSet( (dvoid *)usrhp, (ub4)OCI_HTYPE_SESSION, (dvoid *)"aquser",
                (ub4)strlen("aquser"), OCI_ATTR_USERNAME, errhp);


    OCIAttrSet( (dvoid *)usrhp, (ub4)OCI_HTYPE_SESSION,
                (dvoid *)"aquser", (ub4)strlen("aquser"),
                OCI_ATTR_PASSWORD, errhp);

    checkerr(errhp, OCISessionBegin (svchp, errhp, usrhp, OCI_CRED_RDBMS,
            OCI_DEFAULT));

    OCIAttrSet( (dvoid *)svchp, (ub4)OCI_HTYPE_SVCCTX,
                (dvoid *)usrhp, (ub4)0, OCI_ATTR_SESSION, errhp);

    checkerr(errhp, OCITypeByName(envhp, errhp, svchp,
            (CONST text *)"AQUSER", (ub4)strlen("AQUSER"),
            (CONST text *)"MESSAGE", (ub4)strlen("MESSAGE"), (text *)0, 0,
            OCI_DURATION_SESSION, OCI_TYPEGET_ALL, &mesg_tdo));

    checkerr(errhp, OCIDescriptorAlloc(envhp, (dvoid **)&msgprop,
            OCI_DTYPE_AQMSG_PROPERTIES, 0, (dvoid **)0));

    printf("Enqueueing messages ...\n");
    /* enqueue 100 messages into input_queue */
    for (i = 1; i <= 100; i++)
    {
        mesg->city =  (OCIString *)0;
        priority = i%3 + 1;

        checkerr(errhp, OCINumberFromInt(errhp, &i, sizeof(i), 0, &mesg->id));
        checkerr(errhp, OCINumberFromInt(errhp, &priority,
                    sizeof(priority), 0, &mesg->priority));

        /* Set the priority in message property */
        checkerr(errhp, OCIAttrSet(msgprop, OCI_DTYPE_AQMSG_PROPERTIES,
                (dvoid *)&priority, sizeof(ub4), OCI_ATTR_PRIORITY, errhp));

        if (i%3 == 0)
        {
            checkerr(errhp, OCIStringAssignText(envhp, errhp,
                    (CONST text *)"BELMONT", (ub4)strlen("BELMONT"),
                    &mesg->city));

            nmesg->null_adt = nmesg->null_id = 0;
            nmesg->null_city = nmesg->null_priority = 0;
        }
        else if (i%4 == 0)
        {
            checkerr(errhp, OCIStringAssignText(envhp, errhp,
                    (CONST text *)"REDWOOD SHORES", (ub4)strlen("REDWOOD SHORES"),
                    &mesg->city));

            nmesg->null_adt = nmesg->null_id = 0;
            nmesg->null_city = nmesg->null_priority = 0;
        }
        else if (i%2 == 0)
        {
            checkerr(errhp, OCIStringAssignText(envhp, errhp,
                    (CONST text *)"SUNNYVALE", (ub4)strlen("SUNNYVALE"),
                    &mesg->city));

            nmesg->null_adt = nmesg->null_id = 0;
            nmesg->null_city = nmesg->null_priority = 0;
        }
        else
        {
            checkerr(errhp, OCIStringAssignText(envhp, errhp,
                    (CONST text *)"BURLINGAME", (ub4)strlen("BURLINGAME"),
                    &mesg->city));

            nmesg->null_adt = nmesg->null_id = 0;
            nmesg->null_city = nmesg->null_priority = 0;
        }


        /* Enqueue the message */
        checkerr(errhp, OCIAQEnq(svchp, errhp,
                (text *) "aquser.input_queue", (OCIAQEnqOptions *)0, 
                (OCIAQMsgProperties *)msgprop,
                mesg_tdo, (dvoid **)&mesg, (dvoid **)&nmesg, (OCIRaw **)0, 0));

        if ((i %10) == 0)
        {
            printf("Enqueueing messages %2d through %2d...\n", i-9, i);
            sleep(3);
        }
        checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));
    }
    clean_up(envhp, svchp, srvhp, errhp, usrhp, msgprop);
    printf("Done.\n");
    exit (0);
}


/* end of file ociaqdemo00.c */

