#ifdef RCSID
static char *RCSid =
   "$Header: cdemosyev.c 26-jan-00.08:39:37 mrhodes Exp $ ";
#endif /* RCSID */

/* Copyright (c) Oracle Corporation 1998, 1999, 2000. All Rights Reserved. */

/*

   NAME
     cdemosyev.c - C DEMO for registration for SYstem EVents

   DESCRIPTION
     This program demonstrates registration for predefined subscriptions.
     At time of registration a callback function is specified which is
     invoked when the client needs to be notified.

   PUBLIC FUNCTION(S)
     <list of external functions declared/defined - with one-line descriptions>

   PRIVATE FUNCTION(S)
     <list of static functions defined in .c file - with one-line descriptions>

   RETURNS
     <function return values, for .c file with single function>

   NOTES
      should run cdemosyev.sql before running this program

   MODIFIED   (MM/DD/YY)
   mrhodes  01/26/00 - adjust ifdef
   mpjoshi  07/30/99 - resolve merge conflict 
   mpjoshi  07/28/99 - make 'sleep' call nt compatible 
   mjaeger  07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   sasuri   11/23/98 - add shutdown notification
   sasuri   06/16/98 - system event demo files
   sasuri   06/16/98 - Creation

*/

/* for WINDOWS compatibility of 'sleep' call */
#if defined(WIN32COMMON) || defined(WIN32) || defined(_WIN32) 
#include <windows.h>
#define sleep(x) Sleep(1000*(x))
#endif

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <oci.h>

static text *username = (text *) "PUBSUB";
static text *password = (text *) "PUBSUB";
static int count = 0;
ub4 namespace = OCI_SUBSCR_NAMESPACE_AQ;

static OCIEnv *envhp;
static OCIServer *srvhp;
static OCIError *errhp;
static OCISvcCtx *svchp;

static void checkerr(/*_ OCIError *errhp, sword status _*/);
static void cleanup(/*_ void _*/);
static void myfflush(/*_ void _*/);
void initSubscriptionHn ();
int main();

static sword status;

/* define all callback functions */

/* callback function for notification of server error events */

ub4 notifyDropSch(ctx, subscrhp, pay, payl, desc, mode)
dvoid *ctx;
OCISubscription *subscrhp;
dvoid *pay;
ub4 payl;
dvoid *desc;
ub4 mode;
{
    printf("Notification : Drop on Schema\n");
    return (0);
}

ub4 notifyError(ctx, subscrhp, pay, payl, desc, mode)
dvoid *ctx;
OCISubscription *subscrhp;
dvoid *pay;
ub4 payl;
dvoid *desc;
ub4 mode;
{
    printf("Notification : Error Event\n");
    return (0);
}

/* for startup */

ub4 notifyStartup(ctx, subscrhp, pay, payl, desc, mode)
dvoid *ctx;
OCISubscription *subscrhp;
dvoid *pay;
ub4 payl;
dvoid *desc;
ub4 mode;
{
    printf("Notification : Startup of Database\n");
    return (0);
}


/* for shutdown */

ub4 notifyShutdown(ctx, subscrhp, pay, payl, desc, mode)
dvoid *ctx;
OCISubscription *subscrhp;
dvoid *pay;
ub4 payl;
dvoid *desc;
ub4 mode;
{
    count++;
    printf("Notification : Shutdown of Database\n");
    return (0);
}


ub4 notifyLogon(ctx, subscrhp, pay, payl, desc, mode)
dvoid *ctx;
OCISubscription *subscrhp;
dvoid *pay;
ub4 payl;
dvoid *desc;
ub4 mode;
{
    printf("Notification : Logon on Database\n");
    return (0);
}

ub4 notifySnoop(ctx, subscrhp, pay, payl, desc, mode)
dvoid *ctx;
OCISubscription *subscrhp;
dvoid *pay;
ub4 payl;
dvoid *desc;
ub4 mode;
{
    printf("Notification : User EVENT Logged on\n");
    return (0);
}


ub4 notifyLogoff(ctx, subscrhp, pay, payl, desc, mode)
dvoid *ctx;
OCISubscription *subscrhp;
dvoid *pay;
ub4 payl;
dvoid *desc;
ub4 mode;
{
    printf("Notification : Logoff on Database\n");
    return (0);
}


ub4 notifyCreateSch(ctx, subscrhp, pay, payl, desc, mode)
dvoid *ctx;
OCISubscription *subscrhp;
dvoid *pay;
ub4 payl;
dvoid *desc;
ub4 mode;
{
    printf("Notification : Create on Schema\n");
    return (0);
}

int main()
{
    OCISession *authp = (OCISession *) 0;
    OCISubscription *subscrhpErr = (OCISubscription *)0;
    OCISubscription *subscrhpStartup = (OCISubscription *)0;
    OCISubscription *subscrhpShutdown = (OCISubscription *)0;
    OCISubscription *subscrhpLogon = (OCISubscription *)0;
    OCISubscription *subscrhpLogoff = (OCISubscription *)0;
    OCISubscription *subscrhpCreate = (OCISubscription *)0;
    OCISubscription *subscrhpDrop = (OCISubscription *)0;
    OCISubscription *subscrhpSnoop = (OCISubscription *)0;

    printf("Initializing OCI Process\n");

    (void) OCIInitialize((ub4) OCI_EVENTS|OCI_OBJECT, (dvoid *)0,
        (dvoid * (*)(dvoid *, size_t)) 0,
        (dvoid * (*)(dvoid *, dvoid *, size_t))0,
        (void (*)(dvoid *, dvoid *)) 0 );

    printf("Initialization successful\n");

    printf("Initializing OCI Env\n");
    (void) OCIEnvInit( (OCIEnv **) &envhp, OCI_DEFAULT, (size_t) 0,
        (dvoid **) 0 );
    printf("Initialization successful\n");

    (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &errhp, OCI_HTYPE_ERROR,

        (size_t) 0, (dvoid **) 0);

    /* Set server contexts */
    (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &srvhp,
        OCI_HTYPE_SERVER, (size_t) 0, (dvoid **) 0);

    (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &svchp,
        OCI_HTYPE_SVCCTX, (size_t) 0, (dvoid **) 0);

    printf("connecting to server\n");
    (void) OCIServerAttach( srvhp, errhp, (text *)"", strlen(""), 0);
    printf("connect successful\n");

    /* set attribute server context in the service context */
    (void) OCIAttrSet( (dvoid *) svchp, OCI_HTYPE_SVCCTX, (dvoid *)srvhp,
        (ub4) 0, OCI_ATTR_SERVER, (OCIError *) errhp);

    (void) OCIHandleAlloc((dvoid *) envhp, (dvoid **)&authp,
        (ub4) OCI_HTYPE_SESSION, (size_t) 0, (dvoid **) 0);

    (void) OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION,
        (dvoid *) username, (ub4) strlen((char *)username),
        (ub4) OCI_ATTR_USERNAME, errhp);

    (void) OCIAttrSet((dvoid *) authp, (ub4) OCI_HTYPE_SESSION,
        (dvoid *) password, (ub4) strlen((char *)password),
        (ub4) OCI_ATTR_PASSWORD, errhp);

    checkerr(errhp, OCISessionBegin ( svchp,  errhp, authp, OCI_CRED_RDBMS,
        (ub4) OCI_DEFAULT));

    (void) OCIAttrSet((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
        (dvoid *) authp, (ub4) 0,
        (ub4) OCI_ATTR_SESSION, errhp);


    /* Registration Code */

    /* each call to initSubscriptionHn allocates
           and initialises a registration handle */

    initSubscriptionHn(    &subscrhpErr,
    "PUBSUB.ERROR:ADMIN",
    (dvoid*)notifyError);

    initSubscriptionHn(    &subscrhpStartup,
        "PUBSUB.STARTUP:ADMIN",
        (dvoid*)notifyStartup);

    initSubscriptionHn(    &subscrhpShutdown,
        "PUBSUB.SHUTDOWN:ADMIN",
        (dvoid*)notifyShutdown);

    initSubscriptionHn(    &subscrhpLogon,
        "PUBSUB.LOGON:ADMIN",
        (dvoid*)notifyLogon);

    initSubscriptionHn(    &subscrhpLogoff,
        "PUBSUB.LOGOFF:ADMIN",
        (dvoid*)notifyLogoff);

    initSubscriptionHn(    &subscrhpCreate,
        "PUBSUB.CREATE_SCH:ADMIN",
        (dvoid*)notifyCreateSch);

    initSubscriptionHn(    &subscrhpDrop,
        "PUBSUB.DROP_SCH:ADMIN",
        (dvoid*)notifyDropSch);

    initSubscriptionHn(    &subscrhpSnoop,
        "PUBSUB.LOGON:SNOOP",
        (dvoid*)notifySnoop);

    /* end session */
    checkerr(errhp, OCISessionEnd ( svchp,  errhp, authp, (ub4) OCI_DEFAULT));

    /* detach from server */
    OCIServerDetach( srvhp, errhp, OCI_DEFAULT);

    while (count != 2)
        sleep(1);
}


void checkerr(errhp, status)
OCIError *errhp;
sword status;
{
    text errbuf[512];
    sb4 errcode = 0;

    switch (status)
    {
    case OCI_SUCCESS:
        break;
    case OCI_SUCCESS_WITH_INFO:
        (void) printf("Error - OCI_SUCCESS_WITH_INFO\n");
        break;
    case OCI_NEED_DATA:
        (void) printf("Error - OCI_NEED_DATA\n");
        break;
    case OCI_NO_DATA:
        (void) printf("Error - OCI_NODATA\n");
        break;
    case OCI_ERROR:
        (void) OCIErrorGet((dvoid *)errhp, (ub4) 1, (text *) NULL,
            &errcode,
            errbuf, (ub4) sizeof(errbuf), OCI_HTYPE_ERROR);
        (void) printf("Error - %.*s\n", 512, errbuf);
        break;
    case OCI_INVALID_HANDLE:
        (void) printf("Error - OCI_INVALID_HANDLE\n");
        break;
    case OCI_STILL_EXECUTING:
        (void) printf("Error - OCI_STILL_EXECUTE\n");
        break;
    case OCI_CONTINUE:
        (void) printf("Error - OCI_CONTINUE\n");
        break;
    default:
        break;
    }
}


/*
 *  Exit program with an exit code.
 */
void cleanup()
{
    if (errhp)
        (void) OCIServerDetach( srvhp, errhp, OCI_DEFAULT );
    if (srvhp)
        checkerr(errhp, OCIHandleFree((dvoid *) srvhp, OCI_HTYPE_SERVER));
    if (svchp)
        (void) OCIHandleFree((dvoid *) svchp, OCI_HTYPE_SVCCTX);
    if (errhp)
        (void) OCIHandleFree((dvoid *) errhp, OCI_HTYPE_ERROR);
    return;
}


void myfflush()
{
    eb1 buf[50];

    fgets((char *) buf, 50, stdin);
}


void initSubscriptionHn (subscrhp,
subscriptionName,
func)

OCISubscription **subscrhp;
char* subscriptionName;
dvoid * func;
{

    /* allocate subscription handle */

    (void) OCIHandleAlloc((dvoid *) envhp, (dvoid **)subscrhp,
        (ub4) OCI_HTYPE_SUBSCRIPTION,
        (size_t) 0, (dvoid **) 0);
    /* set subscription name in handle */

    (void) OCIAttrSet((dvoid *) *subscrhp, (ub4) OCI_HTYPE_SUBSCRIPTION,
        (dvoid *) subscriptionName,
        (ub4) strlen((char *)subscriptionName),
        (ub4) OCI_ATTR_SUBSCR_NAME, errhp);

    /* set callback function in handle */

    (void) OCIAttrSet((dvoid *) *subscrhp, (ub4) OCI_HTYPE_SUBSCRIPTION,
        (dvoid *) func, (ub4) 0,
        (ub4) OCI_ATTR_SUBSCR_CALLBACK, errhp);

    (void) OCIAttrSet((dvoid *) *subscrhp, (ub4) OCI_HTYPE_SUBSCRIPTION,
        (dvoid *) 0, (ub4) 0,
        (ub4) OCI_ATTR_SUBSCR_CTX, errhp);


    /* set namespace in handle */

    (void) OCIAttrSet((dvoid *) *subscrhp, (ub4) OCI_HTYPE_SUBSCRIPTION,
        (dvoid *) &namespace, (ub4) 0,
        (ub4) OCI_ATTR_SUBSCR_NAMESPACE, errhp);
    printf("Begining Registration for subscription %s\n", subscriptionName);
    checkerr(errhp, OCISubscriptionRegister(svchp, subscrhp, 1, errhp,

        OCI_DEFAULT));
    printf("done\n");
}

/* end of file cdemosyev.c */
