/* Copyright (c) 2003, 2007, Oracle. All rights reserved.  */

/*

   NAME
     ociaqarrayenq.c - <one-line expansion of the name>

   DESCRIPTION
     <short description of facility this file declares/defines>

   EXPORT FUNCTION(S)
     <external functions defined for use outside package - one-line descriptions>

   INTERNAL FUNCTION(S)
     <other external functions defined - one-line descriptions>

   STATIC FUNCTION(S)
     <static functions defined - one-line descriptions>

   NOTES
    aqdemo09.sql (present in the $ORACLE_HOME/rdbms/demo directory) has to be
    run after connecting as SYS to set up the required queue tables, queues, 
    subscribers etc.

    ociaqarrayenq.c - enqueues a batch of 10 msgs into my_queue
    ociaqarraydeq.c - dequeues a batch of 10 msgs from my_queue

    aqdemo12.sql (present in the $ORACLE_HOME/rdbms/demo directory) has to be
    run as SYS to clean up all objects.

   MODIFIED   (MM/DD/YY)
   dprasann    04/05/07 - lowercase username/password
   dprasann    02/15/06 - bug 4993505 
   rbhyrava    04/05/04 - linux porting 
   aahluwal    10/17/03 - aahluwal_create_arrenqdeq_demos 
   aahluwal    10/07/03 - Creation

*/

#include <oratypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN64
#include <malloc.h>
#endif

#ifndef OCI_ORACLE
#include <oci.h>
#endif

/*---------------------------------------------------------------------------
                     PRIVATE TYPES AND CONSTANTS
  ---------------------------------------------------------------------------*/
struct message
{
  OCIString   *data;
};
typedef struct message message;

struct null_message
{
  OCIInd null_adt;
  OCIInd null_data;
};
typedef struct null_message null_message;

/*---------------------------------------------------------------------------
                     STATIC FUNCTION DECLARATIONS 
  ---------------------------------------------------------------------------*/

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
    break;
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

int main(int argc, char *argv[])
{
  OCIEnv              *envhp;
  OCIServer           *srvhp;
  OCIError            *errhp;
  OCISvcCtx           *svchp;
  OCISession          *usrhp;
  dvoid               *tmp;
  OCIType             *mesg_tdo = (OCIType *) 0;
  message              mesg[100];
  message             *mesgp[100];
  null_message         nmesg[100];
  null_message        *nmesgp[100];
  int                  i, j, k;
  OCIInd               ind[100];
  dvoid               *indptr[100];
  ub4                  priority;
  OCIAQEnqOptions     *enqopt = (OCIAQEnqOptions *)0;
  OCIAQMsgProperties  *msgprop= (OCIAQMsgProperties *)0;
  ub4                  wait = 1;
  ub4                  navigation = OCI_DEQ_NEXT_MSG;
  ub4                  iters = 10;
  text                *qname ; 
  text                 mesgdata[30];
  ub4                  payload_size = 5;
  text                *payload = (text *)0;
  ub4                  batch_size = iters;
  ub4                  enq_size = 2;
  char                 repeat_char = '0';
  
  printf("session start\n");
  /* establish a session */  

  OCIInitialize((ub4) OCI_OBJECT, (dvoid *)0,  (dvoid * (*)(dvoid *,size_t)) 0,
   (dvoid * (*)(dvoid *,dvoid * ,size_t)) 0,  (dvoid (*)(dvoid *,dvoid *)) 0 );
  
  OCIHandleAlloc( (dvoid *) NULL, (dvoid **) &envhp, (ub4) OCI_HTYPE_ENV,
           52, (dvoid **) &tmp);
  
  OCIEnvInit( &envhp, (ub4) OCI_DEFAULT, 21, (dvoid **) &tmp  );
  
  OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &errhp, (ub4) OCI_HTYPE_ERROR,
           52, (dvoid **) &tmp);
  OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &srvhp, (ub4) OCI_HTYPE_SERVER,
           52, (dvoid **) &tmp);
  
  printf("server attach\n");
  OCIServerAttach( srvhp, errhp, (text *) 0, (sb4) 0, (ub4) OCI_DEFAULT);
  
  OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &svchp, (ub4) OCI_HTYPE_SVCCTX,
           52, (dvoid **) &tmp);
  
  /* set attribute server context in the service context */
  OCIAttrSet( (dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX, (dvoid *)srvhp, (ub4) 0,
           (ub4) OCI_ATTR_SERVER, (OCIError *) errhp);
  
  /* allocate a user context handle */
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

  /* get descriptor for enqueue options */
  checkerr(errhp, OCIDescriptorAlloc(envhp, (dvoid **)&enqopt, 
                                         OCI_DTYPE_AQENQ_OPTIONS, 0, 
                                         (dvoid **)0));

  printf("enq options set\n");
  /* set enqueue options - for consumer name, wait and navigation */

  /* construct null terminated payload string */
  payload = (text *)malloc(payload_size+1);

  for (k=0 ; k < batch_size ; k++)
  {
    indptr[k] = &ind[k];
    mesgp[k] = &mesg[k];
    nmesgp[k] = &nmesg[k];
    nmesg[k].null_adt = nmesg[k].null_data = OCI_IND_NOTNULL; 
    mesg[k].data = (OCIString *)0;

    for (j=0 ; j < payload_size ; j++)
      payload[j] = (text)repeat_char;
    payload[payload_size] = (text)'\0';

    OCIStringAssignText(envhp, errhp, (const oratext *)payload,
                        (ub4)strlen((const char *)payload), &(mesg[k].data));
    printf("Message #%i has payload: \"%s\"\n", k, payload);  
    repeat_char++;
  }
  
  printf("check message tdo\n");
  checkerr(errhp, OCITypeByName(envhp, errhp, svchp, 
   (CONST text *)"AQUSER", (ub4)strlen("AQUSER"),
   (CONST text *)"MESSAGE", (ub4)strlen("MESSAGE"), (text *)0, 0, 
   OCI_DURATION_SESSION, OCI_TYPEGET_ALL, &mesg_tdo));

  k=0;

  while (k < iters) 
  {
    enq_size = batch_size;
    checkerr(errhp, OCIAQEnqArray(svchp, errhp, 
                                  (dvoid *)"AQUSER.MY_QUEUE", 
                                  (OCIAQEnqOptions *)0, &enq_size, 
                                  (OCIAQMsgProperties **)0, mesg_tdo, 
                                  (dvoid **)&mesgp, 
                                  (dvoid **)&nmesgp, (OCIRaw **)0, 
                                  (void *)0, (OCICallbackAQEnq) 0, 0));
    k+=(int)batch_size;
  }
  printf("%i messages enqueued\n", k);
  
  checkerr(errhp, OCITransCommit(svchp, errhp, (ub4) 0));

  printf("enqueue committed\n");

  checkerr(errhp, OCISessionEnd ( svchp,  errhp, usrhp, (ub4) OCI_DEFAULT));
  checkerr(errhp, OCIServerDetach( srvhp, errhp, (ub4) OCI_DEFAULT));

  return 0;
}

/* end of file ociaqarrayenq.c */

