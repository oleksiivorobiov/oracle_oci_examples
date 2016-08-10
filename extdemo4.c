#ifdef RCSID
static char *RCSid =
   "$Header: extdemo4.c 08-feb-2001.18:14:54 ayoaz Exp $ ";
#endif /* RCSID */

/* Copyright (c) Oracle Corporation 1998, 2000. All Rights Reserved. */

/*

   NAME
     extdemo4.c - User Defined Aggregates using C safe callouts
                  using external context

   DESCRIPTION
     See extdemo4.sql for the definition of the implementation type.

   NOTES
     The implementation type contains two attributes, key and aggCtx.
     Normally, the key will contains a value which identifes the
     memory in extproc, where the aggregation context is stored, and
     the aggCtx attribute will be NULL.
     In certain situations, such as parallel aggregation, it is neccessary
     to send the aggregation context from one slave to another, so
     the external aggregation context will be "wrapped" into the
     aggCtx attribute of the implementation type instance, and the key
     will be set to NULL. So when the implementation type instance ("self")
     is returned to the server, it will contain the actual aggregation
     context in it.
     Therefore, each of the ODCIAggregate routines has to check if the
     aggregation context is in-line or out-of-line by checking the
     "key" attribute. If it not NULL, than the key value is used to
     access the externally stored context. If it is NULL, than external
     memory is allocated and the context is copied from the aggCtx
     attribute to the external memory, and the key identifying the external
     memory is stored in the implementation type instance. This is done by 
     the GetExtAggCtx function.

     After retrieving the context, each of ODCIAggregate functions
     calls the equivalent do_xxx function with the aggregation context
     as the first argument (e.g. ODCIAggregateIterate calls do_iterate).
     Therefore, the do_xxx functions only deal with actually implementing
     the aggregation logic, using the aggregation context as a C struct,
     without having to deal with the logic of retrieving the aggregation
     context.
  
     The do_wrap and do_unwrap functions translate the aggregation context
     from its OCI representation to a user-defined C representation.
     In this example, the OCI representation of the aggregation context
     is AggCtx_t, which represents the context as a x-y coordinate
     stored as Oracle numbers (OCINumber).  The C representation is 
     ExtAggCtx_t, which represents the context as x-y coordinate stored 
     as C doubles. If the context doesn't have to be transferred between
     slaves, the context will always be stored externally as a user-defined C
     struct (in extproc's memory), so these functions will not be called.

     All the other do_xxx (do_init, do_iterate, do_terminate, do_merge 
     and do_delete) only deal with context in its external format
     (ExtAggCtx_t) which makes it simpler and faster to do the aggregation.

   MODIFIED   (MM/DD/YY)
   ayoaz       02/08/01 - Merged ayoaz_udag_demo
   ayoaz       02/06/01 - Creation

*/

#include "extdemo4.h"
#include <math.h>

/*------------------------------------------------------------------------
                     PRIVATE TYPES AND CONSTANTS
  ----------------------------------------------------------------------*/

/* The external context (stored in extproc's process memory) */

struct ExtAggCtx_t
{
  double x;
  double y;
};
typedef struct ExtAggCtx_t ExtAggCtx_t;

/*-----------------------------------------------------------------------*/

/* OCI Handles */

struct Handles_t
{
  OCIExtProcContext* extProcCtx;
  OCIEnv* envhp;
  OCISvcCtx* svchp;
  OCIError* errhp;
  OCISession* usrhp;
};
typedef struct Handles_t Handles_t;

/*-------------------------------------------------------------------------
                           PRIVATE FUNCTIONS PROTOTYPES
  -----------------------------------------------------------------------*/

static int GetHandles(OCIExtProcContext* extProcCtx, Handles_t* handles);

/*
  NAME:
    GetHandles - get the various OCI handles
  PARAMETERS:
    extProcCtx (IN) - external procedure context
    handles (OUT) - struct which contains the OCI handles
  DESCRIPTION:
    This function retrieves the OCI environment, service, error and
    user handles using the external procedure context, and returns
    then via the handles parameter.
  RETURN:
    Zero on success, -1 in case of error.
*/

/*-----------------------------------------------------------------------*/

static ExtAggCtx_t* GetExtAggCtx(Handles_t* handles, 
                                 Imp_t* self, Imp_Ind_t* self_ind);

/*
  NAME:
    GetExtAggCtx - retrieve the external context
  PARAMETERS:
    handles (IN) - OCI handles
    self (IN) - pointer to the self (implementation type instance)
    self_ind (IN) - indicator struct for self
  DESCRIPTION:
      This function returns a pointer to the external representation
    of the context, by retrieving it using the key attribute in self,
    or allocating the external context and "unwrapping" the aggCtx within
    the self into the external context.
  RETURN:
    Pointer to external context, or NULL in case of error.
*/

/*-----------------------------------------------------------------------*/

static int ChkErr(Handles_t* handles, sword status);

/*
  NAME:
    ChkErr - check the error code, and register exception if neccessary
  PARAMETERS:
    handles (IN) - OCI handles
    sword (IN) - error code
  DESCRIPTION:
      This function checks the input error code, and if it indicates
    an OCI error, it registers an exception with the appropriate message.
  RETURN:
    Zero if there was no error, -1 in case of error.
*/

/*-----------------------------------------------------------------------*/

static int do_init(Handles_t* handles, ExtAggCtx_t* extAggCtx);
/*
  NAME:
    do_init - initialize the aggregation context
  PARAMETERS:
    handles (IN) - OCI handles
    extAggCtx (OUT) - aggregation context
  DESCRIPTION:
      This function initializes the contents of the aggregation context,
      to the (0,0) coordinates.
  RETURN:
    Zero on success, -1 in case of error.
*/

/*-----------------------------------------------------------------------*/

static int do_iterate(Handles_t* handles, ExtAggCtx_t* extAggCtx, 
                      Vector_t* arg, Vector_Ind_t* arg_ind);
/*
  NAME:
    do_iterate - update the aggregation context with a new value
  PARAMETERS:
    handles (IN) - OCI handles
    extAggCtx (IN/OUT) - aggregation context
    arg (IN) - the vector to add
    arg_ind (IN) - the vector's indicator struct
  DESCRIPTION:
      This function updates the aggregation context according by adding
      the vector to the current coordinates in the aggregation context.
  RETURN:
    Zero on success, -1 in case of error.
*/

/*-----------------------------------------------------------------------*/

static int do_terminate(Handles_t* handles, ExtAggCtx_t* extAggCtx,
                        Vector_t* result, Vector_Ind_t* result_ind);
/*
  NAME:
    do_terminate - calculate aggregation result
  PARAMETERS:
    handles (IN) - OCI handles
    extAggCtx (IN) - aggregation context
    result (OUT) - the result vector
    result_ind (OUT) - the result vector indicator struct
  DESCRIPTION:
      This function returns the sum as a vector, by translating the
    current coordinates in the aggregation context into a vector.
  RETURN:
    Zero on success, -1 in case of error.
*/

/*-----------------------------------------------------------------------*/

static int do_merge(Handles_t* handles, ExtAggCtx_t* extAggCtx, 
                    ExtAggCtx_t* extAggCtx2);
/*
  NAME:
    do_merge - merge two aggregation contexts
  PARAMETERS:
    handles (IN) - OCI handles
    extAggCtx (IN/OUT) - aggregation context
    extAggCtx2 (IN) - second aggregation context
  DESCRIPTION:
      Add the coordinates of the second context into the current context.
  RETURN:
    Zero on success, -1 in case of error.
*/

/*-----------------------------------------------------------------------*/

static int do_delete(Handles_t* handles, ExtAggCtx_t* extAggCtx, 
                     Vector_t* arg, Vector_Ind_t* arg_ind);
/*
  NAME:
    do_delete - update the aggregation context by removing a value
  PARAMETERS:
    handles (IN) - OCI handles
    extAggCtx (IN/OUT) - aggregation context
    arg (IN) - the vector to subtract
    arg_ind (IN) - the vector's indicator struct
  DESCRIPTION:
      This function updates the aggregation context according by subtracting
      the vector from the coordinates in the aggregation context.
  RETURN:
    Zero on success, -1 in case of error.
*/

/*-----------------------------------------------------------------------*/

static int do_wrap(Handles_t* handles, ExtAggCtx_t* extAggCtx,
                   AggCtx_t* aggCtx, AggCtx_Ind_t* aggCtx_ind);
/*
  NAME:
    do_wrap - translate the external context into an OCI aggr. context.
  PARAMETERS:
    handles (IN) - OCI handles
    extAggCtx (IN) - aggregation context (external)
    aggCtx (OUT) - embedded OCI aggregation context
    aggCtx_ind (OUT) - indicator of the embedded OCI aggregation context
  DESCRIPTION:
      This function translates the external context into the OCI aggregation
    context (which is embedded in the implementation type instance).
  RETURN:
    Zero on success, -1 in case of error.
*/

/*-----------------------------------------------------------------------*/

static int do_unwrap(Handles_t* handles, ExtAggCtx_t* extAggCtx,
                     AggCtx_t* aggCtx, AggCtx_Ind_t* aggCtx_ind);
/*
  NAME:
    do_unwrap - translate the OCI aggregation context into external context
  PARAMETERS:
    handles (IN) - OCI handles
    extAggCtx (OUT) - aggregation context (external)
    aggCtx (IN) - embedded OCI aggregation context
    aggCtx_ind (IN) - indicator of the embedded OCI aggregation context
  DESCRIPTION:
      This function translates the OCI aggregation context into the 
    an external context.
  RETURN:
    Zero on success, -1 in case of error.
*/

/*-------------------------------------------------------------------------
                           PUBLIC FUNCTIONS
  -----------------------------------------------------------------------*/

/* ODCIAggregateInitialize function */

int Initialize(OCIExtProcContext* extProcCtx, 
               Imp_t* self, Imp_Ind_t* self_ind)
{
  Handles_t handles;
  ExtAggCtx_t* extAggCtx;
  ub4 key; 

  /* Get OCI handles */
  if (GetHandles(extProcCtx, &handles))
    return ODCI_ERROR;

  /* Set up self */
  if (self_ind->_atomic==OCI_IND_NULL)
  {
    self_ind->_atomic = OCI_IND_NOTNULL;
    self_ind->key = OCI_IND_NULL;
    self_ind->aggCtx._atomic = OCI_IND_NULL;
  }

  /* Get context */
  extAggCtx=GetExtAggCtx(&handles,self,self_ind);
  if (!extAggCtx) return ODCI_ERROR;

  /* Initialize context */
  if (do_init(&handles, extAggCtx))
    return ODCI_ERROR;

  return ODCI_SUCCESS;
}

/*-----------------------------------------------------------------------*/

/* ODCIAggregateIterate function */

int Iterate(OCIExtProcContext* extProcCtx, 
            Imp_t* self, Imp_Ind_t* self_ind, 
            Vector_t* arg, Vector_Ind_t* arg_ind)
{
  Handles_t handles;
  ExtAggCtx_t* extAggCtx;

  /* Get OCI handles */
  if (GetHandles(extProcCtx, &handles))
    return ODCI_ERROR;

  /* Get context */
  extAggCtx=GetExtAggCtx(&handles,self,self_ind);
  if (!extAggCtx) return ODCI_ERROR;

  if (do_iterate(&handles,extAggCtx,arg,arg_ind))
    return ODCI_ERROR;

  return ODCI_SUCCESS;
}

/*-----------------------------------------------------------------------*/

/* ODCIAgregateTerminate */

int Terminate(OCIExtProcContext* extProcCtx, 
              Imp_t* self, Imp_Ind_t* self_ind,
              Vector_t* result, Vector_Ind_t* result_ind,
              OCINumber* flags, OCIInd flags_ind)
{
  Handles_t handles;
  ExtAggCtx_t* extAggCtx;
  int flags_val=0;

  /* Get OCI handles */
  if (GetHandles(extProcCtx, &handles))
    return ODCI_ERROR;

  /* Get context */
  extAggCtx=GetExtAggCtx(&handles,self,self_ind);
  if (!extAggCtx) return ODCI_ERROR;

  if (do_terminate(&handles,extAggCtx,result,result_ind))
    return ODCI_ERROR;

  /* get the flags parameter velue */
  if (flags_ind==OCI_IND_NOTNULL)
  {
    if (ChkErr(&handles, OCINumberToInt(handles.errhp, flags,
                             sizeof(flags_val), OCI_NUMBER_SIGNED,
                             &flags_val)))
      return ODCI_ERROR;
  }

  /* Free external context memory (unless ODCI_AGGREGATE_REUSE_CTX is set) */
  if ((flags_val && ODCI_AGGREGATE_REUSE_CTX)==0)
  {
    if (ChkErr(&handles, OCIMemoryFree((dvoid*)handles.usrhp, 
                            handles.errhp, (dvoid*) extAggCtx)))
      return ODCI_ERROR;

    self_ind->key=OCI_IND_NULL;
  }

  return ODCI_SUCCESS;
}

/*-----------------------------------------------------------------------*/

/* ODCIAggregateMerge */

int Merge(OCIExtProcContext* extProcCtx,
          Imp_t* self, Imp_Ind_t* self_ind,
          Imp_t* sctx2, Imp_Ind_t* sctx2_ind)
{
  Handles_t handles;
  ExtAggCtx_t* extAggCtx;
  ExtAggCtx_t* extAggCtx2;

  /* Get OCI handles */
  if (GetHandles(extProcCtx, &handles))
    return ODCI_ERROR;

  /* Get 1st external context */
  extAggCtx=GetExtAggCtx(&handles,self,self_ind);
  if (!extAggCtx) return ODCI_ERROR;

  /* Get 2nd external context */
  extAggCtx2=GetExtAggCtx(&handles,sctx2,sctx2_ind);
  if (!extAggCtx2) return ODCI_ERROR;

  if (do_merge(&handles,extAggCtx,extAggCtx2))
    return ODCI_ERROR;

  /* Free the 2nd external context memory */
  if (sctx2_ind->key == OCI_IND_NOTNULL)
  {
    if (ChkErr(&handles, OCIMemoryFree((dvoid *)handles.usrhp, 
                            handles.errhp, (dvoid *)extAggCtx2)))
      return ODCI_ERROR;
  }

  return ODCI_SUCCESS;
}

/*-----------------------------------------------------------------------*/

/* ODCIAggregateDelete function */

int Delete(OCIExtProcContext* extProcCtx, 
           Imp_t* self, Imp_Ind_t* self_ind, 
           Vector_t* arg, Vector_Ind_t* arg_ind)
{
  Handles_t handles;
  ExtAggCtx_t* extAggCtx;

  /* Get OCI handles */
  if (GetHandles(extProcCtx, &handles))
    return ODCI_ERROR;

  /* Get context */
  extAggCtx=GetExtAggCtx(&handles,self,self_ind);
  if (!extAggCtx) return ODCI_ERROR;

  if (do_delete(&handles,extAggCtx,arg,arg_ind))
    return ODCI_ERROR;

  return ODCI_SUCCESS;
}

/*-----------------------------------------------------------------------*/

/* ODCIAgregateWrapContext */

int WrapContext(OCIExtProcContext* extProcCtx, 
                Imp_t* self, Imp_Ind_t* self_ind)
{
  Handles_t handles;
  ExtAggCtx_t* extAggCtx;

  /* Get OCI handles */
  if (GetHandles(extProcCtx, &handles))
    return ODCI_ERROR;

  /* nothing to do if no external context */
  if (self_ind->key==OCI_IND_NULL)
    return ODCI_SUCCESS;

  /* Get context */
  extAggCtx=GetExtAggCtx(&handles,self,self_ind);
  if (!extAggCtx) return ODCI_ERROR;

  if (do_wrap(&handles,extAggCtx,&self->aggCtx,&self_ind->aggCtx))
    return ODCI_ERROR;

  /* Free external context memory */
  if (ChkErr(&handles, OCIMemoryFree((dvoid*)handles.usrhp, 
                          handles.errhp, (dvoid*) extAggCtx)))
    return ODCI_ERROR;

  self_ind->key=OCI_IND_NULL;

  return ODCI_SUCCESS;
}

/*-------------------------------------------------------------------------
                           PRIVATE FUNCTIONS
  -----------------------------------------------------------------------*/

static int do_init(Handles_t* handles, ExtAggCtx_t* extAggCtx)
{
  /* Initialize context */
  extAggCtx->x=0.0;
  extAggCtx->y=0.0;

  return 0;
}

/*-----------------------------------------------------------------------*/

static int do_iterate(Handles_t* handles, ExtAggCtx_t* extAggCtx, 
                      Vector_t* arg, Vector_Ind_t* arg_ind)
{
  double length=0.0;
  double angle=0.0;

  /* get the input value */
  if (arg_ind->length==OCI_IND_NOTNULL)
  {
    if (ChkErr(handles, OCINumberToReal(handles->errhp, &arg->length,
                            sizeof(length), (dvoid*) &length)))
      return(-1);

  }

  if (arg_ind->angle==OCI_IND_NOTNULL)
  {
    if (ChkErr(handles, OCINumberToReal(handles->errhp, &arg->angle,
                            sizeof(angle), (dvoid*) &angle)))
      return(-1);

  }

  /* update the context by adding the equivalent vector coordinates */
  extAggCtx->x+=length*cos(angle);
  extAggCtx->y+=length*sin(angle);

  return 0;
}

/*-----------------------------------------------------------------------*/

static int do_terminate(Handles_t* handles, ExtAggCtx_t* extAggCtx,
                        Vector_t* result, Vector_Ind_t* result_ind)
{
  double length=0.0;
  double angle=0.0;

  /* calculate the length and angle using the coordinates in context */

  if (extAggCtx->x!=0 || extAggCtx->y!=0)
    length=sqrt(extAggCtx->x*extAggCtx->x+extAggCtx->y*extAggCtx->y);

  if (extAggCtx->x!=0)
    angle=atan(extAggCtx->y/extAggCtx->x);

  /* assign the length and angle to the result vector */
  if (ChkErr(handles, OCINumberFromReal(handles->errhp, &length,
                          sizeof(length), &result->length)))
    return -1;
 
  result_ind->length=OCI_IND_NOTNULL;

  if (ChkErr(handles, OCINumberFromReal(handles->errhp, &angle,
                          sizeof(angle), &result->angle)))
    return -1;
 
  result_ind->angle=OCI_IND_NOTNULL;
  
  result_ind->_atomic=OCI_IND_NOTNULL;

  return 0;
}

/*-----------------------------------------------------------------------*/

static int do_merge(Handles_t* handles, ExtAggCtx_t* extAggCtx, 
                    ExtAggCtx_t* extAggCtx2)
{
  /* add the other context coordiantes to the current coordinates */
  extAggCtx->x+=extAggCtx2->x;
  extAggCtx->y+=extAggCtx2->y;
 
  return 0;
}

/*-----------------------------------------------------------------------*/

static int do_delete(Handles_t* handles, ExtAggCtx_t* extAggCtx, 
                     Vector_t* arg, Vector_Ind_t* arg_ind)
{
  double length=0.0;
  double angle=0.0;

  /* get the input value */
  if (arg_ind->length==OCI_IND_NOTNULL)
  {
    if (ChkErr(handles, OCINumberToReal(handles->errhp, &arg->length,
                            sizeof(length), (dvoid*) &length)))
      return(-1);

  }

  if (arg_ind->angle==OCI_IND_NOTNULL)
  {
    if (ChkErr(handles, OCINumberToReal(handles->errhp, &arg->angle,
                            sizeof(angle), (dvoid*) &angle)))
      return(-1);

  }

  /* update the context by subtracting the equivalent vector coordinates */
  extAggCtx->x-=length*cos(angle);
  extAggCtx->y-=length*sin(angle);

  return 0;
}

/*-----------------------------------------------------------------------*/

static int do_wrap(Handles_t* handles, ExtAggCtx_t* extAggCtx,
                   AggCtx_t* aggCtx, AggCtx_Ind_t* aggCtx_ind)
{
  /* transfer the external context's coordinates to the embedded context */

  if (ChkErr(handles, OCINumberFromReal(handles->errhp, &extAggCtx->x,
                          sizeof(extAggCtx->x), &aggCtx->x)))
    return -1;

  aggCtx_ind->x = OCI_IND_NOTNULL;

  if (ChkErr(handles, OCINumberFromReal(handles->errhp, &extAggCtx->y,
                          sizeof(extAggCtx->y), &aggCtx->y)))
    return -1;

  aggCtx_ind->y = OCI_IND_NOTNULL;

  aggCtx_ind->_atomic = OCI_IND_NOTNULL;

  return 0;
}

/*-----------------------------------------------------------------------*/

static int do_unwrap(Handles_t* handles, ExtAggCtx_t* extAggCtx,
                     AggCtx_t* aggCtx, AggCtx_Ind_t* aggCtx_ind)
{
  extAggCtx->x=0.0;
  extAggCtx->y=0.0;

  /* get the coordinates from the embedded context and store them
     in the external context */

  if (aggCtx_ind->x==OCI_IND_NOTNULL)
  {
    if (ChkErr(handles, OCINumberToReal(handles->errhp, &aggCtx->x,
                            sizeof(extAggCtx->x), &extAggCtx->x)))
      return -1;
  }

  if (aggCtx_ind->x==OCI_IND_NOTNULL)
  {
    if (ChkErr(handles, OCINumberToReal(handles->errhp, &aggCtx->y,
                            sizeof(extAggCtx->y), &extAggCtx->y)))
      return -1;
  }

  return 0;
}

/*-----------------------------------------------------------------------*/

static ExtAggCtx_t* GetExtAggCtx(Handles_t* handles, Imp_t* self,
                                 Imp_Ind_t* self_ind)
{
  ExtAggCtx_t* extCtx;                  /* The real context */

  if (self_ind->key==OCI_IND_NOTNULL)
  {
    /* the key is not null, so we'll use it to get 
       the address of external context */

    ub1* key;                       /* key to retrieve context */
    ub4 keylen;                     /* length of key */

    /* Get the key */
    key = OCIRawPtr(handles->envhp, self->key);
    keylen = OCIRawSize(handles->envhp, self->key);

    /* Retrieve context from key */
    if (ChkErr(handles, OCIContextGetValue((dvoid *)handles->usrhp, 
                            handles->errhp, key, (ub1)keylen, 
                            (dvoid**) &extCtx)))
      return NULL;
  }
  else
  {
    /* the key attribute was NULL, so we will allocate memory for
       the external context */

    ub4 key;

    /* Allocate memory to hold external scan context */
    if (ChkErr(handles, OCIMemoryAlloc((dvoid *)handles->usrhp, 
                             handles->errhp, (dvoid **)&extCtx,
                             OCI_DURATION_STATEMENT, 
                             (ub4)(sizeof(ExtAggCtx_t)), OCI_MEMORY_CLEARED)))
      return NULL;

    /* generate a key */
    if (ChkErr(handles, OCIContextGenerateKey((dvoid *)handles->usrhp, 
                               handles->errhp, &key)))
      return NULL;
   
    /* set the memory address of the struct to be saved in the context */
    if (ChkErr(handles, OCIContextSetValue((dvoid *)handles->usrhp, 
                            handles->errhp, OCI_DURATION_STATEMENT,
                            (ub1 *)&key, (ub1)sizeof(key), (dvoid*) extCtx)))
      return NULL;
  
    /* store the key in self */
    if (ChkErr(handles, OCIRawAssignBytes(handles->envhp, handles->errhp,
                            (ub1 *)&key, (ub4)sizeof(key), &(self->key))))
      return NULL;
  
    self_ind->key = OCI_IND_NOTNULL;

    /* if the embedded context is not null, use it to initialize the
       external context (unwrap) */

    if (self_ind->aggCtx._atomic==OCI_IND_NOTNULL)
    {
      /* unwrap - create external context from embedded context */
      if (do_unwrap(handles,extCtx,&self->aggCtx,&self_ind->aggCtx))
        return NULL;

      /* set the embedded context to NULL */
      self_ind->aggCtx._atomic = OCI_IND_NULL;

    }

  }

  return extCtx;
}

/*-----------------------------------------------------------------------*/

static int GetHandles(OCIExtProcContext* extProcCtx, Handles_t* handles)
{
  handles->extProcCtx=extProcCtx;

  /* Get OCI env, error and service handles */
  if (ChkErr(handles, OCIExtProcGetEnv(extProcCtx, &handles->envhp, 
                          &handles->svchp, &handles->errhp)))
    return -1;

  /* get the user handle */
  if (ChkErr(handles, OCIAttrGet((dvoid *)handles->svchp, 
                          (ub4)OCI_HTYPE_SVCCTX, (dvoid *)&handles->usrhp, 
                          (ub4 *)0, (ub4)OCI_ATTR_SESSION, handles->errhp)))
    return -1;

  return 0;
}

/*-----------------------------------------------------------------------*/

static int ChkErr(Handles_t* handles, sword status)
{
  text errbuf[512];
  sb4 errcode;

  /* check the error code */
  switch (status)
  {
  case OCI_SUCCESS:
  case OCI_SUCCESS_WITH_INFO:
    return 0;
  case OCI_ERROR:
    OCIErrorGet ((dvoid *) handles->errhp, (ub4) 1, (text *) NULL, &errcode,
            errbuf, (ub4) sizeof(errbuf), (ub4) OCI_HTYPE_ERROR);
    sprintf((char*)errbuf, "OCI ERROR code %d",errcode);
    break;
  default:
    sprintf((char*)errbuf, "Warning - error status %d",status);
    break;
  }

  /* register exception */
  OCIExtProcRaiseExcpWithMsg(handles->extProcCtx, 29400, errbuf, 
    strlen((char*)errbuf));
  return 1;
}

/* end of file extdemo4.c */

