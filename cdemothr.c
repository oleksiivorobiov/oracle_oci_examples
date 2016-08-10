/* Copyright (c) 1997, 1999, Oracle Corporation.  All rights reserved.
*/

/*

   NAME
     cdemothr.c - C Demo for ociTHRead

   DESCRIPTION
     This file demonstrates an example of using the OCIThread package

   PUBLIC FUNCTION(S)
     <list of external functions declared/defined - with one-line descriptions>

   PRIVATE FUNCTION(S)
     <list of static functions defined in .c file - with one-line descriptions>

   RETURNS
     <function return values, for .c file with single function>

   NOTES
     <other useful comments, qualifications, etc.>

   MODIFIED   (MM/DD/YY)
   msowdaga    04/30/08 - Fix bug 6236196, multiple threads should not share
                          single error handle
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   tsaulys     10/14/98 - minor changes
   nramakri    12/17/97 - Creation

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <oci.h>
#include <cdemothr.h>

static OCIEnv *envhp;
static OCIError *errhp;

static void  ThrKeyDestroy(/*_ dvoid *arg _*/);
static void  ThrFunc(/*_ dvoid *arg _*/);
static int   ThrMain(/*_ void _*/);
static sword checkerr(/*_ OCIError *errhp, sword status _*/);
int main(/*_ int argc, char *argv[] _*/);

int main(argc, argv)
int argc;
char *argv[];
{
  int status = 0;

  (void) OCIInitialize((ub4) OCI_THREADED, (dvoid *)0,
                       (dvoid * (*)(dvoid *, size_t)) 0,
                       (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                       (void (*)(dvoid *, dvoid *)) 0 );

  (void) OCIEnvInit( (OCIEnv **) &envhp, OCI_DEFAULT, (size_t) 0,
                     (dvoid **) 0 );

  (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &errhp, OCI_HTYPE_ERROR,
                         (size_t) 0, (dvoid **) 0);

  status = ThrMain();

  if (errhp)
    (void) OCIHandleFree((dvoid *) errhp, OCI_HTYPE_ERROR);

  return status;
}

sword checkerr(errhp, status)
OCIError *errhp;
sword status;
{
  text errbuf[512];
  sb4 errcode = 0;

  switch (status)
  {
  case OCI_SUCCESS:
    break;
  case OCI_ERROR:
    (void) OCIErrorGet((dvoid *)errhp, (ub4) 1, (text *) NULL, &errcode,
                        errbuf, (ub4) sizeof(errbuf), OCI_HTYPE_ERROR);
    (void) printf("Error - %.*s\n", 512, errbuf);
    break;
  case OCI_INVALID_HANDLE:
    (void) printf("Error - OCI_INVALID_HANDLE\n");
    break;
  default:
    break;
  }
  return status;
}

/*--------------------------- ThrKeyDestroy() --------------------------*/

/*
   NAME
     ThrKeyDestroy - Thread Key Destructor Function

   DESCRIPTION
     This is the destructor function used with the 'key_' element of the
     'CDemoThrCtx' data type.  See 'thrdemo.h' for info on the
     datatype.

   PARAMETERS
     void ThrKeyDestroy(dvoid *arg)

       arg      This will be the key's value for the thread that just
                terminated.

   RETURNS
     Nothing.

   NOTES
     This sends a diagnostic message to 'stdout' if it is called with a NULL
     arg.

     Otherwise, it sets the 'ub1' pointed to by 'arg' to FALSE.
*/

static void ThrKeyDestroy(arg)
dvoid       *arg;
{
   ub1  *loc = (ub1 *) arg;

   if (arg == (dvoid *)NULL)
   {
      printf("Key destructor called with NULL argument.\n");
   }
   else
     *loc = FALSE;

   return;
}

/*----------------------------- ThrFunc() ------------------------------*/

/*
   NAME
     ThrFunc - Thread Function

   DESCRIPTION
     This is the function that gets spawned as a separate thread by
     'ThrMain()'.

   PARAMETERS
     void ThrFunc(dvoid *arg)

       arg     This should point to the 'CDemoThrCtx' being used

   RETURNS
     Nothing.

   NOTES
     This sends a diagnostic message to 'stdout' if something unexpected
     happens.

*/

static void ThrFunc(arg)
dvoid   *arg;
{
   CDemoThrCtx   *tdctx = (CDemoThrCtx *) arg;
   OCIError  *errhp2 = (OCIError *) 0;
   OCIThreadId      *ourID;  /* This eventually gets this thread's ID.       */
   sword            ourTNum; /* Eventually, this gets our thread number.     */
   uword            i;       /* Loop counter.                                */
   uword            flag;    /* Boolean flag.                                */
   dvoid            *keyval; /* Used to retrieve values from a key.          */
   boolean          result1, result2;


   (void) OCIHandleAlloc((dvoid *) envhp, (dvoid **) &errhp2, OCI_HTYPE_ERROR,
                     (size_t) 0, (dvoid **) 0);

   /* ***** INITIALIZATION ***** */

   if (checkerr(errhp2, OCIThreadIdInit(envhp, errhp2, &ourID)) != OCI_SUCCESS)
     goto exit0;

   /* ***** THREAD ID MANIPULATION ***** */

   if (checkerr(errhp2, OCIThreadIdGet(envhp, errhp2, ourID)) != OCI_SUCCESS)
     goto exit1;

   /* Make sure the ID we just got is not NULL, and is not the same as the  */
   /* ID for the main thread.                                               */

   (void) checkerr(errhp2, OCIThreadIdNull(envhp, errhp2, ourID, &result1));
   (void) checkerr(errhp2, OCIThreadIdSame(envhp, errhp2, ourID,
                                   tdctx->mainTID_CDemoThrCtx, &result2));
   if ((result1 == TRUE) ||
       (result2 == ((OCIThreadIsMulti() == TRUE) ? TRUE : FALSE)))
   {
      printf("A spawned thread's ID doesn't seem to be valid.\n");
   }

   /* ***** DETERMINE THREAD NUMBER ***** */

   ourTNum = -1;

   /* We need to search the 'tdctx->tidAr[]' array.  First, we must acquire  */
   /* its mutex.                                                             */
   (void) checkerr(errhp2, OCIThreadMutexAcquire(envhp, errhp2,
                                         tdctx->tidArMx_CDemoThrCtx));

   /* Search 'tdctx->tidAr[]'. */
   for (i = 0; i < CDEMOTHR_NUMTHREADS; i++)
   {
      (void) checkerr(errhp2, OCIThreadIdSame(envhp, errhp2, ourID,
                                     tdctx->tidAr_CDemoThrCtx[i], &result1));
      if (result1 == TRUE)
      {
         ourTNum = (sword) i;
         break;
      }
   }

   /* We are done with the array.  We can release its mutex. */
   (void) checkerr(errhp2, OCIThreadMutexRelease(envhp, errhp2,
                                         tdctx->tidArMx_CDemoThrCtx));

   if (ourTNum == -1)
   {
      /* We couldn't find our thread ID. */
      printf("A thread's ID was not in the ID array.\n");

      /* There is nothing more we can do.  We should just terminate. */
      goto exit1;
   }

   /* ***** SAVE OUR THREAD NUMBER IN THE NUMBER ARRAY ***** */

   /* We can indicate everything is OK by putting our thread number in the  */
   /* array of thread numbers.                                              */
   (tdctx->tnumAr_CDemoThrCtx)[ourTNum] = ourTNum;

   /* ***** USING KEYS ***** */

   /* With 'tdctx->key_', we will check that the initial value is NULL, and  */
   /* that we can set and retrieve values correctly.                         */

   if (checkerr(errhp2, OCIThreadKeyGet(envhp, errhp2, tdctx->key_CDemoThrCtx,
                                      &keyval)) != OCI_SUCCESS)
   {
      printf("Could not retrieve initial value from key.\n");
   }

   if (keyval != (dvoid *)NULL)
   {
      printf("Initial value of the key is not NULL.\n");
   }

   if (checkerr(errhp2, OCIThreadKeySet(envhp, errhp2, tdctx->key_CDemoThrCtx,
                                      (dvoid *) &ourTNum))
               != OCI_SUCCESS)
   {
      printf("Could not set the value of key.\n");
   }

   if (checkerr(errhp2, OCIThreadKeyGet(envhp, errhp2, tdctx->key_CDemoThrCtx,
                                      &keyval)) != OCI_SUCCESS)
   {
      printf("Could not retrieve value of key after it was set.\n");
   }

   if (keyval != (dvoid *) &ourTNum)
   {
      printf("Incorrect value from key after setting it.\n");
   }

   /* For 'tdctx->key_', we will now set the value in the key to point to    */
   /* our element in 'tdctx->key_'.  Then, some threads will reset the value */
   /* to NULL and clear the element in 'tdctx->key_' to FALSE.               */

   if (checkerr(errhp2, OCIThreadKeySet(envhp, errhp2, tdctx->key_CDemoThrCtx,
               (dvoid *)&(tdctx->keyAr_CDemoThrCtx[ourTNum])))
               != OCI_SUCCESS)
   {
     printf("Could not set value in key.\n");
   }

   if (ourTNum < (CDEMOTHR_NUMTHREADS/2))
   {
      if (checkerr(errhp2, OCIThreadKeySet(envhp, errhp2, tdctx->key_CDemoThrCtx,
                                         (dvoid *)NULL)) != OCI_SUCCESS)
      {
        printf("Could not set value in key to NULL.\n");
      }
      tdctx->keyAr_CDemoThrCtx[ourTNum] = FALSE;
   }

   /* ***** TERMINATION ***** */

   /* This is the exit point.  This terminates the OCITHREAD context (if it  */
   /* is not NULL), and returns.                                             */

 exit1:
   (void) checkerr(errhp2, OCIThreadIdDestroy(envhp, errhp2, &ourID));

 exit0:
   OCIHandleFree((dvoid *)errhp2, OCI_HTYPE_ERROR);
   return;
}

static int ThrMain()
{

   CDemoThrCtx      tdctx;
   OCIThreadId      *tidArray[CDEMOTHR_NUMTHREADS];
   OCIThreadHandle  *tHndArray[CDEMOTHR_NUMTHREADS];

   uword        i;          /* Loop counter.                                */
   dvoid        *keyval;    /* Used to retrieve values for a key.           */
   uword        tJoins;     /* Number of threads we join with.              */
   eword        demoStat;   /* Set to TRUE if the test succeeded.           */
   boolean      result;     /* For Boolean tests                            */

   demoStat = TRUE;
   tJoins = 0;

   /* ***** INITIALIZATION ***** */

   /* In a multi-threaded application, 'OCIThreadProcessInit()' must be
      called before anything else.  In addition, the call must not be
      concurrent with any other OCIThread calls.  Therefore, we will make the
      call here, before we spawn any threads.
   */
   printf("Initializing OCIThread. \n");
   OCIThreadProcessInit();

   if (checkerr(errhp, OCIThreadInit(envhp, errhp)) != OCI_SUCCESS)
   {
     demoStat = FALSE;
     goto exit0;
   }

   printf("Initializing the thread ID structure. \n");
   if (checkerr(errhp, OCIThreadIdInit(envhp, errhp,
                                     &(tdctx.mainTID_CDemoThrCtx)))
                != OCI_SUCCESS)
   {
     demoStat = FALSE;
     goto exit1;
   }

   /*
    * Setup what we need to in 'tdctx'.
    */

   /* Put our thread ID in. */
   printf("Retrieving the current thread ID. \n");
   if (checkerr(errhp, OCIThreadIdGet(envhp, errhp, tdctx.mainTID_CDemoThrCtx))
               != OCI_SUCCESS)
   {
     demoStat = FALSE;
     goto exit0;
   }

   /* Initialize the mutex protecting the thread array. */
   printf("Intializing the mutex protecting the thread array. \n");
   if (checkerr(errhp, OCIThreadMutexInit(envhp, errhp,
                                        &(tdctx.tidArMx_CDemoThrCtx)))
               != OCI_SUCCESS)
   {
     demoStat = FALSE;
     goto exit2;
   }

   printf("Initializing all thread IDs and thread handles. \n");
   for (i = 0; i < CDEMOTHR_NUMTHREADS; i++)
   {
      if (checkerr(errhp, OCIThreadIdInit(envhp, errhp, &(tidArray[i])))
                  != OCI_SUCCESS)
      {
        demoStat = FALSE;
        goto exit2;
      }

      if (checkerr(errhp, OCIThreadHndInit(envhp, errhp,
                                         &(tHndArray[i]))) != OCI_SUCCESS)
      {
        demoStat = FALSE;
        goto exit2;
      }

      if (checkerr(errhp, OCIThreadIdInit(envhp, errhp,
                                        &((tdctx.tidAr_CDemoThrCtx)[i])))
                  != OCI_SUCCESS)
      {
        demoStat = FALSE;
        goto exit2;
      }
   }

   /* Set the thread ID arrays (both 'tidArray' and the one in 'tdctx') to hold
      only NULL thread IDs.  Clear the thread number array in 'tdctx'.
   */
   printf("Setting all thread IDs and thread handles to NULL. \n");
   for (i = 0; i < CDEMOTHR_NUMTHREADS; i++)
   {
      (void) checkerr(errhp, OCIThreadIdSetNull(envhp, errhp, tidArray[i]));
      (void) checkerr(errhp, OCIThreadIdSetNull(envhp, errhp,
                                              (tdctx.tidAr_CDemoThrCtx)[i]));
      (tdctx.tnumAr_CDemoThrCtx)[i] = -1;
   }

   /* Initialize 'tdctx.key_' and its array.  This key will have
      'ThrKeyDestroy()' as its destructor function.
   */
   printf("Initializing the thread key. \n");
   if (OCIThreadIsMulti() == TRUE)
   {
     (void) checkerr(errhp, OCIThreadKeyInit(envhp, errhp,
                                    &(tdctx.key_CDemoThrCtx),
                                     (OCIThreadKeyDestFunc)ThrKeyDestroy));
   }
   else
   {
     (void) checkerr(errhp, OCIThreadKeyInit(envhp, errhp,
                                    &(tdctx.key_CDemoThrCtx), NULL));

   }

   printf("Setting the value for the thread key for each thread. \n");
   for (i = 0; i < CDEMOTHR_NUMTHREADS; i++)
   {
      tdctx.keyAr_CDemoThrCtx[i] = TRUE;
   }

   /* ***** SPAWN OTHER THREADS ***** */

   /* Acquire the mutex protecting the thread ID array in 'tdctx'.  We will  */
   /* release it once the array has been filled.                             */
   (void) checkerr(errhp, OCIThreadMutexAcquire(envhp, errhp,
                                                 tdctx.tidArMx_CDemoThrCtx));

   /* Spawn all the threads. */
   printf("Spawning all threads. \n");
   for (i = 0; i < CDEMOTHR_NUMTHREADS; i++)
   {
      if (OCIThreadIsMulti() == TRUE) /* Multi-threaded environment */
      {
        if (checkerr(errhp, OCIThreadCreate(envhp, errhp, ThrFunc,
                                           (dvoid *) &tdctx,
                                            tidArray[i], tHndArray[i]))
                    != OCI_SUCCESS)
        {
          demoStat = FALSE;
        }
      }
      else
      {
        ThrFunc((dvoid *)&tdctx);
      }

      /* Put the thread ID for the new thread in the array in 'tdctx'. */
      (void) checkerr(errhp, OCIThreadIdSet(envhp, errhp,
                                    (tdctx.tidAr_CDemoThrCtx)[i],
                                     tidArray[i]));
   }

   /* All the threads are spawned; so 'tdctx.tidAr_CDemoThrCtx[]' is full.
      We can release the mutex protecting it.
   */
   (void) checkerr(errhp, OCIThreadMutexRelease(envhp, errhp,
                                         tdctx.tidArMx_CDemoThrCtx));

   printf("Waiting for all threads to finish. \n");
   if (OCIThreadIsMulti() == TRUE)
   {
     /* ***** WAIT FOR OTHER THREADS TO FINISH ***** */
     /* Wait for each thread in turn, and make sure that it put its number */
     /* in the thread number array.                                        */

     for (i=0; i<CDEMOTHR_NUMTHREADS; i++)
     {
       if (checkerr(errhp, OCIThreadJoin(envhp, errhp, tHndArray[i]))
                   != OCI_SUCCESS)
       {
         demoStat = FALSE;
       }

       /* Close the handle to the thread */
       (void) checkerr(errhp, OCIThreadClose(envhp, errhp, tHndArray[i]));
     }
   }

   printf("Setting the thread ID to NULL for each thread. \n");
   for (i=0; i<CDEMOTHR_NUMTHREADS; i++)
   {
     (void) checkerr(errhp, OCIThreadIdNull(envhp, errhp, tidArray[i],
                                             &result));

     if (result == ((OCIThreadIsMulti() == TRUE) ? TRUE : FALSE))
     {
       printf("Invalid Thread Id!\n");
       goto exit2;
     }

     /* Set the current thread ID to NULL, so we know we've tried it. */
     (void) checkerr(errhp, OCIThreadIdSetNull(envhp, errhp, tidArray[i]));

     (void) checkerr(errhp, OCIThreadIdNull(envhp, errhp, tidArray[i],
                                             &result));

     if (result == FALSE)
     {
       printf("Thread Id is not NULL\n");
       goto exit2;
     }

     if (OCIThreadIsMulti() == TRUE)
     {
       if ((tdctx.tnumAr_CDemoThrCtx)[i] != i)
       {
         printf("A thread's num %d was not in the number array.\n", i);
         demoStat = FALSE;
       }
     }
   }

   /* We are all done; we can release and destroy the mutexes. */
   (void) checkerr(errhp, OCIThreadMutexDestroy(envhp, errhp,
                                       &(tdctx.tidArMx_CDemoThrCtx)));

   /* ***** TERMINATION ***** */

   if (checkerr(errhp, OCIThreadKeyDestroy(envhp, errhp,
                                     &(tdctx.key_CDemoThrCtx)))
              != OCI_SUCCESS)
   {
     demoStat = FALSE;
   }

   printf("Destroying all of the mutexes, thread IDs, and thread handles. \n");
   for (i = 0; i < CDEMOTHR_NUMTHREADS; i++)
   {
      if (checkerr(errhp, OCIThreadIdDestroy(envhp, errhp, &(tidArray[i])))
                  != OCI_SUCCESS)
      {
        demoStat = FALSE;
        goto exit2;
      }

      if (checkerr(errhp, OCIThreadHndDestroy(envhp, errhp, &(tHndArray[i])))
                  != OCI_SUCCESS)
      {
        demoStat = FALSE;
        goto exit2;
      }

      if (checkerr(errhp, OCIThreadIdDestroy(envhp, errhp,
                                       &(tdctx.tidAr_CDemoThrCtx)[i]))
                  != OCI_SUCCESS)
      {
        demoStat = FALSE;
        goto exit2;
      }
  }

 exit2:
  (void) checkerr(errhp, OCIThreadIdDestroy(envhp, errhp,
                                           &(tdctx.mainTID_CDemoThrCtx)));

 exit1:
  (void) checkerr(errhp, OCIThreadTerm(envhp, errhp));

 exit0:
  if (demoStat)
  {
     printf("Demo succeeded.\n");
  }
  else
  {
     printf("Demo failed.\n");
  }

  return (demoStat ? 0: -1);
}

/* end of file cdemothr.c */

