#ifdef RCSID
static char *RCSid =
   "$Header: ociucb.c 14-jul-99.13:40:30 mjaeger Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1998, 1999, Oracle Corporation.  All rights reserved.
*/

/*

   NAME
     ociucb.c - OCI - User Call Back

   DESCRIPTION
     Contains the defintion of ociucbInit and ociucbEnvCallback routines.

     The ociucbInit routine is called by OCI to initialize the shared library.
     The ociucbEnvCallback routine is called by OCIEnvInit if ORA_OCI_UCBPKG
     environment variable is set and OCIInitialize is not called with a mode of
     OCI_ENV_NO_UCB.

   PUBLIC FUNCTION(S)
     ociucbInit - OCI Shared Library Callback Main Function
     ociucbEnvCallback - OCI ENVironment handle CALLBACK

   PRIVATE FUNCTION(S)
     None yet.

   RETURNS
     NA

   NOTES


   MODIFIED   (MM/DD/YY)
   slari       09/10/99 - bg989981: remove envCallback from ociucbInit
   slari       08/30/99 - provide ociucbInit
   slari       08/23/99 - add OCIUcb in call to OCIEnvCallback
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   slari       04/12/98 - change message for OCIEnvCallback
   slari       03/17/98 - add comments
   slari       02/12/98 - user callback file to provide code for OCIEnvCallback
   slari       02/12/98 - Creation

*/



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <oci.h>


                    /* the forward declaration must be done so that
                       OCIShareLibInit can be passed a pointer to this function
                       */

sword ociucbEnvCallback(OCIEnv *env, ub4 mode, size_t xtramemsz, dvoid *usrmemp,
                        OCIUcb *ucbDesc);



/*--------------------------------- ociucbInit ---------------------------------*/

/*
   NAME:
       ociucbInit - OCI Shared Library CallBack Main function

   PARAMETERS:
   	meta	- The metacontext
        libCtx	- The context for this package
 	argfmt  - The package argument format
        argc    - The number of arguments passed
        argv	- The arguments to the package
      
   DESCRIPTION:
       This is called by OCI to load and initialize the shared library
       (package).

       The OCI shared library initialization is done by passing all the
       parameters passed to the the shared library initialization function to
       the OCISharedLibInit function.  User's environment callback function of
       type OCIEnvCallbackType is also passed to the OCISharedLibInit call.


   RETURNS:  
   	the return code from OCISharedLibInit function.

   NOTES:  
*/

sword ociucbInit(metaCtx, libCtx, argfmt, argc, argv)
dvoid *		metaCtx;                                  /* The metacontext */
dvoid *		libCtx;       /* The context for this program or package if you
                                 have previously been called. */ 
ub4 		argfmt;                         /* What am I supposed to do? */
sword 		argc;              /* argc if I am being called as a program */
dvoid *		argv[];            /* argv if I am being called as a program */
{
  return  (OCISharedLibInit(metaCtx, libCtx, argfmt, argc, argv,
                            ociucbEnvCallback));
}


/*--------------------------------- ociucbEnvCallback -----------------------*/

/*
   NAME:
        ociucbEnvCallback - OCI ENVironment handle CALLBACK

   DESCRIPTION:
        This function is called by the OCIEnvInit function at the end after it
        has allocated and intialized the environment handle.

    PARAMETERS
        env     - The newly created environment handle
        mode    - The mode passed to the OCIEnvInit call
        xtramemsz - The xtramemsz passed to the OCIEnvInit call
        usrmemp - Pointer to additional memory allocated for user memory
                  requested
        ucbDesc - OCIUcb descriptor

   RETURNS:
        Success/Error Code

   NOTES:
*/


sword ociucbEnvCallback(env, mode, xtramemsz, usrmemp, ucbDesc)
OCIEnv  *env;
ub4     mode;
size_t  xtramemsz;
dvoid   *usrmemp;
OCIUcb	*ucbDesc;
{
  printf("ociucbEnvCallback called.\n");

  return OCI_CONTINUE;
}



/* end of file ociucb.c */

