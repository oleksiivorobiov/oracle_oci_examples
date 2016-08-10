/* Copyright (c) 2008, Oracle.  All rights reserved.  */
 
/* 
   NAME 
     extdemo6.h - Extensible Indexing example implemented as C routines
                  for system-managed local domain index on varchar2 column 
                  of a range partitioned table.

   DESCRIPTION 
     This file contains the definitions of the DML and Query routines
     for the extensible indexing example that implements a simple btree
     (sbtree) using a system-managed approach. See extdemo6.sql for the 
     SQL script that defines the indextype.

   RELATED DOCUMENTS 
 
   PUBLIC FUNCTION(S) 
     qxiqtbsps - QXIQT Btree Start routine
     qxiqtbspf - QXIQT Btree Fetch routine
     qxiqtbspc - QXIQT Btree Close routine
     qxiqtbspi - QXIQT Btree Insert routine
     qxiqtbspd - QXIQT Btree Delete routine
     qxiqtbspu - QXIQT Btree Update routine

   PRIVATE FUNCTION(S)
     qxiqtce - QXIQT error reporting routine

   EXAMPLES

   NOTES
     <other useful comments, qualifications, etc.>

   MODIFIED   (MM/DD/YY)
   yhu         06/06/08 - Creation

*/

#ifndef EXTDEMO6_ORACLE
# define EXTDEMO6_ORACLE

#ifndef OCI_ORACLE
# include <oci.h>
#endif
#ifndef ODCI_ORACLE
# include <odci.h>
#endif

/*---------------------------------------------------------------------------
                     PUBLIC TYPES AND CONSTANTS
  ---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------
                     PRIVATE TYPES AND CONSTANTS
  ---------------------------------------------------------------------------*/

#ifdef WIN32COMMON
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/* index scan context - should be stored in "statement" duration memory
 * and used by start, fetch and close routines.
 */
struct qxiqtcx
{
  OCIStmt *stmthp;
  OCIDefine *defnp;
  OCIBind *bndp;
  char ridp[19];
};
typedef struct qxiqtcx qxiqtcx;

/* The index implementation type is an ADT with a single RAW attribute
 * which will be used to store the context key value.
 * C mapping of the implementation type :
 */
struct qxiqtim
{
   OCIRaw *sctx_qxiqtim;
};
typedef struct qxiqtim qxiqtim;

struct qxiqtin
{
  short atomic_qxiqtin;
  short scind_qxiqtin;
};
typedef struct qxiqtin qxiqtin;

/*---------------------------------------------------------------------------
                           EXPORT FUNCTIONS
  ---------------------------------------------------------------------------*/
/* ODCIIndexStart */
OCINumber DLLEXPORT *qxiqtbsps( OCIExtProcContext *ctx,
                     struct qxiqtim *sctx, struct qxiqtin *sctx_ind,
                     ODCIIndexInfo *ix, ODCIIndexInfo_ind *ix_ind,
                     ODCIPredInfo *pr, ODCIPredInfo_ind *pr_ind,
                     ODCIQueryInfo *qy, ODCIQueryInfo_ind *qy_ind,
                     OCINumber *strt, short strt_ind,
                     OCINumber *stop, short stop_ind,
                     char *cmpval, short cmpval_ind, 
                     ODCIEnv *env, ODCIEnv_ind *env_ind );

/* ODCIIndexFetch */
OCINumber DLLEXPORT *qxiqtbspf( OCIExtProcContext *ctx,
                     struct qxiqtim *self, struct qxiqtin *self_ind,
                     OCINumber *nrows, short nrows_ind,
                     OCIArray **rids, short *rids_ind, 
                     ODCIEnv *env, ODCIEnv_ind *env_ind );

/* ODCIIndexClose */
OCINumber DLLEXPORT *qxiqtbspc( OCIExtProcContext *ctx,
                     struct qxiqtim *self, struct qxiqtin *self_ind, 
                     ODCIEnv *env, ODCIEnv_ind *env_ind );

/* ODCIIndexInsert */
OCINumber DLLEXPORT *qxiqtbspi( OCIExtProcContext *ctx,
                     ODCIIndexInfo *ix,
                     ODCIIndexInfo_ind *ix_ind,
                     char *rid,
                     short rid_ind,
                     char *newval,
                     short newval_ind,  
                     ODCIEnv *env, ODCIEnv_ind *env_ind );

/* ODCIIndexDelete  */
OCINumber DLLEXPORT *qxiqtbspd( OCIExtProcContext *ctx,
                     ODCIIndexInfo *ix,
                     ODCIIndexInfo_ind *ix_ind,
                     char *rid,
                     short rid_ind,
                     char  *oldval,
                     short oldval_ind, 
                     ODCIEnv *env, ODCIEnv_ind *env_ind );

/* ODCIIndexUpdate  */
OCINumber DLLEXPORT *qxiqtbspu( OCIExtProcContext *ctx,
                     ODCIIndexInfo *ix,
                     ODCIIndexInfo_ind *ix_ind,
                     char *rid,
                     short rid_ind,
                     char *oldval,
                     short oldval_ind,
                     char *newval,
                     short newval_ind, 
                     ODCIEnv *env, ODCIEnv_ind *env_ind );


/*---------------------------------------------------------------------------
                          INTERNAL FUNCTIONS
  ---------------------------------------------------------------------------*/

static int qxiqtce( OCIExtProcContext *ctx, OCIError *errhp,
                      sword status );

#endif                                              /* EXTDEMO6_ORACLE */
