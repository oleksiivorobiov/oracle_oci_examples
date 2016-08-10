#ifdef RCSID
static char *RCSid =
   "$Header: cdemodp.c 28-feb-2005.11:23:54 eegolf Exp $ ";
#endif /* RCSID */

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/* Copyright (c) 1998, 2005, Oracle. All rights reserved.  */
/*                                                                         */
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*
**   NAME:
**     cdemodp.c - C Demo program for Direct Path api
**
**
**   DESCRIPTION:
**     - Direct Path Api driver program to demonstrate loading.
**
**   NOTES:
**     Demonstrates usage of the direct path API.
**
**      -- cdemodp.c --
**     This is one of two C files needed to create a demo that loads
**     data through direct path api.
**
**     To build and run the demo, please read directions located in
**     the header section of the cdemdp*.c modules.
**
**
**
**   MODIFIED   (MM/DD/YY)
**   eegolf      02/28/05 - Conditionalize for OpenVMS
**   eegolf      03/30/04 - Update simple_load routine to tkpidrv.c version
**   rphillip    03/12/04 - Delete flush row and always load after cvt error 
**   cmlim       06/11/02 - do not core dump if a null tbl name is given
**   msakayed    11/02/01 - Bug #2094292: add/set OCI_ATTR_DIRPATH_INPUT
**   eegolf      03/04/01 - Updated for 9i
**   cmlim       09/16/98 - Creation (abrumm 04/07/98)
*/

#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <fcntl.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <oratypes.h>
#include <oci.h>
#include <cdemodp0.h>
#include <cdemodp.h>

#ifdef VMS
#include <unistd.h>
#endif

#ifndef bit
# define bit(x, y) ((x) & (y))
#endif

#ifndef OER
# define OER(x) (x)
#endif

struct loadctl
{
  ub4                 nrow_ctl;            /* number of rows in column array */
  ub2                 ncol_ctl;         /* number of columns in column array */
  OCIEnv             *envhp_ctl;                       /* environment handle */
  OCIServer          *srvhp_ctl;                            /* server handle */
  OCIError           *errhp_ctl;                             /* error handle */
  OCIError           *errhp2_ctl;                /* yet another error handle */
  OCISvcCtx          *svchp_ctl;                          /* service context */
  OCISession         *authp_ctl;                   /* authentication context */
  OCIParam           *colLstDesc_ctl;        /* column list parameter handle */
  OCIDirPathCtx      *dpctx_ctl;                      /* direct path context */
  OCIDirPathColArray *dpca_ctl;           /* direct path column array handle */
  OCIDirPathColArray *dpobjca_ctl;          /* dp column array handle for obj*/
  OCIDirPathColArray *dpnestedobjca_ctl;  /* dp col array hndl for nested obj*/
  OCIDirPathStream   *dpstr_ctl;                /* direct path stream handle */
  ub1                *buf_ctl;    /* pre-alloc'd buffer for out-of-line data */
  ub4                 bufsz_ctl;                 /* size of buf_ctl in bytes */
  ub4                 bufoff_ctl;                     /* offset into buf_ctl */
  ub4                *otor_ctl;                  /* Offset to Recnum mapping */
  ub1                *inbuf_ctl;                 /* buffer for input records */
  struct pctx         pctx_ctl;                     /* partial field context */
  boolean             loadobjcol_ctl;             /* load to obj col(s)? T/F */
};

/* Forward references: */

STATICF void  field_flush(/*_ struct loadctl *ctlp, ub4 rowoff _*/);

STATICF sword field_set(/*_ struct loadctl *ctlp, struct tbl *tblp,
                     struct obj *objp, text *recp, ub4 rowoff, ub1 bufflg _*/);

STATICF void init_obj_load(/*_ struct loadctl *ctlp, struct tbl *tblp,
                              struct obj *objp _*/);
STATICF void alloc_obj_ca(/*_ struct loadctl *ctlp, struct tbl *tblp,
                              struct obj *objp _*/);
STATICF void  init_load(/*_ struct loadctl *ctl, struct tbl *table,
                            struct sess *session _*/);

STATICF void simple_load(/*_ struct loadctl *ctlp, struct tbl *tblp,
                              struct sess *session, FILE *inputfp _*/);

STATICF void  finish_load(/*_ struct loadctl *ctl _*/);

STATICF void  errprint(/*_ dvoid *errhp, ub4 htype, sb4 *errcodep _*/);

STATICF void  checkerr(/*_ dvoid *errhp, ub4 htype, sword status,
                           text *note, sb4 state, text *file, sb4 line _*/);
STATICF void  cleanup(/*_ struct loadctl *ctlp, sb4 ex_status _*/);
STATICF sword do_convert(/*_ struct loadctl *ctlp, ub4 startoff, ub4 rowcnt, 
                             ub4 *cvtCntp, ub2 *badcoffp _*/);
STATICF sword do_load(/*_ struct loadctl *ctlp, ub4 *loadCntp _*/);
STATICF int           main(/*_ int argc, char *argv[] _*/);
STATICF void  free_obj_hndls(struct loadctl *ctlp, struct obj *objp);
STATICF void  set_and_get_attributes(struct loadctl *ctlp, struct tbl *tblp);
STATICF void reset_obj_ca(/*_ struct loadctl *ctlp, struct tbl *tblp, struct obj *objp _*/);


/* OCI_CHECK(errhp, ub4 errhptype, sb4 status, struct loadctl *ctlp,
 *          OCIfunction());
 * errhp is typically a (OCIError *), and errhptype is OCI_HTYPE_ERROR.
 * errhp in some cases may be an (OCIEnv *), and errhptype is OCI_HTYPE_ENV.
 */
#define OCI_CHECK(errhp, htype, status, ctlp, OCIfunc) \
if (OCI_SUCCESS != ((status) = (OCIfunc))) \
{ \
  checkerr((dvoid *)(errhp), (ub4)(htype), (sword)(status), (text *)0, \
           (sb4)0, (text *)__FILE__, (sb4)__LINE__); \
  if ((status) != OCI_SUCCESS_WITH_INFO) \
    cleanup((struct loadctl *)ctlp, (sb4)1); \
} else

#define CHECKERR(errhp, htype, status) \
  checkerr((dvoid *)errhp, (ub4)(htype), (sword)(status), (text *)0, \
           (sb4)0, (text *)__FILE__, (sb4)__LINE__);

#define FATAL(note, state) \
do \
{ \
  checkerr((dvoid *)0, (ub4)OCI_HTYPE_ERROR, (sword)OCI_SUCCESS,           \
           (text *)(note), (sb4)(state), (text *)__FILE__, (sb4)__LINE__); \
  cleanup((ctlp), (sb4)2); \
} while (0)

/* External references: */
externref struct tbl    table;
externref struct sess    session;

/* External definitions: */
externdef FILE         *output_fp;                         /* for error msgs */



/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*                               main                                      */
/*                                                                         */
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


int main(argc, argv)
int argc;
char *argv[];
{
  sword   ociret;
  struct  loadctl  ctl;
  struct  loadctl *ctlp = &ctl;

  output_fp = (session.outfn_sess) ? fopen((char *)session.outfn_sess, "w")
                                   : stderr;

  memset((dvoid *)ctlp, 0, sizeof(struct loadctl));

  /* set up OCI environment and connect to the ORACLE server */

  OCI_CHECK((dvoid *)0, (ub4)0, ociret, ctlp,
            OCIInitialize((ub4) OCI_DEFAULT, (dvoid *)0,
                          (dvoid * (*)(dvoid *, size_t)) 0,
                          (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                          (void (*)(dvoid *, dvoid *)) 0 ));

  OCI_CHECK(ctlp->envhp_ctl, OCI_HTYPE_ENV, ociret, ctlp,
            OCIEnvInit((OCIEnv **)&ctlp->envhp_ctl, OCI_DEFAULT, (size_t)0,
                       (dvoid **)0));

  /* allocate error handles */
  OCI_CHECK(ctlp->envhp_ctl, OCI_HTYPE_ENV, ociret, ctlp,
            OCIHandleAlloc((dvoid *)ctlp->envhp_ctl,
                           (dvoid **)&ctlp->errhp_ctl, OCI_HTYPE_ERROR,
                           (size_t)0, (dvoid **)0));
  OCI_CHECK(ctlp->envhp_ctl, OCI_HTYPE_ENV, ociret, ctlp,
            OCIHandleAlloc((dvoid *)ctlp->envhp_ctl,
                           (dvoid **)&ctlp->errhp2_ctl, OCI_HTYPE_ERROR,
                           (size_t)0, (dvoid **)0));

  /* server contexts */
  OCI_CHECK(ctlp->envhp_ctl, OCI_HTYPE_ENV, ociret, ctlp,
            OCIHandleAlloc((dvoid *)ctlp->envhp_ctl,
                           (dvoid **)&ctlp->srvhp_ctl, OCI_HTYPE_SERVER,
                           (size_t)0, (dvoid **)0));

  OCI_CHECK(ctlp->envhp_ctl, OCI_HTYPE_ENV, ociret, ctlp,
            OCIHandleAlloc((dvoid *)ctlp->envhp_ctl,
                           (dvoid **)&ctlp->svchp_ctl, OCI_HTYPE_SVCCTX,
                           (size_t)0, (dvoid **)0));

  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIServerAttach(ctlp->srvhp_ctl, ctlp->errhp_ctl,
                            session.inst_sess,
                            (sb4)strlen((const char *)session.inst_sess),
                            OCI_DEFAULT));

  /* set attribute server context in the service context */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)ctlp->svchp_ctl, OCI_HTYPE_SVCCTX,
                       (dvoid *)ctlp->srvhp_ctl, (ub4)0, OCI_ATTR_SERVER,
                       ctlp->errhp_ctl));

  OCI_CHECK(ctlp->envhp_ctl, OCI_HTYPE_ENV, ociret, ctlp,
            OCIHandleAlloc((dvoid *)ctlp->envhp_ctl,
                           (dvoid **)&ctlp->authp_ctl, (ub4)OCI_HTYPE_SESSION,
                           (size_t)0, (dvoid **)0));

  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)ctlp->authp_ctl, (ub4)OCI_HTYPE_SESSION,
                       (dvoid *)session.username_sess,
                       (ub4)strlen((char *)session.username_sess),
                       (ub4)OCI_ATTR_USERNAME, ctlp->errhp_ctl));

  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)ctlp->authp_ctl, (ub4)OCI_HTYPE_SESSION,
                       (dvoid *)session.password_sess,
                       (ub4)strlen((char *)session.password_sess),
                       (ub4)OCI_ATTR_PASSWORD, ctlp->errhp_ctl));

  /* begin a session */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCISessionBegin(ctlp->svchp_ctl, ctlp->errhp_ctl, ctlp->authp_ctl,
                            OCI_CRED_RDBMS, (ub4)OCI_DEFAULT));

  /* set authentication context into service context */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)ctlp->svchp_ctl, (ub4)OCI_HTYPE_SVCCTX,
                       (dvoid *)ctlp->authp_ctl, (ub4)0, (ub4)OCI_ATTR_SESSION,
                       ctlp->errhp_ctl));

  init_load(ctlp, &table, &session);                  /* initialize the load */
  simple_load(ctlp, &table, &session, stdin);                   /* load data */
  finish_load(ctlp);                                      /* finish the load */

  cleanup(ctlp, (sb4)0);
  /* NOTREACHED */

return 1;
}


/*
**++++++++++++++++++++++++++++++ alloc_obj_ca +++++++++++++++++++++++++++++++++
**
**  Description:
**
** Function allocates the column arrays for any objects or nested object columns.
**
**  Assumptions:
**
**  Parameters:
**
**  ctlp                           load control structure pointer 
**  tblp                           table pointer  
**  objp                           object pointer 
**
**  Returns:
**
**-------------------------------------------------------------------------
*/

STATICF void alloc_obj_ca(ctlp, tblp, objp)
struct loadctl *ctlp;               /* load control structure pointer */
struct tbl     *tblp;               /* table pointer   */ 
struct obj     *objp;               /* object pointer */
{
  struct col   *colp;
  sword         ociret;             /* return code from OCI calls*/
  ub2 i;

  /*
   * Allocate a separate column array for the column object
   */
  OCI_CHECK(ctlp->envhp_ctl, OCI_HTYPE_ENV, ociret, ctlp,
            OCIHandleAlloc((dvoid *)(objp->ctx_obj),
                           (dvoid **)&(objp->ca_obj),
                           (ub4)OCI_HTYPE_DIRPATH_FN_COL_ARRAY,
                           (size_t)0, (dvoid **)0));

  /* get number of rows in the column array just allocated */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)(objp->ca_obj),
                       OCI_HTYPE_DIRPATH_FN_COL_ARRAY,
                       (dvoid *)(&objp->nrows_obj), (ub4 *)0,
                       OCI_ATTR_NUM_ROWS, ctlp->errhp_ctl));

  /* get number of columns in the column array just allocated */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)(objp->ca_obj),
                       OCI_HTYPE_DIRPATH_FN_COL_ARRAY,
                       (dvoid *)(&objp->ncol_obj), (ub4 *)0,
                       OCI_ATTR_NUM_COLS, ctlp->errhp_ctl));

  /*
   * If there are fewer rows in the object column array than in them top-level,
   * one, only use as many rows in the other column array. This will happen
   * when the object requires more space than all of the other columns inits
   * parent table. This simplifies the loop for loading the column arrays
   * so that we only have to worry about when we've filled the top-level
   * column array.
   */
  if (objp->nrows_obj < ctlp->nrow_ctl)
  {
    ctlp->nrow_ctl = objp->nrows_obj;
  }

  /* check each column to see if it is an object, opaque or ref */
  /* and if so, recurse */
  for (i = 0, colp = objp->col_obj; i < objp->ncol_obj; i++, colp++)
  {
    if (colp->exttyp_col == SQLT_NTY || colp->exttyp_col == SQLT_REF)
    {
      alloc_obj_ca(ctlp, tblp, colp->obj_col);
    }
  }

}


/*
**++++++++++++++++++++++++++++++ init_obj_load +++++++++++++++++++++++++++++++++
**
**  Description:
**
**   Function which prepares the load of an object column. This should only
**   be called from init_load or recursively.
**
**  Assumptions:
**
**  Parameters:
**
**  ctlp                           load control structure pointer 
**  tblp                           table pointer   
**  objp                           object pointer 
**
**  Returns:
**
**-------------------------------------------------------------------------
*/

STATICF void init_obj_load(ctlp, tblp, objp)
struct loadctl *ctlp;
struct tbl     *tblp;
struct obj     *objp;
{
  struct  col   *colp;
  struct  fld   *fldp;
  sword          ociret;                       /* return code from OCI calls*/
  ub2            i;
  ub4            pos;
  ub2            numCols;
  ub4            len;
  ub2            type;
  ub1            exprtype;
  ub1            parmtyp;
  OCIParam      *colDesc;                     /* column parameter descriptor*/
  OCIParam      *objColLstDesc;               /* obj col's list param handle*/

  /*
   * create a context for this object type and describe the attributes
   * that will be loaded for this object.
   */
  OCI_CHECK(ctlp->envhp_ctl, OCI_HTYPE_ENV, ociret, ctlp,
            OCIHandleAlloc((dvoid *)ctlp->dpctx_ctl,
                           (dvoid **)&(objp->ctx_obj),
                           (ub4)OCI_HTYPE_DIRPATH_FN_CTX,
                           (size_t)0, (dvoid **)0));

  /* If col is an obj, then its constructor is the type name. (req.)
   * If col is an opaque/sql function, then use the expression given. (req.)
   * If col is a ref, then can set a fixed tbl name.  (optional) 
   */

 if (objp->name_obj)             /* if expression is available */
 {
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)(objp->ctx_obj),
                       (ub4)OCI_HTYPE_DIRPATH_FN_CTX,
                       (dvoid *) objp->name_obj,
                       (ub4)strlen((const char *) objp->name_obj),
                       (ub4)OCI_ATTR_NAME, ctlp->errhp_ctl));

  /* Set the expression type to obj constructor, opaque/sql function, or ref
   * table name.
   */
  if (bit(objp->flag_obj, OBJ_OBJ))
    exprtype = OCI_DIRPATH_EXPR_OBJ_CONSTR;  /* expr is an obj constructor */
  else if (bit(objp->flag_obj, OBJ_OPQ))
    exprtype = OCI_DIRPATH_EXPR_SQL;         /* expr is an opaque/sql func */
  else if (bit(objp->flag_obj, OBJ_REF))
    exprtype = OCI_DIRPATH_EXPR_REF_TBLNAME;  /* expr is a ref table name  */

  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)(objp->ctx_obj),
                       (ub4)OCI_HTYPE_DIRPATH_FN_CTX,
                       (dvoid *) &exprtype,
                       (ub4) 0,
                       (ub4)OCI_ATTR_DIRPATH_EXPR_TYPE, ctlp->errhp_ctl));
 }

  /* set number of columns to be loaded */
  numCols = objp->ncol_obj;

  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)(objp->ctx_obj),
                       (ub4)OCI_HTYPE_DIRPATH_FN_CTX,
                       (dvoid *)&numCols,
                       (ub4)0, (ub4)OCI_ATTR_NUM_COLS, ctlp->errhp_ctl));

  /* get the column parameter list */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((dvoid *)(objp->ctx_obj),
                       OCI_HTYPE_DIRPATH_FN_CTX,
                       (dvoid *)&objColLstDesc, (ub4 *)0,
                       OCI_ATTR_LIST_COLUMNS, ctlp->errhp_ctl));

  /* get attributes of the column parameter list */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)objColLstDesc,
                       OCI_DTYPE_PARAM,
                       (dvoid *)&parmtyp, (ub4 *)0,
                       OCI_ATTR_PTYPE, ctlp->errhp_ctl));

  if (parmtyp != OCI_PTYPE_LIST)
  {
    fprintf(output_fp,
            "ERROR: expected parmtyp of OCI_PTYPE_LIST, got %d\n",
            (int)parmtyp);
  }

  /* Now set the attributes of each column by getting a parameter
   * handle on each column, then setting attributes on the parameter
   * handle for the column.
   * Note that positions within a column list descriptor are 1-based.
   */
  for (i = 0, pos = 1, colp = objp->col_obj, fldp = objp->fld_obj;
       i < objp->ncol_obj;
       i++, pos++, colp++, fldp++)
  {
    /* get parameter handle on the column */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIParamGet((CONST dvoid *)objColLstDesc,
                          (ub4)OCI_DTYPE_PARAM, ctlp->errhp_ctl,
                          (dvoid **)&colDesc, pos));

    colp->id_col = i;                            /* position in column array*/

    /* set external attributes on the column */

    /* column name */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                         (dvoid *)colp->name_col,
                         (ub4)strlen((const char *)colp->name_col),
                         (ub4)OCI_ATTR_NAME, ctlp->errhp_ctl));

    /* column type */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                         (dvoid *)&colp->exttyp_col, (ub4)0,
                         (ub4)OCI_ATTR_DATA_TYPE, ctlp->errhp_ctl));

    /* max data size */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                         (dvoid *)&fldp->maxlen_fld, (ub4)0,
                         (ub4)OCI_ATTR_DATA_SIZE, ctlp->errhp_ctl));

    /* If column is chrdate or date, set column (input field) date mask
     * to trigger client library to check string for a valid date.
     * Note: OCIAttrSet() may be called here w/ a null ptr or null string.
     */
    if (colp->date_col)
    {
      OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
                OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                         (dvoid *)colp->datemask_col,
                         (colp->datemask_col) ?
                           (ub4)strlen((const char *)colp->datemask_col) :0,
                         (ub4)OCI_ATTR_DATEFORMAT, ctlp->errhp_ctl));
    }

    if (colp->prec_col)
    {
      OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
                OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                         (dvoid *)&colp->prec_col, (ub4)0,
                         (ub4)OCI_ATTR_PRECISION, ctlp->errhp_ctl));
    }

    if (colp->scale_col)
    {
      OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
                OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                         (dvoid *)&colp->scale_col, (ub4)0,
                         (ub4)OCI_ATTR_SCALE, ctlp->errhp_ctl));
    }

    if (colp->csid_col)
    {
      OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
                OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                         (dvoid *)&colp->csid_col, (ub4)0,
                         (ub4)OCI_ATTR_CHARSET_ID, ctlp->errhp_ctl));
    }

    /* If this is an object, opaque or ref then recurse */
    if (colp->exttyp_col == SQLT_NTY || colp->exttyp_col == SQLT_REF)
    {
      init_obj_load(ctlp, tblp, colp->obj_col);

      /* set the object function context into the param handle */
      OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
                OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                           (dvoid *)(colp->obj_col->ctx_obj), (ub4)0,
                           (ub4)OCI_ATTR_DIRPATH_FN_CTX, ctlp->errhp_ctl));
    }

    /* free the parameter handle to the column descriptor */
    OCI_CHECK((dvoid *)0, 0, ociret, ctlp,
              OCIDescriptorFree((dvoid *)colDesc, OCI_DTYPE_PARAM));
  }

}


/*
**++++++++++++++++++++++++++++++ init_load +++++++++++++++++++++++++++++++++
**
**  Description:
**
**   Function which prepares for a direct path load using the direct
**   path API on the table described by 'tblp'.
**
**  Assumptions:
**
**  The loadctl structure given by 'ctlp' has an appropriately initialized
**  environment, and service context handles (already connected to
**  the server) prior to calling this function.
**
**  Parameters:
**
**  ctlp                           load control structure pointer
**  tblp                           table pointer 
**  sessp                          session pointer
**
**  Returns:
**
**-------------------------------------------------------------------------
*/

STATICF void init_load(ctlp, tblp, sessp)
struct loadctl *ctlp;                      /* load control structure pointer */
struct tbl     *tblp;                      /* table pointer   */
struct sess    *sessp;                     /* session pointer */
{
  struct  col   *colp;
  struct  fld   *fldp;
  sword         ociret;                    /* return code from OCI calls */
  OCIDirPathCtx *dpctx;                    /* direct path context */
  OCIParam      *objAttrDesc;              /* attribute parameter descriptor */
  OCIParam      *colDesc;                  /* column parameter descriptor */
  ub1            parmtyp;
  ub1           *timestamp = (ub1 *)0;
  ub4            size;
  ub2            i;
  ub4            pos;
  ub1            dirpathinput = OCI_DIRPATH_INPUT_TEXT;

  /* allocate and initialize a direct path context */
  OCI_CHECK(ctlp->envhp_ctl, OCI_HTYPE_ENV, ociret, ctlp,
            OCIHandleAlloc((dvoid *)ctlp->envhp_ctl,
                           (dvoid **)&ctlp->dpctx_ctl,
                           (ub4)OCI_HTYPE_DIRPATH_CTX,
                           (size_t)0, (dvoid **)0));

  dpctx = ctlp->dpctx_ctl;                                      /* shorthand*/

  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)dpctx, (ub4)OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)tblp->name_tbl, 
                       (ub4)0,
                       (ub4)OCI_ATTR_NAME, ctlp->errhp_ctl));

  if (tblp->subname_tbl && *tblp->subname_tbl)    /* set (sub)partition name*/
  {
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)dpctx, (ub4)OCI_HTYPE_DIRPATH_CTX,
                         (dvoid *)tblp->subname_tbl,
                         (ub4)strlen((const char *)tblp->subname_tbl),
                         (ub4)OCI_ATTR_SUB_NAME, ctlp->errhp_ctl));
  }

  if (tblp->owner_tbl)                            /* set schema (owner) name*/
  {
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)dpctx, (ub4)OCI_HTYPE_DIRPATH_CTX,
                         (dvoid *)tblp->owner_tbl,
                         (ub4)strlen((const char *)tblp->owner_tbl),
                         (ub4)OCI_ATTR_SCHEMA_NAME, ctlp->errhp_ctl));
  }

  /* Note: setting tbl default datemask will not trigger client library
   * to check strings for dates - only setting column datemask will.
   */
  if (tblp->dfltdatemask_tbl)
  {
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)dpctx, (ub4)OCI_HTYPE_DIRPATH_CTX,
                         (dvoid *)tblp->dfltdatemask_tbl,
                         (ub4)strlen((const char *)tblp->dfltdatemask_tbl),
                         (ub4)OCI_ATTR_DATEFORMAT, ctlp->errhp_ctl));
  }

  /* set the data input type to be text */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet ((dvoid *)dpctx, OCI_HTYPE_DIRPATH_CTX,
                        (dvoid *)&dirpathinput, (ub4)0,
                        OCI_ATTR_DIRPATH_INPUT, ctlp->errhp_ctl));

  if (tblp->parallel_tbl)                 /* set table level parallel option*/
  {
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)dpctx, (ub4)OCI_HTYPE_DIRPATH_CTX,
                         (dvoid *)&tblp->parallel_tbl,
                         (ub4)0, (ub4)OCI_ATTR_DIRPATH_PARALLEL,
                         ctlp->errhp_ctl));
  }

  if (tblp->nolog_tbl)                       /* set table level nolog option*/
  {
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)dpctx, (ub4)OCI_HTYPE_DIRPATH_CTX,
                         (dvoid *)&tblp->nolog_tbl, (ub4)0,
                         (ub4)OCI_ATTR_DIRPATH_NOLOG, ctlp->errhp_ctl));
  }

  if (tblp->objconstr_tbl)         /* set obj type of tbl to load if exists */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)dpctx,
                         (ub4)OCI_HTYPE_DIRPATH_CTX,
                         (dvoid *) tblp->objconstr_tbl,
                         (ub4)strlen((const char *) tblp->objconstr_tbl),
                         (ub4)OCI_ATTR_DIRPATH_OBJ_CONSTR, ctlp->errhp_ctl));

  /* set number of columns to be loaded */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)dpctx, (ub4)OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&tblp->ncol_tbl,
                       (ub4)0, (ub4)OCI_ATTR_NUM_COLS, ctlp->errhp_ctl));

  /* get the column parameter list */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((dvoid *)dpctx,
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&ctlp->colLstDesc_ctl, (ub4 *)0,
                       OCI_ATTR_LIST_COLUMNS, ctlp->errhp_ctl));

  /* get attributes of the column parameter list */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)ctlp->colLstDesc_ctl,
                       OCI_DTYPE_PARAM,
                       (dvoid *)&parmtyp, (ub4 *)0,
                       OCI_ATTR_PTYPE, ctlp->errhp_ctl));

  if (parmtyp != OCI_PTYPE_LIST)
  {
    fprintf(output_fp, "ERROR: expected parmtyp of OCI_PTYPE_LIST, got%d\n",
            (int)parmtyp);
  }

  /* Now set the attributes of each column by getting a parameter
   * handle on each column, then setting attributes on the parameter
   * handle for the column.
   * Note that positions within a column list descriptor are 1-based.
   */
  for (i = 0, pos = 1, colp = tblp->col_tbl, fldp = tblp->fld_tbl;
       i < tblp->ncol_tbl;
       i++, pos++, colp++, fldp++)
  {
    /* get parameter handle on the column */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIParamGet((CONST dvoid *)ctlp->colLstDesc_ctl,
                          (ub4)OCI_DTYPE_PARAM, ctlp->errhp_ctl,
                          (dvoid **)&colDesc, pos));

    colp->id_col = i;                            /* position in column array*/

    /* set external attributes on the column */

    /* column name */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                         (dvoid *)colp->name_col,
                         (ub4)strlen((const char *)colp->name_col),
                         (ub4)OCI_ATTR_NAME, ctlp->errhp_ctl));

    /* column type */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                         (dvoid *)&colp->exttyp_col, (ub4)0,
                         (ub4)OCI_ATTR_DATA_TYPE, ctlp->errhp_ctl));

    /* max data size */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                         (dvoid *)&fldp->maxlen_fld, (ub4)0,
                         (ub4)OCI_ATTR_DATA_SIZE, ctlp->errhp_ctl));

    /* If column is chrdate or date, set column (input field) date mask
     * to trigger client library to check string for a valid date.
     * Note: OCIAttrSet() may be called here w/ a null ptr or null string.
     */
    if (colp->date_col)
    {
      OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
                OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                         (dvoid *)colp->datemask_col,
                         (colp->datemask_col) ?
                           (ub4)strlen((const char *)colp->datemask_col) :0,
                         (ub4)OCI_ATTR_DATEFORMAT, ctlp->errhp_ctl));
    }

    if (colp->prec_col)
    {
      OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
                OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                         (dvoid *)&colp->prec_col, (ub4)0,
                         (ub4)OCI_ATTR_PRECISION, ctlp->errhp_ctl));
    }

    if (colp->scale_col)
    {
      OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
                OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                         (dvoid *)&colp->scale_col, (ub4)0,
                         (ub4)OCI_ATTR_SCALE, ctlp->errhp_ctl));
    }

    if (colp->csid_col)
    {
      OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
                OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                         (dvoid *)&colp->csid_col, (ub4)0,
                         (ub4)OCI_ATTR_CHARSET_ID, ctlp->errhp_ctl));
    }

    if (bit(colp->flag_col, COL_OID))
    {
      ub1 flg = 1;
      OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
                OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                         (dvoid *)&flg, (ub4)0,
                         (ub4)OCI_ATTR_DIRPATH_OID, ctlp->errhp_ctl));
    }

    /* If this is an object, opaque or ref then call init_obj_load */
    /* to handle it. */
    if (colp->exttyp_col == SQLT_NTY || colp->exttyp_col == SQLT_REF)
    {
      init_obj_load(ctlp, tblp, colp->obj_col);

      /* set the object function context into the param handle */
      OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
                OCIAttrSet((dvoid *)colDesc, (ub4)OCI_DTYPE_PARAM,
                           (dvoid *)(colp->obj_col->ctx_obj), (ub4)0,
                           (ub4)OCI_ATTR_DIRPATH_FN_CTX, ctlp->errhp_ctl));

    }


    /* free the parameter handle to the column descriptor */
    OCI_CHECK((dvoid *)0, 0, ociret, ctlp,
              OCIDescriptorFree((dvoid *)colDesc, OCI_DTYPE_PARAM));
  }

  /* read back some of the attributes for purpose of illustration */
  for (i = 0, pos = 1, colp = tblp->col_tbl, fldp = tblp->fld_tbl;
       i < tblp->ncol_tbl;
       i++, pos++, colp++, fldp++)
  {
    text *s;
    ub4   slen;
    ub4   maxdsz;
    ub2   dty;

    /* get parameter handle on the column */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIParamGet((CONST dvoid *)ctlp->colLstDesc_ctl,
                          (ub4)OCI_DTYPE_PARAM, ctlp->errhp_ctl,
                          (dvoid **)&colDesc, pos));


    /* get column name */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrGet((dvoid *)colDesc, 
                         OCI_DTYPE_PARAM,
                         (dvoid *)&s, (ub4 *)&slen,
                         OCI_ATTR_NAME, ctlp->errhp_ctl));

    /* check string length */
    if (slen != (ub4)strlen((const char *)colp->name_col))
    {
      fprintf(output_fp,
        "*** ERROR *** bad col name len in column parameter\n");
      fprintf(output_fp, "\texpected %d, got %d\n",
             (int)strlen((const char *)colp->name_col), (int)slen);
    }

    if (strncmp((const char *)s, (const char *)colp->name_col, (size_t)slen))
    {
      fprintf(output_fp,"*** ERROR *** bad column name in column parameter\n");
      fprintf(output_fp, "\texpected %s, got %s\n",
              (char *)colp->name_col, (char *)s);
    }

    /* get column type */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrGet((dvoid *)colDesc, 
                         OCI_DTYPE_PARAM,
                         (dvoid *)&dty, (ub4 *)0,
                         OCI_ATTR_DATA_TYPE, ctlp->errhp_ctl));
    if (dty != colp->exttyp_col)
    {
      fprintf(output_fp, "*** ERROR *** bad OCI_ATTR_DATA_TYPE in col param\n");
      fprintf(output_fp, "\tColumn name %s\n", colp->name_col);
      fprintf(output_fp, "\t\texpected %d, got %d\n",
              (int)colp->exttyp_col, (int)dty);
    }

    /* get the max data size */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrGet((dvoid *)colDesc, 
                         OCI_DTYPE_PARAM,
                         (dvoid *)&maxdsz, (ub4 *)0,
                         OCI_ATTR_DATA_SIZE, ctlp->errhp_ctl));
    if (maxdsz != fldp->maxlen_fld)
    {
      fprintf(output_fp, "*** ERROR *** bad OCI_ATTR_DATA_SIZE in col param\n");
      fprintf(output_fp, "\tColumn name %s\n", colp->name_col);
      fprintf(output_fp, "\t\texpected %d, got %d\n",
              (int)fldp->maxlen_fld, (int)maxdsz);
    }

    /* free the parameter handle to the column descriptor */
    OCI_CHECK((dvoid *)0, 0, ociret, ctlp,
              OCIDescriptorFree((dvoid *)colDesc, OCI_DTYPE_PARAM));
  }

  {
    char *vbuf;

    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)dpctx, (ub4)OCI_HTYPE_DIRPATH_CTX,
                         (dvoid *)&tblp->xfrsz_tbl,
                         (ub4)0, (ub4)OCI_ATTR_BUF_SIZE, ctlp->errhp_ctl));

    /* minimize read system calls */
    vbuf = (char *)malloc((size_t)tblp->xfrsz_tbl);
    if (vbuf != (char *)0)
      (void)setvbuf(stdin, vbuf, _IOFBF, (size_t)tblp->xfrsz_tbl);
  }

  /* prepare the load */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIDirPathPrepare(dpctx, ctlp->svchp_ctl, ctlp->errhp_ctl));

  /* Allocate column array and stream handles.
   * Note that for the column array and stream handles
   * the parent handle is the direct path context.
   * Also note that Oracle errors are returned via the
   * environment handle associated with the direct path context.
   */
  OCI_CHECK(ctlp->envhp_ctl, OCI_HTYPE_ENV, ociret, ctlp,
            OCIHandleAlloc((dvoid *)ctlp->dpctx_ctl, (dvoid**)&ctlp->dpca_ctl,
                           (ub4)OCI_HTYPE_DIRPATH_COLUMN_ARRAY,
                           (size_t)0, (dvoid **)0));

  OCI_CHECK(ctlp->envhp_ctl, OCI_HTYPE_ENV, ociret, ctlp,
            OCIHandleAlloc((dvoid *)ctlp->dpctx_ctl,(dvoid**)&ctlp->dpstr_ctl,
                           (ub4)OCI_HTYPE_DIRPATH_STREAM,
                           (size_t)0, (dvoid **)0));

  /* get number of rows in the column array just allocated */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)(ctlp->dpca_ctl),
                       OCI_HTYPE_DIRPATH_COLUMN_ARRAY,
                       (dvoid *)(&ctlp->nrow_ctl), (ub4 *)0,
                       OCI_ATTR_NUM_ROWS, ctlp->errhp_ctl));

  /* get number of columns in the column array just allocated */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)(ctlp->dpca_ctl),
                       OCI_HTYPE_DIRPATH_COLUMN_ARRAY,
                       (dvoid *)(&ctlp->ncol_ctl), (ub4 *)0,
                       OCI_ATTR_NUM_COLS, ctlp->errhp_ctl));

  /* allocate the column arrays for any column objects, opaques or refs */
  for (i = 0, colp = tblp->col_tbl; i < tblp->ncol_tbl; i++, colp++)
  {
    if (colp->exttyp_col == SQLT_NTY || colp->exttyp_col == SQLT_REF)
    {
      alloc_obj_ca(ctlp, tblp, colp->obj_col);
    }
  }

  /* allocate buffer for input records */
  ctlp->inbuf_ctl = (ub1 *)malloc(ctlp->nrow_ctl * sessp->maxreclen_sess);
  if (ctlp->inbuf_ctl == (ub1 *)0)
  {
    perror("malloc");
    FATAL("init_load:malloc:inbuf_ctl alloc failure",
          ctlp->nrow_ctl * sessp->maxreclen_sess);
  }

  /* allocate Offset-TO-Record number mapping array */
  ctlp->otor_ctl = (ub4 *)malloc(ctlp->nrow_ctl * sizeof(ub4));
  if (ctlp->otor_ctl == (ub4 *)0)
  {
    perror("malloc");
    FATAL("init_load:malloc:otor_ctl alloc failure",
          ctlp->nrow_ctl * sizeof(ub4));
  }

  CLEAR_PCTX(ctlp->pctx_ctl);                  /* initialize partial context*/

/*
  fprintf(output_fp, "init_load: %ld column array rows\n",
          (long)ctlp->nrow_ctl);*/

  return;

}


/*
**++++++++++++++++++++++++++++++ simple_load +++++++++++++++++++++++++++++++++
**
**  Description:
**
** This function reads input records from 'inputfp', parses the input
** records into fields according to the field description given by
** tblp->fld_tbl, and loads the data into the database.
**
** LOBs can be loaded with this function in a piecewise manner.  This
** function is written as a state machine, which cycles through the
** following states:
**   RESET, GET_RECORD, FIELD_SET, DO_CONVERT, DO_LOAD, END_OF_INPUT
**
** The normal case of all scalar data, where multiple records fit
** entirely in memory, cycles through the following states:
**   RESET, [[GET_RECORD, FIELD_SET]+, DO_CONVERT, DO_LOAD]+, RESET
**
** The case of loading one or more LOB columns, which do not fit entirely
** in memory, has the following state transitions:
**   RESET, GET_RECORD, [FIELD_SET, DO_CONVERT, DO_LOAD]+, RESET
** Note: The second and subsequent transitions to the FIELD_SET
** state have a partial record context.
**
** A mapping of column array offset to input record number (otor_ctl[])
** is maintained by this function for error reporting and recovery.
**
**  Assumptions:
**
**  Parameters:
**
**  ctlp                           load control structure pointer 
**  tblp                           table pointer   
**  sessp                          session pointer 
**  inputfp                        input file pointer 
**
**  Returns:
**
**-------------------------------------------------------------------------
*/

STATICF void simple_load(ctlp, tblp, sessp, inputfp)
struct loadctl *ctlp;                /* load control structure pointer */
struct tbl     *tblp;                /* table pointer   */
struct sess    *sessp;               /* session pointer */
FILE           *inputfp;             /* input file pointer */
{
  sword  fsetrv;                  /* return value from field_set */
  sword  cvtrv;                   /* return value from do_convert */
  sword  ldrv;                    /* return value from do_load */
  ub4    startoff;                /* starting row offset for conversion */
  ub4    nxtLoadOff;              /* column array offset to be loaded next */
  ub4    rowCnt;                  /* count of rows populated in column array */
  ub4    cvtCnt;                  /* count of rows converted */
  ub4    lastoff;                 /* last row offset used in column array */
  sword  state;                   /* current state machine state */
  sword  done;                    /* set to TRUE when load is complete */
  ub4    input_recnum;            /* current input record number */
  ub4    load_recnum;   /* record number corresponding to last record loaded */
  ub4    err_recnum;              /* record number corresponding to error */
  text   *recp;
  ub4    cvtcontcnt;              /* # of times CONVERT_CONTINUE returned */
  sword  ociResetErr;             /* stream reset return status */
  sword  cvtErrorRowNeedsLoad;    /* row had conversion error, needs load */

  /* set initial state */
  input_recnum = 0;
  load_recnum  = UB4MAXVAL;
  err_recnum   = 0;
  state        = RESET;
  fsetrv       = FIELD_SET_COMPLETE;
  cvtrv        = CONVERT_SUCCESS;
  ldrv         = LOAD_SUCCESS;
  done         = FALSE;
  cvtcontcnt   = 0;
  cvtErrorRowNeedsLoad = FALSE;

  while (!done)
  {
    switch (state)
    {
    case RESET:    /* Reset column array and direct stream state to be empty*/
    {
      ub2 i;
      struct col   *colp;
      startoff   = 0;             /* reset starting offset into column array*/
      lastoff    = 0;                      /* last entry set of column array*/
      rowCnt     = 0;                  /* count of rows partial and complete*/
      cvtCnt     = 0;                             /* count of converted rows*/
      nxtLoadOff = 0;

      /* Reset column array state in case a previous conversion needed
       * to be continued, or a row is expecting more data.
       */
      (void) OCIDirPathColArrayReset(ctlp->dpca_ctl, ctlp->errhp_ctl);
      /* reset the column arrays for any column objects, opaques or refs */
      for (i = 0, colp = tblp->col_tbl; i < tblp->ncol_tbl; i++, colp++)
      {
        if (colp->exttyp_col == SQLT_NTY || colp->exttyp_col == SQLT_REF)
        {
          reset_obj_ca(ctlp, tblp, colp->obj_col);
        }
      }

      /* Reset the stream state since we are starting a new stream
       * (i.e. don't want to append to existing data in the stream.)
       */
      ociResetErr = OCIDirPathStreamReset(ctlp->dpstr_ctl,  ctlp->errhp_ctl);
      if (ociResetErr != OCI_SUCCESS)
        FATAL("OCIDirPathStreamReset failed", ociResetErr);
      state = GET_RECORD;                     /* get some more input records*/
      /* FALLTHROUGH */
    }

    case GET_RECORD:
    {
      assert(lastoff < ctlp->nrow_ctl);                /* array bounds check*/

      recp = (text *)(ctlp->inbuf_ctl + (lastoff * sessp->maxreclen_sess));

      if (fgets((char *)recp, (int)sessp->maxreclen_sess, inputfp)
                != (char *)NULL)
      {
        /* set column array offset to input record number map */
        ctlp->otor_ctl[lastoff] = ++input_recnum;
        /* if ((input_recnum % 10000) == 0)
          fprintf(output_fp, "record number: %d\n", (int)input_recnum); */
        state = FIELD_SET;
        /* FALLTHROUGH */
      }
      else
      {
        if (lastoff)
          lastoff--;
        state = END_OF_INPUT;
        break;
      }
    }

    case FIELD_SET:
    {
      /* map input data fields to DB columns, set column array entries */
      fsetrv = field_set(ctlp, tblp, (struct obj *) 0, recp, lastoff, 0);
      rowCnt = lastoff + 1;

      if (rowCnt == ctlp->nrow_ctl || fsetrv != FIELD_SET_COMPLETE)
      {
        /* array is full, or have a large partial column, or the
         * secondary buffer is in use by an OUTOFLINE field.
         */
        state = DO_CONVERT;
        /* FALLTHROUGH */
      }
      else
      {
        lastoff++;                             /* use next column array slot*/
        state = GET_RECORD;                               /* get next record*/
        break;
      }
    }

    case DO_CONVERT:
    {
      /* Either one of the following is true:
       * - the column array is full
       * - there is a large partial column
       * - the secondary buffer used by field_set() is in use
       * - previous conversion returned CONVERT_CONTINUE and
       *   now the conversion is being resumed.
       *
       * In any case, convert and load the data.
       */
      ub4    cvtBadRoff;                   /* bad row offset from conversion*/
      ub2    cvtBadCoff;                /* bad column offset from conversion*/

      while (startoff <= lastoff)
      {
        ub4 cvtCntPerCall = 0;   /* rows converted in one call to do_convert*/

        /* note that each call to do_convert() will convert all contiguousrows
         * in the colarray until it hit a row in error while converting.
         */
        cvtrv = do_convert(ctlp, startoff, rowCnt, &cvtCntPerCall,
                           &cvtBadCoff);
        cvtCnt += cvtCntPerCall; /* sum of rows converted so far in colarray*/
        if (cvtrv == CONVERT_SUCCESS)
        {
          /* One or more rows converted successfully, break
           * out of the conversion loop and load the rows.
           */
          assert(cvtCntPerCall > 0);
          state = DO_LOAD;
          break;
        }
        else if (cvtrv == CONVERT_ERROR)
        {
          /* Conversion error.  Reject the bad record and
           * continue on with the next record (if any).
           * cvtBadRoff is the 0-based index of the bad row in
           * the column array.  cvtBadCoff is the 0-based index
           * of the bad column (of the bad row) in the column
           * array.
           */
          /* This assert was used in the 10.1.0.4.0 version but removed for
           * 10.2 because compiler warning for pointless comparison of unsigned
           * interger */
          /*  assert(cvtCntPerCall >= (ub4)0); */

          cvtBadRoff = startoff + cvtCntPerCall;
          err_recnum = ctlp->otor_ctl[cvtBadRoff];    /* map to input_recnum*/
          fprintf(output_fp, "Conversion Error on record %d, column %d\n",
                           (int)err_recnum, (int)cvtBadCoff + 1);

          /* print err msg txt */
          errprint((dvoid *)(ctlp->errhp_ctl), OCI_HTYPE_ERROR,
                   (sb4 *)0);

          /* Always need to load a stream after a conversion error */
          cvtErrorRowNeedsLoad = TRUE;

          if (cvtBadRoff == lastoff)
          {
            /* Conversion error occurred on the last populated slot
             * of the column array.
             * Flush the input stream of any data for this row,
             * and re-use this slot for another input record.
             */
            field_flush(ctlp, lastoff);
            state    = GET_RECORD;
            startoff = cvtBadRoff;              /* only convert the last row*/
            rowCnt = 0;    /* already tried converting all rows in col array*/
            assert(startoff <= lastoff);
            break;
          }
          else
          {
            /* Skip over bad row and continue conversion with next row.
             * We don't attempt to fill in this slot with another record.
             */
            startoff = cvtBadRoff + 1;
            assert(startoff <= lastoff);
            continue;
          }
        }
        else if (cvtrv == CONVERT_NEED_DATA)      /* partial col encountered*/
        {
          /* Partial (large) column encountered, load the piece
           * and loop back up to field_set to get the rest of
           * the partial column.
           * startoff is set to the offset into the column array where
           * we need to resume conversion from, which should be the
           * last entry that we set (lastoff).
           */
          state    = DO_LOAD;

          /* Set our row position in column array to resume
           * conversion at when DO_LOAD transitions to DO_CONVERT.
           */
          /* This assert was used in the 10.1.0.4.0 version but removed for
           * 10.2 because compiler warning for pointless comparison of unsigned
           * interger */
          /* assert(cvtCntPerCall >= (ub4)0); */
          startoff = startoff + cvtCntPerCall;
          /* assert(startoff == lastoff); */
          break;
        }
        else if (cvtrv == CONVERT_CONTINUE)
        {
          /* The stream buffer is full and there is more data in
           * the column array which needs to be converted.
           * Load the stream (DO_LOAD) and transition back to
           * DO_CONVERT to convert the remainder of the column array,
           * without calling the field setting function in between.
           * The sequence {DO_CONVERT, DO_LOAD} may occur many times
           * for a long row or column.
           * Note that startoff becomes the offset into the column array
           * where we need to resume conversion from.
           */
          cvtcontcnt++;
          state    = DO_LOAD;

          /* Set our row position in column array (startoff) to
           * resume conversion at when we transition from the
           * DO_LOAD state back to DO_CONVERT.
           */
          /* This assert was used in the 10.1.0.4.0 version but removed for
           * 10.2 because compiler warning for pointless comparison of unsigned
           * interger */
          /* assert(cvtCntPerCall >= (ub4)0); */
          startoff = startoff + cvtCntPerCall;
          assert(startoff <= lastoff);

          break;
        }
      }                                                         /* end while*/
      break;
    }

    case DO_LOAD:
    {
      ub4    loadCnt;                     /* count of rows loaded by do_load*/

      ldrv       = do_load(ctlp, &loadCnt);
      nxtLoadOff = nxtLoadOff + loadCnt;
      cvtErrorRowNeedsLoad = FALSE;

      switch (ldrv)
      {
      case LOAD_SUCCESS:
      {
        /* The stream has been loaded successfully.  What we do next
         * depends on the result of the previous conversion step.
         */
        load_recnum = ctlp->otor_ctl[nxtLoadOff - 1];
        if (cvtrv == CONVERT_SUCCESS || cvtrv == CONVERT_ERROR)
        {
          /* The column array was successfully converted (or the
           * last row was in error).
           * Fill up another array with more input records.
           */
          state = RESET;
        }
        else if (cvtrv == CONVERT_CONTINUE)
        {
          /* There is more data in column array to convert and load. */
          state    = DO_CONVERT;

          /* Note that when do_convert returns CONVERT_CONTINUE that
           * startoff was set to the row offset into the column array
           * of where to resume conversion.  The loadCnt returned by
           * OCIDirPathLoadStream is the number of rows successfully
           * loaded.
           * Do a sanity check on the attributes here.
           */
          if (startoff != nxtLoadOff)                              /* sanity*/
            fprintf(output_fp, "LOAD_SUCCESS/CONVERT_CONTINUE: %ld:%ld\n",
                    (long)nxtLoadOff, startoff);

          /* Reset the direct stream state so conversion starts at
           * the beginning of the stream.
           */
          ociResetErr = OCIDirPathStreamReset(ctlp->dpstr_ctl,ctlp->errhp_ctl);
          if (ociResetErr != OCI_SUCCESS)
            FATAL("OCIDirPathStreamReset failed", ociResetErr);
        }
        else
        {
          /* Note that if the previous conversion step returned
           * CONVERT_NEED_DATA then the load step would have returned
           * LOAD_NEED_DATA too (not LOAD_SUCCESS).
           */
          FATAL("DO_LOAD:LOAD_SUCCESS: unexpected cvtrv", cvtrv);
        }
        break;
      }

      case LOAD_ERROR:
      {
        sb4  oraerr;
        ub4  badRowOff;

        badRowOff   = nxtLoadOff;
        nxtLoadOff += 1;                              /* account for bad row*/
        err_recnum      = ctlp->otor_ctl[badRowOff];  /* map to input_recnum*/
        fprintf(output_fp, "Error on record %ld\n", (long)err_recnum);

        /* print err msg txt */
        errprint((dvoid *)(ctlp->errhp_ctl), OCI_HTYPE_ERROR, &oraerr);

        /* On a load error, all rows up to the row in error are loaded.
         * account for that here by setting load_recnum only when some
         * rows have been loaded.
         */
        if (loadCnt != 0)
          load_recnum = err_recnum - 1;

        if (oraerr == OER(600))
          FATAL("DO_LOAD:LOAD_ERROR: server internal error", oraerr);

        if (err_recnum == input_recnum)
        {
          /* Error occurred on last input row, which may or may not
           * be in a partial state. Flush any remaining input for
           * the bad row.
           */
          field_flush(ctlp, badRowOff);
        }

        /* Continue loading this stream until no errors. Note that 
         * the stream positions itself to the next row on error.
         */
        state = DO_LOAD;

        break;
      }

      case LOAD_NEED_DATA:
      {
        load_recnum = ctlp->otor_ctl[nxtLoadOff];
        if (cvtrv == CONVERT_NEED_DATA)
          state = FIELD_SET;                         /* need more input data*/
        else if (cvtrv == CONVERT_CONTINUE)
          state = DO_CONVERT;   /* have input data, continue with conversion*/
        else
          FATAL("DO_LOAD:LOAD_NEED_DATA: unexpected cvtrv", cvtrv);

        /* Reset the direct stream state so conversion starts at
         * the beginning of the stream.
         */
        ociResetErr = OCIDirPathStreamReset(ctlp->dpstr_ctl, ctlp->errhp_ctl);
        if (ociResetErr != OCI_SUCCESS)
          FATAL("OCIDirPathStreamReset failed", ociResetErr);
        break;
      }

      case LOAD_NO_DATA:
      {
        /* Attempt to either load an empty stream, or a stream
         * which has been completely processed.
         */
        if (cvtrv == CONVERT_CONTINUE)
        {
          /* Reset stream state so we convert into an empty stream buffer.*/
          ociResetErr = OCIDirPathStreamReset(ctlp->dpstr_ctl,ctlp->errhp_ctl);
          if (ociResetErr != OCI_SUCCESS)
            FATAL("OCIDirPathStreamReset failed", ociResetErr);
          state = DO_CONVERT;           /* convert remainder of column array*/
        }
        else
          state = RESET;                      /* get some more input records*/
        break;
      }

      default:
        FATAL("DO_LOAD: unexpected return value", ldrv);
        break;
      }
      break;
    }

    case END_OF_INPUT:
    {
      if (cvtCnt || cvtErrorRowNeedsLoad)
        state = DO_LOAD; /* deal with data already converted, but not loaded*/
      else if (rowCnt)
        state = DO_CONVERT; /* deal with a partially populated column array*/
      else
        done = TRUE;
      break;
    }

    default:
      FATAL("SIMPLE_LOAD: unexpected state", state);
      break;
    }                                                  /* end switch (state)*/
  }

/*
  fprintf(output_fp, "do_convert returned CONVERT_CONTINUE %ld times\n",
          (long)cvtcontcnt);*/

  fprintf(output_fp, "Number of input records processed = %ld\n",
          (long)input_recnum);

}


/*
**++++++++++++++++++++++++++++++ finish_load +++++++++++++++++++++++++++++++++
**
**  Description:
**
**  Completes the loading procedure.
**
**  Assumptions:
**
**  Does not free server data structures related to the load.
**
**  Parameters:
**
**  ctlp                           load control structure pointer
**
**  Returns:
**
**-------------------------------------------------------------------------
*/

STATICF void finish_load(ctlp)
struct loadctl *ctlp;                    /* load control structure pointer */
{
  sword ociret;                          /* return code from OCI call */

  /* Execute load finishing logic without freeing server data structures
   * related to the load.
   */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIDirPathDataSave(ctlp->dpctx_ctl, ctlp->errhp_ctl,
                               (ub4)OCI_DIRPATH_DATASAVE_FINISH));

  /* free up server data structures for the load. */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIDirPathFinish(ctlp->dpctx_ctl, ctlp->errhp_ctl));
}


/*
**++++++++++++++++++++++++++++++ do_convert +++++++++++++++++++++++++++++++++
**
**  Description:
**
**  Convert the data in the column array to stream format.
**
**  Assumptions:
**
**
**  Parameters:
** 
**  ctlp           pointer to control structure   (IN/OUT)       
**  rowcnt         number of rows in column array     (IN)       
**  startoff       starting row offset into column array  (IN)   
**  cvtCntp        count of rows successfully converted (OUT)    
**  badcoffp       column offset into col array of bad col (OUT) 
**
**  Returns:
**
**  CONVERT_SUCCESS:
**     All data in the column array has been successfully converted.
**     *cvtCntp is the number of rows successfully converted.
**  CONVERT_ERROR:
**     Conversion error occurred on the row after the last successfully
**     converted row.
**   Client Action:
**     Continue converting the column array by calling this function
**     again with startoff adjusted to skip over the row in error.
**   CONVERT_NEED_DATA:
**     All data in the column array has been converted, but the last
**     column processed was marked as a partial.
**   CONVERT_CONTINUE:
**     Not all of the data in the column array has been converted due to
**     lack of space in the stream buffer.
**     Client Action:
**       Load the converted stream data, reset the stream, and call this
**       function again without modifying the column array and setting
**       startoff to the appropriate position in the array.
**
**-------------------------------------------------------------------------
*/

sword do_convert(ctlp, startoff, rowcnt, cvtCntp, badcoffp)
struct loadctl *ctlp;         /* pointer to control structure   (IN/OUT) */
ub4             rowcnt;       /* number of rows in column array     (IN) */
ub4             startoff;     /* starting row offset into column array  (IN) */
ub4            *cvtCntp;      /* count of rows successfully converted (OUT) */
ub2            *badcoffp;     /* column offset into col array of bad col (OUT) */
{
  sword retval = CONVERT_SUCCESS;
  sword ocierr, ocierr2;
  ub2   badcol = 0;

  *cvtCntp = 0;

  if (startoff >= rowcnt)
    FATAL("DO_CONVERT: bad startoff", startoff);

  if (rowcnt)
  {
    /* convert array to stream, filter out bad records */
    ocierr = OCIDirPathColArrayToStream(ctlp->dpca_ctl, ctlp->dpctx_ctl,
                                        ctlp->dpstr_ctl, ctlp->errhp_ctl,
                                        rowcnt, startoff);
    switch (ocierr)
    {
    case OCI_SUCCESS:        /* everything succesfully converted to stream */
      retval = CONVERT_SUCCESS;
      break;

    case OCI_ERROR:            /* some error, most likely a conversion error */
      /* Tell the caller that a conversion error occurred along
       * with the number of rows successfully converted (*cvtCntp).
       * Note that the caller is responsible for adjusting startoff
       * accordingly and calling us again to resume conversion of
       * the remaining rows.
       */
      retval  = CONVERT_ERROR;                         /* conversion error */
      break;

    case OCI_CONTINUE:                              /* stream buffer is full */
      /* The stream buffer could not contain all of the data in
       * the column array.
       * The client should load the converted data, and loop
       * back to convert the remaining data in the column array.
       */
      retval  = CONVERT_CONTINUE;
      break;

    case OCI_NEED_DATA:                        /* partial column encountered */
      /* Everything converted, but have a partial column.
       * Load this stream, and return to caller for next piece.
       */
      retval = CONVERT_NEED_DATA;
      break;

    default:                                 /* unexpected OCI return value! */
      FATAL("do_convert:OCIDirPathColArrayToStream:Unexpected OCI return code",
             ocierr);
      /* NOTREACHED */
      break;
    }

    OCI_CHECK(ctlp->errhp2_ctl, OCI_HTYPE_ERROR, ocierr2, ctlp,
              OCIAttrGet((CONST dvoid *)ctlp->dpca_ctl, 
                         OCI_HTYPE_DIRPATH_COLUMN_ARRAY,
                         (dvoid *)(cvtCntp), (ub4 *)0, 
                         OCI_ATTR_ROW_COUNT, ctlp->errhp2_ctl));

    OCI_CHECK(ctlp->errhp2_ctl, OCI_HTYPE_ERROR, ocierr2, ctlp,
              OCIAttrGet((CONST dvoid *)ctlp->dpca_ctl, 
                         OCI_HTYPE_DIRPATH_COLUMN_ARRAY,
                         (dvoid *)(&badcol), (ub4 *)0, 
                         OCI_ATTR_COL_COUNT, ctlp->errhp2_ctl));
  }

  *badcoffp = badcol;

  return retval;
}


/*
**++++++++++++++++++++++++++++++ do_load +++++++++++++++++++++++++++++++++
**
**  Description:
**
**  Load a direct path stream.
**
**  Assumptions:
**
**  errhp_ctl contains error information when an error is returned
**  from this function.
**
**  Parameters:
**
**  ctlp:     Pointer to control structure. 
**  loadCntp: Count of rows loaded on this call.
**
**  Returns:
**
**   LOAD_SUCCESS:
**     All data loaded succesfully.
**     Client Action:
**       Supply another stream and call again, or finish the load.
**
**   LOAD_ERROR:
**     Error while loading occured.  *loadCntp is the number of
**     rows successfully loaded this call.
**     Client Action:
**       Use *loadCntp to compute current column array position and
**       map the column array position to the input record and reject
**       the record.
**
**       if (this is a continuation of a row)
**       {
**         /o server has data for this row buffered o/
**         flush the row data
**       }
**
**       if (end-of-stream has not been reached)
**       {
**         call this function again,
**         stream loading will resume with the next row in the stream.
**       }
**       else if (end-of-stream has been reached)
**       {
**         build another stream and call this function again,
**         or finish the load.
**       }
**
**   LOAD_NEED_DATA:
**     Last row was not complete.
**     Client Action:
**       Caller needs to supply more data for the row (a column is
**       being pieced.)  Note that the row offset can be determined
**       by either the cvtCnt returned from do_convert, or from the
**       loadCntp returned by do_load.  The column offset for the
**       column being pieced is available as an attribute of
**       the column array.
**
**-------------------------------------------------------------------------
*/

sword do_load(ctlp, loadCntp)
struct loadctl *ctlp;             /* pointer to control structure   (IN/OUT) */
ub4            *loadCntp;         /* number of rows loaded (OUT) */
{
  sword ocierr;                   /* OCI return value */
  sword retval;                   /* return value from this function */
  sword getRowCnt = FALSE;        /* return row count if TRUE */

  if (loadCntp != (ub4 *)0)
  {
    *loadCntp = 0;
    getRowCnt = TRUE;
  }

  /* Load the stream.
   * Note that the position in the stream is maintained internally to
   * the stream handle, along with offset information for the column
   * array which produced the stream.  When the conversion to stream
   * format is done, the data is appended to the stream.  It is the
   * responsibility of the caller to reset the stream when appropriate.
   * On errors, the position is moved to the next row, or the end of
   * the stream if the error occurs on the last row.  The next LoadStream
   * call will start on the next row, if any.
   * If a LoadStream call is made, and end of stream has been reached,
   * OCI_NO_DATA is returned.
   */

#if 1
  ocierr = OCIDirPathLoadStream(ctlp->dpctx_ctl, ctlp->dpstr_ctl,
                                ctlp->errhp_ctl);
#else
  {
    ub1 *bufaddr;
    ub4  buflen;

    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ocierr, ctlp,
              OCIAttrGet((CONST dvoid *)(ctlp->dpstr_ctl), 
                         OCI_HTYPE_DIRPATH_STREAM,
                         (dvoid *)&bufaddr, (ub4 *)0, 
                         OCI_ATTR_BUF_ADDR, ctlp->errhp_ctl));
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ocierr, ctlp,
              OCIAttrGet((CONST dvoid *)(ctlp->dpstr_ctl), 
                         OCI_HTYPE_DIRPATH_STREAM,
                         (dvoid *)&buflen, (ub4 *)0, 
                         OCI_ATTR_BUF_SIZE, ctlp->errhp_ctl));
    write(1, (char *)bufaddr, (int)buflen);
    fprintf(output_fp, "Wrote %d bytes from stream\n", (int)buflen);
    getRowCnt = FALSE;
  }
#endif
  
  switch (ocierr)
  {
    case OCI_SUCCESS:
      /* all data succcesfully loaded */
      retval    = LOAD_SUCCESS;
      break;

    case OCI_ERROR:
      /* Error occurred while loading: could be a partition mapping
       * error, null constraint violation, or an out of space
       * condition.  In any case, we return the number of rows
       * processed (successfully loaded).
       */
      retval    = LOAD_ERROR;
      break;

    case OCI_NEED_DATA:
      /* Last row was not complete.
       * The caller needs to supply another piece.
       */
      retval    = LOAD_NEED_DATA;
      break;

    case OCI_NO_DATA:
      /* the stream was empty */
      retval    = LOAD_NO_DATA;
      break;

    default:
      FATAL("do_load:OCIDirPathLoadStream:Unexpected OCI return code", ocierr);
      /* NOTREACHED */
      break;
  }

  if (getRowCnt)
  {
    sword  ocierr2;

    OCI_CHECK(ctlp->errhp2_ctl, OCI_HTYPE_ERROR, ocierr2, ctlp,
              OCIAttrGet((CONST dvoid *)(ctlp->dpstr_ctl), 
                         OCI_HTYPE_DIRPATH_STREAM,
                         (dvoid *)loadCntp, (ub4 *)0, 
                         OCI_ATTR_ROW_COUNT, ctlp->errhp2_ctl));
  }

  return retval;
}


/*
**++++++++++++++++++++++++++++++ field_flush +++++++++++++++++++++++++++++++++
**
**  Description:
**
**  Helper function which cleans up the partial context state, and clears it.
**
**  Assumptions:
**
**  Parameters:
**
**  ctlp                           load control structure pointer 
**  rowoff                         column array row offset 
**
**  Returns:
**
**-------------------------------------------------------------------------
*/

STATICF void field_flush(ctlp, rowoff)
struct loadctl *ctlp;           /* load control structure pointer */
ub4             rowoff;         /* column array row offset */ 
{
  if (ctlp->pctx_ctl.valid_pctx)
  {
    /* Partial context is valid; make sure the request is
     * for the context corresponding to the current row.
     */
    assert(rowoff == ctlp->pctx_ctl.row_pctx);
    (void) close(ctlp->pctx_ctl.fd_pctx);
    free((void *)ctlp->pctx_ctl.fnm_pctx);
  }
  CLEAR_PCTX(ctlp->pctx_ctl);
}


/*
**++++++++++++++++++++++++++++++ field_set +++++++++++++++++++++++++++++++++
**
**  Description:
**
**  Sets the input data fields to their corresponding data columns.
**
** Simple field setting.
** Computes address and length of fields in the input record,
** and sets the corresponding column array entry for each input field.
**
** This function only deals with positional fields.
**
** Leading white space is trimmed from the field if FLD_STRIP_LEAD_BLANK
** is set.
**
** Trailing white space is trimmed from the field if FLD_STRIP_TRAIL_BLANK
** is set.
**
** Fields which consist of all spaces are loaded as null columns.
**
** Fields which are marked as FLD_OUTOFLINE are interpreted
** as being file names which can be passed directly to an
** open system call.
**
** NOTES: Discuss how partials are handled.
** 
**  Assumptions:
**
**  Parameters:
**
**  ctlp                           load control structure 
**  tblp                           table descriptor 
**  recp                           input record 
**  rowoff                         column array row offset 
**  bufflg                         buffer in use flag 
**
**  Returns:
**
**   FIELD_SET_COMPLETE:
**     All fields are complete, the partial context is not valid.
**
**   FIELD_SET_BUF
**     All fields are complete, the partial context is not valid, but
**     data is buffered in a secondary buffer and the column array has
**     one or more pointers into the secondary buffer.  The caller
**     must convert the column array to stream format before calling
**     this function again.
**
**   FIELD_SET_PARTIAL:
**     A field is in the partial state, the partial context is valid
**     and is required to continue processing the field.  Note that
**     when a field is partial, the row which contains the column
**     corresponding to the field is partial also.
**
**   FIELD_SET_ERROR:
**     A read error occured on a secondary (out-of-line) data file.
**
**-------------------------------------------------------------------------
*/

/* Deal with WIN32 CR-LF weirdness */
#if defined(WIN32COMMON) || defined(WIN32) || defined(_WIN32)
#define TKPIDRV_OPEN_MODE (O_RDONLY | O_BINARY)
#else
#define TKPIDRV_OPEN_MODE (O_RDONLY)
#endif

STATICF sword field_set(ctlp, tblp, objp, recp, rowoff, bufflg)
struct loadctl    *ctlp;                 /* load control structure */
struct tbl        *tblp;                 /* table descriptor */
struct obj        *objp;                 /* object descriptor */
text              *recp;                 /* input record */
ub4                rowoff;               /* column array row offset */
ub1                bufflg;               /* buffer in use flag */
{
  ub1  *cval;
  ub4   ncols;
  ub4   thiscol;
  ub4   clen, j;                                            /* column length */
  ub1   cflg;
  sword ociret;
  sword done = FALSE;
  int   fd;                          /* file descriptor for out-of-line data */
  char *filename;                           /* filename for out-of-line data */
  sword  partial;
  static int count = 0;
  struct col *cols;
  struct fld *flds;
  OCIDirPathColArray * ca;
  ub4    recsz = 0;                  /* Current size of record read in */
  ub1    prntflg = 0;                /* Print warning message flag */
  ub4 i = 0;

  /* recsz = strlen ((const char *)recp); */
  /* strlen won't work for binary numbers in record. */
   while (!done)
     {
        for (i = 0; recp[i] != '\n' ; i++)
          {
            recsz = recsz + 1;
          }
        done = TRUE;
     }
  /* Reset the buffer offset if not recursing */
  if (!bufflg)
    ctlp->bufoff_ctl = 0;

  if ((partial = (sword)ctlp->pctx_ctl.valid_pctx) == TRUE)
  {
    /* partial context is valid; resume where we left off */
    assert(rowoff == ctlp->pctx_ctl.row_pctx);
    thiscol = ctlp->pctx_ctl.col_pctx;
  }
  else
    thiscol = 0;

  if (objp != 0)
  {
    cols =  objp->col_obj;
    flds =  objp->fld_obj;
    ncols = objp->ncol_obj;
    ca = objp->ca_obj;
  }
  else
  {
    cols =  tblp->col_tbl;
    flds =  tblp->fld_tbl;
    ncols = tblp->ncol_tbl;
    ca = ctlp->dpca_ctl;
  }

  for (/* empty */; thiscol < ncols; thiscol++)
  {
    struct col *colp =  &cols[thiscol];
    struct fld *fldp =  &flds[thiscol];

    if (partial)
    {
      /* partials are always from a secondary file */
      fd       = ctlp->pctx_ctl.fd_pctx;
      filename = ctlp->pctx_ctl.fnm_pctx;
    }
    else                                                         /* !partial*/
    {
      fd       = -1;
      filename = (char *)0;
      cval     = (ub1 *)recp + fldp->begpos_fld - 1;

      /*
      **  Check the field length is not longer than the current record length.
      **  If it is, issue a warning and set the clen to the record length -
      **  the beginning field position.
      */
       if (fldp->endpos_fld > recsz )
    {
        if (!prntflg)
          {
           fprintf(output_fp, 
            "Warning: Max field length, %d for record %d, is greater than the current record size %d.\n",
             fldp->endpos_fld, (rowoff+1), recsz);
           prntflg = 1;
          }
        clen = recsz - fldp->begpos_fld + 1;
    } 
      else 
          clen = fldp->endpos_fld - fldp->begpos_fld + 1;

      j = 0;
      if (bit(fldp->flag_fld, FLD_STRIP_LEAD_BLANK))
      {
        /* trim leading white space */
        for (/*empty*/; j < clen; j++)
          if (!isspace((int)cval[j]))
            break;
      }

      if (j >= clen)
        clen = 0;                              /* null column, handled below*/
      else
      {
        if (bit(fldp->flag_fld, FLD_STRIP_TRAIL_BLANK))
        {
          /* trim trailing white space or new line char within field length. */
          while ((clen && isspace((int)cval[clen - 1]))||
                 (clen && ((int)cval[clen - 1]== '\n')))
            clen--;
        }
        cval = cval + j;
        clen = clen - j;
      }

      if (colp->exttyp_col == SQLT_NTY || colp->exttyp_col == SQLT_REF)
        goto obj;

      if (clen)
      {
        if (bit(fldp->flag_fld, FLD_INLINE))
        {
          cflg = OCI_DIRPATH_COL_COMPLETE;
        }
        else if (bit(fldp->flag_fld, FLD_OUTOFLINE))
        {
          filename = (char *)malloc((size_t)clen+1);
          if (!filename)
          {
            perror("malloc");
            FATAL("field_set: cannot malloc buf for filename", (clen + 1));
          }
          (void) memcpy((dvoid *)filename, (dvoid *)cval, (size_t)clen);
          filename[clen] = 0;
          fd = open(filename, TKPIDRV_OPEN_MODE);
          SET_PCTX(ctlp->pctx_ctl, rowoff, thiscol, (ub4)0, fd, filename);
          LEN_PCTX(ctlp->pctx_ctl) = 0;
        }
        else
        {
          FATAL("field_set: unknown field type", fldp->flag_fld);
        }
      }
      else
      {
        cflg = OCI_DIRPATH_COL_NULL;               /* all spaces become null*/
        cval = (ub1 *)0;
      }
    }

    if (bit(fldp->flag_fld, FLD_OUTOFLINE))
    {
      char *buf;
      ub4   bufsz;
      int   cnt;

      if (!ctlp->buf_ctl)
      {
        ctlp->buf_ctl   = (ub1 *)malloc((size_t)SECONDARY_BUF_SIZE);
        ctlp->bufsz_ctl = SECONDARY_BUF_SIZE;
      }

      if ((ctlp->bufsz_ctl - ctlp->bufoff_ctl) > SECONDARY_BUF_SLOP)
      {
        buf   = (char *)ctlp->buf_ctl + ctlp->bufoff_ctl;  /* buffer pointer*/
        bufsz = (int)ctlp->bufsz_ctl  - ctlp->bufoff_ctl;     /* buffer size*/

        if (fd == -1)
          cnt = 0;
        else
          cnt = read(fd, buf, bufsz);

        if (cnt != -1)
        {
          cval = (ub1 *)buf;
          clen = (ub4)cnt;

          if (cnt < bufsz)                    /* all file data has been read*/
          {
            /* mark column as null or complete */
            if (cnt == 0 && LEN_PCTX(ctlp->pctx_ctl) == 0)
              cflg = OCI_DIRPATH_COL_NULL;
            else
              cflg = OCI_DIRPATH_COL_COMPLETE;

            field_flush(ctlp, rowoff);          /* close file, free filename*/

            /* adjust offset into buffer for use by next field */
            ctlp->bufoff_ctl += cnt;
          }
          else
            cflg  = OCI_DIRPATH_COL_PARTIAL;
        }
        else
        {
          /* XXX: do something on read failure, like return an error context*/
          field_flush(ctlp, rowoff);          /* close file, free filename*/
          return FIELD_SET_ERROR;
        }
      }
      else
      {
        /* no room in secondary buffer, return a 0 length partial
         * and pick it up next time.
         */
        cflg = OCI_DIRPATH_COL_PARTIAL;
        clen = 0;
        cval = (ub1 *)NULL;
      }
    }

    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIDirPathColArrayEntrySet(ca, ctlp->errhp_ctl,
                                         rowoff, colp->id_col,
                                         cval, clen, cflg));

    if (cflg == OCI_DIRPATH_COL_PARTIAL)
    {
      /* Partials only occur for OutOfLine data
       * remember the row offset, column offset,
       * total length of the column so far,
       * and file descriptor to get data from on
       * subsequent calls to this function.
       */
      LEN_PCTX(ctlp->pctx_ctl) += clen;
      return FIELD_SET_PARTIAL;
    }

obj:
    if (colp->exttyp_col == SQLT_NTY || colp->exttyp_col == SQLT_REF)
    {
      /* check for NULL for the whole object. If clen is not empty, then the
       * object is not null.
       */

      objp = colp->obj_col;
      if (clen)
      {
        field_set(ctlp, tblp, colp->obj_col, recp, objp->rowoff_obj, 1);
        objp->rowoff_obj++;

        /* Set the entry in the parent column array to be the column array
         * for the object/
         */
        cflg = OCI_DIRPATH_COL_COMPLETE;
        clen = sizeof(objp->ca_obj);
        cval = (ub1 *) objp->ca_obj;

        OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
                  OCIDirPathColArrayEntrySet(ca, ctlp->errhp_ctl,
                                             rowoff, colp->id_col,
                                             cval, clen, cflg));

      }
      else
      {
        /* set the entry in the column array to be NULL flag */
        cflg = OCI_DIRPATH_COL_NULL;
        clen = 0;
        cval = (ub1 *)NULL;

        OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
                  OCIDirPathColArrayEntrySet(ca, ctlp->errhp_ctl,
                                             rowoff, colp->id_col,
                                             cval, clen, cflg));
      }

    }

  } /* end of setting attr values in col array */

  CLEAR_PCTX(ctlp->pctx_ctl);
  if (ctlp->bufoff_ctl)            /* data in secondary buffer for this row*/
    return FIELD_SET_BUF;
  else
    return FIELD_SET_COMPLETE;

}


/*
**++++++++++++++++++++++++++++++  errprint +++++++++++++++++++++++++++++++++
**
**  Description:
**
**  Assumptions:
**
**  Parameters:
**
**  errhp
**  htype
**  errcodep
**
**  Returns:
**
**-------------------------------------------------------------------------
*/

STATICF void errprint(errhp, htype, errcodep)
dvoid  *errhp;
ub4     htype;
sb4    *errcodep;
{
  text errbuf[512];

  if (errhp)
  {
    sb4  errcode;

    if (errcodep == (sb4 *)0)
      errcodep = &errcode;

    (void) OCIErrorGet((dvoid *)errhp, (ub4) 1, (text *) NULL, errcodep,
                       errbuf, (ub4) sizeof(errbuf), htype);
    (void) fprintf(output_fp, "Error - %.*s\n", 512, errbuf);
  }
}


/*
**++++++++++++++++++++++++++++++  checkerr +++++++++++++++++++++++++++++++++
**
**  Description:
**
**  Assumptions:
**
**  Parameters:
**
**  errhp
**  htype
**  status
**  note
**  code
**  file
**  line
**
**  Returns:
**
**-------------------------------------------------------------------------
*/

STATICF void checkerr(errhp, htype, status, note, code, file, line)
dvoid *errhp;
ub4    htype;
sword  status;
text  *note;
sb4    code;
text  *file;
sb4    line;
{
  sb4 errcode = 0;

  if ((status != OCI_SUCCESS))
    (void) fprintf(output_fp, "OCI Error %ld occurred at File %s:%ld\n",
                   (long)status, (char *)file, (long)line);

  if (note)
    (void) fprintf(output_fp, "File %s:%ld (code=%ld)  %s\n",
                   (char *)file, (long)line, (long)code, (char *)note);

  switch (status)
  {
  case OCI_SUCCESS:
    break;
  case OCI_SUCCESS_WITH_INFO:
    (void) fprintf(output_fp, "Error - OCI_SUCCESS_WITH_INFO\n");
    errprint(errhp, htype, &errcode);
    break;
  case OCI_NEED_DATA:
    (void) fprintf(output_fp, "Error - OCI_NEED_DATA\n");
    break;
  case OCI_NO_DATA:
    (void) fprintf(output_fp, "Error - OCI_NODATA\n");
    break;
  case OCI_ERROR:
    errprint(errhp, htype, &errcode);
    break;
  case OCI_INVALID_HANDLE:
    (void) fprintf(output_fp, "Error - OCI_INVALID_HANDLE\n");
    break;
  case OCI_STILL_EXECUTING:
    (void) fprintf(output_fp, "Error - OCI_STILL_EXECUTE\n");
    break;
  case OCI_CONTINUE:
    (void) fprintf(output_fp, "Error - OCI_CONTINUE\n");
    break;
  default:
    break;
  }
}


/*
**++++++++++++++++++++++++++++++ cleanup +++++++++++++++++++++++++++++++++
**
**  Description:
**
**  Frees up handles and exit with the supplied exit status code.
**
**  Assumptions:
**
**  Parameters:
**
**  ctlp                      load control structure pointer 
**  ex_status
**
**  Returns:
**    ex_status
**
**-------------------------------------------------------------------------
*/

STATICF void cleanup(ctlp, ex_status)
struct loadctl *ctlp;          /* load control structure pointer */
sb4    ex_status;
{
  sword ociret;

  /* Free the column array and stream handles if they have been
   * allocated.  We don't need to do this since freeing the direct
   * path context will free the heap which these child handles have
   * been allocated from.  I'm doing this just to exercise the code
   * path to free these handles.
   */
  if (ctlp->dpca_ctl)
  {
    ociret = OCIHandleFree((dvoid *)ctlp->dpca_ctl,
                           OCI_HTYPE_DIRPATH_COLUMN_ARRAY);
    if (ociret != OCI_SUCCESS)
      CHECKERR(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret);
  }

  if (ctlp->dpstr_ctl)
  {
    ociret = OCIHandleFree((dvoid *)ctlp->dpstr_ctl,
                           OCI_HTYPE_DIRPATH_STREAM);
    if (ociret != OCI_SUCCESS)
      CHECKERR(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret);
  }

  /* free object-related dpapi handles if loading to obj/opq/ref cols */
  if (ctlp->loadobjcol_ctl) 
  {
    ub2         i;
    struct col *colp;
    struct tbl *tblp = &table;

    for (i = 0, colp = tblp->col_tbl; i < tblp->ncol_tbl; i++, colp++)
    {
      if (colp->exttyp_col == SQLT_NTY || colp->exttyp_col == SQLT_REF)
      {
        free_obj_hndls(ctlp, colp->obj_col);

        if (colp->obj_col->ca_obj)
        {
          ociret = OCIHandleFree((dvoid *)(colp->obj_col->ca_obj),
                                 OCI_HTYPE_DIRPATH_FN_COL_ARRAY);
          if (ociret != OCI_SUCCESS)
              CHECKERR(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret);
        }

        if (colp->obj_col->ctx_obj)
        {
          ociret = OCIHandleFree((dvoid *)(colp->obj_col->ctx_obj), 
                                 OCI_HTYPE_DIRPATH_FN_CTX);
          if (ociret != OCI_SUCCESS)
            CHECKERR(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret);
        }
      }
    }
  }

  if (ctlp->dpctx_ctl)
  {
    ociret = OCIHandleFree((dvoid *)ctlp->dpctx_ctl, OCI_HTYPE_DIRPATH_CTX);
    if (ociret != OCI_SUCCESS)
      CHECKERR(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret);
  }

  if (ctlp->errhp_ctl && ctlp->srvhp_ctl)
  {
    (void) OCIServerDetach(ctlp->srvhp_ctl, ctlp->errhp_ctl, OCI_DEFAULT );
    ociret = OCIHandleFree((dvoid *)ctlp->srvhp_ctl, OCI_HTYPE_SERVER);
    if (ociret != OCI_SUCCESS)
      CHECKERR(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret);
  }

  if (ctlp->svchp_ctl)
    (void) OCIHandleFree((dvoid *) ctlp->svchp_ctl, OCI_HTYPE_SVCCTX);
  if (ctlp->errhp_ctl)
    (void) OCIHandleFree((dvoid *) ctlp->errhp_ctl, OCI_HTYPE_ERROR);

  if ((output_fp != stdout) && (output_fp != stderr))
    fclose(output_fp);

  exit((int)ex_status);
}


/*
**++++++++++++++++++++++++++++ free_obj_hndls +++++++++++++++++++++++++++++++++
**
**  Description:
**
**  Frees up dpapi object handles (function column array & function context).
**
**  Assumptions:
**
**  Parameters:
**
**  ctlp                      load control structure pointer 
**  objp                      object structure pointer
**
**  Returns:
**    Nothing.
**
**-------------------------------------------------------------------------
*/
STATICF void free_obj_hndls(ctlp, objp)
struct loadctl *ctlp;
struct obj     *objp;
{
  ub2         i;
  struct col *colp;                                        /* column pointer */
  sword       ociret;

  for (i = 0, colp = objp->col_obj; i < objp->ncol_obj; i++, colp++)
  {
    if (colp->exttyp_col == SQLT_NTY || colp->exttyp_col == SQLT_REF)
    {
      free_obj_hndls(ctlp, colp->obj_col);

      if (colp->obj_col->ca_obj)
      {
        ociret = OCIHandleFree((dvoid *)(colp->obj_col->ca_obj),
                               OCI_HTYPE_DIRPATH_FN_COL_ARRAY);
        if (ociret != OCI_SUCCESS)
          CHECKERR(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret);
      }

      if (colp->obj_col->ctx_obj)
      {
        ociret = OCIHandleFree((dvoid *)(colp->obj_col->ctx_obj), 
                               OCI_HTYPE_DIRPATH_FN_CTX);
        if (ociret != OCI_SUCCESS)
          CHECKERR(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret);
      }
    }
  }
}

/*
**+++++++++++++++++++++++++set_and_get_attributes++++++++++++++++++++++++++++
**
**  Description:
**       This routine has been provided to improve code coverage of
**       setting and retrieving direct path attributes via OCI. It
**       sets and gets the direct path attributes that are not exercised
**       in any other regression test.
**
**  Assumptions:
**           None
**  Parameters:
**           ctlp        load control structure pointer
**
**  Returns:         
**           Nothing
**-------------------------------------------------------------------------
*/

STATICF void set_and_get_attributes(struct loadctl *ctlp, struct tbl *tblp)
{
  sword   ociret;
  ub1     parallel_opt, nolog_opt, bad_value=100;
  ub1     parallel_enabled=0, nolog_enabled=0;
  ub1     mode, load_mode = OCI_DIRPATH_LOAD;
  ub1     input_type = OCI_DIRPATH_INPUT_TEXT;
  ub1     lock_wait, convert_value=1; 
  ub1     skip_index_maint=OCI_DIRPATH_INDEX_MAINT_SKIP_ALL;
  ub2     charsetid=OCI_UTF16ID, ret_charsetid;
  ub4     rows_loaded=0, scn_base=0, scn_wrap=0;
  ub4     version_text=1, parallel_extent = 100000;
  ub4     ret_version, ret_stream_version;
  ub4     stream_version = OCI_DIRPATH_STREAM_VERSION_1;
  ub4     out_stream_version, stream_offset;
  ub4     attr_nam_len=3;
  ub4     dcache_size=0, ret_obj_constr_len;
  ub8     granule_size=0, granule_offset=512;
  text    attr_nam[3]={'b','u','y'},obj_constr[3]={'h','i','\0'};
  text    *attrp_nam = attr_nam;
  text    *obj_constr_ptr = obj_constr;    
  text    *ret_obj_constr_ptr;
  text    fileinfo[16]="doesnt_exist.dat";

  /* Improve code coverage in the routines, kpusattr and kpugattr, which 
  *  are the routines behind the OCI routines, OCIAttrSet and OCIAttrGet.
  */

  /***********************************************************************
  * This next section of code will SET the following direct path 
  * attributes:
  *           dirpath_mode    - mode of direct path context (load, convert)
  *           version         - user assigned version
  *           stream version  - version of stream supported
  *           storage_initial - initial extent size
  *           storage_next    - next extent size
  *           file            - DB file to load into
  *           charset_id      - character set id
  *           version        (associated with direct path stream handle)
  *           attr_name       - attribute name
  *           obj_constr      - object constructor name
  *           skip_index_maint- index maintenance method
  *           convert_value   - set to 1 to indicate conversion needed
  *           granule_offset  - offset to last granule        
  */

  /* Set the dirpath mode to load */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&load_mode, 0,
                       OCI_ATTR_DIRPATH_MODE, ctlp->errhp_ctl));

  /* Set the dirpath version to 1 */
   OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&version_text, 0,
                       OCI_ATTR_VERSION, ctlp->errhp_ctl));
                 
  /* Set the dirpath stream version to 100 */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&stream_version, 0,
                       OCI_ATTR_DIRPATH_STREAM_VERSION, ctlp->errhp_ctl));

  /* Set the storage for the first parallel extent to 100,000 */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&parallel_extent, 0,
                       OCI_ATTR_DIRPATH_STORAGE_INITIAL, ctlp->errhp_ctl));

  /* Set the storage for subsequent parallel extents to 100,000 */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&parallel_extent, 0,
                       OCI_ATTR_DIRPATH_STORAGE_NEXT, ctlp->errhp_ctl));
                       
  /* Initialize parallel file name to dummy name  */
  /* Note:  The following code to set the parallel file name
  *         twice is intentional.  The 2nd setting of this
  *         name is to exercise a path where a file name has
  *         been set and the memory buffer containing this
  *         name is freed before processing this new input
  *         file name.
  */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&fileinfo, sizeof(fileinfo),
                       OCI_ATTR_DIRPATH_FILE, ctlp->errhp_ctl));

  /* Initialize parallel file name to dummy name  */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&fileinfo, sizeof(fileinfo),
                       OCI_ATTR_DIRPATH_FILE, ctlp->errhp_ctl));
                        
   /* Set the character set id to UTF16*/
   OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&charsetid, 0,
                       OCI_ATTR_CHARSET_ID, ctlp->errhp_ctl));
                       
   /* Set client interface version. */
   OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)(ctlp->dpstr_ctl),
                       OCI_HTYPE_DIRPATH_STREAM,
                       (dvoid *)&stream_version, 0,
                       OCI_ATTR_VERSION, ctlp->errhp_ctl));

   /* Set the attribute name to a dummy name.  This name attribute is
      initialized/set in existing code.  This setting here is to exercise
      the path where the name has already been set and this is the second
      time. */
   OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrSet((dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid **)&attrp_nam, attr_nam_len,
                       OCI_ATTR_NAME, ctlp->errhp_ctl));

  /* Set the object constructor name to a dummy name.  This name attribute is
     initialized/set in existing code.  This setting here is to exercise
      the path where the name has already been set and this is the second
      time.  */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)(ctlp->dpctx_ctl),
                         (ub4)OCI_HTYPE_DIRPATH_CTX,
                         (dvoid *)obj_constr_ptr, 0,
                         OCI_ATTR_DIRPATH_OBJ_CONSTR, ctlp->errhp_ctl));

  /* Set the attribute, dirpath_index_maint_method, to skip all */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)(ctlp->dpctx_ctl),
                         (ub4)OCI_HTYPE_DIRPATH_CTX,
                         &skip_index_maint, 0,
                         OCI_ATTR_DIRPATH_INDEX_MAINT_METHOD, 
                         ctlp->errhp_ctl));

  /* Set the attribute, dirpath_convert, to 1 (meaning conversion needed) */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)(ctlp->dpctx_ctl),
                         (ub4)OCI_HTYPE_DIRPATH_CTX,
                         &convert_value, 0,
                         OCI_ATTR_DIRPATH_CONVERT, ctlp->errhp_ctl));

  /* Set the size of the granule offset to 512 */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrSet((dvoid *)(ctlp->dpctx_ctl),
                         (ub4)OCI_HTYPE_DIRPATH_CTX,
                         &granule_offset, 0,
                         OCI_ATTR_DIRPATH_GRANULE_OFFSET, ctlp->errhp_ctl));

  /* Exercise several error paths.  The OCI_CHECK macro is not used
  *  since an error is expected as the return status.  The attributes
  *  given are not valid for the specified handle.
  */                                   

   ociret = OCIAttrSet((dvoid *)(ctlp->dpctx_ctl),
                       (ub4)OCI_HTYPE_DIRPATH_CTX,
                       (dvoid **) &obj_constr_ptr, 
                       (ub4)strlen((const char *)obj_constr_ptr),
                       OCI_ATTR_ENV, ctlp->errhp_ctl);
  
   ociret = OCIAttrSet((dvoid *)(ctlp->dpstr_ctl),
                         (ub4)OCI_HTYPE_DIRPATH_STREAM,
                         (dvoid **) &obj_constr_ptr, 
                         (ub4)strlen((const char *)obj_constr_ptr),
                         OCI_ATTR_ENV, ctlp->errhp_ctl);

   ociret = OCIAttrGet((CONST dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&mode,(ub4 *)0,
                       OCI_ATTR_ENV, ctlp->errhp_ctl);


   /*******************************************************************/
   /* Now this section will retrieve (GET) the direct path attributes */
    /*******************************************************************/
    /* Using the direct path context handle, fetch the following 
    *  15 items:
    *             direct path mode
    *             number of rows loaded
    *             attribute name
    *             character set id
    *             nolog
    *             parallel
    *             stream version
    *             version
    *             dcache_size
    *             obj_constr 
    *             scn base value
    *             scn wrap value
    *             dirpath input format
    *             granule size for unload
    *             dirpath value for whether the dpapi should wait for lock 
    */  

  /* Get the dirpath mode */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&mode,(ub4 *)0,
                       OCI_ATTR_DIRPATH_MODE, ctlp->errhp_ctl));

  /* Verify the mode. */
  if (mode != load_mode)
  {
    fprintf(output_fp,
            "ERROR: expected direct path load mode of %d, got %d\n",
             load_mode, mode);
  }

   /* Get the number of rows loaded */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       &rows_loaded,(ub4 *)0,
                       OCI_ATTR_NUM_ROWS, ctlp->errhp_ctl));  

 /* Then write this value back to the attribute - expect error on return*/
  
  ociret = OCIAttrSet((dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       &rows_loaded, (ub4)0,
                       OCI_ATTR_NUM_ROWS, ctlp->errhp_ctl);

   /* Get the attribute name */
   OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid **)&attrp_nam, &attr_nam_len,
                       OCI_ATTR_NAME, ctlp->errhp_ctl));


    /* Get the character set id */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&ret_charsetid,(ub4 *)0,
                       OCI_ATTR_CHARSET_ID, ctlp->errhp_ctl));

    /* Verify the character set id. */
    if (charsetid != ret_charsetid)
    {
      fprintf(output_fp,
            "ERROR: expected character set id of %d, got %d\n",
             charsetid, ret_charsetid);
    }

    /* Is parallelism enabled? */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((dvoid *)(ctlp->dpctx_ctl), (ub4)OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&parallel_opt,
                       (ub4 *)0, (ub4)OCI_ATTR_DIRPATH_PARALLEL,
                       ctlp->errhp_ctl));

    if (tblp->parallel_tbl)  parallel_enabled = TRUE;

    /* Verify that the correct setting for parallel was returned. */
    if (parallel_opt != parallel_enabled)
    {
      fprintf(output_fp,
            "ERROR: expected parallel value of %d, got %d\n",
             parallel_enabled, parallel_opt);
    }

    /* Logging enabled? */
    OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((dvoid *)(ctlp->dpctx_ctl), (ub4)OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&nolog_opt, (ub4 *)0,
                       (ub4)OCI_ATTR_DIRPATH_NOLOG, ctlp->errhp_ctl));

    if (tblp->nolog_tbl) nolog_enabled = TRUE;

    /* Verify that the correct value for nolog was returned. */
    if (nolog_opt != nolog_enabled)
    {
      fprintf(output_fp,
            "ERROR: expected value of nolog to be %d, got %d\n",
             nolog_enabled, nolog_opt);
    }

  /* Get the version of the server interface for dirpath */
   OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&ret_version,(ub4 *)0,
                       OCI_ATTR_VERSION, ctlp->errhp_ctl));
                 

  /* Get the dirpath stream version */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&ret_stream_version,(ub4 *)0,
                       OCI_ATTR_DIRPATH_STREAM_VERSION, ctlp->errhp_ctl));

  /* Verify the stream version number. */
  if (stream_version != ret_stream_version)
  {
    fprintf(output_fp,
            "ERROR: expected direct path stream version of %d, got %d\n",
             stream_version, ret_stream_version);
  }

  /* Get the object constructor name */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
              OCIAttrGet((dvoid *)(ctlp->dpctx_ctl),
                         (ub4)OCI_HTYPE_DIRPATH_CTX,
                         (dvoid **) &ret_obj_constr_ptr, &ret_obj_constr_len,
                         (ub4)OCI_ATTR_DIRPATH_OBJ_CONSTR, ctlp->errhp_ctl));

    /* Verify the object constructor name.  First check string length. */
    if (ret_obj_constr_len != strlen((const char *)obj_constr_ptr))
    {
      fprintf(output_fp, "ERROR: bad object constructor name len\n");
      fprintf(output_fp, "\texpected %d, got %d\n",
                     strlen((const char *)obj_constr_ptr), ret_obj_constr_len);
    }

    if (strncmp((const char *)obj_constr_ptr, 
                (const char *)ret_obj_constr_ptr, (size_t)ret_obj_constr_len))
    {
      fprintf(output_fp,"*** ERROR *** bad object constructor name\n");
      fprintf(output_fp, "\texpected %s, got %s\n",
              (char *)obj_constr_ptr, (char *)ret_obj_constr_ptr);
    }

  /* Get the max. size of date cache */
  OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       (dvoid *)&dcache_size, (ub4 *)0,
                       OCI_ATTR_DIRPATH_DCACHE_SIZE, ctlp->errhp_ctl));

   /* Retrieve the SCN base and wrap values */
   OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       &scn_base, (ub4 *)0,
                       OCI_ATTR_SCN_BASE, ctlp->errhp_ctl));

   OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       &scn_wrap, (ub4 *)0,
                       OCI_ATTR_SCN_WRAP, ctlp->errhp_ctl));  

   /* Get the format of the input data (stream or text) */
   OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       &input_type, (ub4 *)0,
                       OCI_ATTR_DIRPATH_INPUT, ctlp->errhp_ctl));  

   /* Was it the size of a granule for an unload operation? */
   OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       &granule_size, (ub4 *)0,
                       OCI_ATTR_DIRPATH_GRANULE_SIZE, ctlp->errhp_ctl));  

   /* Is the dpapi going to wait to acquire a lock that is held? */
   OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)(ctlp->dpctx_ctl),
                       OCI_HTYPE_DIRPATH_CTX,
                       &lock_wait, (ub4 *)0,
                       OCI_ATTR_DIRPATH_LOCK_WAIT, ctlp->errhp_ctl));  

    /*******************************************************************/
    /* Using the direct path stream handle, fetch the stream version and
    *  offset 
    */

   /* First, the stream version */
   OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)(ctlp->dpstr_ctl),
                       OCI_HTYPE_DIRPATH_STREAM,
                       &out_stream_version, (ub4 *)0,
                       OCI_ATTR_VERSION, ctlp->errhp_ctl));

   /* Verify the stream version number. */
   if (stream_version != out_stream_version)
   {
     fprintf(output_fp,
      "ERROR: expected (using stream handle) a stream version of %d, got %d\n",
             stream_version, out_stream_version);
   }

    /* Now the stream offset */
   OCI_CHECK(ctlp->errhp_ctl, OCI_HTYPE_ERROR, ociret, ctlp,
            OCIAttrGet((CONST dvoid *)(ctlp->dpstr_ctl),
                       OCI_HTYPE_DIRPATH_STREAM,
                       &stream_offset, (ub4 *)0,
                       OCI_ATTR_STREAM_OFFSET, ctlp->errhp_ctl));


   /* Now the default case - generates an error since the attribute
                            type is not valid for this handle.  */

   ociret = OCIAttrGet((CONST dvoid *)(ctlp->dpstr_ctl),
                       OCI_HTYPE_DIRPATH_STREAM,
                       &stream_offset, (ub4 *)0,
                       OCI_ATTR_DATEFORMAT, ctlp->errhp_ctl);

           
}


/*
**++++++++++++++++++++++++++++++ reset_obj_ca +++++++++++++++++++++++++++++++++
**
**  Description:
**
** Function resets the column arrays for any objects or nested object columns.
**
**  Assumptions:
**
**  Parameters:
**
**  ctlp                           load control structure pointer 
**  tblp                           table pointer  
**  objp                           object pointer 
**
**  Returns:
**
**-------------------------------------------------------------------------
*/

STATICF void reset_obj_ca(ctlp, tblp, objp)
struct loadctl    *ctlp;                 /* load control structure */
struct tbl        *tblp;                 /* table descriptor */
struct obj        *objp;                 /* object descriptor */
{
  struct col   *colp;
  ub2 i;
  
  (void) OCIDirPathColArrayReset(objp->ca_obj, ctlp->errhp_ctl);

  objp->rowoff_obj = 0;

  /* check each column to see if it is an object, opaque or ref */
  /* and if so, recurse */
  for (i = 0, colp = objp->col_obj; i < objp->ncol_obj; i++, colp++)
  {
    if (colp->exttyp_col == SQLT_NTY || colp->exttyp_col == SQLT_REF)
    {
      reset_obj_ca(ctlp, tblp, colp->obj_col);
    }
  }

}

/* end of file cdemodp.c */
