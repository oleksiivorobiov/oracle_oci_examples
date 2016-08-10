#ifdef RCSID
static char *RCSid =
   "$Header: cdemdpit.c 15-aug-2006.10:06:05 jkaloger Exp $ ";
#endif /* RCSID */

/*
**      Copyright (c)  2001 Oracle Corporation.  All rights reserved.
*/

/* 
** Directions:
** 1. make -f demo_rdbms.mk build_dp EXE=cdemdpit OBJS=cdemdpit.o
**        makes the cdemdpit executable
** 2. sqlplus OE/OE @cdemdp9i.sql 
**        creates the Oracle 9.0 types tbl
** 3. cdemdpit < cdemdpit.dat
**        runs the executable
** 4. to retrieve data, select from "dp_api_demo3" as "OE/OE"
**
*/

/*
   NAME
     cdemdpit.c - An example C program that loads an object table, 
     of a non final type corporate_customer_typ, via direct path API.
     In this example the OID is being loaded explicitly.

   DESCRIPTION
      A client module to be used with the cdemodp.c driver.

   NOTES
     This module loads selected types as defined in the Sample Schema.
     Refer to Cdemdp9i.sql for more information on the use of the
     Sample Schema with the DP API demo modules.

   MODIFIED   (MM/DD/YY)
   jkaloger     08/15/06 - Lowercase passwords for secure verifiers project
   cmlim        09/11/01 - fix lint
   cmlim        04/20/01 - remove TBL_SUBST_OBJ flg
   eegolf       04/04/01 - Merged eegolf_demo_update
   eegolf       03/29/01 - Creation
 */

#include <sys/types.h>
#include <oci.h>
#include <cdemodp.h>


externdef struct col column[] =
{
  {
    (text *)"cust_oid", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0, (struct obj *) 0, (ub4)COL_OID
  },
  {
    (text *)"cust_first_name", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0, (struct obj *) 0, (ub4)0
  },
  {
    (text *)"cust_last_name", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0, (struct obj *) 0, (ub4)0
  },
  {
    (text *)"account_mgr_id", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0, (struct obj *) 0, (ub4)0 
  }
};

/* Field descriptor which maps one-to-one with the column descriptor.
 * For this simple example, fields are strictly positional within
 * an input record.
 */
externdef struct fld field[] =
{
  { 1, 32, 32, FLD_INLINE|FLD_STRIP_TRAIL_BLANK },       /* cust_oid */
  { 33,52, 20, FLD_INLINE|FLD_STRIP_TRAIL_BLANK },       /* cust_first_name */
  { 53,72, 20, FLD_INLINE|FLD_STRIP_TRAIL_BLANK },       /* cust_last_name */
  { 73,78,  6, FLD_INLINE|FLD_STRIP_TRAIL_BLANK }        /* account_mgr_id */
};


externdef struct tbl table =
{
  (text *)"OE",                                          /* table owner */
  (text *)"dp_api_demo3",                                 /* table name */
  (text *)""         ,             /* subname (partition, subpartition) */
  (ub2)(sizeof(column) / sizeof(struct col)),      /* number of columns */
  (text *)"DD-MON-YY",                           /* default date format */
  (struct col *)(&column[0]),                     /* column descriptors */
  (struct fld *)(&field[0]),                       /* field descriptors */
  (ub1)0,                                                   /* parallel */
  (ub1)0,                                                      /* nolog */
  (ub4)(64 * 1024),                             /* transfer buffer size */
  (text *)"corporate_customer_typ"      /* derived object type to be loaded */
};

externdef struct sess session =
{
  (text *)"oe",                                              /* user */
  (text *)"oe",                                            /* passwd */
  (text *)"",                                          /* instance name */
  (text *)0,                   /* output file name; NULL implies stderr */
  (ub4)130                                   /* max input record length */
};


/* end of file cdemdpit.c */

