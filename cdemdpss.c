#ifdef RCSID
static char *RCSid =
   "$Header: cdemdpss.c 15-aug-2006.10:07:47 jkaloger Exp $ ";
#endif /* RCSID */

/*
**      Copyright (c)  2001 Oracle Corporation.  All rights reserved.
*/

/* 
** Directions:
** 1. make -f demo_rdbms.mk build_dp EXE=cdemdpss OBJS=cdemdpss.o
**        makes the cdemdpss executable
** 2. sqlplus OE/OE @cdemdp9i.sql 
**        creates the Oracle 9.0 types tbl
** 3. cdemdpss < cdemdpss.dat
**        runs the executable
** 4. to retrieve data, select from "dp_api_demo1" as "OE/OE"
**
*/

/*
   NAME
     cdemdpss.c - An example C program that loads a sql string 
                        via Direct Path API.

   DESCRIPTION
      A client module to be used with the cdemodp.c driver.

   NOTES
     This module loads selected types as defined in the Sample Schema.
     Refer to Cdemdp9i.sql for more information on the use of the
     Sample Schema with the DP API demo modules.

   IMPORTANT
     Argument names to opaques and sql strings must be unique.


   MODIFIED   (MM/DD/YY)
   jkaloger     08/15/06 - Lowercase passwords for secure verifiers project
   cmlim        09/11/01 - fix lint
   eegolf       04/04/01 - Merged eegolf_demo_update
   eegolf       03/28/01 - Creation
 */

#include <sys/types.h>
#include <oci.h>
#include <cdemodp.h>


externdef struct obj inventory_stock_date =
{
  (text *) "sysdate", 0, 0, 0, 0, 0, 
  (OCIDirPathFuncCtx *)0, (OCIDirPathColArray *)0, OBJ_OPQ};



externdef struct col  objx_02_col [] =
{
  {
    (text *)"name_str2_arg1", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0, (struct obj *) 0
  }
};

externdef struct fld  objx_02_fld [] =
{
  { 66, 67, 2, FLD_INLINE|FLD_STRIP_TRAIL_BLANK }      /* name_str2, arg1 */
};



externdef struct obj name_str2 =
{
  (text *) "lower(add_months(sysdate, :name_str2_arg1))", 1,  objx_02_col,  
  objx_02_fld, 0, 0, (OCIDirPathFuncCtx *)0, (OCIDirPathColArray *)0, OBJ_OPQ};


externdef struct col  objx_01_col [] =
{
  {
    (text *)"name_str_arg1", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0, (struct obj *) 0
  },
  {
    (text *)"name_str_arg2", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0, (struct obj *) 0
  },
  {
    (text *)"name_str_arg3", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0, (struct obj *) 0
  }
};

externdef struct fld  objx_01_fld [] =
{
  { 36, 55,20, FLD_INLINE|FLD_STRIP_TRAIL_BLANK },    /* name_str, arg1 */
  { 56, 60, 5, FLD_INLINE|FLD_STRIP_TRAIL_BLANK },    /* name_str, arg2 */
  { 61, 65, 5, FLD_INLINE|FLD_STRIP_TRAIL_BLANK }     /* name_str, arg3 */
};

externdef struct obj name_str =
{
  (text *) "substr(:name_str_arg1, :name_str_arg2, :name_str_arg3)", 3, 
  objx_01_col, objx_01_fld, 0, 0, 
  (OCIDirPathFuncCtx *)0, (OCIDirPathColArray *)0, OBJ_OPQ};


externdef struct col column[] =
{
  {
    (text *)"cust_first_name", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0
  },
  {
    (text *)"cust_last_name", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0
  },
  {
    (text *)"inventory_string", 0, SQLT_NTY, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0, & name_str 
  },
  {
    (text *)"inventory_string2", 0, SQLT_NTY, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0, & name_str2 
  },
  {
    (text *)"inventory_stock_date", 0, SQLT_NTY, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0, & inventory_stock_date
  }
};

/* Field descriptor which maps one-to-one with the column descriptor.
 * For this simple example, fields are strictly positional within
 * an input record.
 */
externdef struct fld field[] =
{
  { 1, 10, 10, FLD_INLINE|FLD_STRIP_TRAIL_BLANK },   /* cust_first_name */
  { 11,20, 10, FLD_INLINE|FLD_STRIP_TRAIL_BLANK },   /* cust_last_name */
  { 21,25,  5, FLD_INLINE|FLD_STRIP_TRAIL_BLANK },   /* inventory_string */
  { 26,30,  5, FLD_INLINE|FLD_STRIP_TRAIL_BLANK },   /* inventory_string2 */
  { 31,35,  5, FLD_INLINE|FLD_STRIP_TRAIL_BLANK }    /* inventory_stock_date */
};


externdef struct tbl table =
{
  (text *)"OE",                                          /* table owner */
  (text *)"dp_api_demo1",                                 /* table name */
  (text *)""         ,             /* subname (partition, subpartition) */
  (ub2)(sizeof(column) / sizeof(struct col)),      /* number of columns */
  (text *)"DD-MON-YY",                           /* default date format */
  (struct col *)(&column[0]),                     /* column descriptors */
  (struct fld *)(&field[0]),                       /* field descriptors */
  (ub1)0,                                                   /* parallel */
  (ub1)0,                                                      /* nolog */
  (ub4)(64 * 1024)                              /* transfer buffer size */
};

externdef struct sess session =
{
  (text *)"oe",                                              /* user */
  (text *)"oe",                                            /* passwd */
  (text *)"",                                          /* instance name */
  (text *)0,                   /* output file name; NULL implies stderr */
  (ub4)130                                   /* max input record length */
};


/* end of file cdemdpss.c */

