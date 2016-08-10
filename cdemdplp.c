#ifdef RCSID
static char *RCSid =
   "$Header: cdemdplp.c 15-aug-2006.10:06:32 jkaloger Exp $ ";
#endif /* RCSID */

/* Copyright (c) 2001, 2006, Oracle. All rights reserved.  */

/*
 *      -- cdemdplp.c --
 * An example program that loads data via direct path api.
 *
 * Directions:
 * 1. make -f demo_rdbms.mk build_dp EXE=cdemdplp OBJS=cdemdplp.o
 *        makes the cdemdplp executable
 * 2. sqlplus scott/tiger @cdemdplp.sql 
 *        creates lineitem_dp tbl
 * 3. cdemdplp < cdemdplp.dat
 *        runs executable
 * 4. to retrieve data, select from "lineitem_dp" as "scott/tiger"
 *
 */

/*
   NAME
     cdemdplp.c - C Demo for Direct Path api for LineItem Partitioned
                     table.

   DESCRIPTION
     - A client module describing lineitem partition table.
       To be used with cdemodp.c driver prog.

   NOTES
     Loads the lineitem1 partition of the lineitem table.

   MODIFIED   (MM/DD/YY)
   jkaloger    08/15/06 - Lowercase passwords for secure verifiers project
   eegolf      12/01/03 - correct size 7 to 17 for l_shipinstruct
   cmlim       09/11/01 - fix lint
   eegolf      04/04/01 - Merged eegolf_demo_update
   eegolf      03/04/01 - Updated for 9i 
   cmlim       09/16/98 - Creation
 */

#include <sys/types.h>
#include <oci.h>
#include <cdemodp.h>

externdef struct col column[] =
{
  {
    (text *)"l_orderkey", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0
  },
  {
    (text *)"l_partkey", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0
  },
  {
    (text *)"l_suppkey", 0, SQLT_CHR,  (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0
  },
  {
    (text *)"l_linenumber", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0
  },
  {
    (text *)"l_quantity", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0
  },
  {
    (text *)"l_extendedprice", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0
  },
  {
    (text *)"l_discount", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0
  },
  {
    (text *)"l_tax", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0
  },
  {
    (text *)"l_returnflag", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0
  },
  {
    (text *)"l_linestatus", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0
  },
  {
    (text *)"l_shipdate", 0, SQLT_CHR, (text *) "DD-MON-RR",
    (sword)0, (sword)0, (ub2)0, (ub1)1
  },
  {
    (text *)"l_commitdate", 0, SQLT_CHR, (text *) "DD-MON-RR",
    (sword)0, (sword)0, (ub2)0, (ub1)1
  },
  {
    (text *)"l_receiptdate", 0, SQLT_CHR, (text *) "DD-MON-RR",
    (sword)0, (sword)0, (ub2)0, (ub1)1
  },
  {
    (text *)"l_shipinstruct", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0
  },
  {
    (text *)"l_shipmode", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0
  },
  {
    (text *)"l_comment", 0, SQLT_CHR, (text *)0,
    (sword)0, (sword)0, (ub2)0, (ub1)0
  }
};

/* Field descriptor which maps one-to-one with the column descriptor.
 * For this simple example, fields are strictly positional within
 * an input record.
 */
externdef struct fld field[] =
{
  {  1,  6,  6, FLD_INLINE },                             /* l_orderkey      */
  {  7, 11,  5, FLD_INLINE },                             /* l_partkey       */
  { 12, 15,  4, FLD_INLINE },                             /* l_suppkey       */
  { 16, 16,  1, FLD_INLINE },                             /* l_linenumber    */
  { 17, 18,  2, FLD_INLINE },                             /* l_quantity      */
  { 19, 26,  8, FLD_INLINE },                             /* l_extendedprice */
  { 27, 29,  3, FLD_INLINE },                             /* l_discount      */
  { 30, 32,  3, FLD_INLINE },                             /* l_tax           */
  { 33, 33,  1, FLD_INLINE },                             /* l_returnflag    */
  { 34, 34,  1, FLD_INLINE },                             /* l_linestatus    */
  { 35, 43,  9, FLD_INLINE },                             /* l_shipdate      */
  { 44, 52,  9, FLD_INLINE },                             /* l_commitdate    */
  { 53, 61,  9, FLD_INLINE },                             /* l_receiptdate   */
  { 62, 78, 17, FLD_INLINE|FLD_STRIP_TRAIL_BLANK },       /* l_shipinstruct  */
  { 79, 85,  7, FLD_INLINE|FLD_STRIP_TRAIL_BLANK },       /* l_shipmode      */
  { 86,128, 43, FLD_INLINE|FLD_STRIP_TRAIL_BLANK },       /* l_comment       */
};

/* Note setting of subname field */
externdef struct tbl table =
{
  (text *)"scott",                                            /* table owner */
  (text *)"lineitem_dp",                                       /* table name */
  (text *)"lineitem1",                  /* subname (partition, subpartition) */
  (ub2)(sizeof(column) / sizeof(struct col)),           /* number of columns */
  (text *)"DD-MON-YY",                                /* default date format */
  (struct col *)(&column[0]),                          /* column descriptors */
  (struct fld *)(&field[0]),                            /* field descriptors */
  (ub1)0,                                                        /* parallel */
  (ub1)0,                                                           /* nolog */
  (ub4)(64 * 1024)                                   /* transfer buffer size */
};

externdef struct sess session =
{
  (text *)"scott",                                                   /* user */
  (text *)"tiger",                                                 /* passwd */
  (text *)"",                                               /* instance name */
  (text *)0,                        /* output file name; NULL implies stderr */
  (ub4)130                                        /* max input record length */
};


/* end of file cdemdplp.c */

