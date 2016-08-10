#ifdef RCSID
static char *RCSid =
   "$Header: cdemo6.cc 18-feb-2005.06:13:14 sudsrini Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1991, 2005, Oracle. All rights reserved.  
*/

/*
   NAME
     cdemo1.cc - C++ Demo Program
   MODIFIED   (MM/DD/YY)
    sudsrini   02/18/05  - include stdlib
    slari      08/08/01  - b1737025: move extern declaration
    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
    akatti     05/19/98 -  [493012]:bug fix to prevent core dump
    dchatter   07/15/96 -  hda is a ub4 array to prevent bus error
    slari      04/19/95 -  replace orlon with olog
    skrishna   10/20/94 -  Creation
*/
/*
 *      -- cdemo6.cc --
 *  An example program which illustrates how a C++ program
 *  can use the OCI interface to access ORACLE database.
 *
 *  This program retrieves department name, given the
 *  department number.
 *
 *  The program queries the user for data as follows:
 *
 *  Enter department number:
 *
 *  The program terminates if -1 is entered
 *  when the department number is requested.
 */
#include <stdio.h>
#include <stdlib.h>
#include <oratypes.h>

extern "C"
{
#include <ociapr.h>
/* demo constants and structs */
}

#include <ocidem.h>
#include <cdemo6.h>

const text *username = (text *) "SCOTT";
const text *password = (text *) "TIGER";

/* define SQL statements to be used in the program */
const text *seldept = (text *) "SELECT dname FROM dept WHERE deptno = :1";

void err_report(FILE *file, text *errmsg, sword func_code);
void myfflush();

/* connection destructor */
connection::~connection()
{
  // disconnect if connection exists
  if (state == connected)
  {
    if (disconnect())
    {
      display_error(stderr);
    }
  }
}

/* connect to ORACLE */
sword connection::connect(const text *username, const text *password)
{
  sword status;

  if (state == connected)
  {
    // this object is already connected
    return (CONERR_ALRCON);
  }

  if ((status = olog(&lda, hda, (text *) username, -1,
                     (text *)password, -1, (text *) 0, -1,
                     OCI_LM_DEF)) == 0)
  {
    // successful login
    state = connected;
    printf("Connected to ORACLE as %s\n", username);
  }

  return (status);
}

/* disconnect from ORACLE */
sword connection::disconnect()
{
  sword status;

  if (state == not_connected)
  {
    // this object has not been connected
    return (CONERR_NOTCON);
  }

  if ((status = ologof(&lda)) == 0)
  {
    // successful logout
    state = not_connected;
  }

  return (status);
}

/* write error message to the given file */
void connection::display_error(FILE *file) const
{
  if (lda.rc != 0)
  {
    sword n;
    text msg[512];

    n = oerhms((cda_def *)&lda, lda.rc, msg, (sword) sizeof(msg));
    err_report(file, msg, lda.fc);
  }
}

/* cursor destructor */
cursor::~cursor()
{
  if (state == opened)
  {
    if (close())
      display_error(stderr);
  }
}

/* open the cursor */
sword cursor::open(connection *conn_param)
{
  sword status;

  if (state == opened)
  {
    // this cursor has already been opened
    return (CURERR_ALROPN);
  }

  if ((status = oopen(&cda, &conn_param->lda, (text *)0, -1, -1,
                      (text *)0, -1)) == 0)
  {
    // successfull open
    state = opened;
    conn = conn_param;
  }

  return (status);
}

/* close the cursor */
sword cursor::close()
{
  sword status;

  if (state == not_opened)
  {
    // this cursor has not been opened
    return (CURERR_NOTOPN);
  }

  if ((status = oclose(&cda)) == 0)
  {
    // successful cursor close
    state = not_opened;
    conn = (connection *)0;
  }

  return (status);
}

/* write error message to the given file */
void cursor::display_error(FILE *file) const
{
  if (cda.rc != 0)
  {
    sword n;
    text msg[512];

    n = oerhms(&conn->lda, cda.rc, msg, (sword) sizeof(msg));
    err_report(file, msg, cda.fc);
  }
}

int main()
{
    sword deptno;
    sword len, dsize;
    sb4   deptlen;
    sb2   db_type;
    sb1   name_buf[20];
    text  *dept;

/*
 *  Connect to ORACLE and open a cursor.
 *  Exit on any error.
 */
    connection conn;
    if (conn.connect(username, password))
    {
      conn.display_error(stderr);
      return(EXIT_FAILURE);
    }

    cursor crsr;
    if (crsr.open(&conn))
    {
      crsr.display_error(stderr);
      return(EXIT_FAILURE);
    }

    /* parse the SELDEPT statement */
    if (crsr.parse(seldept))
    {
      crsr.display_error(stderr);
      return(EXIT_FAILURE);
    }

    /* bind the placeholder in the SELDEPT statement */
    if (crsr.bind_by_position(1, (ub1 *) &deptno, (sword) sizeof(deptno),
                              INT_TYPE, -1, (sb2 *) 0))
    {
      crsr.display_error(stderr);
      return(EXIT_FAILURE);
    }

    /* describe the select-list field "dname" */
    len = sizeof (name_buf);
    if (crsr.describe(1, (sb4 *) &deptlen, &db_type,
                      name_buf, (sb4 *) &len, (sb4 *) &dsize, (sb2 *) 0,
                      (sb2 *) 0, (sb2 *) 0))
    {
      crsr.display_error(stderr);
      return(EXIT_FAILURE);
    }

    /* allocate space for dept name now that you have length */
    dept = new text[(int) deptlen + 1];

    /* define the output variable for the select-list */
    if (crsr.define_by_position(1, (ub1 *) dept, (sword)deptlen+1,
                               STRING_TYPE, -1, (sb2 *) 0, (ub2 *) 0,
                               (ub2 *) 0))
    {
      crsr.display_error(stderr);
      delete dept;
      return(EXIT_FAILURE);
    }

    for (;;)
    {
        /* prompt for department number, break if given number == -1 */
        printf("\nEnter department number (or -1 to EXIT): ");
        while (scanf("%d", &deptno) != 1)
        {
          myfflush();
          printf("Invalid input, please enter a number (-1 to EXIT): ");
        }
        if (deptno == -1)
        {
          printf("Exiting... ");
          break;
        }

        /* display the name of the corresponding department */
        if (crsr.execute() || crsr.fetch())
        {
          if (crsr.get_error_code() != NO_DATA_FOUND)
          {
            crsr.display_error(stderr);
            delete dept;
            return(EXIT_FAILURE);
          }
          else
            printf(
                "\n  The department number that you entered doesn't exist.\n");
        }
        else
        {
          printf("\n  Department name = %s    Department number = %d\n",
                 dept, deptno);
        }
    }

    delete dept;
    printf ("\nG'day\n");

  return 0;
}


void err_report(FILE *file, text *errmsg, sword func_code)
{
    fprintf(file, "\n-- ORACLE error--\n\n%s\n", errmsg);
    if (func_code > 0)
        fprintf(file, "Processing OCI function %s\n",
            oci_func_tab[func_code]);
}

void myfflush()
{
  eb1 buf[50];

  fgets((char *) buf, 50, stdin);
}


