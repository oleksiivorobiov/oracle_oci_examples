/* Copyright (c) 1991, 2006, Oracle. All rights reserved.  
*/

/*
   NAME
     cdemo2.c - C demo Program # 2
   MODIFIED   (MM/DD/YY)
    kmohan     03/28/06  - change hda to size_t
    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
    ehayes     05/29/97 -  Fix olint errors
    echen      03/11/97 -  fix a test bug
    dchatter   01/05/96 -  Creation of the Solaris version with hda as
                           ub4 array of HDA_SIZE/sizeof(ub4) for alignment
                           reasons.
*/

/* This program accepts arbitrary SQL statements from the user,
   and processes the statement.  Statements may be entered on
   multiple lines, and must be terminated by a semi-colon.
   If a query, the results are printed.

   Statements are entered at the OCISQL prompt.

   To quit the program, type EXIT at the OCISQL prompt.

   The size of the HDA is defined by the HDA_SIZE constant,
   which is declared in ocidem.h to be 256 bytes for 32-
   bit architectures and 512 bytes for 64-bit architectures.
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>


/* Include OCI-specific headers. */
#include <oratypes.h>
#include <ocidfn.h>
#ifdef __STDC__
#include <ociapr.h>
#else
#include <ocikpr.h>
#endif
#include <ocidem.h>


/* Constants used in this program. */
#define MAX_BINDS               12
#define MAX_ITEM_BUFFER_SIZE    33
#define MAX_SELECT_LIST_SIZE    12
#define MAX_SQL_IDENTIFIER      31

#define PARSE_NO_DEFER           0
#define PARSE_V7_LNG             2


/* Define one logon data area and one cursor data area
   Also define a host data area for olog.
   (See ocidfn.h for declarations). */
Lda_Def lda;
Cda_Def cda;
size_t  hda[HDA_SIZE/sizeof(size_t)];

/* Declare an array of bind values. */
text bind_values[MAX_BINDS][MAX_ITEM_BUFFER_SIZE];

/* Declare structures for query information. */
struct describe
{
    sb4             dbsize;
    sb2             dbtype;
    sb1             buf[MAX_ITEM_BUFFER_SIZE];
    sb4             buflen;
    sb4             dsize;
    sb2             precision;
    sb2             scale;
    sb2             nullok;
};

struct define
{
    ub1             buf[MAX_ITEM_BUFFER_SIZE];
    float           flt_buf;
    sword           int_buf;
    sb2             indp;
    ub2             col_retlen, col_retcode;
};


/* Define arrays of describe and define structs. */
struct describe desc[MAX_SELECT_LIST_SIZE];
struct define   def[MAX_SELECT_LIST_SIZE];

/*  Declare this programs functions. */
sword  connect_user();
sword  describe_define();
sword  do_binds();
void   do_exit();
void   oci_error();
sword  get_sql_statement();
void   print_header();
void   print_rows();

/* Globals */
static text sql_statement[2048];
static sword sql_function;
static sword numwidth = 8;


main()
{
    sword col, errno, n, ncols;
    text *cp;

    /* Connect to ORACLE. */
    if (connect_user())
        exit(-1);

    /* Open a cursor, exit on error (unrecoverable). */
    if (oopen(&cda, &lda, (text *) 0, -1, -1, (text *) 0, -1))
    {
        printf("Error opening cursor.  Exiting...\n");
        ologof(&lda);
        exit(-1);
    }

    /* Process user's SQL statements. */

    for (;;)
    {
        /* Get the statement, exit on "exit". */
        if (get_sql_statement()) {
            printf("\n");
            do_exit(0);
        }

        /* Parse the statement; do not defer the parse,
           so that errors come back right away. */
        if (oparse(&cda, (text *) sql_statement, (sb4) -1,
                 (sword) PARSE_NO_DEFER, (ub4) PARSE_V7_LNG))
        {
            oci_error(&cda);
            continue;
        }

        /* Save the SQL function code right after parse. */
        sql_function = cda.ft;

        /* Bind any input variables. */
        if ((ncols = do_binds(&cda, sql_statement)) == -1)
            continue;

        /* If the statement is a query, describe and define
           all select-list items before doing the oexec. */
        if (sql_function == FT_SELECT)
            if ((ncols = describe_define(&cda)) == -1)
                continue;

        /* Execute the statement. */
        if (oexec(&cda))
        {
            oci_error(&cda);
            continue;
        }

        /* Fetch and display the rows for the query. */
        if (sql_function == FT_SELECT)
        {
            print_header(ncols);
            print_rows(&cda, ncols);
        }

        /* Print the rows-processed count. */
        if (sql_function == FT_SELECT ||
            sql_function == FT_UPDATE ||
            sql_function == FT_DELETE ||
            sql_function == FT_INSERT)
            printf("\n%d row%c processed.\n", cda.rpc,
                   cda.rpc == 1 ? ' ' : 's');
        else
            printf("\nStatement processed.\n");

    } /* end for (;;) */


}     /* end main() */


sword
connect_user()
{
    text username[132];
    text password[132];
    sword n;

    /* Three tries to connect. */
    for (n = 3; --n >= 0; )
    {
        printf("Username: ");
        gets((char *) username);
        printf("Password: ");
        gets((char *) password);

        if (olog(&lda, (ub1 *)hda, username, -1, password, -1,
                 (text *) 0, -1, (ub4)OCI_LM_DEF))
        {
            printf("Cannot connect as %s.\n", username);
            printf("Try again.\n\n");
        }
        else
        {
            return 0;
        }
    }
    printf("Connection failed.  Exiting...\n");
    return -1;
}


/*  Describe select-list items. */

sword
describe_define(cda)
Cda_Def *cda;
{
    sword col, deflen, deftyp;
    static ub1 *defptr;

    /* Describe the select-list items. */
    for (col = 0; col < MAX_SELECT_LIST_SIZE; col++)
    {
        desc[col].buflen = MAX_ITEM_BUFFER_SIZE;
        if (odescr(cda, col + 1, &desc[col].dbsize,
                   &desc[col].dbtype, &desc[col].buf[0],
                   &desc[col].buflen, &desc[col].dsize,
                   &desc[col].precision, &desc[col].scale,
                   &desc[col].nullok))
        {
            /* Break on end of select list. */
            if (cda->rc == VAR_NOT_IN_LIST)
                break;
            else
            {
                oci_error(cda);
                return -1;
            }
        }
        /* adjust sizes and types for display */
        switch (desc[col].dbtype)
        {
        case NUMBER_TYPE:
            desc[col].dbsize = numwidth;
            /* Handle NUMBER with scale as float. */
            if (desc[col].scale != 0)
            {
                defptr = (ub1 *) &def[col].flt_buf;
                deflen = (sword) sizeof(float);
                deftyp = FLOAT_TYPE;
                desc[col].dbtype = FLOAT_TYPE;
            }
            else
            {
                defptr = (ub1 *) &def[col].int_buf;
                deflen = (sword) sizeof(sword);
                deftyp = INT_TYPE;
                desc[col].dbtype = INT_TYPE;
            }
            break;
        default:
            if (desc[col].dbtype == DATE_TYPE)
                desc[col].dbsize = 9;
            if (desc[col].dbtype == ROWID_TYPE)
                desc[col].dbsize = 18;
            defptr = def[col].buf;
            deflen = desc[col].dbsize > MAX_ITEM_BUFFER_SIZE ?
              MAX_ITEM_BUFFER_SIZE : desc[col].dbsize + 1;
            deftyp = STRING_TYPE;
            break;
        }
        if (odefin(cda, col + 1,
                   defptr, deflen, deftyp,
                   -1, &def[col].indp, (text *) 0, -1, -1,
                   &def[col].col_retlen,
                   &def[col].col_retcode))
        {
            oci_error(cda);
            return -1;
        }
    }
    return col;
}


/*  Bind input variables. */

sword
do_binds(cda, stmt_buf)
Cda_Def *cda;
text *stmt_buf;
{
    sword i, in_literal, n;
    text *cp, *ph;

    /* Find and bind input variables for placeholders. */
    for (i = 0, in_literal = FALSE, cp = stmt_buf;
              *cp && i < MAX_BINDS; cp++)
    {
        if (*cp == '\'')
            in_literal = ~in_literal;
        if (*cp == ':' && !in_literal)
        {
            for (ph = ++cp, n = 0;
                 *cp && (isalnum(*cp) || *cp == '_')
                     && n < MAX_SQL_IDENTIFIER;
                 cp++, n++
                )
                ;
            *cp = '\0';
            printf("Enter value for %s: ", ph);
            gets((char *) &bind_values[i][0]);

            /* Do the bind, using obndrv().
               NOTE:  the bind variable address must be static.
               This would not work if bind_values were an
               auto on the do_binds stack. */
            if (obndrv(cda, ph, -1, (ub1 *)&bind_values[i][0], -1,
                       VARCHAR2_TYPE,
                       -1, (sb2 *) 0, (text *) 0, -1, -1))
            {
                oci_error(cda);
                return -1;
            }
            i++;
        }   /* end if (*cp == ...) */
    }       /* end for () */
    return i;
}



/*  Clean up and exit.  LDA and CDA are
    global. */
void
do_exit(rv)
sword rv;
{
    if (oclose(&cda))
        fputs("Error closing cursor!\n", stdout);
    if (ologof(&lda))
        fputs("Error logging off!\n", stdout);
    exit(rv);
}


void
oci_error(cda)
Cda_Def *cda;
{
    text msg[512];
    sword n;

    fputs("\n-- ORACLE ERROR --\n", stderr);
    n = oerhms(&lda, cda->rc, msg, (sword) sizeof (msg));
    fprintf(stderr, "%.*s", n, msg);
    fprintf(stderr, "Processing OCI function %s\n",
            oci_func_tab[cda->fc]);
    fprintf(stderr, "Do you want to continue? [yn]: ");
    fgets((char *) msg, (int) sizeof (msg), stdin);
    if (*msg != '\n' && *msg != 'y' && *msg != 'Y')
        do_exit(1);
    fputc('\n', stdout);
}


sword
get_sql_statement()
{
    text cbuf[1024];
    text *cp;
    sword stmt_level;


    for (stmt_level = 1; ;)
    {
        if (stmt_level == 1)
        {
            /* Init statement buffer and print prompt. */
            *sql_statement = '\0';
            fputs("\nOCISQL> ", stdout);
        }
        else
        {
            printf("%3d     ", stmt_level);
        }

        /* Get (part of) a SQL statement. */
        gets((char *) cbuf);
        if (*cbuf == '\0')
            continue;
        if (strncmp((char *) cbuf, "exit", 4) == 0)
            return -1;

        /* Concatenate to statement buffer. */
        if (stmt_level > 1)
            strcat((char *) sql_statement, " ");
        strcat((char *) sql_statement, (char *) cbuf);

        /* Check for possible terminator. */
        cp = &sql_statement[strlen((char *) sql_statement) - 1];

        while (isspace(*cp))
            cp--;
        if (*cp == ';')
        {
            *cp = '\0';
            break;
        }
        stmt_level++;
    }
    return 0;
}


void
print_header(ncols)
sword ncols;
{
    sword col, n;

    fputc('\n', stdout);

    for (col = 0; col < ncols; col++)
    {
        n = desc[col].dbsize - desc[col].buflen;
        if (desc[col].dbtype == FLOAT_TYPE ||
            desc[col].dbtype == INT_TYPE)
        {
            printf("%*c", n, ' ');
            printf("%*.*s", desc[col].buflen,
                   desc[col].buflen, desc[col].buf);
        }
        else
        {
            printf("%*.*s", desc[col].buflen,
                   desc[col].buflen, desc[col].buf);
            printf("%*c", n, ' ');
        }
        fputc(' ', stdout);
    }
    fputc('\n', stdout);

    for (col = 0; col < ncols; col++)
    {
        for (n = desc[col].dbsize; --n >= 0; )
            fputc('-', stdout);
        fputc(' ', stdout);
    }
    fputc('\n', stdout);
}


void
print_rows(cda, ncols)
Cda_Def *cda;
sword ncols;
{
    sword col, n;

    for (;;)
    {
        fputc('\n', stdout);
        /* Fetch a row.  Break on end of fetch,
           disregard null fetch "error". */
        if (ofetch(cda))
        {
            if (cda->rc == NO_DATA_FOUND)
                break;
            if (cda->rc != NULL_VALUE_RETURNED)
                oci_error(cda);
        }
        for (col = 0; col < ncols ; col++)
        {
            /* Check col. return code for null.  If
               null, print n spaces, else print value. */
            if (def[col].indp < 0)
                printf("%*c", desc[col].dbsize, ' ');
            else
            {
                switch (desc[col].dbtype)
                {
                case FLOAT_TYPE:
                    printf("%*.*f", numwidth, 2, def[col].flt_buf);
                    break;
                case INT_TYPE:
                    printf("%*d", numwidth, def[col].int_buf);
                    break;
                default:
                    printf("%s", def[col].buf);
                    n = desc[col].dbsize - strlen((char *) def[col].buf);
                    if (n > 0)
                        printf("%*c", n, ' ');
                    break;
                }
            }
            fputc(' ', stdout);
        }
    }  /* end for (;;) */
}

