      *
      * $Header: cbdem2.cob 14-jul-99.14:31:06 mjaeger Exp $
      *
      * Copyright (c) 1991, 1999, Oracle Corporation.  All rights reserved.
      *
      *   NAME
      *     cbdem2.cob - Cobol demo program #2
      *   MODIFIED   (MM/DD/YY)
      *    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
      *    plocke     11/14/95 -  to update for v7.3
      *    dchatter   07/20/95 -  merge changes from branch 1.1.720.1
      *    dchatter   04/14/95 -  fix test to work on sun
      *    sjain      03/16/92 -  Creation
      *
      * The program CBDEM2 accepts SQL statements from the
      * user at run time and processes them.

      * If the statement was a Data Definition Language (DDL),
      * Data Control Language (DCL), or Data Manipulation
      * Language (DML) statement, it is parsed and executed,
      * and the next statement is retrieved.  (Note that
      * performing the execute step for a DDL or DCL statement
      * is not necessary, but it does no harm, and simplifies
      * the program logic.)

      * If the statement was a query, the program describes
      * the select list, and defines output variables of the
      * appropriate type and size, depending on the internal
      * datatype of the select-list item.

      * Then, each row of the query is fetched, and the results
      * are displayed.

      * To keep the size of this example program to a
      * reasonable limit for this book, the following
      * restrictions are present:

      * (1) The SQL statement can contain only 25 elements (words
      *   and punctuation), and must be entered on a single line.
      *   There is no terminating ';'.
      * (2) A maximum of 8 bind (input) variables is permitted.
      *   Additional input variables are not bound, which will
      *   cause an error at execute time.  Input values must be
      *   enterable as character strings
      *   (numeric or alphanumeric).
      *   Placeholders for bind variables are :bv,
      *   as for OBNDRV.
      * (3) A maximum of 8 select-list items per table are
      *   described and defined.  Additional columns are
      *   not defined, which will cause unpredictable behavior
      *   at fetch time.
      * (4) Not all internal datatypes are handled for queries.
      *   Selecting a RAW or LONG column could cause problems.


       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CBDEM2.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Logon, cursor, and host data areas.
       01  LDA.
           02      LDA-V2RC    PIC S9(4) COMP.
           02      FILLER      PIC X(10).
           02      LDA-RC      PIC S9(4) COMP.
           02      FILLER      PIC X(50).
       01  CDA.
           02      C-V2RC      PIC S9(4) COMP.
           02      C-TYPE      PIC S9(4) COMP.
           02      C-ROWS      PIC S9(9) COMP.
           02      C-OFFS      PIC S9(4) COMP.
           02      C-FNC       PIC S9(4) COMP.
           02      C-RC        PIC S9(4) COMP.
           02      FILLER      PIC X(50).
       01  HDA                 PIC X(512).

      * Error message variables for the OERHMS routine.
       01  MSGBUF              PIC X(256).
       01  MSGBUF-L            PIC S9(9) VALUE 256 COMP.
       01  ERR-FNC-D           PIC ZZZ.

      * Connect info.  Link the program single-task, or
      * modify to use a SQL*Net connect string appropriate
      * to your site.
       01  USER-ID             PIC X(5)  VALUE "SCOTT".
       01  USER-ID-L           PIC S9(9) VALUE 5 COMP.
       01  PSW                 PIC X(5)  VALUE "TIGER".
       01  PSW-L               PIC S9(9) VALUE 5 COMP.
       01  CONN                PIC S9(9) VALUE 0 COMP.
       01  CONN-L              PIC S9(9) VALUE 0 COMP.
       01  CONN-MODE           PIC S9(9) VALUE 0 COMP.

      * Parameters for OPARSE.
       01  SQL-STMT            PIC X(132).
       01  SQLL                PIC S9(9) COMP.
       01  DEF-MODE            PIC S9(9) VALUE 1 COMP.
       01  NO-DEF-MODE         PIC S9(9) VALUE 0 COMP.
       01  V7-FLG              PIC S9(9) VALUE 2 COMP.

      * Parameters for OBNDRV.
       01  BVNX.
           03  BV-NAME         OCCURS 25 TIMES.
               05 BV-NAMEX     OCCURS 10 TIMES PIC X.
       01  BVVX.
           03  BV-VAL          OCCURS 10 TIMES PIC X(10).
       01  BV-VAL-L            PIC S9(9) VALUE 10 COMP.
       01  N-BV                PIC S9(9) COMP.

      * Parameters for ODESCR.  Note: some are two bytes (S9(4))
      * some are four bytes (S9(9)).
       01  DBSIZEX.
           03  DBSIZE          OCCURS 8 TIMES PIC S9(9) COMP.
       01  DBTYPEX.
           03  DBTYPE          OCCURS 8 TIMES PIC S9(4) COMP.
       01  NAMEX.
           03  DBNAME            OCCURS 8 TIMES PIC X(10).
       01  NAME-LX.
           03  NAME-L          OCCURS 8 TIMES PIC S9(9) COMP.
       01  DSIZEX.
           03  DSIZE           OCCURS 8 TIMES PIC S9(9) COMP.
       01  PRECX.
           03  PREC            OCCURS 8 TIMES PIC S9(4) COMP.
       01  SCALEX.
           03  SCALE           OCCURS 8 TIMES PIC S9(4) COMP.
       01  NULL-OKX.
           03  NULL-OK         OCCURS 8 TIMES PIC S9(4) COMP.

      * Parameters for ODEFIN.
       01  OV-CHARX.
           03  OV-CHAR         OCCURS 8 TIMES PIC X(10).
       01  OV-NUMX.
           03  OV-NUM          OCCURS 8 TIMES
                                  PIC S99999V99 COMP-3.
       01  INDPX.
           03  INDP            OCCURS 8 TIMES PIC S9(4) COMP.
       01  N-OV                PIC S9(9) COMP.
       01  N-ROWS              PIC S9(9) COMP.
       01  N-ROWS-D            PIC ZZZ9 DISPLAY.
       01  OV-CHAR-L           PIC S9(9) VALUE 10 COMP.
       01  SEVEN               PIC S9(9) VALUE 7 COMP.
       01  PACKED-DEC-L        PIC S9(9) VALUE 4 COMP.
       01  PACKED-DEC-T        PIC S9(9) VALUE 7 COMP.
       01  NUM-DISP            PIC ZZZZZ.ZZ.
       01  FMT                 PIC X(6) VALUE "08.+02".
       01  FMT-L               PIC S9(9) VALUE 6 COMP.
       01  FMT-NONE            PIC X(6).

      * Miscellaneous parameters.
       01  ZERO-A              PIC S9(9) VALUE 0 COMP.
       01  ZERO-B              PIC S9(9) VALUE 0 COMP.
       01  ZERO-C              PIC S9(4) VALUE 0 COMP.
       01  ONE                 PIC S9(9) VALUE 1 COMP.
       01  TWO                 PIC S9(9) VALUE 2 COMP.
       01  FOUR                PIC S9(9) VALUE 4 COMP.
       01  INDX                PIC S9(9) COMP.
       01  NAME-D8             PIC X(8).
       01  NAME-D10            PIC X(10).
       01  VARCHAR2-T          PIC S9(9) VALUE 1 COMP.
       01  NUMBER-T            PIC S9(9) VALUE 2 COMP.
       01  INTEGER-T           PIC S9(9) VALUE 3 COMP.
       01  DATE-T              PIC S9(9) VALUE 12 COMP.
       01  CHAR-T              PIC S9(9) VALUE 96 COMP.



       PROCEDURE DIVISION.
       BEGIN.

      * Connect to ORACLE in non-blocking mode.
      * HDA must be initialized to all zeros before call to OLOG.

           MOVE LOW-VALUES TO HDA.

           CALL "OLOG" USING LDA, HDA, USER-ID, USER-ID-L,
                 PSW, PSW-L, CONN, CONN-L, CONN-MODE.

      * Check for error, perform error routine if required.
           IF LDA-RC NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-STOP.

           DISPLAY "Logged on to ORACLE as user " USER-ID ".".
           DISPLAY "Type EXIT at SQL prompt to quit."

      * Open a cursor.  Only the first two parameters are
      * used, the remainder (for V2 compatibility) are ignored.
           CALL "OOPEN" USING CDA, LDA, USER-ID, ZERO-A,
                 ZERO-A, USER-ID, ZERO-A.
           IF C-RC IN CDA NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-LOGOFF.

      * Process each SQL statement.
       STMT-LOOP.
           PERFORM DO-SQL-STMT.
           GO TO STMT-LOOP.

       EXIT-CLOSE.
           CALL "OCLOSE" USING CDA.
       EXIT-LOGOFF.
           CALL "OLOGOF" USING LDA.
       EXIT-STOP.
           STOP RUN.

      * Perform paragraphs.

       DO-SQL-STMT.
           MOVE " " TO SQL-STMT.
           DISPLAY " ".
           DISPLAY "SQL > " NO ADVANCING.
           ACCEPT SQL-STMT.
      * Get first word of statement.
           UNSTRING SQL-STMT DELIMITED BY ALL " "
                    INTO BV-NAME(1).
           IF (BV-NAME(1) = "exit" OR BV-NAME(1) = "EXIT")
              GO TO EXIT-CLOSE.
           MOVE 132 TO SQLL.
      * Use non-deferred parse, to catch syntax errors
      * right after the parse.
           CALL "OPARSE" USING CDA, SQL-STMT, SQLL,
                NO-DEF-MODE, V7-FLG.
           IF C-RC IN CDA NOT = 0
              PERFORM ORA-ERROR
              GO TO DO-SQL-STMT.

           PERFORM BIND-VARS.
           DISPLAY " ".
           MOVE N-BV TO ERR-FNC-D.

           DISPLAY "There were" ERR-FNC-D
                   " bind variables.".

      * Execute the statement.
           CALL "OEXN" USING CDA, ONE, ZERO-B.
           IF C-RC IN CDA NOT = 0
              PERFORM ORA-ERROR
              GO TO DO-SQL-STMT.

      * Describe the SQL statement, and define output
      * variables if it is a query.  Limit output variables
      * to eight.
           PERFORM DESCRIBE-DEFINE THRU DESCRIBE-DEFINE-EXIT.

           SUBTRACT 1 FROM N-OV.
           IF (N-OV > 0)
               MOVE N-OV TO ERR-FNC-D
               DISPLAY "There were" ERR-FNC-D
                       " define variables."
               DISPLAY " "
               PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > N-OV
                  IF (DBTYPE(INDX) NOT = 2)
                     MOVE DBNAME(INDX) TO NAME-D10
                     DISPLAY NAME-D10 NO ADVANCING
                  ELSE
                     MOVE DBNAME(INDX) TO NAME-D8
                     DISPLAY NAME-D8 NO ADVANCING
                  END-IF
                  DISPLAY " " NO ADVANCING
               END-PERFORM
               DISPLAY " "
               PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > N-OV
                  DISPLAY "--------" NO ADVANCING
                  IF DBTYPE(INDX) NOT = 2
                     DISPLAY "--" NO ADVANCING
                  END-IF
                  DISPLAY " " NO ADVANCING
               END-PERFORM
               DISPLAY " "
           END-IF.

      * If the statement was a query, fetch the rows and
      * display them.
           IF (C-TYPE IN CDA = 4)
              PERFORM FETCHN THRU FETCHN-EXIT
              MOVE N-ROWS TO N-ROWS-D
              DISPLAY " "
              DISPLAY N-ROWS-D " rows returned.".
      * End of DO-SQL-STMT.

       BIND-VARS.
           MOVE 0 TO N-BV.
           PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > 25
             MOVE " " TO BV-NAME(INDX)
           END-PERFORM.
           UNSTRING SQL-STMT
             DELIMITED BY "(" OR "," OR ";" OR "="
                       OR ")" OR ALL " "
               INTO BV-NAME(1)
                    BV-NAME(2)
                    BV-NAME(3)
                    BV-NAME(4)
                    BV-NAME(5)
                    BV-NAME(6)
                    BV-NAME(7)
                    BV-NAME(8)
                    BV-NAME(9)
                    BV-NAME(10)
                    BV-NAME(11)
                    BV-NAME(12)
                    BV-NAME(13)
                    BV-NAME(14)
                    BV-NAME(15)
                    BV-NAME(16)
                    BV-NAME(17)
                    BV-NAME(18)
                    BV-NAME(19)
                    BV-NAME(20)
                    BV-NAME(21)
                    BV-NAME(22)
                    BV-NAME(23)
                    BV-NAME(24)
                    BV-NAME(25).

      * Scan the words in the SQL statement.  If the
      * word begins with ':', it is a placeholder for
      * a bind variable.  Get a value for it (as a string)
      * and bind using the OBNDRV routine, datatype 1.
           MOVE 0 TO INDP(1).

           PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > 25
              IF BV-NAMEX(INDX,1) = ':'
                 ADD 1 TO N-BV
                 MOVE 0 TO SQLL
                 INSPECT BV-NAME(INDX) TALLYING SQLL
                    FOR CHARACTERS BEFORE INITIAL ' '
                 DISPLAY "Enter value for " BV-NAME(INDX) " --> "
                    NO ADVANCING
                 ACCEPT BV-VAL(N-BV)
                 CALL "OBNDRV" USING CDA, BV-NAME(INDX), SQLL,
                      BV-VAL(N-BV), BV-VAL-L, VARCHAR2-T,
                      ZERO-A, INDP(1), FMT-NONE, ZERO-A, ZERO-A
                 IF C-RC IN CDA NOT = 0
                    PERFORM ORA-ERROR
                    GO TO EXIT-CLOSE
                 ELSE
                    DISPLAY "Bound " BV-VAL(N-BV)
                 END-IF
              END-IF
           END-PERFORM.

       DESCRIBE-DEFINE.
           MOVE 0 TO N-OV.
           PERFORM 9 TIMES
              ADD 1 TO N-OV
              IF (N-OV > 8)
                 GO TO DESCRIBE-DEFINE-EXIT
              END-IF
              MOVE 10 TO NAME-L(N-OV)
              MOVE " " TO DBNAME(N-OV)

              CALL "ODESCR" USING CDA, N-OV, DBSIZE(N-OV),
                    DBTYPE(N-OV),
                    DBNAME(N-OV), NAME-L(N-OV), DSIZE(N-OV),
                    PREC(N-OV), SCALE(N-OV), NULL-OK(N-OV)
      * Check for end of select list.
              IF (C-RC IN CDA = 1007)
                 GO TO DESCRIBE-DEFINE-EXIT
              END-IF

      * Check for error.
              IF (C-RC IN CDA NOT = 0)
                 PERFORM ORA-ERROR
                 GO TO DESCRIBE-DEFINE-EXIT
              END-IF
      * Define an output variable for the select-list item.
      * If it is a number, define a packed decimal variable,
      * and create a format string for it.

              IF (DBTYPE(N-OV) = 2)
                 CALL "ODEFIN" USING CDA, N-OV, OV-NUM(N-OV),
                      PACKED-DEC-L, PACKED-DEC-T, TWO,
                      INDP(N-OV), FMT, FMT-L, PACKED-DEC-T,
                      ZERO-C, ZERO-C
              ELSE
      * For all other types, convert to a VARCHAR2 of length 10.
                 CALL "ODEFIN" USING CDA, N-OV, OV-CHAR(N-OV),
                      OV-CHAR-L, VARCHAR2-T, ZERO-A, INDP(N-OV),
                      FMT, ZERO-A, ZERO-A, ZERO-C, ZERO-C
              END-IF
              IF (C-RC IN CDA NOT = 0)
                 PERFORM ORA-ERROR
                 GO TO DESCRIBE-DEFINE-EXIT
              END-IF
           END-PERFORM.
       DESCRIBE-DEFINE-EXIT.


       FETCHN.

           MOVE 0 TO N-ROWS.

           PERFORM 10000 TIMES

      * Clear any existing values from storage buffers
              MOVE SPACES TO OV-CHARX
              MOVE LOW-VALUES TO OV-NUMX

              CALL "OFETCH" USING CDA

      * Check for end of fetch ("no data found")
              IF C-RC IN CDA = 1403
                 GO TO FETCHN-EXIT
              END-IF
              IF C-RC IN CDA NOT = 0
                 PERFORM ORA-ERROR
                 GO TO FETCHN-EXIT
              END-IF
              ADD 1 TO N-ROWS
              PERFORM VARYING INDX FROM 1
                      BY 1 UNTIL INDX > N-OV
                 IF (DBTYPE(INDX) = 2)
                    MOVE OV-NUM(INDX) TO NUM-DISP
                    INSPECT NUM-DISP REPLACING ALL ".00" BY "   "
                    DISPLAY NUM-DISP NO ADVANCING
                 ELSE
                    DISPLAY OV-CHAR(INDX) NO ADVANCING
                 END-IF
                 DISPLAY " " NO ADVANCING
              END-PERFORM
              DISPLAY " "
           END-PERFORM.
           DISPLAY "LEAVING FETCHN...".
       FETCHN-EXIT.


      * Report an error.  Obtain the error message
      * text using the OERHMS routine.
       ORA-ERROR.
           IF LDA-RC IN LDA NOT = 0
              DISPLAY "OLOGON error"
              MOVE 0 TO C-FNC IN CDA
              MOVE LDA-RC IN LDA TO C-RC IN CDA.
           DISPLAY "ORACLE error " NO ADVANCING.
           IF C-FNC NOT = 0
              DISPLAY "processing OCI function" NO ADVANCING
              MOVE C-FNC IN CDA TO ERR-FNC-D
              DISPLAY ERR-FNC-D
           ELSE
              DISPLAY ":".

           MOVE " " TO MSGBUF.
           CALL "OERHMS" USING LDA, C-RC IN CDA, MSGBUF,MSGBUF-L.
           DISPLAY MSGBUF.


