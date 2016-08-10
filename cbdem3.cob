      *
      * $Header: cbdem3.cob 14-jul-99.14:31:45 mjaeger Exp $
      *
      * Copyright (c) 1991, 1999, Oracle Corporation.  All rights reserved.
      *
      *   NAME
      *     cbdem3.cob - Cobol demo program # 3
      *   MODIFIED   (MM/DD/YY)
      *    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
      *    plocke     11/14/95 -  to update for v7.3
      *    dchatter   07/20/95 -  merge changes from branch 1.1.720.1
      *    dchatter   04/14/95 -  fix test to work on sun
      *    sjain      03/16/92 -  Creation
      *
      *  The program CBDEM3 creates a table called
      *  "VOICE_MAIL" that contains three fields:
      *  a message ID, and message length, and a LONG RAW
      *  column that contains a digitized voice
      *  message.  The program fills one row of the table with a
      *  (simulated) message, then plays the message by
      *  extracting 64 kB chunks of it using the OFLNG routine,
      *  and sending them to a (simulated) digital-to-analog
      *  (DAC) converter routine.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CBDEM3.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

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

       01  ERRMSG              PIC X(256).
       01  ERRMSG-L            PIC S9(9) VALUE 256 COMP.
       01  ERR-RC              PIC S9(9) COMP.
       01  ERR-FNC-D           PIC ZZ9.

       01  USER-ID             PIC X(5)  VALUE "SCOTT".
       01  USER-ID-L           PIC S9(9) VALUE 5 COMP.
       01  PSW                 PIC X(5)  VALUE "tiger".
       01  PSW-L               PIC S9(9) VALUE 5 COMP.
       01  CONN                PIC S9(9) VALUE 0 COMP.
       01  CONN-L              PIC S9(9) VALUE 0 COMP.
       01  CONN-MODE           PIC S9(9) VALUE 0 COMP.

       01  SQL-STMT            PIC X(132).
       01  SQLL                PIC S9(9) COMP.
       01  ZERO-A              PIC S9(9) VALUE 0 COMP.
       01  ZERO-B              PIC S9(9) VALUE 0 COMP.
       01  FMT                 PIC X(6).

      *  Establish a 200000 byte buffer.  (On most systems,
      *  including the VAX, a PIC 99 reserves one byte.)
       01  MSGX.
           02 MSG              OCCURS 200000 TIMES PIC 99.
       01  MSGX-L              PIC S9(9) VALUE 200000 COMP.
       01  MSG-L               PIC S9(9) COMP.
       01  MSG-L-D             PIC ZZZZZZ.
       01  MSG-ID              PIC S9(9) COMP.
       01  MSG-ID-L            PIC S9(9) VALUE 4 COMP.
       01  MSG-ID-D            PIC ZZZZ.
       01  LEN                 PIC 9(9) COMP.
       01  LEN-D               PIC ZZZZ9.
       01  INDX                PIC S9(9) COMP.
       01  INTEGER-T           PIC S9(9) VALUE 3 COMP.
       01  DEF-MODE            PIC S9(9) VALUE 1 COMP.
       01  LONG-RAW            PIC S9(9) VALUE 24 COMP.
       01  ONE                 PIC S9(9) VALUE 1 COMP.
       01  TWO                 PIC S9(9) VALUE 2 COMP.
       01  THREE               PIC S9(9) VALUE 3 COMP.

       01  ANSX.
           02      ANSWER      OCCURS 6 TIMES PIC X.
       01  VERSION-7           PIC S9(9) VALUE 2 COMP.
       01  INDP                PIC S9(4) COMP.
       01  RCODE               PIC S9(4) COMP.
       01  RLEN                PIC S9(4) COMP.
       01  RETL                PIC S9(9) COMP.
       01  OFF1                PIC S9(9) COMP.


       PROCEDURE DIVISION.
       BEGIN.

      *  Connect to ORACLE in non-blocking mode.
      *  HDA must be initialized to all zeros before call to OLOG.

           MOVE LOW-VALUES TO HDA.

           CALL "OLOG" USING LDA, HDA, USER-ID, USER-ID-L,
                 PSW, PSW-L, CONN, CONN-L, CONN-MODE.

           IF LDA-RC NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-STOP.

           DISPLAY "Logged on to ORACLE as user ", USER-ID.

      *  Open a cursor.
           CALL "OOPEN" USING CDA, LDA, USER-ID, ZERO-A,
                 ZERO-A, USER-ID, ZERO-A.
           IF C-RC IN CDA NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-LOGOFF.

      *  Drop the VOICE_MAIL table.
           DISPLAY "OK to drop VOICE_MAIL table (Y or N)? : "
      -  WITH NO ADVANCING.
           ACCEPT ANSX.
           IF (ANSWER(1) NOT = 'y' AND ANSWER(1) NOT = 'Y')
              DISPLAY "Exiting program now."
              GO TO EXIT-CLOSE.
           MOVE "DROP TABLE VOICE_MAIL" TO SQL-STMT.
           MOVE 132 TO SQLL.

      *  Call OPARSE with no deferred parse to execute the DDL
      *  statement immediately.
           CALL "OPARSE" USING CDA, SQL-STMT, SQLL,
                ZERO-A, VERSION-7.
           IF C-RC IN CDA NOT = 0
              IF (C-RC IN CDA = 942)
                 DISPLAY "Table did not exist."
              ELSE
                 PERFORM ORA-ERROR
                 GO TO EXIT-LOGOFF
              END-IF
           ELSE
              DISPLAY "Table dropped."
           END-IF

      *  Create the VOICE_MAIL table anew.
           MOVE "CREATE TABLE VOICE_MAIL (MSG_ID NUMBER(6),
      -    "MSG_LEN NUMBER(12), MSG LONG RAW)" TO SQL-STMT.
           MOVE 132 TO SQLL.

      *  Non-deferred parse to execute the DDL SQL statement.
           DISPLAY "Table VOICE_MAIL " NO ADVANCING.

           CALL "OPARSE" USING CDA, SQL-STMT, SQLL,
                ZERO-A, VERSION-7.
           IF C-RC IN CDA NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-LOGOFF.
           DISPLAY "created.".

      *  Insert some data into the table.
           MOVE "INSERT INTO VOICE_MAIL VALUES (:1, :2, :3)"
                TO SQL-STMT.
           MOVE 132 TO SQLL.
           CALL "OPARSE" USING CDA, SQL-STMT, SQLL,
                 ZERO-A, VERSION-7.
           IF C-RC IN CDA NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-LOGOFF.

      *  Bind the inputs.
           MOVE 0 TO INDP.
           CALL "OBNDRN" USING CDA, ONE, MSG-ID, MSG-ID-L,
                INTEGER-T, ZERO-A, INDP, FMT, ZERO-A, ZERO-A.
           IF C-RC IN CDA NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-LOGOFF.

           CALL "OBNDRN" USING CDA, TWO, MSG-L, MSG-ID-L,
                INTEGER-T, ZERO-A, INDP, FMT, ZERO-A, ZERO-A.
           IF C-RC IN CDA NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-LOGOFF.

           CALL "OBNDRN" USING CDA, THREE, MSGX, MSGX-L,
                LONG-RAW, ZERO-A, INDP, FMT, ZERO-A, ZERO-A.
           IF C-RC IN CDA NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-LOGOFF.

      *  Set input variables, then execute the INSERT statement.
           MOVE 100 TO MSG-ID.
           MOVE 200000 TO MSG-L.
           PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MSG-L
              MOVE 42 TO MSG(INDX)
           END-PERFORM.
           CALL "OEXN" USING CDA, ONE, ZERO-B.
           IF C-RC IN CDA NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-LOGOFF.

           MOVE "SELECT MSG_ID, MSG_LEN, MSG FROM VOICE_MAIL
      -    " WHERE MSG_ID = 100" TO SQL-STMT.

      *  Call OPARSE in deferred mode to select a message.
           CALL "OPARSE" USING CDA, SQL-STMT, SQLL,
                DEF-MODE, VERSION-7.
           IF C-RC IN CDA NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-LOGOFF.
      *  Define the output variables.
           CALL "ODEFIN" USING CDA, ONE, MSG-ID,
                MSG-ID-L, INTEGER-T, ZERO-A, ZERO-A, ZERO-A,
                ZERO-A, ZERO-A, ZERO-A, ZERO-A.
           IF C-RC IN CDA NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-LOGOFF.

           CALL "ODEFIN" USING CDA, TWO, MSG-L,
                MSG-ID-L, INTEGER-T, ZERO-A, ZERO-A, ZERO-A,
                ZERO-A, ZERO-A, ZERO-A, ZERO-A.
           IF C-RC IN CDA NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-LOGOFF.

           MOVE 100 TO MSG-ID-L.
           CALL "ODEFIN" USING CDA, THREE, MSGX,
                MSG-ID-L, LONG-RAW, ZERO-A, INDP, ANSX, ZERO-A, ZERO-A,
                RLEN, RCODE.
           IF C-RC IN CDA NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-LOGOFF.

      *  Do the query, getting the message ID and just the first
      *  100 bytes of the message.  This query basically just sets
      *  the cursor to the right row.  The message contents are
      *  fetched by the OFLNG routine.

           CALL "OEXFET" USING CDA, ONE, ZERO-A, ZERO-A.
           IF C-RC IN CDA NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-LOGOFF.

           MOVE MSG-ID TO MSG-ID-D.
           DISPLAY " ".
           DISPLAY "Message " MSG-ID-D " is available.".
           MOVE MSG-L TO MSG-L-D.
           DISPLAY "The length is " MSG-L-D " bytes.".

           PERFORM VARYING OFF1 FROM 0 BY 65536
                 UNTIL MSG-L <= 0
              IF (MSG-L < 65536)
                 MOVE MSG-L TO LEN
              ELSE
                 MOVE 65536 TO LEN
              END-IF
              PERFORM PLAY-MSG THRU PLAY-MSG-EXIT
              SUBTRACT LEN FROM MSG-L
      *        IF (MSG-L < 0 OR MSG-L = 0)
      *           GO TO END-LOOP
      *        END-IF
           END-PERFORM.

       END-LOOP.
           DISPLAY " ".
           DISPLAY "End of message.".


       EXIT-CLOSE.
           CALL "OCLOSE" USING CDA.
       EXIT-LOGOFF.
           CALL "OLOGOF" USING LDA.
       EXIT-STOP.
           STOP RUN.


       PLAY-MSG.
           MOVE LEN TO LEN-D.
           DISPLAY "Playing " LEN-D " bytes.".
       PLAY-MSG-EXIT.



      * Report an error.  Obtain the error message
      * text using the OERHMS routine.
       ORA-ERROR.
           IF LDA-RC IN LDA NOT = 0
              DISPLAY "OLOGON error"
              MOVE 0 TO C-FNC IN CDA
              MOVE LDA-RC IN LDA TO C-RC IN CDA.
           DISPLAY "ORACLE error" NO ADVANCING.
           IF C-FNC NOT = 0
              DISPLAY " processing OCI function " NO ADVANCING
              MOVE C-FNC IN CDA TO ERR-FNC-D
              DISPLAY ERR-FNC-D
           ELSE
              DISPLAY ".".

           MOVE " " TO ERRMSG.
           CALL "OERHMS" USING LDA, C-RC IN CDA,
                ERRMSG, ERRMSG-L.
           DISPLAY ERRMSG.



