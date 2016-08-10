      *
      * $Header: cbdem1.cob 14-jul-99.14:30:27 mjaeger Exp $
      *
      * Copyright (c) 1991, 1999, Oracle Corporation.  All rights reserved.
      *
      *   NAME
      *     cbdem1.cob - Cobol demo program # 1
      *   MODIFIED   (MM/DD/YY)
      *    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
      *    plocke     11/14/95 -  to update for v7.3
      *    dchatter   07/20/95 -  merge changes from branch 1.1.720.1
      *    dchatter   04/14/95 -  fix test to work on sun
      *    sjain      03/16/92 -  Creation
      * ---------------------------------------------------------
      * CBDEM1 IS A SIMPLE EXAMPLE PROGRAM WHICH ADDS
      * NEW EMPLOYEE ROWS TO THE PERSONNEL DATA BASE. CHECKING
      * IS DONE TO INSURE THE INTEGRITY OF THE DATA BASE.
      * THE EMPLOYEE NUMBERS ARE AUTOMATICALLY SELECTED USING
      * THE CURRENT MAXIMUM EMPLOYEE NUMBER AS THE START.
      * IF ANY EMPLOYEE NUMBER IS A DUPLICATE, IT IS SKIPPED.
      * THE PROGRAM QUERIES THE USER FOR DATA AS FOLLOWS:
      *
      *          Enter employee name  :
      *          Enter employee job   :
      *          Enter employee salary:
      *          Enter employee dept  :
      *
      * TO EXIT THE PROGRAM, ENTER A CARRIAGE RETURN AT THE
      * PROMPT FOR EMPLOYEE NAME.
      *
      * IF THE ROW IS SUCCESSFULLY INSERTED, THE FOLLOWING
      * IS PRINTED:
      *
      * ENAME added to DNAME department as employee # NNNNN
      *
      * THE MAXIMUM LENGTHS OF THE 'ENAME', 'JOB', AND 'DNAME'
      * COLUMNS WILL BE DETERMINED BY THE ODESCR CALL.
      *
      *----------------------------------------------------------


       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CBDEM1.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  LDA.
           02   LDA-V2RC       PIC S9(4) COMP.
           02   FILLER         PIC X(10).
           02   LDA-RC         PIC S9(4) COMP.
           02   FILLER         PIC X(50).
       01  HDA                 PIC X(512).

       01  CURSOR-1.
           02   C-V2RC         PIC S9(4) COMP.
           02   C-TYPE         PIC S9(4) COMP.
           02   C-ROWS         PIC S9(9) COMP.
           02   C-OFFS         PIC S9(4) COMP.
           02   C-FNC          PIC S9(4) COMP.
           02   C-RC           PIC S9(4) COMP.
           02   FILLER         PIC X(50).
       01  CURSOR-2.
           02   C-V2RC         PIC S9(4) COMP.
           02   C-TYPE         PIC S9(4) COMP.
           02   C-ROWS         PIC S9(9) COMP.
           02   C-OFFS         PIC S9(4) COMP.
           02   C-FNC          PIC S9(4) COMP.
           02   C-RC           PIC S9(4) COMP.
           02   FILLER         PIC X(50).

       77   USER-ID            PIC X(5)  VALUE "SCOTT".
       77   USER-ID-L          PIC S9(9) VALUE 5 COMP.
       77   PSW                PIC X(5)  VALUE "tiger".
       77   PSW-L              PIC S9(9) VALUE 5 COMP.
       77   CONN               PIC S9(9) VALUE 0 COMP.
       77   CONN-L             PIC S9(9) VALUE 0 COMP.
       77   CONN-MODE          PIC S9(9) VALUE 0 COMP.

       77   SQL-SEL            PIC X(38) VALUE
               "SELECT DNAME FROM DEPT WHERE DEPTNO=:1".
       77   SQL-SEL-L          PIC S9(9) VALUE 38 COMP.

       77   SQL-INS            PIC X(150) VALUE
               "INSERT INTO EMP (EMPNO,ENAME,JOB,SAL,DEPTNO)
      -        " VALUES (:EMPNO,:ENAME,:JOB,:SAL,:DEPTNO)".
       77   SQL-INS-L          PIC S9(9) VALUE 150 COMP.

       77   SQL-SELMAX         PIC X(33) VALUE
               "SELECT NVL(MAX(EMPNO),0) FROM EMP".
       77   SQL-SELMAX-L       PIC S9(9) VALUE 33 COMP.

       77   SQL-SELEMP         PIC X(26) VALUE
               "SELECT ENAME,JOB FROM EMP".
       77   SQL-SELEMP-L       PIC S9(9) VALUE 26 COMP.

       77   EMPNO              PIC S9(9) COMP.
       77   EMPNO-D            PIC ZZZZ9.
       77   ENAME              PIC X(12).
       77   JOB                PIC X(12).
       77   SAL                PIC X(10).
       77   DEPTNO             PIC X(10).
       77   FMT                PIC X(6).
       77   CBUF               PIC X(10).
       77   DNAME              PIC X(15).
       77   ENAME-L            PIC S9(9) VALUE 12 COMP.
       77   ENAME-SIZE         PIC S9(4) COMP.
       77   JOB-L              PIC S9(9) VALUE 12 COMP.
       77   JOB-SIZE           PIC S9(4) COMP.
       77   SAL-L              PIC S9(9) VALUE 10 COMP.
       77   DEPTNO-L           PIC S9(9) VALUE 10 COMP.
       77   DNAME-L            PIC S9(9) VALUE 15 COMP.
       77   DNAME-SIZE         PIC S9(4) COMP.

       77   EMPNO-N            PIC X(6) VALUE ":EMPNO".
       77   ENAME-N            PIC X(6) VALUE ":ENAME".
       77   JOB-N              PIC X(4) VALUE ":JOB".
       77   SAL-N              PIC X(4) VALUE ":SAL".
       77   DEPTNO-N           PIC X(7) VALUE ":DEPTNO".

       77   EMPNO-N-L          PIC S9(9) VALUE 6 COMP.
       77   ENAME-N-L          PIC S9(9) VALUE 6 COMP.
       77   JOB-N-L            PIC S9(9) VALUE 4 COMP.
       77   SAL-N-L            PIC S9(9) VALUE 4 COMP.
       77   DEPTNO-N-L         PIC S9(9) VALUE 7 COMP.

       77   INTEGER            PIC S9(9) COMP VALUE 3.
       77   ASC                PIC S9(9) COMP VALUE 1.
       77   ZERO-A             PIC S9(9) COMP VALUE 0.
       77   ZERO-B             PIC S9(4) COMP VALUE 0.
       77   ONE                PIC S9(9) COMP VALUE 1.
       77   TWO                PIC S9(9) COMP VALUE 2.
       77   FOUR               PIC S9(9) COMP VALUE 4.
       77   SIX                PIC S9(9) COMP VALUE 6.
       77   EIGHT              PIC S9(9) COMP VALUE 8.
       77   ERR-RC             PIC S9(4) COMP.
       77   ERR-FNC            PIC S9(4) COMP.
       77   ERR-RC-D           PIC ZZZ9.
       77   ERR-FNC-D          PIC ZZ9.
       77   MSGBUF             PIC X(160).
       77   MSGBUF-L           PIC S9(9) COMP VALUE 160.

       77   ASK-EMP            PIC X(25) VALUE
                                 "Enter employee name: ".
       77   ASK-JOB            PIC X(25) VALUE
                                 "Enter employee job: ".
       77   ASK-SAL            PIC X(25) VALUE
                                 "Enter employee salary: ".
       77   ASK-DEPTNO         PIC X(25) VALUE
                                 "Enter employee dept: ".

       PROCEDURE DIVISION.
       BEGIN.

      *----------------------------------------------------------
      * CONNECT TO ORACLE IN NON-BLOCKING MODE.
      * HDA MUST BE INITIALIZED TO ALL ZEROS BEFORE CALL TO OLOG.
      *----------------------------------------------------------

           MOVE LOW-VALUES TO HDA.

           CALL "OLOG" USING LDA, HDA, USER-ID, USER-ID-L,
                 PSW, PSW-L, CONN, CONN-L, CONN-MODE.

           IF LDA-RC NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-STOP.

           DISPLAY "Connected to ORACLE as user ", USER-ID.

      *----------------------------------------------------------
      * OPEN THE CURSORS.
      *----------------------------------------------------------

           CALL "OOPEN" USING CURSOR-1, LDA.
           IF C-RC IN CURSOR-1 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-LOGOF.

           CALL "OOPEN" USING CURSOR-2, LDA.
           IF C-RC IN CURSOR-2 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-LOGOF.

      *----------------------------------------------------------
      * DISABLE AUTO-COMMIT.
      * NOTE: THE DEFAULT IS OFF, SO THIS COULD BE OMITTED.
      *----------------------------------------------------------

           CALL "OCOF" USING LDA.
           IF LDA-RC NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

      *----------------------------------------------------------
      * RETRIEVE THE CURRENT MAXIMUM EMPLOYEE NUMBER.
      *----------------------------------------------------------

           CALL "OPARSE" USING CURSOR-1, SQL-SELMAX, SQL-SELMAX-L,
                 ZERO-A, TWO.
           IF C-RC IN CURSOR-1 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

           CALL "ODEFIN" USING CURSOR-1, ONE, EMPNO, FOUR,
                INTEGER, ZERO-A, ZERO-B, FMT, ZERO-A, ZERO-A,
                ZERO-B, ZERO-B.
           IF C-RC IN CURSOR-1 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

           CALL "OEXEC" USING CURSOR-1.
           IF C-RC IN CURSOR-1 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

           CALL "OFETCH" USING CURSOR-1.
           IF C-RC IN CURSOR-1 NOT = 0
              IF C-RC IN CURSOR-1 NOT = 1403
                 PERFORM ORA-ERROR
                 GO TO EXIT-CLOSE
              ELSE
                 MOVE 10 TO EMPNO.

      *----------------------------------------------------------
      * DETERMINE THE MAX LENGTH OF THE EMPLOYEE NAME AND
      * JOB TITLE.  PARSE THE SQL STATEMENT -
      * IT WILL NOT BE EXECUTED.
      * DESCRIBE THE TWO FIELDS SPECIFIED IN THE SQL STATEMENT.
      *----------------------------------------------------------

           CALL "OPARSE" USING CURSOR-1, SQL-SELEMP, SQL-SELEMP-L,
                ZERO-A, TWO.
           IF C-RC IN CURSOR-1 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

           CALL "ODESCR" USING CURSOR-1, ONE, ENAME-SIZE, ZERO-B,
                CBUF, ZERO-A, ZERO-A, ZERO-B, ZERO-B, ZERO-B.
           IF C-RC IN CURSOR-1 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

           CALL "ODESCR" USING CURSOR-1, TWO, JOB-SIZE, ZERO-B,
                CBUF, ZERO-A, ZERO-A, ZERO-B, ZERO-B, ZERO-B.
           IF C-RC IN CURSOR-1 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

           IF ENAME-SIZE > ENAME-L
              DISPLAY "ENAME too large for buffer."
              GO TO EXIT-CLOSE.
           IF JOB-SIZE > JOB-L
              DISPLAY "JOB too large for buffer."
              GO TO EXIT-CLOSE.

      *----------------------------------------------------------
      * PARSE THE INSERT AND SELECT STATEMENTS.
      *----------------------------------------------------------

           CALL "OPARSE" USING CURSOR-1, SQL-INS, SQL-INS-L,
                ZERO-A, TWO.
           IF C-RC IN CURSOR-1 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

           CALL "OPARSE" USING CURSOR-2, SQL-SEL, SQL-SEL-L,
                ZERO-A, TWO.
           IF C-RC IN CURSOR-2 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

      *----------------------------------------------------------
      * BIND ALL SQL SUBSTITUTION VARIABLES.
      *----------------------------------------------------------

           CALL "OBNDRV" USING CURSOR-1, EMPNO-N, EMPNO-N-L,
                EMPNO, FOUR, INTEGER, ZERO-A.
           IF C-RC IN CURSOR-1 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

           CALL "OBNDRV" USING CURSOR-1, ENAME-N, ENAME-N-L,
                ENAME, ENAME-L, ASC.
           IF C-RC IN CURSOR-1 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

           CALL "OBNDRV" USING CURSOR-1, JOB-N, JOB-N-L,
                JOB, JOB-L, ASC.
           IF C-RC IN CURSOR-1 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

           CALL "OBNDRV" USING CURSOR-1, SAL-N, SAL-N-L, SAL,
                SAL-L, ASC.
           IF C-RC IN CURSOR-1 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

           CALL "OBNDRV" USING CURSOR-1, DEPTNO-N, DEPTNO-N-L,
                DEPTNO, DEPTNO-L, ASC.
           IF C-RC IN CURSOR-1 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

      *----------------------------------------------------------
      * BIND THE DEPTNO SUBSTITUTION VAR IN THE SELECT STATEMENT.
      *----------------------------------------------------------

           CALL "OBNDRN" USING CURSOR-2, ONE, DEPTNO,
                DEPTNO-L, ASC.
           IF C-RC IN CURSOR-2 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

      *----------------------------------------------------------
      * DESCRIBE THE 'DNAME' COLUMN - ONLY THE LENGTH.
      *----------------------------------------------------------

           CALL "ODSC" USING CURSOR-2, ONE, DNAME-SIZE.
           IF C-RC IN CURSOR-2 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

           IF DNAME-SIZE > DNAME-L
              DISPLAY "DNAME is to large for buffer."
              GO TO EXIT-CLOSE.

      *----------------------------------------------------------
      * DEFINE THE BUFFER TO RECEIVE 'DNAME'.
      *----------------------------------------------------------

           CALL "ODEFIN" USING CURSOR-2, ONE, DNAME,
                DNAME-L, ASC.
           IF C-RC IN CURSOR-2 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

      *----------------------------------------------------------
      * ASK THE USER FOR EMPLOYEE NAME, JOB, SAL, AND DEPTNO.
      *----------------------------------------------------------

       NEXT-EMP.

           DISPLAY ASK-EMP WITH NO ADVANCING.
           ACCEPT ENAME.
           IF ENAME = " "
              GO TO EXIT-CLOSE.

           DISPLAY ASK-JOB WITH NO ADVANCING.
           ACCEPT JOB.

           DISPLAY ASK-SAL WITH NO ADVANCING.
           ACCEPT SAL.

       ASK-DPT.
           DISPLAY ASK-DEPTNO WITH NO ADVANCING.
           ACCEPT DEPTNO.

      *----------------------------------------------------------
      * CHECK FOR A VALID DEPARTMENT NUMBER BY EXECUTING.
      * THE SELECT STATEMENT.
      *----------------------------------------------------------

           CALL "OEXEC" USING CURSOR-2.
           IF C-RC IN CURSOR-2 NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

      *----------------------------------------------------------
      * FETCH THE ROWS - DEPTNO IS A PRIMARY KEY SO A MAX.
      * OF 1 ROW WILL BE FETCHED.
      * IF THE CURSOR RETURN CODE IS 1403 THEN
      * NO SUCH DEPARTMENT EXISTS.
      *----------------------------------------------------------

           MOVE SPACES TO DNAME.

           CALL "OFETCH" USING CURSOR-2.
           IF C-RC IN CURSOR-2 = 0 THEN GO TO ADD-ROW.
           IF C-RC IN CURSOR-2 = 1403
              DISPLAY "No such department."
              GO TO ASK-DPT.

      *----------------------------------------------------------
      * INCREMENT EMPNO BY 10.
      * EXECUTE THE INSERT STATEMENT.
      *----------------------------------------------------------

       ADD-ROW.

           ADD 10 TO EMPNO.
           IF EMPNO > 9999
              MOVE EMPNO TO EMPNO-D
              DISPLAY "Employee number " EMPNO-D " too large."
              GO TO EXIT-CLOSE.

           CALL "OEXEC" USING CURSOR-1.
           IF C-RC IN CURSOR-1 = 0 THEN GO TO PRINT-RESULT.

      *----------------------------------------------------------
      * IF THE RETURN CODE IS 1 (DUPLICATE VALUE IN INDEX),
      * THEN GENERATE THE NEXT POSSIBLE EMPLOYEE NUMBER.
      *----------------------------------------------------------

           IF C-RC IN CURSOR-1 = 1
              ADD 10 TO EMPNO
              GO TO ADD-ROW
           ELSE
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.

       PRINT-RESULT.

           MOVE EMPNO TO EMPNO-D.
           DISPLAY ENAME " added to the " DNAME
             " department as employee number " EMPNO-D.

      *----------------------------------------------------------
      * THE ROW HAS BEEN ADDED - COMMIT THIS TRANSACTION.
      *----------------------------------------------------------

           CALL "OCOM" USING LDA.
           IF LDA-RC NOT = 0
              PERFORM ORA-ERROR
              GO TO EXIT-CLOSE.
           GO TO NEXT-EMP.

      *----------------------------------------------------------
      * CLOSE CURSORS AND LOG OFF.
      *----------------------------------------------------------

       EXIT-CLOSE.

           CALL "OCLOSE" USING CURSOR-1.
           IF C-RC IN CURSOR-1 NOT = 0
              PERFORM ORA-ERROR.

           CALL "OCLOSE" USING CURSOR-2.
           IF C-RC IN CURSOR-2 NOT = 0
              PERFORM ORA-ERROR.

       EXIT-LOGOF.

           CALL "OLOGOF" USING LDA.
           IF LDA-RC NOT = 0
              PERFORM ORA-ERROR.

       EXIT-STOP.

           DISPLAY "End of the OCIDEMO1 program."
           STOP RUN.

      *----------------------------------------------------------
      * DISPLAY ORACLE ERROR NOTICE.
      *----------------------------------------------------------

       ORA-ERROR.

           IF LDA-RC NOT = 0
              DISPLAY "OLOGON error"
              MOVE LDA-RC TO ERR-RC
              MOVE "0" TO ERR-FNC
           ELSE IF C-RC IN CURSOR-1 NOT = 0
              MOVE C-RC IN CURSOR-1 TO ERR-RC
              MOVE C-FNC IN CURSOR-1 TO ERR-FNC
           ELSE
              MOVE C-RC IN CURSOR-2 TO ERR-RC
              MOVE C-FNC IN CURSOR-2 TO ERR-FNC.

           DISPLAY "ORACLE error" WITH NO ADVANCING.
           IF ERR-FNC NOT = 0
              MOVE ERR-FNC TO ERR-FNC-D
              DISPLAY " processing OCI function"
                   ERR-FNC-D "."
           ELSE
              DISPLAY ".".

           MOVE " " TO MSGBUF.
           CALL "OERHMS" USING LDA, ERR-RC, MSGBUF, MSGBUF-L.
           DISPLAY MSGBUF.
