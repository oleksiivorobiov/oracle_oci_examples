C
C $Header: fdemo2.for 14-jul-99.13:43:24 mjaeger Exp $
C
C Copyright (c) 1991, 1999, Oracle Corporation.  All rights reserved.
C
C    NAME
C      fdemo2.for - Fortran demo program # 2
C    MODIFIED   (MM/DD/YY)
C     mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
C     plocke     11/14/95 -  to update for v7.3
C     lchidamb   05/16/95 -  merge changes from branch 1.1.720.1
C     sjain      03/16/92 -  Creation

*  FDEMO2.FOR
*
*  A dynamic SQL OCI example program.  Processes
*  SQL statements entered interactively by the user.
*
*  There is a 132-character limit on the length of
*  the SQL statements.  It is not necessary to
*  terminate the SQL statement with a semicolon.
*
*  To end the demo, type 'exit' or 'EXIT' at the
*  prompt.

      PROGRAM FDEMO2
      IMPLICIT INTEGER*4 (A-Z)

*  Data structures
*  Logon and cursor areas
      INTEGER*2      CDA(32), LDA(32), HDA(256)

*  Bind values
      CHARACTER*20   BVARV(8)
      INTEGER        NBV

*  Output values
      CHARACTER*10   DVARC(8)
      INTEGER        DVARI(8)
      REAL*4         DVARF(8)
      INTEGER*2      DBTYPE(8), RLEN(8), RCODE(8)
      INTEGER*2      INDP(8)
      INTEGER        NOV

*  Column names for SELECT
      CHARACTER*10   COLNAM(8)

*  SQL statement buffer and logon info
      CHARACTER*80   SQLSTM, UID, PWD, PROMPT
      INTEGER        UIDL, PWDL, SQLL, NOBLOK

      UID = 'SCOTT'
      PWD = 'TIGER'
      UIDL = LEN_TRIM(UID)
      PWDL = LEN_TRIM(PWD)
      NOBLOK = 0

*  Connect to ORACLE in non-blocking mode.
*  HDA must be initialized to all zeros before call to OLOG.

      DATA HDA/256*0/
      CALL OLOG(LDA, HDA, UID, UIDL, PWD, PWDL, 0, -1, NOBLOK)
      IF (LDA(7) .NE. 0) THEN
        CALL ERRRPT(LDA, CDA)
        GO TO 999
      ENDIF
      WRITE (*, '(1X, A, A)') 'Connected to ORACLE as user ', UID

*  Open a cursor.
      CALL OOPEN(CDA, LDA, UID, 0, -1, PWD, 0)
      IF (LDA(7) .NE. 0) THEN
        CALL ERRRPT(LDA, CDA)
        GO TO 900
      ENDIF

*  Beginning of the main program loop.
*  Get and process SQL statements.
      PROMPT = 'Enter SQL statement (132 char max) or EXIT to quit >'
100   WRITE (*, '(/, A)') PROMPT
      READ '(A)', SQLSTM

      SQLL = LEN_TRIM(SQLSTM)
      IF (SQLL .EQ. 0) GO TO 100

      I = INDEX(SQLSTM, ';')
      IF (I .GT. 0) THEN
        SQLL = I - 1
      ENDIF

      IF ((SQLSTM(1:4) .EQ. 'exit') .OR.
     +    (SQLSTM(1:4) .EQ. 'EXIT')) GO TO 900

*  Parse the statement.
      CALL OPARSE(CDA, SQLSTM, SQLL, 0, 2)
      IF (CDA(7) .NE. 0) THEN
        CALL ERRRPT(LDA, CDA)
        GO TO 100
      ENDIF

*  If there are bind values, obtain them from user.
      CALL GETBNV(LDA, CDA, SQLSTM, BVARV, NBV)
      IF (NBV .LT. 0) GO TO 100

*  Define the output variables.  If the statement is not a
*  query, NOV returns as 0.  If there were errors defining
*  the output variables, NOV returns as -1.
      CALL DEFINE(LDA, CDA, COLNAM, DBTYPE, DVARC, DVARI,
     +            DVARF, INDP, RLEN, RCODE, NOV)
      IF (NOV .LT. 0) GO TO 100

*  Execute the statement.
      CALL OEXN(CDA, 1, 0)
      IF (CDA(7) .NE. 0) THEN
        CALL ERRRPT(LDA, CDA)
        GO TO 100
      ENDIF

*  Fetch rows and display output if the statement was a query.
      CALL FETCHN(LDA, CDA, COLNAM, NOV, DBTYPE, DVARC,
     +            DVARI, DVARF, INDP, RV)
      IF (RV .LT. 0) GO TO 100

*  Loop back to statement 100 to process
*  another SQL statement.
      GO TO  100

*  End of main program loop.  Here on exit or fatal error.
900   CALL OCLOSE(CDA)
      CALL OLOGOF(LDA)

*  End of program.  Come here if connect fails.
999   END


*  Begin subprograms.

      SUBROUTINE GETBNV(LDA, CDA, STMT, BVARV, N)
      IMPLICIT INTEGER*4 (A-Z)
      INTEGER*2     LDA(32), CDA(32)
      CHARACTER*(*) STMT
      CHARACTER*(*) BVARV(8)

*     Arrays for bind variable info.
      INTEGER       BVARI(8), BVARL(8)

*  Scan the SQL statement for placeholders (:ph).
*  Note that a placeholder must be terminated with
*  a space, a comma, or a close parentheses.
*  Two arrays are maintained: an array of starting
*  indices in the string (BVARI), and an array of
*  corresponding lengths (BVARL).
      POS = 1
      DO 300 K = 1, 8            ! maximum of 8 per statement
        I = INDEX(STMT(POS:), ':')
        IF (I .EQ. 0) GO TO 400
        POS = I + POS - 1
        BVARI(K) = POS
        DO 100 J = POS, LEN(STMT)
          IF (STMT(J:J) .EQ. ' '
     +       .OR. STMT(J:J) .EQ. ','
     +       .OR. STMT(J:J) .EQ. ')') THEN
            BVARL(K) = J - POS
            GO TO 200
          ENDIF
100     CONTINUE

200     POS = POS + 1               ! index past the ':'
300   CONTINUE

400   N = K - 1                     ! N is the number of BVs

      DO 500 K = 1, N
        CALL OBNDRV(CDA, STMT(BVARI(K) :), BVARL(K),
     +              BVARV(K), 20, 1,-1,0,0,-1,-1)
        IF (CDA(7) .NE. 0) THEN
          CALL ERRRPT(LDA, CDA)
          N = -1
          RETURN
        ENDIF
        WRITE (*, '( A, A, A)') 'Enter value for ',
     +        STMT(BVARI(K)+1:BVARI(K)+BVARL(K)-1), '  --> '
        READ '(A)', BVARV(K)
500   CONTINUE

      RETURN
      END


*  Define output variables for queries.
*  Returns the number of select-list items (N)
*  and the names of the select-list items (COLNAM).
*  A maximum of 8 select-list items is permitted.
*  (Note that this program does not check if there
*   are more, but a production-quality program
*   must do this.)

      SUBROUTINE DEFINE(LDA, CDA, COLNAM, DBTYPE, DVARC,
     +                  DVARI, DVARF, INDP, RLEN, RCODE, RV)
      IMPLICIT INTEGER*4 (A-Z)
      INTEGER*2       LDA(32), CDA(32), DBTYPE(8)
      INTEGER*2       RLEN(8), RCODE(8), INDP(8)
      CHARACTER*(*)   DVARC(8), COLNAM(8)
      INTEGER         DVARI(8), RV
      REAL*4          DVARF(8)

      INTEGER         DBSIZE(8), COLNML(8), DSIZE(8)
      INTEGER*2       PREC(8), SCALE(8), NOK(8)

*  If not a query (SQL function code .ne. 4), return.
      IF (CDA(2) .NE. 4) THEN
        RV = 0
        RETURN
      ENDIF

*  Describe the select-list (up to 8 items max),
*  and define an output variable for each item, with the
*  external (hence, FORTRAN) type depending on the
*  internal ORACLE type, and its attributes.

      DO 100 N = 1, 8
        COLNML(N) = 10  ! COL length must be set on the call
        CALL ODESCR(CDA, N, DBSIZE(N), DBTYPE(N),
     +       COLNAM(N), COLNML(N), DSIZE(N),
     +       PREC(N), SCALE(N), NOK(N))

*  If the return code from ODESCR is 1007, then you have
*  reached the end of the select list.
        IF (CDA(7) .EQ. 1007) THEN
          GO TO 200
*  Otherwise, if the return code is non-zero, an
*  error occurred. Exit the subroutine, signalling
*  an error.
        ELSE IF (CDA(7) .NE. 0) THEN
          CALL ERRRPT(LDA, CDA)
          RV = -1                ! Error on return
          RETURN
        ENDIF

*  Check the datatype of the described item.  If it's a
*  NUMBER, check if the SCALE is 0.  If so, define the
*  output variable as INTEGER (3). If it's NUMBER with SCALE != 0,
*  define the output variable as REAL (4).  Otherwise,
*  it's assumed to be a DATE, LONG, CHAR, or VARCHAR2,
*  so define the output as 1 (VARCHAR2).

        IF (DBTYPE(N) .EQ. 2) THEN
          IF (SCALE(N) .EQ. 0) THEN
            DBTYPE(N) = 3
          ELSE
            DBTYPE(N) = 4
          ENDIF
        ELSE
          DBTYPE(N) = 1
        ENDIF

*  Define the output variable.  Do not define RLEN if
*  the external datatype is 1.
        IF (DBTYPE(N) .EQ. 3) THEN
          CALL ODEFIN(CDA, N, DVARI(N), 4, 3, 0, INDP(N),
     +                FMT, 0, 0, RLEN(N), RCODE(N))
        ELSE IF (DBTYPE(N) .EQ. 4) THEN
          CALL ODEFIN(CDA, N, DVARF(N), 4, 4, 0, INDP(N),
     +                FMT, 0, 0, RLEN(N), RCODE(N))
        ELSE
          CALL ODEFIN(CDA, N, DVARC(N), 10, 1, 0, INDP(N),
     +                FMT, 0, 0, %VAL(-1), RCODE(N))
        ENDIF
        IF (CDA(7) .NE. 0) THEN
          CALL ERRRPT(LDA, CDA)
          RV = -1
          RETURN
        ENDIF
100   CONTINUE

200   RV = N - 1             ! Decrement to get correct count

      RETURN
      END


*  FETCHN uses OFETCH to fetch the rows that satisfy
*  the query, and displays the output.  The data is
*  fetched 1 row at a time.

      SUBROUTINE FETCHN(LDA, CDA, NAMES, NOV, DBTYPE, DVARC,
     +                  DVARI, DVARF, INDP, RV)
      IMPLICIT INTEGER*4 (A-Z)
      INTEGER*2     LDA(32), CDA(32), DBTYPE(8), INDP(8)
      CHARACTER*(*) NAMES(8), DVARC(8)
      INTEGER       DVARI(8), NOV, RV
      REAL*4        DVARF(8)

      IF (CDA(2) .NE. 4) THEN         ! not a query
        RV = 0
        RETURN
      ENDIF

      DO 50 COL = 1, NOV
        IF (DBTYPE(COL) .EQ. 1) THEN
          WRITE (*, 900) NAMES(COL), ' '
900       FORMAT ('+', A10, A1, $)
        ELSE
          WRITE (*, 902) NAMES(COL), ' '
902       FORMAT ('+', A8, A1, $)
        ENDIF
50    CONTINUE

      WRITE (*, '(1X, A, /)') '------------------------------
     +-----------------------------------------------'

      DO 200 NROWS = 1, 10000
        CALL OFETCH(CDA)
        IF (CDA(7) .EQ. 1403) GO TO 300
        IF (CDA(7) .NE. 0 .AND. CDA(7) .NE. 1406) THEN
          CALL ERRRPT(LDA, CDA)
          RV = -1
          RETURN
        ENDIF
        DO 100 COL = 1, NOV
          IF (INDP(COL) .LT. 0 .AND. DBTYPE(COL) .NE. 1) THEN
            WRITE (*, 903), '         '
903         FORMAT ('+', A9, $)
          ELSE IF (INDP(COL) .LT. 0 .AND. DBTYPE(COL) .EQ. 1) THEN
            WRITE (*, 905), '           '
905         FORMAT ('+', A11, $)
          ELSE
            IF (DBTYPE(COL) .EQ. 3) THEN
              WRITE (*, 904) DVARI(COL), '   '
904           FORMAT ('+', I6, A3, $)
            ELSE IF (DBTYPE(COL) .EQ. 4) THEN
              WRITE (*, 906) DVARF(COL), ' '
906           FORMAT ('+', F8.2, A1, $)
            ELSE
              WRITE (*, 908) DVARC(COL), ' '
908           FORMAT ('+', A10, A1, $)
            ENDIF
          ENDIF
100     CONTINUE
        WRITE (*, '(1X)')
200   CONTINUE

300   NROWS = NROWS - 1
      WRITE (*, '(/, 1X, I3, A)') NROWS, ' rows returned'

      RETURN
      END



      SUBROUTINE ERRRPT(LDA, CDA)
      INTEGER*2 LDA(32), CDA(32)

      CHARACTER*132  MSG

      MSG = ' '
      IF (LDA(7) .NE. 0) THEN
         CDA(7) = LDA(7)
         CDA(6) = 0
      ENDIF

      IF (CDA(6) .NE. 0) THEN
        WRITE (*, '(1X, A, I3)') 'Error processing OCI function',
     +   CDA(6)
      ENDIF

      CALL OERHMS (LDA, CDA(7), MSG, 132)
      WRITE (*, '(1X, A)') MSG

      RETURN
      END


      INTEGER FUNCTION LEN_TRIM(STRING)
      CHARACTER*(*) STRING

      INTEGER NEXT

      DO 10 NEXT = LEN(STRING), 1, -1
        IF (STRING(NEXT : NEXT) .NE. ' ') THEN
          LEN_TRIM = NEXT
          RETURN
        ENDIF
10    CONTINUE

      LEN_TRIM = 0

      RETURN
      END

