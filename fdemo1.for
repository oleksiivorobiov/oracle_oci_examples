C
C $Header: fdemo1.for 25-jul-2001.04:07:17 ssappara Exp $
C
C Copyright (c) 1991, 1999, Oracle Corporation.  All rights reserved.
C
C    NAME
C      fdemo1.for - Fortran demo program #1
C    MODIFIED   (MM/DD/YY)
C     ssappara   07/25/01 -  bug1788355:Don't use return length NULL in ODEFIN
C     mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
C     plocke     11/14/95 -  to update for v7.3
C     lchidamb   05/16/95 -  merge changes from branch 1.2.720.1
C     rkooi2     11/05/92 -  Portability mods
C     sjain      03/16/92 -  Creation

      PROGRAM FDEMO1

*---------------------------------------------------------------
*  FDEMO1 is a demonstration program that adds new employee
*  rows to the personnel data base.  Checking
*  is done to insure the integrity of the data base.
*  The employee numbers are automatically selected using
*  the current maximum employee number as the start.
*  If any employee number is a duplicate, it is skipped.
*  The program queries the user for data as follows:
*
*    Enter employee name  :
*    Enter employee job   :
*    Enter employee salary:
*    Enter employee dept  :
*
*  If just <cr> is entered for the employee name,
*  the program terminates.
*
*  If the row is successfully inserted, the following
*  is printed:
*
*  ENAME added to DNAME department as employee N.
*
*  The maximum lengths of the 'ename', 'job', and 'dname'
*  columns are determined by an ODESCR call.
*
*  Note: VAX FORTRAN, by default, passes all CHARACTER variables
*  (variables declared as CHARACTER*N) by descriptor.
*  To compile this program on systems that pass character
*  variables by descriptor, insert %REF() where necessary.
*---------------------------------------------------------------

      IMPLICIT INTEGER (A-Z)

      INTEGER*2         LDA(32)
      INTEGER*2         CURS(32,2)
      INTEGER*2         HDA(256)
      CHARACTER*20      UID, PSW
      INTEGER*4         NOBLOK

*  CHARACTER string vars to hold the SQL statements

      CHARACTER*60      SMAX
      CHARACTER*60      SEMP
      CHARACTER*150     INS
      CHARACTER*60      SEL

      INTEGER*4         SMAXL, SEMPL, INSL, SELL

*  Program vars to be bound to SQL placeholders and
*  select-list fields .

      INTEGER*4         EMPNO, DEPTNO, SAL
      CHARACTER*10      ENAME
      CHARACTER*10      JOB
      CHARACTER*14      DNAME

*  Actual lengths of columns .

      INTEGER*4         ENAMES, JOBS, DNAMES

*  Character strings for SQL placeholders.

      CHARACTER*6       ENON
      CHARACTER*6       ENAN
      CHARACTER*4       JOBN
      CHARACTER*4       SALN
      CHARACTER*7       DEPTN

*  Lengths of character strings for SQL placeholders.

      INTEGER*4         ENONL, ENANL, JOBNL, SALNL, DEPTNL

*  Parameters for OPARSE.

      INTEGER*4         NODEFP, V7FLAG

*  Parameters for ODESCR.

      INTEGER*2     DTYPE, PREC, SCALE, NULLOK
      INTEGER*4     DSIZE, CNAMEL
      CHARACTER*80  CNAME

*---------------------------------------------------------------
*  Initialize variables.
*---------------------------------------------------------------

      SMAX = 'SELECT NVL(MAX(EMPNO),0) FROM EMP'
      SMAXL = LEN_TRIM(SMAX)

      SEMP = 'SELECT ENAME,JOB FROM EMP'
      SEMPL=  LEN_TRIM(SEMP)

      INS = 'INSERT INTO EMP(EMPNO,ENAME,JOB,SAL,
     + DEPTNO) VALUES (:EMPNO,:ENAME,:JOB,:SAL,:DEPTNO)'
      INSL = LEN_TRIM(INS)

      SEL = 'SELECT DNAME FROM DEPT WHERE DEPTNO = :1'
      SELL = LEN_TRIM(SEL)

*  All in Deferred Mode
      NODEFP = 1
      V7FLAG = 2
      ENAMEL = 10
      JOBL   = 10
      EMPNOL = 4
      DEPTL  = 4
      SALL   = 4

      ENON = ':EMPNO'
      ENAN = ':ENAME'
      JOBN = ':JOB'
      SALN = ':SAL'
      DEPTN = ':DEPTNO'

      ENONL = 6
      ENANL = 6
      JOBNL = 4
      SALNL = 4
      DEPTNL = 7

*---------------------------------------------------------------
*  Connect to ORACLE in non-blocking mode.
*  HDA must be initialized to all zeros before call to OLOG.
*---------------------------------------------------------------

      UID = 'SCOTT'
      PSW = 'TIGER'
      NOBLOK = 0
      DATA HDA/256*0/

      CALL OLOG(LDA, HDA, UID, LEN_TRIM(UID),
     +           PSW, LEN_TRIM(PSW), 0, -1, NOBLOK)
      IF (LDA(7).NE.0) THEN
          CALL ERRRPT(LDA(1), LDA(1))
          GO TO 900
      END IF

      WRITE (*, '(1X, A, A20)') 'Logged on to ORACLE as user ',
     +      UID

*---------------------------------------------------------------
*  Open two cursors for the personnel data base.
*---------------------------------------------------------------

      CALL OOPEN(CURS(1,1), LDA, 0, -1, -1, 0, -1)
      IF (CURS(1,1).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,1))
        GO TO 700
      END IF

      CALL OOPEN(CURS(1,2),LDA(1),0, -1, -1, 0, -1)
      IF (CURS(1,2).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,2))
        GO TO 700
      END IF

*---------------------------------------------------------------
*  Turn off auto-commit.  Note: the default is off,
*  so this could be omitted.
*---------------------------------------------------------------

      CALL OCOF(LDA(1))
      IF (LDA(1).NE.0) THEN
        CALL ERRRPT(LDA(1), LDA(1))
        GO TO 700
      END IF

*---------------------------------------------------------------
*  Retrieve the current maximum employee number.
*----------------------------------------------------------------

*  Parse the SQL statement.

      CALL OPARSE(CURS(1,1), SMAX, SMAXL, NODEFP, V7FLAG)
      IF (CURS(1,1).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,1))
        GO TO 700
      END IF

*  Define a buffer to receive the MAX(EMPNO) from ORACLE.

      CALL ODEFIN(CURS(1,1), 1, EMPNO, 4, 3, -1, INDP, 0, -1, -1,
     +		 RLEN,RCODE)
      IF (CURS(1,1).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,1))
        GO TO 700
      END IF


*  Execute the SQL statement.

      CALL OEXEC(CURS(1,1))
      IF (CURS(1,1).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,1))
        GO TO 700
      END IF

*  Fetch the data from ORACLE into the defined buffer.

      CALL OFETCH(CURS(1,1))
      IF (CURS(1,1).EQ.0) GO TO 50
      IF (CURS(7,1).NE.1403) THEN
        CALL ERRRPT(LDA(1), CURS(1,1))
        GO TO 700
      END IF

*  A cursor return code of 1403 means that no row
*  satisfied the query, so generate the first empno.

      EMPNO=10
50    CONTINUE

*---------------------------------------------------------------
*  Determine the max length of the employee name and job title.
*  Parse the SQL statement - it will not be executed.
*  Describe the two fields specified in the SQL statement.
*----------------------------------------------------------------

      CALL OPARSE(CURS(1,1), SEMP, SEMPL, NODEFP, V7FLAG)
      IF (CURS(1,1).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,1))
        GO TO 700
      END IF

      CNAMEL = 80
      CALL ODESCR(CURS(1,1), 1, ENAMES, DTYPE, CNAME,
     +            CNAMEL, DSIZE, PREC, SCALE, NULLOK)
      IF (CURS(1,1).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,1))
        GO TO 700
      END IF

      IF (ENAMES .GT. ENAMEL) THEN
        WRITE (*, '(1X, A, I2, A, I2)') 'ENAME too large (',
     +   ENAMES, ' for buffer (', ENAMEL, ').'
        GO TO 700
      END IF

      CNAMEL = 80
      CALL ODESCR(CURS(1,1), 2, JOBS, DTYPE, CNAME,
     +            CNAMEL, DSIZE, PREC, SCALE, NULLOK)
      IF (CURS(1,1).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,1))
        GO TO 700
      END IF

      IF (JOBS .GT. JOBL) THEN
        WRITE (*, '(1X, A, I2, A, I2)') 'JOB too large (',
     +   JOBS, ' for buffer (', JOBL, ').'
        GO TO 700
      END IF

*--------------------------------------------------------------
*  Parse the insert and select statements.
*--------------------------------------------------------------

      CALL OPARSE(CURS(1,1), INS, INSL, NODEFP, V7FLAG)
      IF (CURS(1,1).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,1))
        GO TO 700
      END IF

      CALL OPARSE(CURS(1,2), SEL, SELL, NODEFP, V7FLAG)
      IF (CURS(1,2).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,2))
        GO TO 700
      END IF

*--------------------------------------------------------------
*  Bind all placeholders.
*--------------------------------------------------------------

      CALL OBNDRV(CURS(1,1),ENON,LEN(ENON),EMPNO,EMPNOL,3,-1,0,0,-1,-1)
      IF (CURS(1,1).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,1))
        GO TO 700
      END IF

      CALL OBNDRV(CURS(1,1),ENAN,ENANL,ENAME,ENAMEL,1,-1,0,0,-1,-1)
      IF (CURS(1,1).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,1))
        GO TO 700
      END IF

      CALL OBNDRV(CURS(1,1),JOBN,JOBNL,JOB,JOBL,1,-1,0,0,-1,-1)
      IF (CURS(1,1).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,1))
        GO TO 700
      END IF

      CALL OBNDRV(CURS(1,1),SALN,SALNL,SAL,SALL,3,-1,0,0,-1,-1)
      IF (CURS(1,1).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,1))
        GO TO 700
      END IF

      CALL OBNDRV(CURS(1,1),DEPTN,DEPTNL,DEPTNO,DEPTL,3,-1,0,0,-1,-1)
      IF (CURS(1,1).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,1))
        GO TO 700
      END IF

*--------------------------------------------------------------
*  Bind the DEPTNO variable.
*--------------------------------------------------------------

      CALL OBNDRN(CURS(1,2), 1, DEPTNO, DEPTL, 3,-1,0,0,-1,-1)
      IF (CURS(1,2).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,2))
        GO TO 700
      END IF

*--------------------------------------------------------------
*  Describe the DNAME column - get the name and length.
*--------------------------------------------------------------

      DNAMEL = 14
      CALL ODESCR(CURS(1,1), 1, DNAMES, DTYPE, DNAME,
     +            DNAMEL, DSIZE, PREC, SCALE, NULLOK)
      IF (CURS(1,2).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,2))
        GO TO 700
      END IF

      IF (DNAMES .GT. DNAMEL) THEN
        WRITE (*, '(1X, A)') 'DNAME too large for buffer.'
        GO TO 700
      END IF

*--------------------------------------------------------------
*  Define the buffer to receive DNAME.
*--------------------------------------------------------------

      CALL ODEFIN(CURS(1,2),1,DNAME,DNAMEL,1,-1,INDP,0,-1,-1,
     + 		RLEN,RCODE)
      IF (CURS(1,2).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,2))
        GO TO 700
      END IF

*--------------------------------------------------------------
*  Read the user's input.  Statement 100
*  starts the main program loop.
*--------------------------------------------------------------

100   WRITE (*, '( A)')
     +  'Enter employee name (CR to QUIT) : '
      READ (*, '(A)'), ENAME
      IF (LEN_TRIM(ENAME) .EQ. 0) GO TO 700
      WRITE (*, '( A)'), 'Enter employee job   : '
      READ (*, '(A)'), JOB
      WRITE (*, '( A)') 'Enter employee salary: '
      READ (*, '(I6)'), SAL
300   WRITE (*, '( A)') 'Enter employee dept  : '
      READ (*, '(I6)'), DEPTNO

*--------------------------------------------------------------
*  Check for a valid department number by
*  executing the SELECT statement.
*--------------------------------------------------------------

      CALL OEXEC(CURS(1,2))
      IF (CURS(1,2).NE.0) THEN
        CALL ERRRPT(LDA(1), CURS(1,2))
        GO TO 700
      END IF

*--------------------------------------------------------------
*  Fetch the rows - DEPTNO is a primary key, so a max of
*  one row will be fetched.
*  If the return code is 1403 no such department exists.
*--------------------------------------------------------------

      CALL OFETCH(CURS(1,2))
      IF (CURS(1,2).EQ.0) GO TO 500
      IF (CURS(7,2).NE.1403) THEN
        CALL ERRRPT(LDA(1), CURS(1,2))
        GO TO 700
      END IF

      WRITE (*, '(1X, A)') 'No such department number'
      GO TO 300

*--------------------------------------------------------------
*  Increment EMPNO by 10.
*  Execute the insert statement.
*--------------------------------------------------------------

500   EMPNO = EMPNO + 10
      CALL OEXEC(CURS(1,1))
      IF (CURS(1,1).EQ.0) GO TO 600

*--------------------------------------------------------------
*  If the call returns code 1 (duplicate value in index),
*  generate the next possible employee number.
*--------------------------------------------------------------

      IF (CURS(7,1).NE.1) THEN
        CALL ERRRPT(LDA(1), CURS(1,1))
        GO TO 700
      END IF

      EMPNO=EMPNO+10
      GO TO 500

600   WRITE (*, 610) ENAME, DNAME, EMPNO
610   FORMAT(/, 1X, A10, ' added to the ', A14,
     +    ' department as employee# ', I4, /)

*--------------------------------------------------------------
*  The row has been added - commit this transaction.
*--------------------------------------------------------------

      CALL OCOM(LDA(1))
      IF (LDA(1).NE.0) THEN
        CALL ERRRPT(LDA(1), LDA(1))
        GO TO 700
      END IF

      GO TO 100

*--------------------------------------------------------------
*  Either a fatal error has occurred or the user typed
*  <CR> for the employee name.
*  Close the cursors, disconnect, and end the program.
*--------------------------------------------------------------

700   CONTINUE

      CALL OCLOSE(CURS(1,1))
      IF (CURS(1,1).NE.0) CALL ERRRPT(LDA(1), CURS(1,1))
      CALL OCLOSE(CURS(1,2))
      IF (CURS(1,2).NE.0) CALL ERRRPT(LDA(1), CURS(1,2))

      CALL OLOGOF(LDA(1))
      IF (LDA(1).NE.0) CALL ERRRPT(LDA(1), LDA(1))
900   STOP
      END


*--------------------------------------------------------------
*  ERRRPT prints the cursor number, the error code, and the
*  OCI function code.
*
*  CURS is a cursor.
*  N is the cursor number.
*--------------------------------------------------------------

      SUBROUTINE ERRRPT(LDA, CURS)
      INTEGER*2 CURS(32), LDA(32)

      CHARACTER*160 ERRMSG

      IF (CURS(6) .GT. 0) THEN
        WRITE (*, '(1X, A, I3)') 'ORACLE error processing OCI
     + function ', CURS(6)
      END IF

      CALL OERHMS(LDA(1), CURS(7), ERRMSG, 160)
      WRITE (*, '(1X, A)') ERRMSG

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

