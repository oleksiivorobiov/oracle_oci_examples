C
C $Header: fdemo3.for 14-jul-99.13:44:03 mjaeger Exp $
C
C Copyright (c) 1991, 1999, Oracle Corporation.  All rights reserved.
C
C    NAME
C      fdemo3.for - Fortran demo program # 3
C    MODIFIED   (MM/DD/YY)
C     mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
C     plocke     11/14/95 -  to update for v7.3
C     lchidamb   05/16/95 -  merge changes from branch 1.1.720.1
C     sjain      03/16/92 -  Creation

*  FDEMO3.FOR
*  OCI FORTRAN Sample Program 3
*
*  Demonstrates using the OFLNG routine to retrieve
*  part of a LONG RAW column
*
*  This example "plays" a digitized voice message
*  by repeatedly extracting 64 kB chunks of the message
*  from the row in the table, and sending them to a
*  converter buffer (for example, a Digital-to-Analog
*  Converter (DAC) FIFO buffer).
*
*  The hardware-specific DAC routine is merely simulated
*  in this example.

      PROGRAM FDEMO3
      IMPLICIT INTEGER(A-Z)

*  Connect and Cursor Data Structures

      INTEGER*2        CDA(32)
      INTEGER*2        LDA(32)
      INTEGER*2        HDA(256)

*  Program Variables

      CHARACTER*132    SQLSTM
      CHARACTER*20     UID, PWD
      INTEGER          MSGID, MSGLEN
      INTEGER*2        INDP, RLEN, RCODE
      INTEGER          RETL
      BYTE             DBIN(200000)
      CHARACTER*6      FMT
      INTEGER*4        NOBLOK

*  Connect to ORACLE in non-blocking mode.
*  The HDA must be initialized to all zeros before calling OLOG.
      UID = 'SCOTT'
      PWD = 'TIGER'
      DATA HDA/256*0/

      CALL OLOG(LDA, HDA, UID, LEN_TRIM(UID),
     +          PWD, LEN_TRIM(PWD), 0, -1, NOBLOK)
      IF (LDA(1) .NE. 0) THEN
          WRITE (*, '(1X, A)') 'Cannot connect as scott/tiger...'
          WRITE (*, '(1X, A)') 'Application terminating...'
          GOTO 999
      END IF

*  Open the cursor.  (Use UID as a dummy parameter--it
*  won't be looked at.)
      CALL OOPEN(CDA, LDA, UID, 0, 0, UID, 0)
      IF (CDA(7) .NE. 0) THEN
          CALL ERRPT(CDA)
          GOTO 900
      END IF

*  Drop the old table.
      WRITE (*, '( A)') 'OK to drop table VOICE_MAIL (Y or N)? : '
      READ '(A)', FMT
      IF (FMT(1:1) .EQ. 'y' .OR. FMT(1:1) .EQ. 'Y') THEN
         GO TO 10
      ELSE
         GO TO 900
      ENDIF

*  Parse the DROP TABLE statement.
10    SQLSTM = 'DROP TABLE VOICE_MAIL'
      CALL OPARSE(CDA, SQLSTM, LEN_TRIM(SQLSTM), 0, 2)
      IF (CDA(7) .EQ. 0) THEN
          WRITE (*, '(1X, A)') 'Table VOICE_MAIL dropped.'
      ELSEIF (CDA(7) .EQ. 942) THEN
         WRITE (*, '(1X, A)') 'Table did not exist.'
      ELSE
         CALL ERRPT(LDA, CDA)
         GO TO 900
      ENDIF

*  Create new table.  Parse with DEFFLG set to zero,
*  to immediately execute the DDL statement.  The LNGFLG
*  is set to 2 (Version 7).

      SQLSTM = 'CREATE TABLE VOICE_MAIL
     +   (MSG_ID NUMBER(6), MSG_LEN NUMBER(12), MSG LONG RAW)'

*  Parse the statement.  Do not defer the parse, so that the
*  DDL statement is executed immediately.
      CALL OPARSE(CDA, SQLSTM, LEN_TRIM(SQLSTM), 0, 2)
      IF (CDA(7) .EQ. 0) THEN
          WRITE (*, '(1X, A)') 'Created table VOICE_MAIL.'
      ELSE
          CALL ERRPT(LDA, CDA)
          GOTO 900
      END IF

*  Insert some dummy data into the table.
      SQLSTM = 'INSERT INTO VOICE_MAIL VALUES (:1, :2, :3)'
      CALL OPARSE(CDA, SQLSTM, LEN_TRIM(SQLSTM), 1, 2)
      IF (CDA(7) .NE. 0) THEN
          CALL ERRPT(LDA, CDA)
          GOTO 900
      END IF

*  Do the binds for the input data to set values
*  in the new table.
      INDP = 0
      CALL OBNDRN(CDA, 1, MSGID, 4, 3, 0, INDP, FMT, 0, 0)
      IF (CDA(7) .NE. 0) THEN
          CALL ERRPT(LDA, CDA)
          GOTO 900
      END IF

      CALL OBNDRN(CDA, 2, MSGLEN, 4, 3, 0, INDP, FMT, 0, 0)
      IF (CDA(7) .NE. 0) THEN
          CALL ERRPT(LDA, CDA)
          GOTO 900
      END IF

      CALL OBNDRN(CDA, 3, DBIN, 200000, 24, 0, INDP, FMT, 0, 0)
      IF (CDA(7) .NE. 0) THEN
          CALL ERRPT(LDA, CDA)
          GOTO 900
      END IF

*  Fill the input buffer with some dummy data.
      MSGID = 100
      MSGLEN = 200000
      DO 100 I = 1, 200000
100       DBIN(I) = 42

*  Execute the statement to INSERT the data
      WRITE (*, '(1X, A)') 'Inserting data into the table.'
      CALL OEXN(CDA, 1, 0)
      IF (CDA(7) .NE. 0) THEN
          CALL ERRPT(LDA, CDA)
          GOTO 900
      END IF

*  Do the selects.  First position the cursor at the
*  proper row, using the MSG_ID.  Then fetch the data
*  in 64K chunks, using OFLNG.
      SQLSTM = 'SELECT MSG_ID, MSG_LEN, MSG
     + FROM VOICE_MAIL WHERE MSG_ID = 100'

      CALL OPARSE(CDA, SQLSTM, LEN_TRIM(SQLSTM), 1, 2)
      IF (CDA(7) .NE. 0) THEN
          CALL ERRPT(LDA, CDA)
          GOTO 900
      END IF

*  Define the output variables for the SELECT.
      CALL ODEFIN(CDA, 1, MSGID, 4, 3, 0, %VAL(-1), %VAL(-1),
     +     0, 0, %VAL(-1), %VAL(-1))
      IF (CDA(7) .NE. 0) THEN
          CALL ERRPT(LDA, CDA)
          GOTO 900
      END IF

      CALL ODEFIN(CDA, 2, MSGLEN, 4, 3, 0, %VAL(-1), %VAL(-1),
     +     0, 0, %VAL(-1), %VAL(-1))
      IF (CDA(7) .NE. 0) THEN
          CALL ERRPT(LDA, CDA)
          GOTO 900
      END IF

      CALL ODEFIN(CDA, 3, DBIN, 200000, 24, 0, INDP, %VAL(-1),
     +     0, 0, RLEN, RCODE)
      IF (CDA(7) .NE. 0) THEN
          CALL ERRPT(LDA, CDA)
          GOTO 900
      END IF

*  Do the query, getting the MSG_ID to position the cursor, and
*  the first 100 bytes of the message.
*  CANCEL and EXACT are FALSE.
      CALL OEXFET(CDA, 1, 0, 0)
      IF (CDA(7) .NE. 0) THEN
          CALL ERRPT(LDA, CDA)
          GOTO 900
      END IF

      WRITE (*, '(1X, A, I4, A)') 'Message', MSGID,
     +' is available.'
      WRITE (*, '(1X, A, I7, A)') 'The length is', MSGLEN,
     +' bytes.'

*  Play out the message, calling the DAC routine for each
*  64K chunk fetched by OFLNG.

      OFFSET = 0
      N = MSGLEN/65536 + 1
      DO 200 J = 1, N
        IF (MSGLEN .LT. 65536) THEN
           LEN = MSGLEN
        ELSE
           LEN = 65536
        ENDIF
        CALL OFLNG(CDA, 3, DBIN, LEN, 24, RETL, OFFSET)
        IF (CDA(7) .NE. 0) THEN
           CALL ERRPT(LDA, CDA)
           GOTO 900
        ENDIF
        CALL PLAYMSG(DBIN, LEN)
        MSGLEN = MSGLEN - LEN
        IF (MSGLEN .LT. 0) GO TO 900
200   CONTINUE

900   CALL OCLOSE(CDA)
      IF (CDA(7) .NE. 0) THEN
         CALL ERRPT(LDA, CDA)
         GOTO 900
      END IF
      CALL OLOGOF(LDA)
      IF (LDA(7) .NE. 0) THEN
         CALL ERRPT(LDA, CDA)
         GOTO 900
      END IF

999   STOP 'End of OCIDEMO3.'
      END

      SUBROUTINE PLAYMSG(OUT, LEN)
      BYTE     OUT(65536)
      INTEGER  LEN

      WRITE (*, '(1X, A, I7, A)') 'Playing', LEN, ' bytes.'
      RETURN
      END


      SUBROUTINE ERRPT(LDA, CDA)
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


