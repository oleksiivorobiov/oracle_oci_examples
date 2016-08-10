Rem
Rem $Header: xademo2.sql 30-nov-2005.01:13:59 yohu Exp $
Rem
Rem xademo2.sql
Rem
Rem Copyright (c) 2005, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      xademo2.sql - XA PL/SQL API usage demo 2
Rem
Rem    DESCRIPTION
Rem      This demo program illustrates how XA PL/SQL APIs may be used
Rem      to have a single transaction working across sessions. 
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    yohu        11/21/05 - yohu_xa_plsql
Rem    yohu        10/26/05 - Created
Rem

Rem Session 1: starts a transaction and do some work

connect scott/tiger
SET SERVEROUTPUT ON
DECLARE
     rc  PLS_INTEGER;
     oer PLS_INTEGER;
     XAE exception;
BEGIN
  rc  := dbms_xa.XA_START(DBMS_XA_XID(123), dbms_xa.TMNOFLAGS);
  IF rc!=dbms_xa.XA_OK THEN
    oer := dbms_xa.XA_GETLASTOER();
    dbms_output.put_line('ORA-' || oer || ' occured, xa_start failed!');
    raise XAE;
  ELSE dbms_output.put_line('XA_START(new xid=123)     okay');
  END IF;

  UPDATE emp SET sal=sal*1.1 WHERE empno = 7788;

  rc  := dbms_xa.XA_END(DBMS_XA_XID(123), dbms_xa.TMSUSPEND);
  IF rc!=dbms_xa.XA_OK THEN
    oer := dbms_xa.XA_GETLASTOER();
    dbms_output.put_line('ORA-' || oer || ' occured, xa_end failed!');
    raise XAE;
  ELSE dbms_output.put_line('XA_END(suspend xid=123)   okay');
  END IF;
EXCEPTION
  WHEN OTHERS THEN
      dbms_output.put_line('XA error('||rc||
                           ') occured, rolling back the transaction...');
      rc  := dbms_xa.XA_END(DBMS_XA_XID(123), dbms_xa.TMSUCCESS);
      rc := dbms_xa.XA_ROLLBACK(DBMS_XA_XID(123));
      IF rc != dbms_xa.XA_OK THEN
             oer := dbms_xa.XA_GETLASTOER();
             dbms_output.put_line('XA-'||rc||', ORA-' || oer ||
                                  ' xa_rollback does not return XA_OK!');
      raise_application_error(-20001, 'ORA-'||oer||
                              ' error in rolling back a failed transaction!');
      END IF;
      raise_application_error(-20002, 'ORA-'||oer||
                 ' error in transaction processing, transction rolled back!');
END;
/
show errors
disconnect


Rem Session 2: resumes the transaction and do some work

connect scott/tiger
SET SERVEROUTPUT ON
DECLARE
     rc  PLS_INTEGER;
     oer PLS_INTEGER;
     s   NUMBER;
     XAE exception;
BEGIN
  rc  := dbms_xa.XA_START(DBMS_XA_XID(123), dbms_xa.TMRESUME);
  IF rc!=dbms_xa.XA_OK THEN
    oer := dbms_xa.XA_GETLASTOER();
    dbms_output.put_line('ORA-' || oer || ' occured, xa_start failed!');
    raise XAE;
  ELSE dbms_output.put_line('XA_START(resume xid=123)  okay');
  END IF;

  SELECT sal INTO s FROM emp WHERE empno = 7788;
  dbms_output.put_line('empno = 7788, sal = ' || s);

  rc  := dbms_xa.XA_END(DBMS_XA_XID(123), dbms_xa.TMSUCCESS);
  IF rc!=dbms_xa.XA_OK THEN
    oer := dbms_xa.XA_GETLASTOER();
    dbms_output.put_line('ORA-' || oer || ' occured, xa_end failed!');
    raise XAE;
  ELSE dbms_output.put_line('XA_END(detach xid=123)    okay');
  END IF;
EXCEPTION
    WHEN OTHERS THEN
      dbms_output.put_line('XA error('||rc||
                           ') occured, rolling back the transaction...');
      rc := dbms_xa.XA_END(DBMS_XA_XID(123), dbms_xa.TMSUCCESS);
      rc := dbms_xa.XA_ROLLBACK(DBMS_XA_XID(123));
      IF rc != dbms_xa.XA_OK THEN
             oer := dbms_xa.XA_GETLASTOER();
             dbms_output.put_line('XA-'||rc||', ORA-' || oer ||
                                  ' xa_rollback does not return XA_OK!');
      raise_application_error(-20001, 'ORA-'||oer||
                              ' error in rolling back a failed transaction!');
      END IF;
      raise_application_error(-20002, 'ORA-'||oer||
                 ' error in transaction processing, transction rolled back!');
END;
/
show errors
disconnect


Rem Session 3: commit the transaction

connect scott/tiger
SET SERVEROUTPUT ON
DECLARE
     rc  PLS_INTEGER;
     oer PLS_INTEGER;
     XAE exception;
BEGIN
  rc := dbms_xa.XA_COMMIT(DBMS_XA_XID(123), TRUE);
  IF rc!=dbms_xa.XA_OK THEN
    oer := dbms_xa.XA_GETLASTOER();
    dbms_output.put_line('ORA-' || oer || ' occured, xa_commit failed!');
    raise XAE;
  ELSE dbms_output.put_line('XA_COMMIT(commit xid=123) okay');
  END IF;

EXCEPTION
    WHEN XAE THEN
        dbms_output.put_line('XA error('||rc||
                             ') occured, rolling back the transaction...');
        rc := dbms_xa.XA_ROLLBACK(DBMS_XA_XID(123));
        IF rc != dbms_xa.XA_OK THEN
               oer := dbms_xa.XA_GETLASTOER();
               dbms_output.put_line('XA-'||rc||', ORA-' || oer ||
                                    ' xa_rollback does not return XA_OK!');
        raise_application_error(-20001, 'ORA-'||oer||
                                ' error in rolling back a failed transaction!');
        END IF;
        raise_application_error(-20002, 'ORA-'||oer||
                   ' error in transaction processing, transction rolled back!');
END;
/
show errors
disconnect
quit
