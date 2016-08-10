Rem
Rem $Header: xademo1.sql 30-nov-2005.00:34:26 yohu Exp $
Rem
Rem xademo1.sql
Rem
Rem Copyright (c) 2005, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      xademo1.sql - XA PL/SQL APIs demo 1
Rem
Rem    DESCRIPTION
Rem      This demo program illustrates how XA PL/SQL APIs may be used
Rem      in programming PL/SQL procedures/functions. 
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    yohu        11/21/05 - yohu_xa_plsql
Rem    yohu        10/26/05 - Created
Rem

connect scott/tiger
set serveroutput on
CREATE or REPLACE PROCEDURE xa_demo(gtrid IN NUMBER, op IN VARCHAR2, 
                                    emp_no IN NUMBER, e_name IN VARCHAR2) is
     xid DBMS_XA_XID;
     rc  PLS_INTEGER;
     oer PLS_INTEGER;
     XAE exception;
BEGIN
  xid := DBMS_XA_XID(gtrid);

  rc  := dbms_xa.XA_START(xid, dbms_xa.TMNOFLAGS);
  IF rc!=dbms_xa.XA_OK THEN
    oer := dbms_xa.XA_GETLASTOER();
    dbms_output.put_line('ORA-' || oer || ' occured, xa_start failed!');
    raise XAE;
  ELSE dbms_output.put_line('XA_START   okay');
  END IF;

  CASE
   WHEN op = 'add' THEN 
    INSERT INTO emp(empno, ename, sal) VALUES(emp_no, e_name, 1111);
   WHEN op = 'del' THEN 
    DELETE FROM emp where empno = emp_no AND ename = e_name;
   WHEN op = 'inc' THEN
    UPDATE emp SET sal=sal+1 WHERE empno = emp_no AND ename = e_name;
   ELSE
    BEGIN
     dbms_output.put_line('Unknown op code!');
     RAISE XAE;
    END;
  END CASE;

  rc  := dbms_xa.XA_END(xid, dbms_xa.TMSUCCESS);
  IF rc!=dbms_xa.XA_OK THEN
    oer := dbms_xa.XA_GETLASTOER();
    dbms_output.put_line('ORA-' || oer || ' occured, xa_end failed!');
    raise XAE;
  ELSE dbms_output.put_line('XA_END     okay');
  END IF;

  rc  := dbms_xa.XA_PREPARE(xid);
  IF rc=dbms_xa.XA_OK THEN
    dbms_output.put_line('XA_PREPARE okay');

    rc := dbms_xa.XA_COMMIT(xid, FALSE);
    IF rc!=dbms_xa.XA_OK THEN
      oer := dbms_xa.XA_GETLASTOER();
      dbms_output.put_line('ORA-' || oer || ' occured, xa_commit failed!');
      raise XAE;
    ELSE dbms_output.put_line('XA_COMMIT  okay');
    END IF;

  ELSIF rc=dbms_xa.XA_RDONLY THEN
    dbms_output.put_line('XA_PREPARE readonly');
  ELSE
    oer := dbms_xa.XA_GETLASTOER();
    dbms_output.put_line('ORA-' || oer || ' occured, xa_prepare failed!');
    raise XAE;
  END IF;
EXCEPTION
  WHEN OTHERS THEN
      dbms_output.put_line('XA error('||rc||
                           ') occured, rolling back the transaction...');
      rc := dbms_xa.XA_END(xid, dbms_xa.TMSUCCESS);
      rc := dbms_xa.XA_ROLLBACK(xid);
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

REM sample usage:

BEGIN xa_demo(111, 'add', 1234, 'xa_demo1'); END;
/
Rem the following rollback should be a noop
ROLLBACK; 
select empno,ename,sal from emp where empno=1234 and ename='xa_demo1';

BEGIN xa_demo(222, 'inc', 1234, 'xa_demo1'); END;
 /
Rem the following rollback should be a noop
ROLLBACK;
select empno,ename,sal from emp where empno=1234 and ename='xa_demo1';

BEGIN xa_demo(333, 'del', 1234, 'xa_demo1'); END;
/
select empno,ename,sal from emp where empno=1234 and ename='xa_demo1';

disconnect
quit
