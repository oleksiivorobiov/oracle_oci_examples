rem
rem $Header: fgacdemo.sql 09-may-2003.13:35:57 rvissapr Exp $
rem
rem Copyright (c) 1999, 2003, Oracle Corporation.  All rights reserved.  
rem
rem   NAME
rem     fgacdemo.sql - Build Oracle8i Security Demonstration Tables/Users
rem   DESCRIPTION
rem     This SQL script builds Oracle8i Security demonstration tables/user
rem   MODIFIED   (MM/DD/YY)
rem     rvissapr   05/09/03  - remove temporary tablespace clause
rem     rvissapr   01/08/03  - bug 2368367
rem     dmwong     03/08/00  - remove MURRAY .
rem     dmwong     01/05/00  - merge the split line
rem     mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines

rem  This script builds a database that enforces security based on
rem  server enforced security using fine grain access control and application
rem  context. Appropriate security context is setup using application
rem  context and fine grain access control enforces data access control using
rem  these secure application context.

rem ========================================================================
rem   First clean up previous installs
rem ========================================================================

CONNECT SYSTEM/MANAGER
DROP TRIGGER SECDEMO.SET_EXPENSE_CTX ;
DROP USER MOREAU;
DROP USER MILLER;

DROP USER SECDEMO CASCADE;
DROP CONTEXT expenses_reporting;

rem ========================================================================
rem  Then create users for the demo  (you may want to customize tablespaces)
rem ========================================================================

CREATE USER "SECDEMO" IDENTIFIED BY "SECDEMO"
DEFAULT TABLESPACE "SYSTEM"
PROFILE DEFAULT
QUOTA UNLIMITED ON "SYSTEM" ACCOUNT UNLOCK;

GRANT CONNECT TO SECDEMO;
GRANT CREATE ANY CONTEXT TO SECDEMO;
GRANT CREATE ANY SYNONYM TO SECDEMO;
GRANT CREATE TRIGGER TO "SECDEMO";
GRANT CREATE PROCEDURE TO SECDEMO;
GRANT "EXECUTE_CATALOG_ROLE" TO "SECDEMO";
ALTER USER "SECDEMO" DEFAULT ROLE ALL;


CREATE USER "MOREAU" IDENTIFIED BY "MOREAU"
DEFAULT TABLESPACE "SYSTEM"
PROFILE DEFAULT
QUOTA UNLIMITED ON "SYSTEM" ACCOUNT UNLOCK;
GRANT CONNECT TO MOREAU;


CREATE USER "MILLER" IDENTIFIED BY "MILLER"
DEFAULT TABLESPACE "SYSTEM"
PROFILE DEFAULT
QUOTA UNLIMITED ON "SYSTEM" ACCOUNT UNLOCK;
GRANT CONNECT TO MILLER;


GRANT "EXECUTE_CATALOG_ROLE" TO "MOREAU";
ALTER USER "MOREAU" DEFAULT ROLE ALL;
GRANT "EXECUTE_CATALOG_ROLE" TO "MILLER";
ALTER USER "MILLER" DEFAULT ROLE ALL;

CREATE PUBLIC SYNONYM exprep_ctx FOR secdemo.exprep_ctx;

rem ========================================================================
rem  Now Create the tables/views for the demo
rem ========================================================================

CONNECT SECDEMO/SECDEMO

rem Create new datasources

CREATE TABLE EMPLOYEE (
        EMPLOYEE_ID             NUMBER(4),
        LAST_NAME               VARCHAR2(15),
        FIRST_NAME              VARCHAR2(15),
        MIDDLE_INITIAL          VARCHAR2(1),
        COST_CENTER_ID          NUMBER(4),
        MANAGER_ID              NUMBER(4));

CREATE TABLE COST_CENTER (
        COST_CENTER_ID          NUMBER(4),
        MANAGER_ID              NUMBER(4),
        DESCRIPTION             VARCHAR2(30));

CREATE TABLE EXP_REPORT (
        REPORT_ID               NUMBER(4),
        EMPLOYEE_ID             NUMBER(4),
        COST_CENTER_ID          NUMBER(4),
        SUBMISSION_DATE         DATE,
        APPROVAL_DATE           DATE,
        PURPOSE                 VARCHAR2(30));

CREATE TABLE EXP_LINE  (
        REPORT_ID               NUMBER(4),
        LINE_ID                 NUMBER(4),
        TYPE_ID                 NUMBER(4),
        RECEIVED_AMOUNT         NUMBER(7,2),
        RECEIPT                 NUMBER(1),
        EXP_DATE                DATE,
        CURRENCY_ID             NUMBER(4));

CREATE TABLE EXP_TYPE   (
        TYPE_ID                 NUMBER(4),
        DESCRIPTION             VARCHAR2(30));

CREATE TABLE EXP_CURRENCY (
        CURRENCY_ID             NUMBER(4),
        DESCRIPTION             VARCHAR2(30),
        RATE                    NUMBER(7,2),
        SYMBOL                  VARCHAR2(2));

rem Add views

DROP VIEW EXP_REPORT_SUM_VIEW;

CREATE VIEW EXP_REPORT_SUM_VIEW AS
SELECT  E.EMPLOYEE_ID,E.LAST_NAME,R.REPORT_ID, R.PURPOSE,
  SUM(L.RECEIVED_AMOUNT) TOTAL_AMOUNT, R.COST_CENTER_ID,
  R.SUBMISSION_DATE,R.APPROVAL_DATE
FROM EMPLOYEE E,EXP_REPORT R, EXP_LINE L
WHERE  E.EMPLOYEE_ID=R.EMPLOYEE_ID
AND R.REPORT_ID=L.REPORT_ID
GROUP BY  E.EMPLOYEE_ID,R.REPORT_ID,E.LAST_NAME, R.PURPOSE, R.COST_CENTER_ID,
  R.SUBMISSION_DATE,R.APPROVAL_DATE;


rem Grant SELECT privileges to PUBLIC on the datasources

GRANT SELECT ON EMPLOYEE TO PUBLIC;
GRANT SELECT ON COST_CENTER TO PUBLIC;
GRANT SELECT ON EXP_REPORT TO PUBLIC;
GRANT SELECT ON EXP_LINE TO PUBLIC;
GRANT SELECT ON EXP_TYPE TO PUBLIC;
GRANT SELECT ON EXP_CURRENCY TO PUBLIC;
GRANT SELECT ON EXP_REPORT_SUM_VIEW TO PUBLIC;

GRANT UPDATE ON EXP_REPORT TO PUBLIC;
GRANT DELETE ON EXP_REPORT TO PUBLIC;
GRANT INSERT ON EXP_REPORT  TO PUBLIC;
GRANT DELETE ON EXP_LINE TO PUBLIC;
GRANT INSERT ON EXP_LINE TO PUBLIC;


rem Insert Data

INSERT INTO EXP_CURRENCY VALUES (1, '$ US DOLLAR',1.0,'$');
INSERT INTO EXP_CURRENCY VALUES (2, 'FF - FRENCH FRANC',0.2,'FF');
INSERT INTO EXP_CURRENCY VALUES (3, '£ - UK POUNDS',2.0,'£');
INSERT INTO EXP_CURRENCY VALUES (4, 'DM - DEUTCH MARKS',0.6,'DM');

INSERT INTO EXP_TYPE VALUES (1, 'AIRFARE');
INSERT INTO EXP_TYPE VALUES (2, 'TAXI');
INSERT INTO EXP_TYPE VALUES (3, 'RENTAL CAR');
INSERT INTO EXP_TYPE VALUES (4, 'LIMO/CAR SERVICE');
INSERT INTO EXP_TYPE VALUES (5, 'ROOM');
INSERT INTO EXP_TYPE VALUES (6, 'FOOD/DRINKS ON HOTEL BILL');
INSERT INTO EXP_TYPE VALUES (7, 'PHONE/OTHER ON HOTEL BILL');
INSERT INTO EXP_TYPE VALUES (8, 'BREAKFAST');
INSERT INTO EXP_TYPE VALUES (9, 'LUNCH');
INSERT INTO EXP_TYPE VALUES (10, 'DINNER');

INSERT INTO COST_CENTER  VALUES (692, 7839, 'ADMINISTRATION');
INSERT INTO COST_CENTER  VALUES (672, 7839, 'US SALES');
INSERT INTO COST_CENTER  VALUES (667,7506, 'ASIAN SALES');
INSERT INTO COST_CENTER  VALUES (670,7569, 'EUROPEAN SALES');
INSERT INTO COST_CENTER  VALUES (668,7507, 'WW SUPPORT');
INSERT INTO COST_CENTER  VALUES (671,7839, 'WW MARKETING');
INSERT INTO COST_CENTER  VALUES (673, 7505, 'US MARKETING');
INSERT INTO COST_CENTER  VALUES (674, 7698, 'ASIAN MARKETING');
INSERT INTO COST_CENTER  VALUES (669, 7566, 'EUROPEAN MARKETING');


INSERT INTO EMPLOYEE VALUES
    (7369,'SMITH','JOHN','Q',667,7902);
INSERT INTO EMPLOYEE VALUES
    (7499,'ALLEN','KEVIN','J',670,7698);
INSERT INTO EMPLOYEE VALUES
    (7505,'DOYLE','JEAN','K',671,7839);
INSERT INTO EMPLOYEE VALUES
    (7506,'DENNIS','LYNN','S',671,7839);
INSERT INTO EMPLOYEE VALUES
    (7507,'BAKER','LESLIE','D',671,7839);
INSERT INTO EMPLOYEE VALUES
    (7521,'WARD','CYNTHIA','D',670,7698);
INSERT INTO EMPLOYEE VALUES
    (7555,'PETERS','DANIEL','T',670,7505);
INSERT INTO EMPLOYEE VALUES
    (7557,'SHAW','KAREN','P',670,7505);
INSERT INTO EMPLOYEE VALUES
    (7560,'DUNCAN','SARAH','S',670,7506);
INSERT INTO EMPLOYEE VALUES
    (7564,'LANGE','GREGORY','J',670,7506);
INSERT INTO EMPLOYEE VALUES
    (7566,'JONES','TERRY','M',671,7839);
INSERT INTO EMPLOYEE VALUES
    (7569,'MOREAU','ALBERT','L',670,7839);
INSERT INTO EMPLOYEE VALUES
    (7600,'PORTER','RAYMOND','Y',670,7505);
INSERT INTO EMPLOYEE VALUES
    (7609,'LEWIS','RICHARD','M',668,7507);
INSERT INTO EMPLOYEE VALUES
    (7654,'MARTIN','KENNETH','J',670,7698);
INSERT INTO EMPLOYEE VALUES
    (7676,'SOMMERS','DENISE','D',668,7507);
INSERT INTO EMPLOYEE VALUES
    (7698,'JURGEN','DIETER','S',670,7839);
INSERT INTO EMPLOYEE VALUES
    (7782,'CLARK','CAROL','F',671,7839);
INSERT INTO EMPLOYEE VALUES
    (7788,'SCOTT','DONALD','T',669,7566);
INSERT INTO EMPLOYEE VALUES
    (7789,'WEST','LIVIA','N',670,7506);
INSERT INTO EMPLOYEE VALUES
    (7799,'FISHER','MATTHEW','G',669,7569);
INSERT INTO EMPLOYEE VALUES
    (7820,'ROSS','PAUL','S',670,7505);
INSERT INTO EMPLOYEE VALUES
    (7839,'KING','FRANCIS','A',672,7839);
INSERT INTO EMPLOYEE VALUES
    (7844,'TURNER','MARY','A',670,7698);
INSERT INTO EMPLOYEE VALUES
    (7876,'ADAMS','DIANE','G',667,7788);
INSERT INTO EMPLOYEE VALUES
    (7900,'JAMES','FRED','S',667,7698);
INSERT INTO EMPLOYEE VALUES
    (7902,'FORD','JENNIFER','D',669,7566);
INSERT INTO EMPLOYEE VALUES
    (7916,'ROBERTS','GRACE','M',669,7569);
INSERT INTO EMPLOYEE VALUES
    (7919,'DOUGLAS','MICHAEL','A',667,7799);
INSERT INTO EMPLOYEE VALUES
    (7934,'MILLER','BARBARA','M',670,7782);
INSERT INTO EMPLOYEE VALUES
    (7950,'JENSEN','ALICE','B',667,7505);
INSERT INTO EMPLOYEE VALUES
    (7954,'MURRAY','JAMES','T',670,7506);


INSERT INTO EXP_REPORT VALUES
    (1,7954,667,SYSDATE-3,SYSDATE,'Customer Visit');

INSERT INTO EXP_REPORT VALUES
    (2,7950,670,SYSDATE,NULL,'Customer Visit');
INSERT INTO EXP_REPORT VALUES
    (3,7954,670,SYSDATE,NULL,'Customer Visit');
INSERT INTO EXP_REPORT VALUES
    (4,7954,671,SYSDATE,NULL,'Customer Visit');
INSERT INTO EXP_REPORT VALUES
    (5,7934,670,SYSDATE-1,NULL,'Training ');
INSERT INTO EXP_REPORT VALUES
    (6,7698,670,SYSDATE-7,NULL,'Training ');
INSERT INTO EXP_REPORT VALUES
    (7,7698,670,SYSDATE-7,NULL,'Customer Visit ');
INSERT INTO EXP_REPORT VALUES
    (8,7934,671,SYSDATE-7,NULL,'Training ');
INSERT INTO EXP_REPORT VALUES
    (9,7934,670,SYSDATE-7,NULL,'Training ');
INSERT INTO EXP_REPORT VALUES
    (10,7954,670,SYSDATE-20,SYSDATE-20,'Customer Visit');
INSERT INTO EXP_REPORT VALUES
    (11,7954,670,SYSDATE-20,SYSDATE-17,'Customer Visit');
INSERT INTO EXP_REPORT VALUES
    (12,7569,670,SYSDATE-22,SYSDATE-19,'Customer Visit');


INSERT INTO EXP_LINE VALUES
    (1,1,1,500.2,1,SYSDATE-33,1);
INSERT INTO EXP_LINE VALUES
    (1,2,8,12.2,1,SYSDATE-33,1);
INSERT INTO EXP_LINE VALUES
    (1,3,9,20.0,1,SYSDATE-33,1);
INSERT INTO EXP_LINE VALUES
    (2,1,2,21.0,1,SYSDATE-23,1);
INSERT INTO EXP_LINE VALUES
    (2,2,5,200.0,1,SYSDATE-23,1);
INSERT INTO EXP_LINE VALUES
    (2,3,9,12.0,1,SYSDATE-23,1);
INSERT INTO EXP_LINE VALUES
    (2,4,10,20.0,1,SYSDATE-23,1);
INSERT INTO EXP_LINE VALUES
    (3,1,9,10.2,1,SYSDATE-33,1);
INSERT INTO EXP_LINE VALUES
    (4,1,5,210.3,1,SYSDATE-31,1);
INSERT INTO EXP_LINE VALUES
    (4,2,6,21.0,1,SYSDATE-31,1);
INSERT INTO EXP_LINE VALUES
    (4,3,7,12.1,1,SYSDATE-31,1);
INSERT INTO EXP_LINE VALUES
    (4,4,8,10.3,1,SYSDATE-31,1);
INSERT INTO EXP_LINE VALUES
    (5,1,10,53.2,1,SYSDATE-28,1);
INSERT INTO EXP_LINE VALUES
    (6,1,10,23.2,1,SYSDATE-44,2);
INSERT INTO EXP_LINE VALUES
    (7,1,5,210.3,1,SYSDATE-28,1);
INSERT INTO EXP_LINE VALUES
    (7,2,6,21.0,1,SYSDATE-28,1);
INSERT INTO EXP_LINE VALUES
    (7,3,7,12.1,1,SYSDATE-28,1);
INSERT INTO EXP_LINE VALUES
    (7,4,8,10.3,1,SYSDATE-28,1);
INSERT INTO EXP_LINE VALUES
    (8,1,5,1120.3,1,SYSDATE-27,3);
INSERT INTO EXP_LINE VALUES
    (8,2,6,20.0,1,SYSDATE-27,3);
INSERT INTO EXP_LINE VALUES
    (8,3,7,17.1,1,SYSDATE-27,3);
INSERT INTO EXP_LINE VALUES
    (8,4,8,20.3,1,SYSDATE-27,3);

INSERT INTO EXP_LINE VALUES
    (9,1,5,1120.3,1,SYSDATE-20,3);
INSERT INTO EXP_LINE VALUES
    (9,2,6,20.0,1,SYSDATE-20,3);
INSERT INTO EXP_LINE VALUES
    (9,3,7,17.1,1,SYSDATE-20,3);
INSERT INTO EXP_LINE VALUES
    (9,4,8,20.3,1,SYSDATE-20,3);
INSERT INTO EXP_LINE VALUES
    (9,5,5,1120.3,1,SYSDATE-20,3);
INSERT INTO EXP_LINE VALUES
    (9,6,6,20.0,1,SYSDATE-20,3);

INSERT INTO EXP_LINE VALUES
    (10,1,5,1120.3,1,SYSDATE-45,1);
INSERT INTO EXP_LINE VALUES
    (11,1,5,1120.3,1,SYSDATE-38,1);
INSERT INTO EXP_LINE VALUES
    (12,1,5,1120.3,1,SYSDATE-38,2);
INSERT INTO EXP_LINE VALUES
    (12,2,5,1120.3,1,SYSDATE-38,2);


rem Create indexes

CREATE UNIQUE INDEX I_EMPLOYEE$EMPLOYEE_ID ON EMPLOYEE (EMPLOYEE_ID);
CREATE UNIQUE INDEX I_COST_CENTER$COST_CENTER_ID
  ON COST_CENTER (COST_CENTER_ID);
CREATE UNIQUE INDEX I_EXP_REPORT$REPORT_ID ON EXP_REPORT (REPORT_ID);
CREATE UNIQUE INDEX I_EXP_LINE ON EXP_LINE (REPORT_ID,LINE_ID);
CREATE UNIQUE INDEX I_EXP_TYPE$TYPE_ID ON EXP_TYPE (TYPE_ID);
CREATE UNIQUE INDEX I_EXP_CURRENCY$CURRENCY_ID ON EXP_CURRENCY (CURRENCY_ID);

rem Add constraints

ALTER TABLE EMPLOYEE ADD
   CHECK (EMPLOYEE_ID IS NOT NULL);

ALTER TABLE EXP_REPORT ADD
   CHECK (REPORT_ID IS NOT NULL);
ALTER TABLE EXP_REPORT ADD
   CHECK (EMPLOYEE_ID IS NOT NULL);
ALTER TABLE EXP_REPORT ADD
   CHECK (COST_CENTER_ID IS NOT NULL);

rem PK
ALTER TABLE EMPLOYEE ADD
   PRIMARY KEY (EMPLOYEE_ID);
ALTER TABLE COST_CENTER ADD
   PRIMARY KEY (COST_CENTER_ID);
ALTER TABLE EXP_REPORT ADD
   PRIMARY KEY (REPORT_ID);
ALTER TABLE EXP_LINE ADD
   PRIMARY KEY (REPORT_ID,LINE_ID);
ALTER TABLE EXP_TYPE ADD
   PRIMARY KEY (TYPE_ID);
ALTER TABLE EXP_CURRENCY ADD
   PRIMARY KEY (CURRENCY_ID);

rem FK
ALTER TABLE EMPLOYEE ADD
   FOREIGN KEY (COST_CENTER_ID) REFERENCES COST_CENTER;
ALTER TABLE EMPLOYEE ADD
   FOREIGN KEY (MANAGER_ID) REFERENCES EMPLOYEE;
ALTER TABLE EXP_REPORT ADD
   FOREIGN KEY (EMPLOYEE_ID) REFERENCES EMPLOYEE;
ALTER TABLE EXP_REPORT ADD
   FOREIGN KEY (COST_CENTER_ID) REFERENCES COST_CENTER;
ALTER TABLE EXP_LINE ADD
   FOREIGN KEY (REPORT_ID) REFERENCES EXP_REPORT;
ALTER TABLE EXP_LINE ADD
   FOREIGN KEY (TYPE_ID) REFERENCES EXP_TYPE;
ALTER TABLE EXP_LINE ADD
   FOREIGN KEY (CURRENCY_ID) REFERENCES EXP_CURRENCY;



rem ========================================================================
rem  Now setting Oracle8i Security features
rem ========================================================================

rem =================================================================
rem Creation of the application context
rem =================================================================
rem
rem SECDEMO User must have the CREATE ANY CONTEXT system privilege
rem

CONNECT SECDEMO/SECDEMO

CREATE CONTEXT expenses_reporting USING secdemo.exprep_ctx;

rem =================================================================
rem Creation of the package witch implements the context
rem =================================================================

CREATE OR REPLACE PACKAGE exprep_ctx AS
  PROCEDURE set_ctx;
END;
/
SHOW ERRORS

CREATE OR REPLACE PACKAGE BODY  exprep_ctx IS

  PROCEDURE set_ctx IS
    empnum number;
    countrec number;
    cc    number;

    role   varchar2(20);
  BEGIN

    -- SET emp_number
    select EMPLOYEE_ID into empnum from employee
        where last_name = sys_context('userenv', 'session_user');

    dbms_session.set_context('expenses_reporting','emp_number', empnum);

    -- SET ROLE ?
    select count(*) into countrec from cost_center where manager_id=empnum;
    IF (countrec > 0) THEN
      dbms_session.set_context('expenses_reporting','exp_role', 'MANAGER');
    ELSE
      dbms_session.set_context('expenses_reporting','exp_role', 'EMPLOYEE');
    END IF;

    -- SET cc_number
    select COST_CENTER_ID into cc from employee
        where last_name = sys_context('userenv', 'session_user');
    dbms_session.set_context('expenses_reporting','cc_number', cc);


  END;
END;
/
SHOW ERRORS
/
rem CREATE PUBLIC SYNONYM exprep_ctx FOR secdemo.exprep_ctx;
GRANT EXECUTE ON secdemo.exprep_ctx TO public;
/

rem =================================================================
rem Creation of the policy function
rem =================================================================

CREATE OR REPLACE PACKAGE exp_security AS
  FUNCTION empview_sec(owner varchar2, objname varchar2) RETURN varchar2;
  FUNCTION empnum_sec(owner varchar2, objname varchar2) RETURN varchar2;
  FUNCTION empnumline_sec(owner varchar2, objname varchar2) RETURN varchar2;
  FUNCTION ccid_mgr_sec(owner varchar2, objname varchar2) RETURN varchar2;

END exp_security;
/
SHOW ERRORS

CREATE OR REPLACE PACKAGE BODY exp_security IS

  FUNCTION empview_sec(owner varchar2, objname varchar2) RETURN varchar2 IS
    predicate varchar2(2000);
  BEGIN

    IF (sys_context('expenses_reporting', 'exp_role') = 'MANAGER') THEN
        predicate :=
          'COST_CENTER_ID = sys_context(''expenses_reporting'',''cc_number'')';
    ELSE
        predicate :=
          'EMPLOYEE_ID = sys_context(''expenses_reporting'',''emp_number'')';
    END IF;

    RETURN predicate;
  END empview_sec;

  FUNCTION empnum_sec(owner varchar2, objname varchar2) RETURN varchar2 IS
    predicate varchar2(2000);
  BEGIN
    predicate :=
      'EMPLOYEE_ID = sys_context(''expenses_reporting'',''emp_number'')';
    RETURN predicate;
  END empnum_sec;

  FUNCTION empnumline_sec(owner varchar2, objname varchar2) RETURN varchar2 IS
    predicate varchar2(2000);
  BEGIN
    predicate := 'REPORT_ID IN (SELECT REPORT_ID FROM EXP_REPORT WHERE \
            EMPLOYEE_ID = sys_context(''expenses_reporting'',''emp_number''))';
    RETURN predicate;
  END empnumline_sec;

  FUNCTION ccid_mgr_sec(owner varchar2, objname varchar2) RETURN varchar2 IS
    predicate varchar2(2000);
  BEGIN
    predicate :=
      'COST_CENTER_ID = (SELECT COST_CENTER_ID FROM COST_CENTER WHERE \
             MANAGER_ID = sys_context(''expenses_reporting'',''emp_number''))';
    RETURN predicate;
  END ccid_mgr_sec;


END;
/
SHOW ERRORS


rem =================================================================
rem Association of the policy function with a table or a view
rem =================================================================
rem SELECT POLICY_NAME FROM ALL_POLICIES;

rem POLICY #1
rem     YOU CAN SELECT ONLY YOUR REPORTS IF YOU ARE A EMPLOYEE
rem     YOU CAN SELECT YOUR REPORTS AND ALL THE REPORT IN YOUR COST CENTER
rem     IF YOU ARE THE COST CENTER MANAGER
EXECUTE DBMS_RLS.DROP_POLICY('secdemo','EXP_REPORT_SUM_VIEW', 'exp_report_VIEW_policy');
EXECUTE DBMS_RLS.ADD_POLICY('secdemo','EXP_REPORT_SUM_VIEW',  'exp_report_VIEW_policy','secdemo','exp_security.empview_sec','SELECT');

rem POLICY #2
rem     YOU CAN DELETE YOUR REPORTS ONLY
EXECUTE DBMS_RLS.DROP_POLICY('secdemo','exp_report','exp_report_policy');
EXECUTE DBMS_RLS.ADD_POLICY('secdemo','exp_report','exp_report_policy', 'secdemo','exp_security.empnum_sec','DELETE');

rem POLICY #2
rem     YOU CAN INSET A REPORT FOR YOURSELF ONLY
EXECUTE DBMS_RLS.DROP_POLICY('secdemo','exp_report',  'exp_report_insert_policy');
EXECUTE DBMS_RLS.ADD_POLICY('secdemo','exp_report','exp_report_insert_policy', 'secdemo','exp_security.empnum_sec','INSERT');

rem POLICY #4
rem     YOU CAN DELETE A LINE IN YOUR REPORT ONLY
rem     IF YOU ARE THE OWNER OF THE REPORT
EXECUTE DBMS_RLS.DROP_POLICY('secdemo','exp_line','exp_line_policy');
EXECUTE DBMS_RLS.ADD_POLICY('secdemo','exp_line','exp_line_policy','secdemo', 'exp_security.empnumline_sec','DELETE');

rem POLICY #5
rem     YOU CAN APPROVE (UPDATE) AN REPORT ONLY
rem     IF YOU ARE THE COST CENTER MANAGER
EXECUTE DBMS_RLS.DROP_POLICY('secdemo','exp_report', 'exp_report_Approve_policy');
EXECUTE DBMS_RLS.ADD_POLICY('secdemo','exp_report',  'exp_report_Approve_policy','secdemo','exp_security.ccid_mgr_sec','UPDATE');

rem LIST ALL POLICIES
SELECT POLICY_NAME FROM ALL_POLICIES;

rem =================================================================
rem Create a trigger on logon on database to setup the context
rem =================================================================
rem COMPATIBLE parameter needs to be 8.1.0.0.0 or greater
rem This trigger should be created by an account with BDA role

rem CONNECT SYSTEM/MANAGER
rem CREATE OR REPLACE TRIGGER SECDEMO.SET_EXPENSE_CTX
rem    AFTER LOGON
rem    ON DATABASE
rem    BEGIN
rem     SECDEMO.EXPREP_CTX.SET_CTX;
rem    END;
rem /













