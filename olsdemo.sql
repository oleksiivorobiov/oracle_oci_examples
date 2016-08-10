Rem
Rem $Header: olsdemo.sql 29-may-2007.01:55:55 srramara Exp $
Rem
Rem olsdemo.sql
Rem
Rem Copyright (c) 2001, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      olsdemo.sql - Oracle Label Security demo
Rem
Rem    DESCRIPTION
Rem      Builds Oracle Label Security demonstration labels and tables
Rem
Rem    NOTES
Rem      Executing user must have DBA and LBAC_DBA roles
Rem      Install with NUMERIC labels assumed
Rem      First clean up previous install using olsdrp.sql
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    srramara    05/29/07 - passwd case sensitivity changes
Rem    vpesati     01/21/02 - modify label function
Rem    vpesati     01/18/02 - change to varchar2, trim blank padding
Rem    snamudur    10/17/01 - For adding orderby clause
Rem    gmurphy     02/02/01 - Merged gmurphy_ols_2rdbms
Rem    vpesati     01/18/01 - oracle label security demo
Rem    vpesati     01/18/01 - Created
Rem

spool olsdemo.log
rem ====================================================================
rem  Building Oracle Label Security demo
rem ====================================================================

rem ====================================================================
rem  Create admin users and demo owner
rem ====================================================================

CONNECT SYSTEM/manager; 
CREATE USER "HR_ADMIN" IDENTIFIED BY "HR_ADMIN";
GRANT CONNECT, RESOURCE, SELECT_CATALOG_ROLE TO HR_ADMIN;

CREATE USER "DEFENSE_ADMIN" IDENTIFIED BY "DEFENSE_ADMIN";
GRANT CONNECT, RESOURCE, SELECT_CATALOG_ROLE TO DEFENSE_ADMIN;

CREATE USER "SA_DEMO" IDENTIFIED BY "SA_DEMO";
GRANT CONNECT, RESOURCE, SELECT_CATALOG_ROLE TO SA_DEMO;

rem ====================================================================
rem Connect as LBACSYS to grant authorization to SA_DEMO
rem ====================================================================

CONNECT LBACSYS/lbacsys

GRANT EXECUTE ON sa_components TO SA_DEMO WITH GRANT OPTION;
GRANT EXECUTE ON sa_user_admin TO SA_DEMO WITH GRANT OPTION;
GRANT EXECUTE ON sa_user_admin TO SA_DEMO WITH GRANT OPTION;
GRANT EXECUTE ON sa_label_admin TO SA_DEMO WITH GRANT OPTION;
GRANT EXECUTE ON sa_policy_admin TO SA_DEMO WITH GRANT OPTION;
GRANT EXECUTE ON sa_audit_admin  TO SA_DEMO WITH GRANT OPTION;

GRANT LBAC_DBA TO SA_DEMO;
GRANT EXECUTE ON sa_sysdba TO SA_DEMO;
GRANT EXECUTE ON to_lbac_data_label TO SA_DEMO;

CONNECT SA_DEMO/SA_DEMO
rem ====================================================================
rem  Creating Employee Policy: 'human_resources'
rem ====================================================================

EXECUTE SA_SYSDBA.CREATE_POLICY('human_resources','hr_label');
GRANT HUMAN_RESOURCES_DBA TO HR_ADMIN;

GRANT EXECUTE ON sa_components TO HR_ADMIN;
GRANT EXECUTE ON sa_user_admin TO HR_ADMIN;
GRANT EXECUTE ON sa_user_admin TO HR_ADMIN;
GRANT EXECUTE ON sa_label_admin TO HR_ADMIN;
GRANT EXECUTE ON sa_policy_admin TO HR_ADMIN;
GRANT EXECUTE ON sa_audit_admin TO HR_ADMIN;

rem ====================================================================
rem Creating DEFENSE Policy: 'defense'
rem ====================================================================
EXECUTE SA_SYSDBA.CREATE_POLICY('defense','defense_label');
GRANT DEFENSE_DBA TO DEFENSE_ADMIN;

GRANT EXECUTE ON sa_components TO DEFENSE_ADMIN;
GRANT EXECUTE ON sa_user_admin TO DEFENSE_ADMIN;
GRANT EXECUTE ON sa_user_admin TO DEFENSE_ADMIN;
GRANT EXECUTE ON sa_label_admin TO DEFENSE_ADMIN;
GRANT EXECUTE ON sa_policy_admin TO DEFENSE_ADMIN;
GRANT EXECUTE ON sa_audit_admin TO DEFENSE_ADMIN;

rem ====================================================================
rem  Creating Demo Tables: Dept, Emp and URLTable
rem
rem  The Department Table will be the standard Department table
rem  with no Secure Access Policy Protections.
rem
rem  The Emp Table will be Protected by Secure Access and 
rem  Labels with a Rules Based Policy.
rem
rem  The URLTable Table will be Protected by Secure Access 
rem  and rely on explicit Labeling
rem
rem ====================================================================
rem  Creating Secure Access Definitions to Support Label Policies
rem ====================================================================
 
rem ====================================================================
rem  Creating Levels, Compartments and Groups for Emp table
rem ====================================================================

CONNECT HR_ADMIN/HR_ADMIN

EXECUTE SA_COMPONENTS.CREATE_LEVEL('human_resources',50,'L3','Level 3');
EXECUTE SA_COMPONENTS.CREATE_LEVEL('human_resources',30,'L2','Level 2');
EXECUTE SA_COMPONENTS.CREATE_LEVEL('human_resources',10,'L1','Level 1');

EXECUTE SA_COMPONENTS.CREATE_COMPARTMENT('human_resources',100,'M','MANAGEMENT');
EXECUTE SA_COMPONENTS.CREATE_COMPARTMENT('human_resources',110,'E','EMPLOYEE');

EXECUTE SA_COMPONENTS.CREATE_GROUP('human_resources',10,'D10','DEPARTMENT 10');
EXECUTE SA_COMPONENTS.CREATE_GROUP('human_resources',20,'D20','DEPARTMENT 20');
EXECUTE SA_COMPONENTS.CREATE_GROUP('human_resources',30,'D30','DEPARTMENT 30');
EXECUTE SA_COMPONENTS.CREATE_GROUP('human_resources',40,'D40','DEPARTMENT 40');

EXECUTE SA_USER_ADMIN.SET_USER_PRIVS('human_resources','sa_demo','FULL');
EXECUTE SA_USER_ADMIN.SET_USER_PRIVS('human_resources','hr_admin','FULL,PROFILE_ACCESS');


rem ====================================================================
rem  Creating Dept Table
rem ====================================================================
 
CONNECT SA_DEMO/SA_DEMO

CREATE TABLE SA_DEMO.DEPT (
 DEPTNO              NUMBER(2) NOT NULL,
 DNAME               VARCHAR2(14),
 LOC                 VARCHAR2(13),
 CONSTRAINT DEPT_PRIMARY_KEY PRIMARY KEY (DEPTNO));

GRANT SELECT, INSERT, UPDATE, DELETE ON DEPT to PUBLIC;

rem ====================================================================
rem  Populating Dept table
rem ====================================================================

INSERT INTO SA_DEMO.DEPT VALUES (10,'ACCOUNTING','NEW YORK');
INSERT INTO SA_DEMO.DEPT VALUES (20,'RESEARCH','DALLAS');
INSERT INTO SA_DEMO.DEPT VALUES (30,'SALES','CHICAGO');
INSERT INTO SA_DEMO.DEPT VALUES (40,'OPERATIONS','BOSTON');

COMMIT;

rem ====================================================================
rem  Creating Emp Table
rem ====================================================================

CREATE TABLE SA_DEMO.EMP (
 EMPNO               NUMBER(4) NOT NULL,
 ENAME               VARCHAR2(10),
 JOB                 VARCHAR2(9),
 MGR                 NUMBER(4),
 HIREDATE            DATE,
 SAL                 NUMBER(7,2),
 COMM                NUMBER(7,2),
 DEPTNO              NUMBER(2) NOT NULL,
 CONSTRAINT EMP_PRIMARY_KEY PRIMARY KEY (EMPNO));

GRANT SELECT, INSERT, UPDATE, DELETE ON EMP TO PUBLIC;

rem ====================================================================
rem  Creating Rules Based Label Policy for EMP Table
rem ====================================================================

CREATE OR REPLACE FUNCTION sa_demo.gen_emp_label
                 (Job varchar2, 
                  Deptno number, 
                  Total_sal number)
  Return LBACSYS.LBAC_LABEL
as
  i_label  varchar2(80);
Begin
  /* Determine Class Level */
  if total_sal > 2000 then 
     i_label := 'L3:';
  elsif total_sal >1000 then 
     i_label := 'L2:';
  else
     i_label := 'L1:';
  end if;

  /* Determine Compartment */
  IF TRIM(' ' from UPPER(Job)) in ('MANAGER','PRESIDENT') then 
     i_label := i_label||'M:';
  else
     i_label := i_label||'E:';
  end if;
  /* Determine Groups */
  i_label := i_label||'D'||to_char(deptno);
  return TO_LBAC_DATA_LABEL('human_resources',i_label);
End;
/

SHOW ERRORS

rem ====================================================================
rem  Populating Emp Table
rem ====================================================================

INSERT INTO SA_DEMO.EMP (EMPNO,ENAME,JOB,MGR,HIREDATE,SAL,COMM,DEPTNO)
	VALUES (7839,'KING','PRESIDENT',NULL,'17-NOV-81',5000,NULL,10);
INSERT INTO SA_DEMO.EMP (EMPNO,ENAME,JOB,MGR,HIREDATE,SAL,COMM,DEPTNO)
	VALUES (7698,'BLAKE','MANAGER',7839,'1-MAY-81',2850,NULL,30);
INSERT INTO SA_DEMO.EMP (EMPNO,ENAME,JOB,MGR,HIREDATE,SAL,COMM,DEPTNO)
	VALUES (7782,'CLARK','MANAGER',7839,'9-JUN-81',2450,NULL,10);
INSERT INTO SA_DEMO.EMP (EMPNO,ENAME,JOB,MGR,HIREDATE,SAL,COMM,DEPTNO)
	VALUES (7566,'JONES','MANAGER',7839,'2-APR-81',2975,NULL,20);
INSERT INTO SA_DEMO.EMP (EMPNO,ENAME,JOB,MGR,HIREDATE,SAL,COMM,DEPTNO)
	VALUES (7654,'MARTIN','SALESMAN',7698,'28-SEP-81',1250,1400,30);
INSERT INTO SA_DEMO.EMP (EMPNO,ENAME,JOB,MGR,HIREDATE,SAL,COMM,DEPTNO)
	VALUES (7499,'ALLEN','SALESMAN',7698,'20-FEB-81',1600,300,30);
INSERT INTO SA_DEMO.EMP (EMPNO,ENAME,JOB,MGR,HIREDATE,SAL,COMM,DEPTNO)
	VALUES (7844,'TURNER','SALESMAN',7698,'8-SEP-81',1500,0,30);
INSERT INTO SA_DEMO.EMP (EMPNO,ENAME,JOB,MGR,HIREDATE,SAL,COMM,DEPTNO)
	VALUES (7900,'JAMES','CLERK',7698,'3-DEC-81',950,NULL,30);
INSERT INTO SA_DEMO.EMP (EMPNO,ENAME,JOB,MGR,HIREDATE,SAL,COMM,DEPTNO)
	VALUES (7521,'WARD','SALESMAN',7698,'22-FEB-81',1250,500,30);
INSERT INTO SA_DEMO.EMP (EMPNO,ENAME,JOB,MGR,HIREDATE,SAL,COMM,DEPTNO)
	VALUES (7902,'FORD','ANALYST',7566,'3-DEC-81',3000,NULL,20);
INSERT INTO SA_DEMO.EMP (EMPNO,ENAME,JOB,MGR,HIREDATE,SAL,COMM,DEPTNO)
	VALUES (7369,'SMITH','CLERK',7902,'17-DEC-80',800,NULL,20);
INSERT INTO SA_DEMO.EMP (EMPNO,ENAME,JOB,MGR,HIREDATE,SAL,COMM,DEPTNO)
	VALUES (7788,'SCOTT','ANALYST',7566,'09-DEC-82',3000,NULL,20);
INSERT INTO SA_DEMO.EMP (EMPNO,ENAME,JOB,MGR,HIREDATE,SAL,COMM,DEPTNO)
	VALUES (7876,'ADAMS','CLERK',7788,'12-JAN-83',1100,NULL,20);
INSERT INTO SA_DEMO.EMP (EMPNO,ENAME,JOB,MGR,HIREDATE,SAL,COMM,DEPTNO)
	VALUES (7934,'MILLER','CLERK',7782,'23-JAN-82',1300,NULL,10);
COMMIT;

rem ====================================================================
rem  Apply HR policy to EMP to add HR_LABEL colulmn
rem  Specify NO_CONTROL to disable policy enforcement
rem  HR_ADMIN is authorized to apply policies, not SA_DEMO
rem ====================================================================

CONNECT HR_ADMIN/HR_ADMIN

BEGIN
  SA_POLICY_ADMIN.APPLY_TABLE_POLICY('human_resources',
                                     'sa_demo','emp','no_control');
END;
/

rem ====================================================================
rem  Initialize hr_label to 'L1' for existing data.
rem  Note: if label is NULL and policy is ENABLED, 
rem        then no access will be allowed.
rem ====================================================================

UPDATE SA_DEMO.EMP SET hr_label = CHAR_TO_LABEL('human_resources','L1');
COMMIT;

rem ====================================================================
rem  Remove policy and re-apply with LABEL FUNCTION specified
rem ====================================================================

BEGIN
   SA_POLICY_ADMIN.REMOVE_TABLE_POLICY('human_resources','sa_demo','emp');
   SA_POLICY_ADMIN.APPLY_TABLE_POLICY (
     POLICY_NAME => 'human_resources',
     SCHEMA_NAME => 'sa_demo',
     TABLE_NAME  => 'emp',
     TABLE_OPTIONS => 'READ_CONTROL,WRITE_CONTROL,CHECK_CONTROL',
     LABEL_FUNCTION => 'sa_demo.gen_emp_label(:new.job,:new.deptno,:new.sal)',
     PREDICATE => NULL);
END;
/

rem ====================================================================
rem  Now force an Update to Relabel the data with rules in place.
rem ====================================================================

UPDATE SA_DEMO.EMP SET deptno=deptno;
COMMIT;

col hr_label format a15
SELECT empno, sal, deptno, label_to_char(hr_label) AS hr_label FROM SA_DEMO.EMP;

rem ====================================================================
rem  Displaying generated labels
rem ====================================================================

COLUMN policy_name FORMAT A15
COLUMN label FORMAT A15
SELECT * FROM DBA_SA_LABELS WHERE policy_name='HUMAN_RESOURCES' 
    ORDER BY POLICY_NAME, LABEL_TAG;

rem ====================================================================
rem  Now re-apply policy with READ, WRITE, and CHECK_CONTROL enforcement
rem ====================================================================

BEGIN
   SA_POLICY_ADMIN.REMOVE_TABLE_POLICY('human_resources','sa_demo','emp');
   SA_POLICY_ADMIN.APPLY_TABLE_POLICY (
     POLICY_NAME => 'human_resources',
     SCHEMA_NAME => 'sa_demo',
     TABLE_NAME  => 'emp',
     TABLE_OPTIONS => 'READ_CONTROL,WRITE_CONTROL,CHECK_CONTROL',
     LABEL_FUNCTION => 'sa_demo.gen_emp_label(:new.job,:new.deptno,:new.sal)',
     PREDICATE => NULL);
END;
/

rem ====================================================================
rem  Create Trusted Function compute average salary
rem ====================================================================

CONNECT SA_DEMO/SA_DEMO

CREATE OR REPLACE FUNCTION sa_demo.average_sal
RETURN NUMBER IS
   a NUMBER;
BEGIN
   SELECT avg(sal) INTO a FROM emp;
   RETURN a;
END;
/
GRANT EXECUTE ON average_sal TO PUBLIC;


CONNECT HR_ADMIN/HR_ADMIN
EXECUTE SA_USER_ADMIN.SET_PROG_PRIVS('Human_resources','sa_demo','average_sal','READ');

rem ====================================================================
rem  Creating Levels, Compartments and Groups for URLTABLE table
rem ====================================================================

CONNECT DEFENSE_ADMIN/DEFENSE_ADMIN

EXECUTE SA_COMPONENTS.CREATE_LEVEL('defense',500,'TS','TOP SECRET');
EXECUTE SA_COMPONENTS.CREATE_LEVEL('defense',300,'SE','SECRET');
EXECUTE SA_COMPONENTS.CREATE_LEVEL('defense',100,'UN','UNCLASSIFIED');

EXECUTE SA_COMPONENTS.CREATE_COMPARTMENT('defense',100,'US_SPCL','United States Special Data');
EXECUTE SA_COMPONENTS.CREATE_COMPARTMENT('defense',200,'US_ONLY','United States Only Data');

EXECUTE SA_COMPONENTS.CREATE_GROUP('defense',10,'COAL','Coalition');
EXECUTE SA_COMPONENTS.CREATE_GROUP('defense',11,'US','United States','COAL');
EXECUTE SA_COMPONENTS.CREATE_GROUP('defense',12,'CA','Canada','COAL');
EXECUTE SA_COMPONENTS.CREATE_GROUP('defense',13,'UK','United Kingdom','COAL');
EXECUTE SA_COMPONENTS.CREATE_GROUP('defense',14,'AU','Austrailia','COAL');

rem ====================================================================
rem  Creating Table URLTable
rem  This example includes the DEFENSE_LABEL in the design
rem ====================================================================

CONNECT SA_DEMO/SA_DEMO
CREATE TABLE SA_DEMO.URLTABLE(
   ITEM_NUMBER  NUMBER,
   FILE_NAME    VARCHAR2(2000),
   CRITERION    VARCHAR2(500) Not Null,
   TYPEDATA     VARCHAR2(100) Not Null,
   NOTATION     VARCHAR2(2000) Not Null,
   RLABEL       NUMBER,
   OWNER        VARCHAR2(30),
   LAST_UPDATED DATE,
   DEFENSE_LABEL   NUMBER(10) NOT NULL,
   CONSTRAINT URL_PRIMARY_KEY PRIMARY KEY (ITEM_NUMBER));

GRANT SELECT, UPDATE, DELETE, INSERT ON URLTABLE TO PUBLIC;

rem ====================================================================
rem  Add Explicit Security Labels to support the DEFENSE Policy
rem ====================================================================

CONNECT DEFENSE_ADMIN/DEFENSE_ADMIN
EXECUTE SA_LABEL_ADMIN.CREATE_LABEL('defense',1100,'UN:US_SPCL:');
EXECUTE SA_LABEL_ADMIN.CREATE_LABEL('defense',2100,'SE:US_SPCL:');
EXECUTE SA_LABEL_ADMIN.CREATE_LABEL('defense',2110,'SE:US_SPCL:US');
EXECUTE SA_LABEL_ADMIN.CREATE_LABEL('defense',2120,'SE:US_SPCL:US,CA');
EXECUTE SA_LABEL_ADMIN.CREATE_LABEL('defense',2130,'SE:US_SPCL:US,CA,UK,AU');
EXECUTE SA_LABEL_ADMIN.CREATE_LABEL('defense',2140,'SE:US_SPCL:COAL');
EXECUTE SA_LABEL_ADMIN.CREATE_LABEL('defense',3110,'TS:US_SPCL:US');
EXECUTE SA_LABEL_ADMIN.CREATE_LABEL('defense',2200,'SE:US_ONLY:');
EXECUTE SA_LABEL_ADMIN.CREATE_LABEL('defense',3200,'TS:US_ONLY:');

rem ====================================================================
rem  Displaying created labels
rem ====================================================================

COLUMN policy_name FORMAT A12
COLUMN label FORMAT A25
SELECT * FROM DBA_SA_LABELS WHERE policy_name='DEFENSE' 
    ORDER BY POLICY_NAME, LABEL_TAG;

rem ====================================================================
rem  Inserted data into table with Explicit label included in statement.
rem ====================================================================

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values
(5,
'http://security.us.oracle.com:80/demo_images/sbox.gif',
'UAV Imagery', 
'SAR',
'Sensor Box US Demo36 Data',
char_to_label('defense','SE:US_SPCL:US'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                     
(6, 
'http://security.us.oracle.com:80/demo_images/image_nyc14.jpg',                                    
'JSTARS Imagery',                                                                                   
'Imagery',                                                                                          
'Korona imagery minefield',                                                                         
char_to_label('defense','SE:US_SPCL:US'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(7,                                                                                                 
'http://security.us.oracle.com:80/demo_images/radar_sumara_russia.gif',                            
'JSTARS Imagery',                                                                                   
'SAR',                                                                                              
'Terrorist training camp',                                                                          
char_to_label('defense','SE:US_SPCL:US,CA')); 

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(8,                                                                                                 
'http://security.us.oracle.com:80/demo_images/radar_rondonia_brazil.gif',                          
'UAV Imagery',                                                                                      
'SAR',                                                                                              
'UAV detects probable WMD storage sites',                                                           
char_to_label('defense','SE:US_SPCL:US,CA'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(9,                                                                                                 
'http://security.us.oracle.com:80/demo_images/p0086.gif',                                          
'Triggers',                                                                                         
'19.06.073',                                                                                        
'Mine Recovery and exploit, mine image',                                                            
char_to_label('defense','SE:US_SPCL:US,CA,UK,AU'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(10,                                                                                                
'http://security.us.oracle.com:80/demo_images/p0392.gif',                                          
'Triggers',                                                                                         
'19.06.073',                                                                                        
'Mine Profile for Recovery',                                                                        
char_to_label('defense','SE:US_SPCL:US,CA,UK,AU'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(11,                                                                                                
'http://security.us.oracle.com:80/demo_images/nyc.jpg',                                            
'JSTARS Imagery',                                                                                   
'SAR',                                                                                              
'Potential Rendezvous sites',                                                                       
char_to_label('defense','TS:US_SPCL:US'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(12,                                                                                                
'http://security.us.oracle.com:80/demo_images/track.gif',                                          
'Triggers',                                                                                         
'19.01.276',                                                                                        
'Track Data DEFENSE Day3 from COP, Demo85',                                                            
char_to_label('defense','TS:US_SPCL:US'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(13,                                                                                                
'http://security.us.oracle.com:80/demo_images/sigs0014.gif',                                       
'Triggers',                                                                                         
'19.01.112',                                                                                        
'Pull for OPS primary image GBS Misson planning',                                                   
char_to_label('defense','UN:US_SPCL:'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(14,                                                                                                
'http://security.us.oracle.com:80/demo_images/sigs0060.gif',                                       
'Triggers',                                                                                         
'19.01.112',                                                                                        
'Misson planning, SIGS imagery from Sensor Box',                                                    
char_to_label('defense','UN:US_SPCL:'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(15,                                                                                                
'http://security.us.oracle.com:80/demo_images/sigs0081.gif',                                       
'Triggers',                                                                                         
'22.01.112',                                                                                        
'SIGS Imagery for coalition planning, from Sensor Box',                                             
char_to_label('defense','SE:US_SPCL:'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(16,                                                                                                
'http://security.us.oracle.com:80/demo_images/coalition_sar_day1.gif',                             
'UAV Imagery',                                                                                      
'SAR',                                                                                              
'Coalition UAV SAR for DEFENSE97 day3 US Demo85',                                                      
char_to_label('defense','SE:US_SPCL:'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(21,                                                                                                
'http://security.us.oracle.com:80/demo_images/photo_c130_hercules.jpg',                            
'Transport',                                                                                        
'Photo',                                                                                            
'C-130 Hercules kicking up a lot of dust',                                                          
char_to_label('defense','SE:US_SPCL:COAL'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(22,                                                                                                
'http://security.us.oracle.com:80/demo_images/photo_f14_tomcat.jpg',                               
'Fighter',                                                                                          
'Photo',                                                                                            
'In-flight view of an F-14 Tomcat escorting a pair of Russian bombers',                             
char_to_label('defense','SE:US_SPCL:COAL'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(17,                                                                                                
'http://security.us.oracle.com:80/demo_images/sigs0271.gif',                                       
'UAV Imagery',                                                                                      
'SAR',                                                                                              
'Coalition DEFENSE97 day3 Kartuna Tank Platoon',                                                       
char_to_label('defense','SE:US_ONLY:'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(18,                                                                                                
'http://security.us.oracle.com:80/demo_images/sar.gif',                                            
'UAV Imagery',                                                                                      
'SAR',                                                                                              
'US Demo85 SAR data',                                                                               
char_to_label('defense','SE:US_ONLY:'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(19,                                                                                                
'http://security.us.oracle.com:80/demo_images/onslow.gif',                                         
'Triggers',                                                                                         
'19.01.085',                                                                                        
'METOC Amphibious Opernations Area Forecast, Onslow Beach',                                         
char_to_label('defense','TS:US_ONLY:'));

INSERT INTO SA_DEMO.URLTABLE (ITEM_NUMBER, FILE_NAME, CRITERION,TYPEDATA,NOTATION, DEFENSE_LABEL)                   
Values                                                                                              
(20,                                                                                                
'http://security.us.oracle.com:80/demo_images/nyc.jpg',                                            
'UAV Imagery',                                                                                      
'Photo',                                                                                            
'Kartuna field operations HQ',                                                                      
char_to_label('defense','TS:US_ONLY:'));                                                                                                 
rem ====================================================================
rem  Apply Secure Acess DEFENSE policy to URLTABLE
rem ====================================================================

Begin
   SA_POLICY_ADMIN.APPLY_TABLE_POLICY (
     POLICY_NAME => 'defense',
     SCHEMA_NAME => 'sa_demo',
     TABLE_NAME  => 'urltable',
     TABLE_OPTIONS => 'ALL_CONTROL',
     LABEL_FUNCTION => NULL,
     PREDICATE => NULL);
End;
/

rem ====================================================================
rem  Create Secure Access User Accounts
rem ====================================================================
CONNECT SYSTEM/manager; 

CREATE USER "PRES" IDENTIFIED BY "SA";
CREATE USER "MD10" IDENTIFIED BY "SA";
CREATE USER "ED10" IDENTIFIED BY "SA";
CREATE USER "ED20" IDENTIFIED BY "SA";

CREATE USER "UN_SP" IDENTIFIED BY "SA";
CREATE USER "SE_SP" IDENTIFIED BY "SA";
CREATE USER "TS_SP" IDENTIFIED BY "SA";
CREATE USER "SE_US" IDENTIFIED BY "SA";
CREATE USER "SE_CA" IDENTIFIED BY "SA";
CREATE USER "SE_UK" IDENTIFIED BY "SA";
CREATE USER "SE_AU" IDENTIFIED BY "SA";
CREATE USER "SE_CO" IDENTIFIED BY "SA";
CREATE USER "TS_US" IDENTIFIED BY "SA";
CREATE USER "SE_USO" IDENTIFIED BY "SA";
CREATE USER "TS_USO" IDENTIFIED BY "SA";


rem ====================================================================
rem  Set User Labels For HUMAN_RESOURCES Policy
rem ====================================================================
CONNECT HR_ADMIN/HR_ADMIN

EXECUTE SA_USER_ADMIN.SET_USER_LABELS('human_resources','PRES','L3:M,E');
EXECUTE SA_USER_ADMIN.SET_USER_LABELS('human_resources','MD10','L3:M,E:D10');
EXECUTE SA_USER_ADMIN.SET_USER_LABELS('human_resources','ED10','L3:E:D10');
EXECUTE SA_USER_ADMIN.SET_USER_LABELS('human_resources','ED20','L3:E:D20');

EXECUTE SA_USER_ADMIN.ALTER_COMPARTMENTS('human_resources','PRES','E',sa_utl.read_only);

rem ====================================================================
rem  Set User Labels For DEFENSE Policy
rem ====================================================================
CONNECT DEFENSE_ADMIN/DEFENSE_ADMIN

EXECUTE SA_USER_ADMIN.SET_USER_LABELS('defense','UN_SP','UN:US_SPCL:');
EXECUTE SA_USER_ADMIN.SET_USER_LABELS('defense','SE_SP','SE:US_SPCL:');
EXECUTE SA_USER_ADMIN.SET_USER_LABELS('defense','TS_SP','TS:US_SPCL:');

EXECUTE SA_USER_ADMIN.SET_USER_LABELS('defense','SE_US','SE:US_SPCL:US');
EXECUTE SA_USER_ADMIN.SET_USER_LABELS('defense','SE_CA','SE:US_SPCL:CA');
EXECUTE SA_USER_ADMIN.SET_USER_LABELS('defense','SE_UK','SE:US_SPCL:UK');
EXECUTE SA_USER_ADMIN.SET_USER_LABELS('defense','SE_AU','SE:US_SPCL:AU');
EXECUTE SA_USER_ADMIN.SET_USER_LABELS('defense','SE_CO','SE:US_SPCL:COAL');
EXECUTE SA_USER_ADMIN.SET_USER_LABELS('defense','TS_US','TS:US_SPCL:US');

EXECUTE SA_USER_ADMIN.SET_USER_LABELS('defense','SE_USO','SE:US_ONLY:');
EXECUTE SA_USER_ADMIN.SET_USER_LABELS('defense','TS_USO','TS:US_ONLY:');

EXECUTE SA_USER_ADMIN.ALTER_COMPARTMENTS('defense','SE_CA','US_SPCL',sa_utl.read_only);
EXECUTE SA_USER_ADMIN.ALTER_COMPARTMENTS('defense','SE_UK','US_SPCL',sa_utl.read_only);
EXECUTE SA_USER_ADMIN.ALTER_COMPARTMENTS('defense','SE_AU','US_SPCL',sa_utl.read_only);
EXECUTE SA_USER_ADMIN.ALTER_GROUPS('defense','SE_CA','CA',sa_utl.read_only);
EXECUTE SA_USER_ADMIN.ALTER_GROUPS('defense','SE_UK','UK',sa_utl.read_only);
EXECUTE SA_USER_ADMIN.ALTER_GROUPS('defense','SE_AU','AU',sa_utl.read_only);

rem ====================================================================
rem  Set User PRIVILEGES For DEFENSE Policy
rem ====================================================================

EXECUTE SA_USER_ADMIN.SET_USER_PRIVS('defense','defense_admin','full,profile_access');
EXECUTE SA_USER_ADMIN.SET_USER_PRIVS('defense','se_co','compaccess,writeacross');
EXECUTE SA_USER_ADMIN.SET_USER_PRIVS('defense','Se_sp','compaccess,writeacross,writeup,writedown');


rem ====================================================================
rem Check data dictionary views and other functionality
rem ====================================================================

COLUMN NOTATION FORMAT A50
COLUMN LABEL FORMAT A25
COLUMN POLICY_NAME FORMAT A15
COLUMN USER_NAME FORMAT A15
COLUMN USER_PRIVILEGES FORMAT A30

SET PAGESIZE 80
SET ECHO ON
rem ====================================================================
rem  Display Secure Access data dictionary views
rem ====================================================================

CONNECT HR_ADMIN/HR_ADMIN

SELECT * FROM DBA_SA_POLICIES
ORDER BY policy_name;

SELECT * FROM DBA_SA_LABELS 
ORDER BY policy_name, label_tag;

SELECT * FROM DBA_SA_USERS
ORDER BY policy_name, user_name;

SELECT * FROM DBA_SA_PROG_PRIVS
ORDER BY policy_name, schema_name, program_name;

SELECT * FROM DBA_SA_SCHEMA_POLICIEs
ORDER BY policy_name, schema_name;

SELECT * FROM DBA_SA_TABLE_POLICIES
ORDER BY policy_name, schema_name, table_name;

rem ====================================================================
rem  Use average_sal Trusted Function
rem  HR_ADMIN has FULL privilege
rem ====================================================================

SELECT avg(sal) FROM SA_DEMO.EMP;

rem ====================================================================
rem  ED10 has NO privileges
rem ====================================================================

EXECUTE SA_SESSION.SET_ACCESS_PROFILE('human_resources','ED10');
SELECT avg(sal) FROM SA_DEMO.EMP;

rem ====================================================================
rem  average_sal function has READ privilege
rem ====================================================================
SET SERVEROUTPUT ON
DECLARE averg NUMBER;
BEGIN
   averg := sa_demo.average_sal;
   DBMS_OUTPUT.PUT_LINE('Average is ' || averg);
END;
/
SET SERVEROUTPUT OFF

rem ====================================================================
rem  Retrieve different results as different users
rem ====================================================================

CONNECT DEFENSE_ADMIN/DEFENSE_ADMIN

SET PAGESIZE 0

rem ====================================================================
rem  DEFENSE_ADMIN as FULL and PROFILE_ACCESS privilege
rem ====================================================================

SELECT * FROM USER_SA_SESSION 
WHERE policy_name='DEFENSE'; 
SELECT label_to_char(defense_label) as label, notation FROM SA_DEMO.URLTABLE;

EXECUTE SA_SESSION.SET_ACCESS_PROFILE('defense','ts_us');
SELECT * FROM USER_SA_SESSION
WHERE policy_name='DEFENSE';
SELECT label_to_char(defense_label) as label, notation FROM SA_DEMO.URLTABLE;

EXECUTE SA_SESSION.SET_ACCESS_PROFILE('defense','se_ca');
SELECT * FROM USER_SA_SESSION
WHERE policy_name='DEFENSE';
SELECT label_to_char(defense_label) as label, notation FROM SA_DEMO.URLTABLE;

EXECUTE SA_SESSION.SET_ACCESS_PROFILE('defense','un_sp');
SELECT * FROM USER_SA_SESSION
WHERE policy_name='DEFENSE';
SELECT label_to_char(defense_label) as label, notation FROM SA_DEMO.URLTABLE;


rem ====================================================================
rem  Demo Build ..... Complete
rem  Review olsdemo.log file
rem ====================================================================

spool off
