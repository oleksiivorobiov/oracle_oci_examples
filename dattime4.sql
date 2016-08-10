Rem
Rem $Header: dattime4.sql 30-mar-2006.08:56:23 lburgess Exp $
Rem
Rem dattime4.sql
Rem
Rem Copyright (c) 2001, 2006, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      dattime4.sql - A Comprehensive DateTime Demo
Rem
Rem    DESCRIPTION
Rem      This is a sample program to demonstrate 
Rem      -the usage of datetime types available in 9i database. 
Rem      -different possible combination of datetime assignments
Rem      -the usage of datetime built-ins
Rem      -the usage of datetime arithmetic
Rem      -the usage of ERROR_ON_OVERLAP_TIME Session parameter
Rem
Rem    NOTES
Rem      TS    stands for TIMESTAMP
Rem      TSTZ  stands for TIMESTAMP WITH TIME ZONE
Rem      TSLTZ stands for TIMESTAMP WITH LOCAL TIME ZONE
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    lburgess    03/30/06 - lowercase passwords 
Rem    nmeng       10/23/02 - 
Rem    rchennoj    10/22/02 - Query from v$timezone_names
Rem    rchennoj    05/07/01 - 
Rem    rchennoj    04/30/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 132
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO ON

CONNECT SYSTEM/manager;

DROP USER dtdemo CASCADE;
GRANT CONNECT, RESOURCE TO dtdemo IDENTIFIED BY dtdemo;

CONNECT dtdemo/dtdemo;


ALTER SESSION SET TIME_ZONE='-08:00';
SELECT dbtimezone, sessiontimezone from dual;


------------------------------------------------------
--1. TIMESTAMP datatype
------------------------------------------------------

CREATE TABLE tab_ts (c_id number, c_ts    TIMESTAMP);

--Use fractional seconds
INSERT INTO tab_ts
VALUES (1, TIMESTAMP'1980-1-12 15:13:23.33');

--Use TimeZone info, should truncate the zone info as
--Timestamp type doesnot store zone info.
INSERT INTO tab_ts
VALUES (2, TIMESTAMP'1980-1-12 15:13:23 -07:00');


ALTER SESSION SET NLS_TIMESTAMP_FORMAT='YYYY-MM-DD HH24:MI:SSXFF';
SELECT c_id, c_ts from tab_ts order by c_id;

ALTER SESSION SET NLS_TIMESTAMP_FORMAT='DD-MON-YYYY HH:MI:SSXFF AM';
SELECT c_id, c_ts from tab_ts order by c_id;


------------------------------------------------------
--2. TIMESTAMP WITH TIME ZONE datatype
------------------------------------------------------

CREATE TABLE tab_tstz (c_id number, c_tstz  TIMESTAMP WITH TIME ZONE);

--Use No TimeZone info, should take Sessiontimezone
INSERT INTO tab_tstz
VALUES ( 1, TIMESTAMP'2000-10-28 11:26:38');

--Use TimeZone Offset
INSERT INTO tab_tstz
VALUES ( 2, TIMESTAMP'2000-10-28 11:26:38 -07:00');

--Use TimeZone Region Name
INSERT INTO tab_tstz
VALUES ( 3, TIMESTAMP'2000-10-28 11:26:38 AMERICA/LOS_ANGELES');

INSERT INTO tab_tstz
VALUES ( 4, TIMESTAMP'2000-01-28 11:26:38 AMERICA/LOS_ANGELES PST');

INSERT INTO tab_tstz
VALUES ( 5, TIMESTAMP'2000-01-28 11:26:38 GMT');


--show zone info in OFFSET format
ALTER SESSION SET NLS_TIMESTAMP_TZ_FORMAT='DD-MON-YYYY HH:MI:SSXFF AM TZH:TZM';
SELECT c_id, c_tstz from tab_tstz order by c_id;

--show zone info in Regionname if one inserted
ALTER SESSION SET NLS_TIMESTAMP_TZ_FORMAT='DD-MON-YYYY HH:MI:SSXFF AM TZR';
SELECT c_id, c_tstz from tab_tstz order by c_id;

--show zone info in Regionname and Zone Abbreviation if one inserted
ALTER SESSION SET NLS_TIMESTAMP_TZ_FORMAT='DD-MON-YYYY HH:MI:SSXFF AM TZR TZD';
SELECT c_id, c_tstz from tab_tstz order by c_id;


------------------------------------------------------
--3. TIMESTAMP WITH LOCAL TIME ZONE datatype
------------------------------------------------------

CREATE TABLE tab_tsltz (c_id number, c_tsltz TIMESTAMP WITH LOCAL TIME ZONE);

--Use fractional seconds
INSERT INTO tab_tsltz
VALUES (1, TIMESTAMP'1980-1-12 15:13:23.33');

--Use TimeZone info, should convert the value from specified time zone
--to Sessiontimezone
INSERT INTO tab_tsltz
VALUES (2, TIMESTAMP'1980-1-12 15:13:23 -07:00');


ALTER SESSION SET NLS_TIMESTAMP_FORMAT='YYYY-MM-DD HH24:MI:SSXFF';
SELECT c_id, c_tsltz from tab_tsltz order by c_id;

ALTER SESSION SET NLS_TIMESTAMP_FORMAT='DD-MON-YYYY HH:MI:SSXFF AM';
SELECT c_id, c_tsltz from tab_tsltz order by c_id;


--Change Sessiontimezone. TSLTZ value should be adjusted to the
--new sessiontimezone
ALTER SESSION SET TIME_ZONE='GMT';
SELECT c_id, c_tsltz from tab_tsltz order by c_id;

ALTER SESSION SET TIME_ZONE='Asia/Calcutta';
SELECT c_id, c_tsltz from tab_tsltz order by c_id;


------------------------------------------------------
--4. INTERVAL YEAR TO MONTH  datatype
------------------------------------------------------

CREATE TABLE tab_iym (c_id number, c_iym INTERVAL YEAR(3) TO MONTH);

INSERT INTO tab_iym
VALUES(1, INTERVAL '01-01' YEAR TO MONTH);

INSERT INTO tab_iym
VALUES(2, INTERVAL '-01-01' YEAR TO MONTH);

INSERT INTO tab_iym
VALUES(3, INTERVAL '100-10' YEAR(3) TO MONTH);


SELECT c_id, c_iym from tab_iym order by c_id;


------------------------------------------------------
--5. INTERVAL DAY TO SECOND datatype
------------------------------------------------------

CREATE TABLE tab_ids (c_id number, c_ids INTERVAL DAY(3) TO SECOND(9));

INSERT INTO tab_ids
VALUES(1, INTERVAL '01 01:01:01.000001' DAY TO SECOND);

INSERT INTO tab_ids
VALUES(2, INTERVAL '100 10:10:10' DAY(3) TO SECOND);

INSERT INTO tab_ids
VALUES(3, INTERVAL '100 10:10:10.123456789' DAY(3) TO SECOND(9));


SELECT c_id, c_ids from tab_ids order by c_id;


------------------------------------------------------
--6. Use Datetime Builtins
------------------------------------------------------

--------FROM_TZ
SELECT FROM_TZ(TIMESTAMP'1997-01-01 01:00:00', 'US/Pacific') FROM DUAL;
SELECT FROM_TZ(TIMESTAMP'1999-04-04 01:59:59', 'US/Pacific') FROM DUAL;
SELECT FROM_TZ(TIMESTAMP'1999-04-04 03:00:00', 'US/Pacific') FROM DUAL;
 
--------EXTRACT
SELECT EXTRACT(TIMEZONE_HOUR   FROM TIMESTAMP'1999-12-31 01:30:00 US/Pacific')
  FROM DUAL;
SELECT EXTRACT(TIMEZONE_MINUTE FROM TIMESTAMP'1999-12-31 01:30:00 US/Pacific')
  FROM DUAL;
SELECT EXTRACT(TIMEZONE_REGION FROM TIMESTAMP'1999-12-31 01:30:00 US/Pacific')
  FROM DUAL;
SELECT EXTRACT(TIMEZONE_ABBR   FROM TIMESTAMP'1999-12-31 01:30:00 US/Pacific')
  FROM DUAL;

--------TZ_OFFSET
SELECT TZ_OFFSET('GMT') FROM DUAL;
SELECT TZ_OFFSET('Asia/Calcutta') FROM DUAL;

--------TO_CHAR
SELECT TO_CHAR(TIMESTAMP '1997-01-01 01:00:00 US/Pacific', 
                         'YYYY-MM-DD HH24:MI:SS TZR')
  FROM DUAL;
SELECT TO_CHAR(TIMESTAMP '1997-01-01 01:00:00 US/Pacific PST', 
                         'YYYY-MM-DD HH24:MI:SS TZR TZD')
  FROM DUAL;

--------CAST
SELECT CAST(TIMESTAMP'1999-10-31 01:01:01.123 US/Pacific'
            AS TIMESTAMP(0)) 
  FROM DUAL;

SELECT CAST(TIMESTAMP'1999-10-31 01:01:01.123 US/Pacific PST'
            AS TIMESTAMP(6)) 
  FROM DUAL;

-------TO_TIMESTAMP_TZ
SELECT TO_TIMESTAMP_TZ('14-MAR-1970 01:00:00 AM  US/Pacific PST', 
                       'DD-MON-YYYY HH:MI:SS AM TZR TZD')
  FROM DUAL;
SELECT TO_TIMESTAMP_TZ('14-MAR-1970 01:00:00 US/Pacific PST', 
                       'DD-MON-YYYY HH24:MI:SS TZR TZD')
  FROM DUAL;

-------TO_DSINTERVAL
SELECT TO_DSINTERVAL('100 10:00:00') FROM dual;

-------TO_YMINTERVAL
SELECT TO_YMINTERVAL('01-02') FROM dual;

-------NUMTODSINTERVAL
SELECT NUMTODSINTERVAL(12, 'DAY') FROM dual;
SELECT NUMTODSINTERVAL(12, 'MINUTE') FROM dual;

-------NUMTOYMINTERVAL
SELECT NUMTOYMINTERVAL('01', 'YEAR') FROM dual;
SELECT NUMTOYMINTERVAL('01', 'MONTH') FROM dual;

-------SYS_EXTRACT_UTC
--extracts the UTC(GMT) from a datetime with timezone displacement
SELECT SYS_EXTRACT_UTC(TIMESTAMP'2000-03-28 11:30:00 -08:00')
  FROM dual;

-------SYSTIMESTAMP, returns TSTZ type
--returns the system date and timezone of the system the database resides
SELECT SYSTIMESTAMP FROM dual;

-------CURRENT_TIMESTAMP, returns TSTZ type
--returns the current date and time in session time zone
SELECT CURRENT_TIMESTAMP FROM dual;

-------LOCALTIMESTAMP, returns TS type
--returns the current date and time in session time zone
SELECT LOCALTIMESTAMP FROM dual;
------------------------------------------------------
--7. Use AT TIME ZONE
------------------------------------------------------

SELECT TIMESTAMP'1999-01-01 01:01:01' AT TIME ZONE 'US/Pacific'
  FROM dual; 

SELECT TIMESTAMP'1999-01-01 01:01:01' AT TIME ZONE TZ_OFFSET('GMT') FROM DUAL;

CREATE OR REPLACE FUNCTION myzone RETURN varchar2 IS
BEGIN
 RETURN('US/Pacific');
END;
/
show errors

SELECT TIMESTAMP'1997-01-01 01:00:00' AT TIME ZONE myzone FROM dual;


------------------------------------------------------
--8. Datetime Arithmetic
------------------------------------------------------
--Expect 10-NOV-99 01.30.00.000000000 AM -07:00
SELECT TIMESTAMP'1999-10-31 01:30:00 -7:00'
      +INTERVAL '10 00:00:00' DAY TO SECOND FROM dual;

--Expect 1999-10-30 11:00:00 -07:00
SELECT TIMESTAMP'1999-10-31 10:00:00 US/Pacific PST'
      + INTERVAL '24' HOUR   FROM dual;

--Expect 1999-04-04 12:00:00.000000000-08:00
SELECT TIMESTAMP'1999-04-05 01:00:00 US/Pacific PDT'
       -INTERVAL '1440' MINUTE FROM dual;

SELECT TIMESTAMP'1997-01-01 01:00:00 GMT'
       +NUMTOYMINTERVAL('01','YEAR') FROM dual;


------------------------------------------------------
--9. Use ERROR_ON_OVERLAP_TIME session parameter
------------------------------------------------------

--ERROR_ON_OVERLAP_TIME=TRUE
ALTER SESSION SET ERROR_ON_OVERLAP_TIME=TRUE;

--error ORA-01883
SELECT TIMESTAMP'1999-10-31 01:30:00 US/Pacific' FROM dual;

SELECT TIMESTAMP'1999-10-31 01:30:00 US/Pacific PST' FROM dual;


--ERROR_ON_OVERLAP_TIME=FALSE
ALTER SESSION SET ERROR_ON_OVERLAP_TIME=FALSE;

SELECT TIMESTAMP'1999-10-31 01:30:00 US/Pacific' FROM dual;

SELECT TIMESTAMP'1999-10-31 01:30:00 US/Pacific PST' FROM dual;

------------------------------------------------------
--10. Query from v$timezone_names
------------------------------------------------------

CONNECT sys/change_on_install as sysdba;
SELECT DISTINCT tzname FROM v_$timezone_names 
 WHERE upper(tzname) LIKE '%AMERICA%'
 ORDER BY tzname;



connect system/manager;
drop user dtdemo cascade;
exit;
