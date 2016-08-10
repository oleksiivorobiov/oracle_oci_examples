/* Copyright (c) Oracle Corporation 1995.  All Rights Reserved. */

/*
  NAME
    cdemoin1 - Demo program which modifies an inherited type in a table and
    displays a record from the table.

  DESCRIPTION
    This program pins an inherited instance in the object cache and displays
    the attributes in it. It also updates a record of a table.

  MODIFIED
    rdwajan   08/29/00   Created

*/

SET SERVEROUTPUT ON
CONNECT scott/tiger;

DROP TABLE i_manager_tab;
DROP TABLE i_people_tab;
DROP TABLE i_residence_tab;
DROP TYPE i_manager;
DROP TYPE i_people;
DROP TYPE i_residence_nest;
DROP TYPE i_residence_arr;
DROP TYPE i_residence;

CREATE TYPE i_residence AS OBJECT
( hno NUMBER, street VARCHAR2(50) ) 
/

CREATE TYPE i_residence_arr AS VARRAY(3) OF REF i_residence;
/
CREATE TYPE i_residence_nest AS TABLE OF i_residence;
/

CREATE TYPE i_people AS OBJECT
(name VARCHAR2(10), ssn NUMBER, addr i_residence, atladrs i_residence_nest) NOT FINAL;
/

CREATE TYPE i_manager UNDER i_people
( empno NUMBER, workadd i_residence_arr) ;
/

CREATE TABLE i_residence_tab OF i_residence;

INSERT INTO i_residence_tab values (100, 'Oracle Parkway');
INSERT INTO i_residence_tab values (200, 'Oracle Parkway');
INSERT INTO i_residence_tab values (300, 'Oracle Parkway');

CREATE TABLE i_people_tab OF i_people
NESTED TABLE atladrs STORE AS i_people_nt_tab1;

CREATE TABLE i_manager_tab OF i_manager
NESTED TABLE atladrs STORE AS i_manager_nt_tab2;

INSERT INTO i_manager_tab VALUES (i_manager('ROOPA',101, 
i_residence (1000, 'Silver Blvd'),
i_residence_nest (i_residence (201, 'Willow Road'), i_residence (505, 'Springfield St')),
45001,
i_residence_arr ((SELECT REF(a) FROM i_residence_tab a WHERE a.hno = 100),
(SELECT REF(a) FROM i_residence_tab a WHERE a.hno = 200),
(SELECT REF(a) FROM i_residence_tab a WHERE a.hno = 300))) );

COMMIT;
