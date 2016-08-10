/* Copyright (c) Oracle Corporation 1995.  All Rights Reserved. */

/*
  NAME
    cdemoin2 - Demo program to perform attribute substitutability.

  DESCRIPTION
    This program demonstrates attribute substitutability, wherein a column
    which is of REF to a supertype is substituted with a REF to a subtype.
    All the data from the table are then displayed.

  MODIFIED
    rdwajan   08/03/00   Created

*/
CONNECT scott/tiger; 
DROP TABLE cdemoin2_person_tab;
DROP TABLE cdemoin2_address_tab;
DROP TABLE cdemoin2_sec_address_tab;
DROP TYPE cdemoin2_sec_address;
DROP TYPE cdemoin2_address;

CREATE TYPE cdemoin2_address AS OBJECT
( hno NUMBER, street VARCHAR2(20) ) NOT FINAL;
/

CREATE TYPE cdemoin2_sec_address UNDER cdemoin2_address
( city VARCHAR2(20), state CHAR(20) );
/
show errors;  
CREATE TABLE cdemoin2_sec_address_tab OF cdemoin2_sec_address;
INSERT INTO cdemoin2_sec_address_tab values 
(cdemoin2_sec_address(100, 'MAIN STREET', 'NEW YORK', 'NY'));
INSERT INTO cdemoin2_sec_address_tab values 
(cdemoin2_sec_address(200, 'BROADWAY BLVD', 'CHICAGO', 'IL'));

CREATE TABLE cdemoin2_address_tab OF cdemoin2_address;
INSERT INTO cdemoin2_address_tab values (cdemoin2_address(300, 'SHORE RD'));
INSERT INTO cdemoin2_address_tab values (cdemoin2_address(400, 'ESCONDIDO RD'));

CREATE TABLE cdemoin2_person_tab 
( ssn NUMBER,
  vacation_home REF cdemoin2_sec_address,
  first_home REF cdemoin2_address
);

INSERT INTO cdemoin2_person_tab (ssn, vacation_home, first_home) VALUES 
(999, (SELECT REF(a) FROM cdemoin2_sec_address_tab a WHERE a.hno = 100),
(SELECT REF(a) FROM cdemoin2_address_tab a WHERE a.hno = 300));

COMMIT;

