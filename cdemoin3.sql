/* Copyright (c) Oracle Corporation 1995.  All Rights Reserved. */

/*
  NAME
    cdemoin3 - Demo program to describe an object, inherited types, object 
    table and subtable.

  DESCRIPTION
    This program describes an object, an inherited object, methods of an
    object, object table and a subtable and prints out the new type level
    attributes, the new method level attributes and the new table level
    attributes.


  NOTES

  MODIFIED
    rdwajan   08/07/00   Created

*/
SET SERVEROUTPUT ON
connect scott/tiger;

DROP TABLE i_people_tab3;
DROP TABLE i_people_tab2;
DROP TABLE i_people_tab1;
DROP TABLE i_employee_tab;
DROP TABLE i_student_tab;
DROP TABLE i_person_tab;
DROP TYPE i_employee;
DROP TYPE i_student;
DROP TYPE i_person;
DROP TYPE i_address_nest;
DROP TYPE i_address_arr;
DROP TYPE i_address;

CREATE TYPE i_address AS OBJECT
( hno NUMBER, street VARCHAR2(10)
 , MEMBER FUNCTION i_address_fun(arg1 NUMBER) RETURN NUMBER 
);
/

CREATE TYPE i_address_arr AS VARRAY(3) OF REF i_address;
/

CREATE TYPE i_address_nest AS TABLE OF i_address;
/

CREATE TYPE i_person AS OBJECT
( ssn NUMBER, adrs i_address
  ,
  NOT INSTANTIABLE 
  MEMBER PROCEDURE i_person_proc(arg1 NUMBER)
) NOT INSTANTIABLE NOT FINAL;
/

CREATE TYPE i_student UNDER i_person
( stud_id NUMBER, stud_add i_address_arr ,
OVERRIDING 
MEMBER PROCEDURE i_person_proc(arg1 NUMBER) 
) NOT FINAL NOT INSTANTIABLE;
/

CREATE TYPE i_employee UNDER i_person
( emp_id NUMBER, emp_name VARCHAR2(10) ,
  FINAL 
  MEMBER PROCEDURE i_employee_proc(arg1 NUMBER) 
) NOT FINAL NOT INSTANTIABLE; 
/

CREATE TABLE i_person_tab OF i_person;

CREATE TABLE i_student_tab OF i_student;

CREATE TABLE i_employee_tab OF i_employee;

CREATE TABLE i_people_tab1 (region varchar2(10), minister REF i_person);

CREATE TABLE i_people_tab2 of i_address;

CREATE TABLE i_people_tab3 (add_col i_address_nest) NESTED TABLE add_col STORE AS
i_people_tab3_nt_tab;

COMMIT;
