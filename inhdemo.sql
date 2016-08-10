Rem
Rem $Header: inhdemo.sql 24-jan-2002.15:48:51 cbarclay Exp $
Rem
Rem inhdemo.sql
Rem
Rem Copyright (c) 2001, 2002, Oracle Corporation.  All rights reserved.  
Rem
Rem    NAME
Rem      inhdemo.sql - Demonstrate Inheritance feature
Rem
Rem    DESCRIPTION
Rem      Demonstrate Inheritance feature
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    cbarclay    01/24/02 - demo updated
Rem    cbarclay    01/15/02 - plsql is of and treat
Rem    hyeh        05/01/01 - fix query format
Rem    hyeh        04/10/01 - Merged hyeh_inh_tyev_demos
Rem    hyeh        04/09/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
COLUMN mysubtype FORMAT A70
SET ECHO ON

connect system/manager
drop user inhdemo1 cascade;
drop user inhdemo3 cascade;

grant connect, resource to inhdemo1 identified by inhdemo1;
grant connect, resource to inhdemo3 identified by inhdemo3;

connect inhdemo1/inhdemo1

Rem
Rem Inheriting Attributes
Rem

create or replace type Person_t as object
( ssn number,
  name varchar2(30),
  address varchar2(100),
  map member function mapfn  return number,
  static function check_type (in1 Person_t) return varchar2,
  member function whatami return varchar2 
) not final;
/
show error

create or replace type Student_t under Person_t
( deptid number,
  major varchar2(30)
) not final;
/
show error

create or replace type Employee_t under Person_t
( empid number,
  mgr varchar2(30),
  salary number(10,2),
  overriding map member function mapfn return number,
  member function raise_salary return Employee_t,
  overriding member function whatami return varchar2
) not final;
/
show error

create type PartTimeStudent_t under Student_t
( numhours number,
  overriding map member function mapfn return number
);
/
show error

create or replace type body PartTimeStudent_t as
overriding map member function mapfn return number as
  begin
    return self.numhours;
  end;
end;
/
show errors

create type dup_t as object 
(dup student_t);
/
show errors

/* create and populate tables depend on types */
create table person_obj of Person_t
  not substitutable at all levels
  (constraint pobj_pkey primary key(ssn));
create table employee_obj of Employee_t
  not substitutable at all levels
  (constraint eobj_pkey primary key(ssn,empid),
   mgr not null);
create table employee_obj2 of Employee_t
  not substitutable at all levels;
create table student_obj of Student_t
  not substitutable at all levels
  (unique (name, deptid));

create table all_person (PersRefCol REF Person_t, tabname varchar2(20));

create table dup_students (ssn number, p student_t)
 column p not substitutable at all levels;

insert into person_obj values(12345, 'Joe','SF');
insert into employee_obj values (2345, 'Max','LA',56, 'King',100.20);
insert into employee_obj2 values (2345, 'Max','LA',56, 'King',100.20);
insert into student_obj values (2345, 'Max','LA',300,'PLAY');

insert into all_person select REF(p), 'PERSON_OBJ' from person_obj p;
insert into all_person select REF(p), 'EMPLOYEE_OBJ' from employee_obj p;
insert into all_person select REF(p), 'EMPLOYEE_OBJ2' from employee_obj2 p;
insert into all_person select REF(p), 'EMPLOYEE_OBJ2-2' from employee_obj2 p;
insert into all_person select REF(p), 'STUDENT_OBJ' from student_obj p;

insert into dup_students select ssn, value(p) from student_obj p;

commit;

select * from person_obj;
select * from employee_obj;
select * from employee_obj2;
select * from student_obj;
select * from dup_students;

select deref(p.persrefcol), tabname from all_person p order by p.tabname;

/* cannot insert subtype to non-substitutable column/table */
insert into person_obj values
  (Employee_t(12345, 'Joe','SF',null,null,null));
insert into person_obj select treat(value(p) as person_t) from employee_obj p;

update dup_students set p=PartTimeStudent_t(2345, 'Max','LA',300,'PLAY',0);

/* create type body */
create or replace type body Person_t as
  map member function mapfn return number as
    begin
      return self.ssn;
    end;

  static function check_type (in1 Person_t) return varchar2 as
    n varchar2(40);
    begin
      n := 
        case 
         when in1 is of (ONLY Person_t) then 'PERSON_T'
         when in1 is of (ONLY Employee_t) then 'EMPLOYEE_T'
         when in1 is of (ONLY Student_t) then 'STUDENT_T'
         when in1 is of (ONLY PartTimeStudent_t) then 'PARTTIMESTUDENT_T'
        else 'UNKNOWN' end;    
      return n;  
     end;

    member function whatami return varchar2 as
    begin
      return 'PERSON_T';
    end;
end;
/
show error

create or replace type body Employee_t as
  overriding map member function mapfn return number as
  begin
    return self.empid;
  end;

  member function raise_salary return Employee_t as
  e_v1 Employee_t;
  begin
    update employee_obj e set salary = self.salary + 999.99
      where self.ssn = ssn and self.salary is null
      returning value(e) into e_v1;
    return e_v1;
  end;

  overriding member function whatami return varchar2 as
  begin
    return 'EMPLOYEE_T';
  end;
end;
/
show error

Rem
Rem Assignments
Rem

Rem Object (REF) assignment

create table t 
( perscol Person_t,
  empcol Employee_t,
  stucol Student_t)
column perscol not substitutable at all levels,
column empcol not substitutable at all levels,
column stucol not substitutable at all levels
;

insert into t values
( Person_t(12345, 'Joe','SF'),
  Employee_t(9876, 'Bill','SJ',1111,NULL,100),
  Student_t(2345, 'Max','LA',300,'PLAY')
);

commit;

Rem Widening

update t set perscol = empcol;
select * from t;

set serveroutput on
declare
  var1 Person_t;
  var2 Employee_t;
  var3 Employee_t;
begin
  select empcol into var2 from t t where t.perscol.ssn=12345;
  var1 := var2;
  dbms_output.put_line('ssn:'||to_char(var1.ssn));
  var3 := treat(var1 as Employee_t);
  dbms_output.put_line('empid:'||to_char(var3.empid));
  insert into t values (Person_t(999,'Boss','KS'),
    treat(var1 as Employee_t),null);
  select empcol into var3 from t t where t.perscol.ssn=999;
  dbms_output.put_line('empid:'||to_char(var3.empid));
end;
/

rollback;

Rem Narrowing

update t set empcol = TREAT(perscol AS Employee_t);
select * from t;

rollback;

Rem Compile Error

update t set empcol = perscol;
update t set empcol = stucol;

declare
  var1 Student_t;
  var2 Employee_t;
begin
  var1 := var2;
end;
/

rollback;

Rem Collection Assignment

create type PersonSet as table of Person_t;
/
create type StudentSet as table of Student_t;
/

declare
  var1 PersonSet; var2 StudentSet;
  elem1 Person_t; elem2 Student_t;
begin
  var1 := var2; /* ILLEGAL - collections not of same type */
end;
/

declare
  var1 PersonSet; var2 StudentSet;
  elem1 Person_t; elem2 Student_t;
begin
  var1 := PersonSet (elem1, elem2); /* LEGAL : Element is of subtype */
end;
/

Rem
Rem Comparison
Rem

Rem Object Comparison

insert into t values
(Person_t(99, 'Joey', 'LA'),
 Employee_t(99, 'Joey', 'LA', 1,'Max',90.99),
 Student_t(99, 'Joey', 'LA', 2022, 'Minor'));

select * from t where perscol=empcol or perscol=stucol;
select * from t where perscol=treat(empcol as person_t);
select * from t where treat(stucol as person_t)=treat(empcol as person_t);
select * from t where stucol=empcol;

Rem REF Comparison

select count(distinct(persrefcol)) from all_person;
select count(distinct(deref(persrefcol))) from all_person;

/* valid compare */
select deref(p.persrefcol), p.tabname
  from all_person p where 
  exists (select p2.persrefcol from all_person p2 where 
          p.persrefcol = p2.persrefcol and p.tabname != p2.tabname)
  order by p.tabname;

select p.tabname
  from all_person p where
  (select ref(s) from person_obj s) = (select ref(k) from employee_obj k);

select p.tabname
  from all_person p where
  (select ref(s) from student_obj s) = persrefcol;

select p.tabname
  from all_person p where
  (select treat(ref(s) as ref person_t) from student_obj s) = 
    (select ref(k) from employee_obj k);

/* invalid compare */
select p.tabname
  from all_person p where
  (select ref(s) from student_obj s) = (select ref(k) from employee_obj k);

Rem
Rem SQL Operators
Rem

create view Person_v of Person_t with object identifier(ssn) as
  select * from person_obj p;
create view Student_v of Student_t under Person_v as
  select * from student_obj p;

Rem VALUE

select VALUE(p) from Person_v p order by p.ssn;
select VALUE(p) from ONLY(Person_v) p order by p.ssn;

Rem REF

select REF(p) from Person_v p;

select REF(p) from Person_v p
minus
select REF(p) from person_obj p
minus
select REF(p) from student_obj p;

Rem DEREF

select DEREF(REF(p)) from person_v p order by p.ssn desc;

/* Do some PL/SQL with VALUE, REF, DEREF of person_v */

Rem TREAT

select TREAT(VALUE(p) as Student_t) from Person_v p order by p.ssn desc;

select DEREF(a) from 
  (select TREAT(REF(p) as REF Student_t) a from Person_v p) k
  order by k.a.ssn;

insert into employee_obj values 
  (Employee_t(2929, 'Billy', 'TX', 9, 'Max', NULL));

create global temporary table tmptb1 (a Employee_t)
  column a not substitutable at all levels;

set serveroutput on
declare
  var1 Person_t := Employee_t(2929, 'Billy', 'TX', 9, 'Max', NULL);
  var2 Person_t;
  var3 Employee_t;
  v_type varchar2(20);
begin
  insert into tmptb1 values(TREAT(var1 as Employee_t))
    returning a into var2;
  if var2 is null then
    dbms_output.put_line('var2 is null - WRONG');
  else
    var3 := TREAT(var2 as Employee_t);
    v_type := Person_t.check_type(var3);
    if v_type = 'EMPLOYEE_T' then
      dbms_output.put_line('row updated, ssn:'||var3.ssn||', salary:'||
        to_char(var3.salary));
    else
      dbms_output.put_line('var3 is of type is '||v_type||' - WRONG');
    end if;
  end if;
end;
/

select * from employee_obj order by ssn;

rollback;

Rem IS OF

select REF(p) from person_v p 
  where VALUE(p) IS OF (Employee_t, Student_t);

select VALUE(p) from person_v p
  where VALUE(p) IS OF (ONLY Person_t);

select TREAT(VALUE(p) AS Student_t)
  from Person_v p
  where VALUE(p) IS OF (ONLY Student_t);

select * from all_person 
  where DEREF(PersRefCol) IS OF (Student_t);

Rem
Rem View Hierarchy
Rem
create table persons (
  ssn number constraint pers_pkey primary key,
  name varchar2(30),
  address varchar2(100));

create table employees (
  ssn number constraint emp_pkey primary key,
  name varchar2(30),
  address varchar2(100),
  empid number,
  mgr varchar2(30),
  salary number(7,2));

create table students (
  ssn number constraint stu_pkey primary key,
  name varchar2(30),
  address varchar2(100),
  deptid number,
  major varchar2(30));

insert into employees select * from employee_obj;
insert into students select * from student_obj;
insert into persons select * from person_obj;
commit;

create or replace view person_v of person_t with object id(ssn) as
  select ssn, name, address from person_obj;

create or replace view student_v of student_t under person_v as
  select Student_t(ssn, name, address, deptid, major) from students;

select * from person_v p order by p.ssn;
select value(p) from person_v p order by p.ssn;
select * from ONLY (person_v) order by ssn;
select * from student_v order by ssn;

Rem Polymorphic Views

create view Persons_view of Person_t with object id(ssn) as
  select Person_t(ssn, name, address) from persons
  union all
  select TREAT(Employee_t(ssn, name, address, empid, mgr, salary) as Person_t)
  from employees;

select value(p) from Persons_view p order by p.ssn;
select (TREAT(value(p) as Employee_t)).empid, ssn from Persons_view p
  where value(p) IS OF (Employee_t)
  order by ssn; 

Rem
Rem Substitutable columns/table
Rem 

drop table person_obj;
drop table employee_obj;
drop table employee_obj2;
drop table student_obj;
drop table all_person;
drop table dup_students;

/* create and populate tables depend on types */
create table person_obj of Person_t
  (constraint pobj_pkey primary key(ssn));
create table employee_obj of Employee_t
  (constraint eobj_pkey primary key(ssn,empid),
   mgr not null);
create table student_obj of Student_t
  (unique (name, deptid));

create table all_person (PersRefCol REF Person_t, tabname varchar2(20));

create table dup_students (ssn number, p student_t);

insert into person_obj values(12345, 'Joe','SF');
insert into employee_obj values (2345, 'Max','LA',56, 'King',100.20);
insert into student_obj values (2345, 'Max','LA',300,'PLAY');
insert into student_obj values 
 (PartTimeStudent_t(90, 'Kay', 'TX', 300, 'PLAY',3));
insert into person_obj values
  (Employee_t(9999, 'Joe','SF',null,null,null));
insert into person_obj select value(p) from student_obj p;

insert into all_person select REF(p), 'PERSON_OBJ' from person_obj p;
insert into all_person select REF(p), 'EMPLOYEE_OBJ' from employee_obj p;
insert into all_person select REF(p), 'STUDENT_OBJ' from student_obj p;

insert into dup_students select ssn, value(p) from student_obj p;
update dup_students set p=PartTimeStudent_t(2345, 'Max','LA',300,'PLAY',0)
  where p IS OF (ONLY student_t);

commit;

select value(p) from person_obj p order by p.ssn;
select value(p) from employee_obj p order by p.ssn;
select value(p) from student_obj p order by p.ssn;
select d.ssn from dup_students d where p is of (only student_t);
select d.ssn, d.p.ssn, Person_t.check_type(p) 
  from dup_students d order by d.ssn;

select deref(p.persrefcol), tabname from all_person p 
  order by p.tabname, p.persrefcol.ssn;

delete from person_obj p where value(p) IS OF (Employee_t, PartTimeStudent_t);
select value(p) from person_obj p order by p.ssn;
select deref(p.persrefcol), tabname from all_person p 
  where persrefcol is not dangling 
  order by p.tabname, p.persrefcol.ssn;

rollback;

select value(p) from person_obj p order by p.ssn;

select deref(p.persrefcol), tabname from all_person p 
  order by p.tabname, p.persrefcol.ssn;

Rem
Rem Overriding Methods
Rem

create or replace type MyBaseType as object
( a1 number,
  final member procedure foo (in1 varchar2),
  static function foo1 (in1 number) return number,
  static function foo3 (in1 number) return number,
  member function foo2 (in1 varchar2) return date
) not instantiable not final;
/
show error

Rem can only redefines (override) NOT final method
create or replace type MySubType under MyBaseType
( a2 number,
  a3 date,
  overriding member procedure foo (in1 varchar2)
) not final;
/
show error

/* overloading final method ok */
create or replace type MySubType under MyBaseType
( a2 number,
  a3 date,
  member procedure foo (in2 IN OUT varchar2)
) not final;
/
show error

create or replace type MySubType under MyBaseType
( a2 number,
  a3 date,
  static function foo1 (in2 char) return varchar2,
  member function foo2 (in1 varchar2, in2 number) return varchar2,
  overriding member function foo2 (in1 varchar2) return date
) not final;
/
show error
  
/* can't have not instantiable static method */
create or replace type MySubType2 under MySubType
(not instantiable static function foo3 (in1 varchar2) return number)
not instantiable not final;
/
show errors

create or replace type MySubType2 under MySubType
(not instantiable member function foo3 (in1 varchar2) return number,
 not instantiable member function foo5 (in1 varchar2) return number
)
not instantiable not final;
/
show errors

Rem
Rem Dynamic Method Dispatch
Rem

create or replace type body MyBaseType as
  final member procedure foo (in1 varchar2) as
    begin
      dbms_output.put_line('MyBaseType.foo:'||in1);
    end;
  
  static function foo1 (in1 number) return number as
    begin
      return in1;
    end;

  static function foo3 (in1 number) return number as
    begin
      return in1+3;
    end;

  member function foo2 (in1 varchar2) return date as
    begin
      return to_date(in1,'MM/DD/YYYY');
    end;
end;
/
show error

create or replace type body MySubType as
  static function foo1 (in2 char) return varchar2 as
    begin
      return ('MySubType.foo1:'||in2);
    end;

  member function foo2 (in1 varchar2, in2 number) return varchar2 as
    begin
      return ('MySubType.foo2:'||in1||' '||in2);
    end;

  overriding member function foo2 (in1 varchar2) return date as
    begin
      return to_date(in1,'DD/MM/YYYY');
    end;
end;
/
show error

/* cannot instantiate type */
select MyBaseType(1) from dual;

select treat(MySubType(1,2,to_date('10/20/2000','MM/DD/YYYY')) as MyBaseType) 
  mysubtype
  from dual;
select treat(MySubType2(1,2,to_date('10/20/2000','MM/DD/YYYY')) as MySubType) 
  from dual;

/* ok */
select MySubType(1,2,to_date('10/20/2000','MM/DD/YYYY')) mysubtype from dual;
select MySubType2.foo1(3) from dual;

set serveroutput on

declare
  v1 MyBaseType;
  v2 MySubType;
  v3 date;
begin
  v3 := to_date('09/30/2000','MM/DD/YYYY');
  v1 := MySubType(1,2,v3);
  v2 := MySubType(1,2,v3);
  dbms_output.put_line('MySubType.foo1(1):'||MySubType.foo1(1));
  dbms_output.put_line('MySubType.foo1(''1''):'||MySubType.foo1('1'));
  dbms_output.put_line('MySubType.foo3(2):'||MySubType.foo3(2));
  dbms_output.put_line('MyBaseType.foo3(3):'||MyBaseType.foo3(3));
  v2.foo('a');
  v1.foo('a');
end;
/

declare
  v1 MyBaseType;
  v2 MySubType;
  v3 date;
begin
  v3 := to_date('09/30/2000','MM/DD/YYYY');
  v1 := MySubType(1,2,v3);
  dbms_output.put_line('v1.foo2(''11/12/1999''):'||v1.foo2('11/12/1999'));
end;
/

Rem
Rem Rights Model
Rem

create or replace type deftype1 as object
( a1 number,
  a2 varchar2(10)
) not final;
/
show error

create or replace type subtype1 under deftype1 (a3 date);
/
show error

grant execute on deftype1 to inhdemo3;
grant under on deftype1 to inhdemo3;

connect inhdemo3/inhdemo3
Rem can't create subtype in different schema
create or replace type inhdemo3.subtype2 under inhdemo1.deftype1 
(a3 number);
/
show error

connect inhdemo1/inhdemo1
Rem create root type with INVOKER RIGHT
create or replace type invtype1 authid current_user as object
( a1 number,
  a2 varchar2(10)
) not final;
/
show error

grant execute on invtype1 to inhdemo3;
grant under on invtype1 to inhdemo3;

connect inhdemo3/inhdemo3
Rem ok to create subtype in different schema
create or replace type inhdemo3.subtype2 under inhdemo1.invtype1 
(a3 varchar2(10));
/
show error

Rem cleanup

connect system/manager
drop user inhdemo1 cascade;
drop user inhdemo3 cascade;

