Rem
Rem $Header: tyevdemo.sql 01-may-2001.14:30:34 hyeh Exp $
Rem
Rem tyevdemo.sql
Rem
Rem  Copyright (c) Oracle Corporation 2001. All Rights Reserved.
Rem
Rem    NAME
Rem      tyevdemo.sql - Demonstrate Type Evolution feature
Rem
Rem    DESCRIPTION
Rem      Demonstrate Type Evolution feature
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    hyeh        05/01/01 - fix query format
Rem    hyeh        04/10/01 - Merged hyeh_inh_tyev_demos
Rem    hyeh        04/10/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
COLUMN constraint_nm FORMAT A20
COLUMN index_nm FORMAT A20
SET ECHO ON

connect system/manager
drop user tyevdemo1 cascade;

grant connect, resource to tyevdemo1 identified by tyevdemo1;

connect tyevdemo1/tyevdemo1

-- create some types and tables
create type type1 as object (c1 number, c2 number, c3 number);
/
show errors
create type type2 as object (c2 varchar2(10), c3 number, c4 char(10),
  member function mf1 (in1 varchar2) return number);
/
show errors

create type body type2 as
  member function mf1 (in1 varchar2) return number as
    begin
      if in1 is null then
         return length(self.c2);
      end if;
      return length(self.c2) + length(in1);
    end;
end;
/
show errors

create type type3 as object (c1 ref type2, c2 varchar2(10));
/
show errors
create type type4 as object (c1 type2, c2 varchar2(10));
/
show errors
create type nttype1 as table of char(6);
/
show errors
create type vatype1 as varray(7) of ref type2;
/
show errors
create type nttype2 as table of type4;
/
show errors

create table otb1 of type1;
create table rtb1 (c1 type1, c2 number);
create table otb2 of type2 (constraint opk2 primary key (c2));

create table rtb2 (c1 type2, c2 vatype1, c3 number, c4 nttype2,
  constraint rpk2 primary key (c1.c2))
  nested table c4 store as rtb2_c4,
  varray c2 store as lob rtb2_c2 (index rlobix2_c2);
create index rix2 on rtb2 (c1.c4, c1.c2);

insert into otb1 values (1,1,1);
insert into rtb1 select value(p), 1 from otb1 p;

insert into otb2 values ('abcdefghij',1,'abcdefghij');
insert into rtb2 values 
  (type2('abcdefghij',1,'abcdefghij'), null, 1,nttype2(type4(null,null)));

update rtb2 p set p.c2=vatype1(null,null,null,null,null,null,null);
update table (select c4 from rtb2) e
 set value(e)=type4((select value(p) from otb2 p), 'a');

declare
  vref ref type2;
  va1 vatype1;
  va_cnt integer;
begin
  select c2 into va1 from rtb2;
  select ref(a) into vref from otb2 a;
  va1 := vatype1(vref,null,null,null,null,null,null);
  update rtb2 set c2 = va1;
end;
/

/*******************/
/* Add/Drop Method */
/*******************/

alter type type2 
  add order member function om (in1 type2) return number,
  add static procedure sp1 (in1 type2),
  drop member function mf1 (in1 varchar2) return number,
  add member function mf1 (in2 varchar2) return varchar2 
  cascade;

create or replace type body type2 as
  member function mf1 (in2 varchar2) return varchar2 as
    begin
      if in2 is null then
         return self.c2;
      end if;
      return (self.c2 || in2);
    end;

  order member function om (in1 type2) return number as
    begin
      return (self.c3 - in1.c3);
    end;

  static procedure sp1 (in1 type2) as
    begin
      dbms_output.put_line('in1.c3 = '||in1.c3);
    end;
end;
/
show errors

desc type2 

/* populate tables */
update table(select c4 from rtb2) e 
  set value(e) = type4((select value(p) from otb2 p), '1a2b3c');

select * from otb1;
select * from rtb1;
select * from otb2;
select c1, c3, c4 from rtb2;
select deref(column_value) from table(select c2 from rtb2) order by 1;
select value(e) from table(select c4 from rtb2) e;

select substr(constraint_name,1,20) constraint_nm, constraint_type,
  TABLE_NAME, STATUS, INDEX_NAME,INVALID, VIEW_RELATED
  from user_constraints order by 3,1;

select substr(index_name,1,20) index_nm, index_type, table_name 
  from user_indexes
  order by 3, 1;

/********************/
/* Modify attribute */
/********************/
/* can't increase length of fixed variable */
alter type type2 modify attribute (c4 char(13)) cascade;
alter type type2 modify attribute (c2 varchar2(13)) cascade;

/* workaround - reconnect */
connect tyevdemo1/tyevdemo1

alter type type2 modify attribute (c2 varchar2(25)) invalidate; 

desc otb2
desc rtb2
desc type2

select * from otb2; 
select c3 from rtb2;

/******************/
/* Drop attribute */
/******************/

alter type type1 drop attribute(c2) cascade not including table data;
alter type type2 drop attribute(c2) cascade not including table data;

connect tyevdemo1/tyevdemo1

/* fix type2 body not to refer to dropped attribute */
create or replace type body type2 as
  member function mf1 (in2 varchar2) return varchar2 as
    begin
      if in2 is null then
         return self.c4;
      end if;
      return (self.c4 || in2);
    end;

  order member function om (in1 type2) return number as
    begin
      return (self.c3 - in1.c3);
    end;

  static procedure sp1 (in1 type2) as
    begin
      dbms_output.put_line('in1.c3 = '||in1.c3);
    end;
end;
/
show errors

select * from otb1;
select * from rtb1;
select * from otb2;

select c1, c3, c4 from rtb2;
select deref(column_value) from table(select c2 from rtb2) order by 1;

select substr(constraint_name,1,20) constraint_nm, constraint_type,
  TABLE_NAME, STATUS, INDEX_NAME,INVALID, VIEW_RELATED
  from user_constraints order by 3,1;

select substr(index_name,1,20) index_nm, index_type, table_name 
  from user_indexes
  order by 3, 1;

alter type type1 drop attribute(c3) invalidate;

select * from otb1;
select c2 from rtb1;

/* should get get ORA-22337 */
select * from rtb1;

/* reconnect to get new defn */
connect tyevdemo1/tyevdemo1
select * from rtb1;

/******************/
/* Add attribute  */
/******************/

alter table rtb1 upgrade;

alter type type1 add attribute (c2 date) cascade;

connect tyevdemo1/tyevdemo1
desc otb1
desc rtb1
desc type1

alter type type1 drop attribute (c2) cascade;

/* should get get ORA-22337 */
select * from rtb1;

connect tyevdemo1/tyevdemo1
/* get new defn */
desc type1
select * from rtb1;

alter type type1 add attribute (c3 clob) invalidate;

/******************/
/* Upgrade table  */
/******************/

alter table otb1 upgrade not including data;
alter table rtb1 upgrade not including data;

select c2 from rtb1;
select * from otb1;

update rtb1 set c2=c2;
update rtb1 set c1=c1;

alter table rtb1 drop (c1);
alter table rtb1 add (c1 type1);
alter table rtb1 upgrade including data;
select * from otb1;

connect tyevdemo1/tyevdemo1
desc type1
desc type2

update otb2 p set value(p)=value(p);

select * from otb2;
select p.c1.c2 from rtb2 p;

alter table rtb2 drop (c1);
select deref(column_value) from table(select c2 from rtb2 where c3=1)
  order by 1;

select p.column_value.c2 c2 
  from table(select c2 from rtb2 where c3=1) p;

truncate table rtb2;

set serveroutput on
-- show objects depend on types
exec dbms_utility.get_dependency('type','tyevdemo1','type1');
exec dbms_utility.get_dependency('type','tyevdemo1','type2');
exec dbms_utility.get_dependency('type','tyevdemo1','type3');
exec dbms_utility.get_dependency('type','tyevdemo1','nttype1');
exec dbms_utility.get_dependency('type','tyevdemo1','vatype1');

/********************************************************/
/* demonstrate type evolution with inheritance features */
/********************************************************/
connect tyevdemo1/tyevdemo1

alter table rtb1 drop (c1);
alter table rtb1 add (nc1 type1);

insert into rtb1 values
  (10, type1(10, empty_clob()));

/* change type1 to not final */
alter type type1 not final cascade;
alter type type1 
  add attribute (c10 char(6)) cascade not including table data;
alter type type1 
  add map member function mm1 return char cascade;

/* create subtype */

create or replace type type1s under type1 (c11 nttype1);
/
show errors

/* override map method */
alter type type1s
  add overriding map member function mm1 return char;

/* column rtb1.nc1 remains not substitutable */
insert into rtb1 values
  (1, type1s(1,empty_clob(),null,'a'));

/* create table with substitutable column */
create table rtb1s (c1 number, c2 type1, c3 date)
  nested table treat(c2 as type1s).c11 store as rtb1s_c2_c11;
insert into rtb1s
  select c2, type1s(p.nc1.c1, p.nc1.c3, p.nc1.c10, nttype1('A','B','C')), 
         to_date('08/21/2000','MM/DD/YYYY') from rtb1 p;

select * from rtb1s order by c1;

connect system/manager
drop user tyevdemo1 cascade;

