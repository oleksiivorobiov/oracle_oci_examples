rem
rem $Header: maporder.sql 12-aug-2004.06:03:09 rsriragh Exp $
rem
rem maporder.sql
rem
rem Copyright (c) 1996, 2004, Oracle. All rights reserved.  
rem
rem    NAME
rem      maporder.sql - Oracle 8 demo of map and order functions
rem
rem    DESCRIPTION
rem      This demo features Oracle8's object extensions, specifically map and
rem      order functions of abstract data types
rem
rem    NOTES
rem
rem    MODIFIED   (MM/DD/YY)
rem    rsriragh    08/12/04 - fix order by diffs
rem    ytsai       05/12/04 - order by 
rem    hyeh        08/10/99 - use sqlplus syntax
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    yaggarwa    08/06/98 - Add New syntax changes
rem    cchau       08/18/97 - enable dictionary protection
rem    mchien      05/29/97 - fix type syntax
rem    mchien      07/17/96 -
rem    mchien      07/16/96 - Created
rem

REMARK >>>> Set System Variables For Current SQLPlus Session <<<<
SET FEEDBACK 1
SET NUMWIDTH 10
SET PAGESIZE 24
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET DEFINE '^'

set echo on
connect sys/knl_test7 as sysdba;
grant connect, resource to mmo2 identified by mmo2;

connect mmo2/mmo2

REM  create type with MAP function with parameters should error!
create type rational AS OBJECT (
  numerator INTEGER,
  denominator INTEGER,
  MAP MEMBER FUNCTION rat_to_real (x number) RETURN REAL,
   PRAGMA RESTRICT_REFERENCES
  (rat_to_real, RNDS, WNDS, RNPS, WNPS)
);
/
show errors

create or replace type rational AS OBJECT (
  numerator INTEGER,
  denominator INTEGER,
  MAP MEMBER FUNCTION rat_to_real RETURN REAL,
  PRAGMA RESTRICT_REFERENCES
  (rat_to_real, RNDS, WNDS, RNPS, WNPS)
);
/
show errors

REM  create body with missing MAP specification in the body should error
create type body rational AS
  MEMBER FUNCTION rat_to_real RETURN REAL IS
  BEGIN
   IF denominator = 0 THEN
      RETURN 0;
   ELSE
      RETURN numerator/denominator;
   END IF;
   --Unqualified attributes refer to the "self" object.
  END;
END;
/
show errors

REM  create body with ORDER function instead of MAP as specified in type spec
REM  should error
create or replace type body rational AS
  ORDER MEMBER FUNCTION rat_to_real RETURN REAL IS
  BEGIN
   IF denominator = 0 THEN
      RETURN 0;
   ELSE
      RETURN numerator/denominator;
   END IF;
   --Unqualified attributes refer to the "self" object.
  END;
END;
/
show errors
create or replace type body rational AS
  MAP MEMBER FUNCTION rat_to_real RETURN REAL IS
  BEGIN
   IF denominator = 0 THEN
      RETURN 0;
   ELSE
      RETURN numerator/denominator;
   END IF;
   --Unqualified attributes refer to the "self" object.
  END;
END;
/
show errors


create table tb1(
  c1   varchar2(10),
  c2   rational,
  c3   rational,
  c4   real);

insert into tb1 values('row1', rational(9,10), rational(9,10), .9);
insert into tb1 values('row2', rational(7,8), rational(7,8), .8);
insert into tb1 values('row3', rational(5,6), rational(5,6), .7);
insert into tb1 values('row4', rational(3,4), rational(3,4), .6);
insert into tb1 values('row5', rational(1,2), rational(1,2), .5);
insert into tb1 values('row6', rational(0,1), rational(0,1), .4);
insert into tb1 values('bomb1', rational(1,0), rational(1,0), .3);
insert into tb1 values('bomb2', rational(1,null), rational(1,null), null);
insert into tb1 values('bomb3', rational(null,1), rational(null,1), null);
insert into tb1
  values('bomb4', rational(null,null), rational(null,null), null);
insert into tb1 values('bomb5', null, null, null);
select c1, sys_op_dump(c2), sys_op_dump(c3) from tb1 order by c2, c3, c1;
select c1, sys_op_dump(c2), sys_op_dump(c3) from tb1 order by c2, c3 desc, c1;
select c1, sys_op_dump(c3) from tb1 order by c3 desc, c1;
select c1, sys_op_dump(c2), sys_op_dump(c3) from tb1 order by c3, c2, c1;
select c1, sys_op_dump(c2), sys_op_dump(c3) from tb1 order by c3, c2 desc, c1;
select c1, sys_op_dump(c2) from tb1 order by c2 desc, c1;

update tb1 p set c4 = p.c2.rat_to_real();
select sys_op_dump(c2), c4 from tb1 order by 1, c4;
update tb1 p set c4 = p.c3.rat_to_real();
select sys_op_dump(c3), c4 from tb1 order by c4, c1;

select c1, sys_op_dump(c2-c3) from tb1 order by c2, c3, c1;
select c1 from tb1 where c2-c3 = 0 order by c1;

select sys_op_dump(c2) from tb1 where c2 > c3 order by c2, c1;
select sys_op_dump(c2) from tb1 where c2 > rational(20,1) order by c1;
select sys_op_dump(c2) from tb1 where c2 = rational(9,10) order by c1;

select sys_op_dump(c3) from tb1 where c3 > rational(1,20) order by c3, c1;
select sys_op_dump(c3) from tb1 where c3 = rational(9,10) order by c1;
select sys_op_dump(c3) from tb1 where c3 in (rational(9,10), rational(1,2))
  order by c3, c1;

REM  Now test order function for rational numbers
REM  error case where order function does not have arguments!
create type rat AS OBJECT (
  numerator INTEGER,
  denominator INTEGER,
  ORDER MEMBER FUNCTION order_rat RETURN INTEGER,
  PRAGMA RESTRICT_REFERENCES
  (order_rat, RNDS, WNDS, RNPS, WNPS)
);
/

show errors

create or replace type rat AS OBJECT (
  numerator INTEGER,
  denominator INTEGER,
  ORDER MEMBER FUNCTION order_rat (rat1 rat) RETURN INTEGER,
  PRAGMA RESTRICT_REFERENCES
  (order_rat, RNDS, WNDS, RNPS, WNPS)
);
/
show errors

create type body rat AS
  ORDER MEMBER FUNCTION order_rat(rat1 rat) RETURN INTEGER IS
    den1  INTEGER;
    den2  INTEGER;
    num1  INTEGER;
    num2  INTEGER;
  BEGIN
   den1 := nvl(self.denominator, 0);
   den2 := nvl(rat1.denominator, 0);
   num1 := nvl(self.numerator, 0);
   num2 := nvl(rat1.numerator, 0);
   if den1 = den2 then
     return num1 - num2;
   elsif (den1=0) then
     return 1;
   elsif (den2=0) then
     return -1;
   else
     return (num1*den2) - (den1*num2);
   END IF;
END;
END;
/
show errors


create table tb2(
  c1   varchar2(10),
  c2   rat,
  c3   rat,
  c4   real);

insert into tb2 values('row1', rat(9,10), rat(9,10), .9);
insert into tb2 values('row2', rat(7,8), rat(7,8), .8);
insert into tb2 values('row3', rat(5,6), rat(5,6), .7);
insert into tb2 values('row4', rat(3,4), rat(3,4), .6);
insert into tb2 values('row5', rat(1,2), rat(1,2), .5);
insert into tb2 values('row6', rat(0,1), rat(0,1), .4);
insert into tb2 values('bomb1', rat(1,0), rat(1,0), .3);
insert into tb2 values('bomb2', rat(1,null), rat(1,null), null);
insert into tb2 values('bomb3', rat(null,1), rat(null,1), null);
insert into tb2 values('bomb4', rat(null,null), rat(null,null), null);
insert into tb2 values('bomb5', null, null, null);
select c1, sys_op_dump(c2), sys_op_dump(c3) from tb2 order by c2, c3, c1;
select c1, sys_op_dump(c2), sys_op_dump(c3) from tb2 order by c2, c3 desc, c1;
select c1, sys_op_dump(c3) from tb1 order by c3 desc, c1;
select c1, sys_op_dump(c2), sys_op_dump(c3) from tb2 order by c3, c2, c1;
select c1, sys_op_dump(c2), sys_op_dump(c3) from tb2 order by c3, c2 desc, c1;
select c1, sys_op_dump(c2) from tb1 order by c2 desc, c1;
update tb2 p set c4 = p.c2.order_rat(rat(3,10));
select sys_op_dump(c2), c4 from tb2 order by c4, c1;
update tb2 p set c4 = p.c2.order_rat(c3);
select sys_op_dump(c2), sys_op_dump(c3), c4 from tb2 order by c4, c1;
select sys_op_dump(c2) from tb1 minus select sys_op_dump(c2) from tb2;
select sys_op_dump(c2) from tb1 minus select sys_op_dump(c3) from tb2;
select sys_op_dump(c3) from tb1 minus select sys_op_dump(c3) from tb2;

select sys_op_dump(c2) from tb1 intersect select sys_op_dump(c2) from tb2;
select sys_op_dump(c2) from tb1 intersect select sys_op_dump(c3) from tb2;
select sys_op_dump(c3) from tb1 intersect select sys_op_dump(c3) from tb2;

select sys_op_dump(c3) from tb2 where c2 in (select c2 from tb2) order by c3;
select sys_op_dump(c3) from tb2
  where c3 between rat(1,100) and rat(99,100) order by c3;
select sys_op_dump(c2) from tb2
  where c2 = ANY (SELECT c2 from tb2 where c3 = rat(9,10));
select sys_op_dump(c3) from tb2 where c2 >= ALL (rat(1,10), rat(9,10));
select c1 from tb2 where c2-c3 = 0 order by c1;

REM #### test a recursive method
REM  method is the factorial function

REM  note: pragma for map method is not required!

create or replace type tfact AS OBJECT (c1 int,
map member function mfact return int,
member function fact(a int) return int ,
PRAGMA RESTRICT_REFERENCES(fact,WNDS,RNDS,WNPS,RNPS) );
/
show errors

create or replace type body tfact AS
 member function fact(a int) return int is
  begin
   if a is null then return 0;
    elsif a = 0 then return 1;
    else return a*fact(a-1);
   end if;
  end;
 map member function mfact return int is
  begin
   return c1;
  end;
END;
/
show errors

CREATE TABLE tbfact (a tfact, b int, constraint aci check (a.c1 >= 0),
 constraint bci check (b >= 0));
INSERT INTO tbfact values(tfact(3), 4);
INSERT INTO tbfact values(tfact(-1), 1);
INSERT INTO tbfact values(tfact(1), -1);
INSERT INTO tbfact values(tfact(0), 1);
INSERT INTO tbfact values(tfact(null), 0);
INSERT INTO tbfact values (null,0);
INSERT INTO tbfact values(tfact(2), 3);
SELECT p.a.c1 a, p.a.fact(p.a.c1) afact, b, p.a.fact(b) bfact FROM tbfact p
WHERE a is not null ORDER by a;
SELECT p.a.c1 a, p.a.fact(p.a.c1) afact, b, p.a.fact(b) bfact FROM tbfact p
WHERE a is not null ORDER by a desc;

drop type body rat;

create or replace type body rat as
 ORDER MEMBER FUNCTION order_rat(rat1 rat) RETURN INTEGER IS
 BEGIN
  if self.denominator = rat1.denominator then
      return self.numerator - rat1.numerator;
  else
      return (self.numerator * rat1.denominator) -
         (self.denominator * rat1.numerator);
  END IF;
 END;
END;
/

select c1, sys_op_dump(c2), sys_op_dump(c3) from tb2 order by c2, c3;
select c1, sys_op_dump(c2), sys_op_dump(c3) from tb2 order by c2, c3 desc;
select c1, sys_op_dump(c2), sys_op_dump(c3) from tb2 order by c3, c2;

connect sys/knl_test7 as sysdba
drop user mmo2 cascade;


