Rem
Rem $Header: exfdemo.sql 16-sep-2003.10:14:13 ayalaman Exp $
Rem
Rem exfdemo.sql
Rem
Rem Copyright (c) 2003, Oracle Corporation.  All rights reserved.  
Rem
Rem    NAME
Rem      exfdemo.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    ayalaman    09/16/03 - ayalaman_quoted_names_bug 
Rem    ayalaman    08/27/03 - Created
Rem

SET ECHO ON
SET FEEDBACK 1
SET NUMWIDTH 10
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100

connect scott/tiger
set linesize 200;
column associated_table format a12;
column attribute format a15;
column attribute_exp format a13;
column attribute_set_name format a8;
column data_type format a12;
column expr_table format a10;
column expr_column format a10;
column expset_table format a10;
column expset_column format a10;
column index_name format a18;
column interest format a40;
column operator_list format a25;
column subexpression format a12;
column udf_name format a20;
column xmltype_attr format a10;

/*****************************************************************
  The tests in this file are organized as follows: 
    1. Basic Functionality 
      1.1 Creation of the attribute set
      1.2 Adding user-defined functions to the attribute set
      1.3 Creation of an Expression datatype column in a user table
      1.4 Multiple forms of EVALUATE operator 
    2. Expression Filter Index (EE)
      2.1 Using Default index parameters 
      2.2 Using Exact index parameters
      2.3 Using Expression Statistics
    3. EVALUATE operator used in joins
      3.1 Batch evaluation
      3.2 Active application using trigger
    4. Expressions defined for table data
    5. XPath predicates in expressions
      5.1 Use of XMLType attribute
      5.2 Indexing XPath expressions
    6. DML With EVALUATE Operator
******************************************************************/
--------------------------
--- 1. Basic functionality 
--------------------------

-- 1.1 Creation of attribute set 

--- create the attribute set incrementally
BEGIN
  dbms_expfil.create_attribute_set(attr_set => 'Car4Sale');

  dbms_expfil.add_elementary_attribute (attr_set  => 'Car4Sale',
                                        attr_name => 'Model',
                                        attr_type => 'VARCHAR2(20)');

  dbms_expfil.add_elementary_attribute (attr_set  => 'Car4Sale',
                                        attr_name => 'Year',
                                        attr_type => 'NUMBER');

  dbms_expfil.add_elementary_attribute (attr_set  => 'Car4Sale',
                                        attr_name => 'Price',
                                        attr_type => 'NUMBER');

  dbms_expfil.add_elementary_attribute (attr_set  => 'Car4Sale',
                                        attr_name => 'Mileage',
                                        attr_type => 'NUMBER');
END;
/

select attribute_set_name, attribute, data_type
  from user_expfil_attributes;

exec dbms_expfil.drop_attribute_set (attr_set => 'Car4Sale');

--- create the same attribute set using an abstract type
CREATE OR REPLACE TYPE Car4Sale AS OBJECT
                   (Model    VARCHAR2(20),
                    Year     NUMBER,
                    Price    NUMBER,
                    Mileage  NUMBER);
/

BEGIN
  dbms_expfil.create_attribute_set (attr_set => 'Car4Sale',
                                    from_type => 'YES');
END;
/

select attribute_set_name, attribute, data_type
  from user_expfil_attributes;

-- 1.2 Adding user-defined functions to the attribute set

--- create the functions required in the expressions
CREATE or REPLACE FUNCTION HorsePower(Model VARCHAR2, Year VARCHAR2)
   return NUMBER is
BEGIN
  return 200;
END HorsePower;
/

CREATE or REPLACE FUNCTION CrashTestRating(Model VARCHAR2, Year VARCHAR2)
  return NUMBER is
BEGIN
  return 5;
END CrashTestRating;
/

--- add the functions to the attribute set so that they can be used 
--- in the corresponding expressions 
BEGIN
  dbms_expfil.add_functions (attr_set => 'Car4Sale',
                             funcs_name => 'HorsePower');
  dbms_expfil.add_functions (attr_set => 'Car4Sale',
                             funcs_name => 'CrashTestRating');
END;
/

select attribute_set_name, udf_name, object_type  
  from user_expfil_aset_functions;

-- 1.3 Creation of an Expression datatype column in a user table

--- create a table with a VARCHAR2 column to hold expressions --
CREATE TABLE Consumer (CId       NUMBER,
                       Zipcode   NUMBER,
                       Phone     VARCHAR2(12),
                       Interest  VARCHAR2(200));

--- assign the attribute set to the VARCHAR2 column to convert it --
--- into an expression column --
BEGIN
  dbms_expfil.assign_attribute_set (attr_set => 'Car4Sale',
                                    expr_tab => 'Consumer',
                                    expr_col => 'Interest');
END;
/

select expr_table, expr_column, attribute_set
  from user_expfil_expression_sets; 

--- insert data (with expressions) into the Consumer table --
INSERT INTO Consumer VALUES (1, 32611, '917 768 4633',
         'Model=''Taurus'' and Price < 15000 and Mileage < 25000');
INSERT INTO Consumer VALUES (2, 03060, '603 983 3464',
         'Model=''Mustang'' and Year > 1999 and Price < 20000');
INSERT INTO Consumer VALUES (3, 03060, '603 484 7013',
         'HorsePower(Model, Year) > 200 and Price < 20000');


-- 1.4 Multiple forms of EVALUATE operator to evaluate the expressions
--- string formatted data item
SELECT * FROM Consumer
  WHERE EVALUATE (Consumer.Interest, 'Model =>''Mustang'',
                                      Year => 2000,
                                      Price => 18000,
                                      Mileage => 22000') = 1;

--- data item as an Anydata instance
SELECT * FROM Consumer
  WHERE EVALUATE (Consumer.Interest,
                    AnyData.convertObject(Car4Sale('Mustang',
                                                   2000,
                                                   18000,
                                                   22000))) = 1;

--- string formatted data item generated by STATIC getVarchar API
SELECT * FROM Consumer 
  WHERE EVALUATE (Consumer.Interest,
                    Car4Sale.getVarchar('Mustang', 
                                        2000,
                                        18000,
                                        22000)) = 1;

--- string formatted data item generated by MEMBER getVarchar API
SELECT * FROM Consumer 
  WHERE EVALUATE (Consumer.Interest,
                    Car4Sale('Mustang',
                             2000,
                             18000,
                             22000).getVarchar()) = 1;

--- applying additional predicates in the same query
SELECT * FROM Consumer 
  WHERE Zipcode between 03060 and 03070 and 
        EVALUATE (Consumer.Interest,
                    Car4Sale.getVarchar('Mustang', 
                                        2000,
                                        18000,
                                        22000)) = 1 

-----------------------------------------------------
--- 2. Expression Filter indexes on Expression column 
-----------------------------------------------------

-- 2.1 Default parameters associated with the attribute set

--- assign default index parameters to the attribute set
BEGIN
  dbms_expfil.default_index_parameters (
    attr_set  => 'Car4Sale',
    attr_list =>  exf$attribute_list (
      exf$attribute (attr_name => 'Model',    --- LHS for predicate group
                     attr_oper => exf$indexoper('='),
                     attr_indexed => 'TRUE'), --- indexed predicate group
      exf$attribute (attr_name => 'Price',
                     attr_oper => exf$indexoper('all'),
                     attr_indexed => 'TRUE'),
      exf$attribute (attr_name => 'HorsePower(Model, Year)',
                     attr_oper => exf$indexoper('=','<','>','>=','<='),
                     attr_indexed => 'FALSE') --- stored predicate group
    ));
END;
/

select attribute_set_name, attribute, indexed, operator_list 
  from user_expfil_def_index_params; 

--- create the index using default parameters
CREATE INDEX InterestIndex ON Consumer (Interest) 
  INDEXTYPE IS EXFSYS.EXPFILTER;

select index_name, subexpression, indexed, operator_list 
  from user_expfil_predtab_attributes;

DROP INDEX InterestIndex;

--- extend the default parameters at the time of index creation
CREATE INDEX InterestIndex ON Consumer (Interest) 
  INDEXTYPE IS EXFSYS.EXPFILTER PARAMETERS (
    'ADD TO DEFAULTS STOREATTRS (CrashTestRating(Model, Year))');

select index_name, subexpression, indexed, operator_list 
  from user_expfil_predtab_attributes;

DROP INDEX InterestIndex;

-- 2.2 Exact index paremeters for each expression column 

--- associate the index parameters with the expression column by 
--- deriving from defaults and fine tuning them
BEGIN
  -- derive index parameters from defaults 
  dbms_expfil.index_parameters (
                expr_tab  => 'Consumer',
                expr_col  => 'Interest',
                attr_list => null,
                operation => 'DEFAULT');

  -- fine-tune the parameters by adding another stored attribute
  dbms_expfil.index_parameters(
                expr_tab => 'Consumer',
                expr_col => 'Interest',
                attr_list => exf$attribute_list (
                              exf$attribute (
                                attr_name => 'CrashTestRating(Model, Year)',
                                attr_oper => exf$indexoper('all'),
                                attr_indexed => 'FALSE')),
                operation => 'ADD');
END;
/

select expset_table, expset_column, attribute, indexed, operator_list
  from user_expfil_index_params; 

-- create the index; uses the parameters associated with expression column
CREATE INDEX InterestIndex ON Consumer (Interest) 
  INDEXTYPE IS EXFSYS.EXPFILTER;

select index_name, subexpression, indexed, operator_list 
  from user_expfil_predtab_attributes;

DROP INDEX InterestIndex;

-- 2.3 Using Expression Statistics for the expression column 

--- collect the expression set statistics --
BEGIN
  dbms_expfil.get_exprset_stats (expr_tab => 'Consumer',
                                 expr_col => 'Interest');
END;
/

select expr_table, expr_column, attribute_exp, pct_occurrence, 
       pct_eq_oper
 from user_expfil_exprset_stats order by pct_occurrence, attribute_exp desc;

--- clear the exact parameters so that we can use the statistics 
BEGIN
  -- derive index parameters from defaults
  dbms_expfil.index_parameters (
                expr_tab  => 'Consumer',
                expr_col  => 'Interest',
                attr_list => null,
                operation => 'CLEAR');
END;
/

--- create the index using the statistics ---
CREATE INDEX InterestIndex ON Consumer (Interest)
  INDEXTYPE IS EXFSYS.EXPFILTER PARAMETERS (
     'STOREATTRS TOP 4 INDEXATTRS TOP 2');

select index_name, subexpression, indexed, operator_list 
  from user_expfil_predtab_attributes;

--------------------------------------
--- 3. EVALUATE Operator used in joins
--------------------------------------

--- 3.1 Batch evaluation of expression using joins 

--- Create a table to store information about cars ---
CREATE TABLE CarInventory OF Car4Sale;

INSERT INTO CarInventory VALUES ('Mustang',2000, 18000,22000);
INSERT INTO CarInventory VALUES ('Mustang',2000, 18000,22000);
INSERT INTO CarInventory VALUES ('Taurus',1997, 14000,24500);

--- Join the CarInventory with the Consumer table --
SELECT Consumer.CId, Consumer.Interest, Car.Model 
  FROM Consumer, CarInventory Car
  WHERE EVALUATE (Consumer.Interest, Car.getVarchar()) = 1;

DROP TABLE CarInventory;

--- A table not relying on the Car4Sale object type --
CREATE TABLE inventory (Model    VARCHAR2(20),
                        Year     NUMBER,
                        Price    NUMBER,
                        Mileage  NUMBER);

INSERT INTO Inventory VALUES ('Mustang',2000, 18000, 22000);
INSERT INTO Inventory VALUES ('Mustang',2000, 18000, 22000);
INSERT INTO Inventory VALUES ('Taurus',1997, 14000, 24500);

--- simple join --
SELECT Consumer.CId, Consumer.Interest, Inventory.Model 
  FROM Consumer, Inventory
  WHERE EVALUATE (Consumer.Interest, 
                   Car4Sale.getVarchar(Inventory.Model,
                                       Inventory.Year,
                                       Inventory.Price,
                                       Inventory.Mileage)) = 1;


--- demand analysis for the cars in the inventory
SELECT DISTINCT Inventory.Model, count(*) as Demand
  FROM Consumer, Inventory
  WHERE EVALUATE (Consumer.Interest,
                   Car4Sale.getVarchar(Inventory.Model,
                                       Inventory.Year,
                                       Inventory.Price,
                                       Inventory.Mileage)) = 1
  GROUP BY Inventory.Model
  ORDER BY Demand DESC;

truncate table Inventory;

-- 3.2 Active application using trigger

--- create a trigger on the inventory table to track new data 
CREATE TRIGGER activechk AFTER insert OR update ON Inventory
  FOR EACH ROW
DECLARE
  cursor c1 (ditem VARCHAR2) is
    SELECT CId, Phone FROM Consumer WHERE EVALUATE (Interest, ditem) = 1;
  ditem VARCHAR2(200);
BEGIN
  ditem := Car4Sale.getVarchar(:new.Model, :new.Year, :new.Price, :new.Mileage);
  for cur in c1(ditem) loop
    dbms_output.put_line(' For Model '||:new.Model||' Call '||cur.CId||
                         ' @ '||cur.Phone);
  end loop;
END;
/

set serveroutput on;
--- insert data into the inventory table
INSERT INTO Inventory VALUES ('Mustang',2000, 18000,22000);
INSERT INTO Inventory VALUES ('Mustang',2000, 18000,22000);
INSERT INTO Inventory VALUES ('Taurus',1997, 14000, 24500);

set serveroutput off;

drop table Inventory;
drop table Consumer;
exec dbms_expfil.drop_attribute_set (attr_set => 'Car4Sale');
drop type Car4Sale;
drop function HorsePower;
drop function CrashTestRating;

----------------------------------------------------------------
--- 4. Expressions defined for table data (use of table aliases)
----------------------------------------------------------------

--- create an attribute set with two table alias attributes referring
--- to SCOTT schema's EMP and DEPT tables. 

BEGIN
  dbms_expfil.create_attribute_set('hrdb');
  -- add elementary attributes to the Attribute Set --
  dbms_expfil.add_elementary_attribute('hrdb', 'hrmgr', 'VARCHAR2(20)');

  -- define elementary attributes of EXF$TABLE_ALIAS type --
  dbms_expfil.add_elementary_attribute('hrdb', 'emp',
                                       exf$table_alias('scott.emp'));
  dbms_expfil.add_elementary_attribute('hrdb', 'dept',
                                       exf$table_alias('scott.dept'));
END;
/

select attribute_set_name, attribute, data_type, associated_table
  from user_expfil_attributes;

--- create an expression column in a user table and configure it with
--- HEDB attribute set. 
CREATE TABLE HRInterest (SubId number, Interest VARCHAR2(100));

BEGIN
  dbms_expfil.assign_attribute_set('hrdb', 'HRInterest', 'Interest');
END;
/

--- insert expressions referring to table data --
INSERT INTO HRInterest VALUES (1,
  'hrmgr=''Greg'' and emp.job=''SALESMAN'' and emp.deptno = dept.deptno
     and dept.loc = ''CHICAGO''');
INSERT INTO HRInterest VALUES (2,
  'emp.job=''MANAGER'' and emp.sal < 2500');

--- create index for the expression column --
CREATE INDEX HRIndex ON HRInterest (Interest) 
  INDEXTYPE IS EXFSYS.EXPFILTER PARAMETERS (
   'STOREATTRS (emp.job, dept.loc, hrmgr) INDEXATTRS (emp.job, hrmgr)');

--- assign values for table alias attributes through joins --
SELECT empno, job, sal, loc, SubId, Interest
  FROM emp, dept, HRInterest
  WHERE emp.deptno = dept.deptno and
        EVALUATE(Interest, 
                 hrdb.getVarchar('Greg',emp.rowid,dept.rowid)) = 1;

SELECT empno, job, sal, loc, SubId, Interest
  FROM emp, dept, HRInterest
  WHERE emp.deptno = dept.deptno and emp.sal > 1400 and
        EVALUATE(Interest, 
                 hrdb.getVarchar('Greg',emp.rowid,dept.rowid)) = 1;

drop table HRInterest;
exec dbms_expfil.drop_attribute_set('hrdb');

--------------------------------------
--- 5. XPath predicates in expressions
--------------------------------------

--- 5.1 SYS.XMLType attribute in the attribute set
CREATE OR REPLACE TYPE Car4Sale AS OBJECT
          (Model    VARCHAR2(20),
           Year     NUMBER,
           Price    NUMBER,
           Mileage  NUMBER,
           Details  sys.XMLType);
/

BEGIN
  dbms_expfil.create_attribute_set (attr_set  => 'Car4Sale',
                                    from_type => 'YES');
END;
/

--- create expression column in a user table
CREATE TABLE Consumer (CId      NUMBER,
                       Zipcode  NUMBER,
                       Phone    VARCHAR2(12),
                       Interest VARCHAR2(200));

BEGIN
  dbms_expfil.assign_attribute_set (attr_set => 'Car4Sale',
                                    expr_tab => 'Consumer',
                                    expr_col => 'Interest');
END;
/

--- insert expression with XPath predicates in the table
INSERT INTO Consumer VALUES (1, 32611, '917 768 4633',
'Model=''Taurus'' and Price < 15000 and Mileage < 25000 and
extract(Details, ''//stereo[@make="Koss"]'') is not null');

INSERT INTO Consumer VALUES (2, 03060, '603 983 3464',
'Model=''Mustang'' and Year > 1999 and Price < 20000 and 
extract(Details, ''//stereo[@make="Koss" and /*/*/GPS/memory[text()="64MB"]]'') is not null');

INSERT INTO Consumer VALUES (3, 03060, '603 484 7013',
'Model=''Taurus'' and Price < 15000 and Mileage < 25000 and
existsNode(Details, ''//stereo[@make="Koss"]'') = 1');

--- evaluate the expressions for a data item --
SELECT * FROM Consumer 
  WHERE  EVALUATE (Consumer.Interest,
                   'Model => ''Mustang'',
                    Year => 2000,
                    Price => 18000,
                    Mileage => 22000,
                    Details =>
                      sys.XMLType(''<details>
                                      <color>White</color>
                                      <accessory>
                                        <stereo make="Koss">CD</stereo>
                                        <GPS>
                                           <resolution>1FT</resolution>
                                           <memory>64MB</memory>
                                        </GPS>
                                      </accessory>
                                    </details>'')') = 1;

SELECT * FROM Consumer 
  WHERE  EVALUATE (Consumer.Interest,
                    AnyData.convertObject(
                      Car4Sale('Mustang', 
                               2000, 
                               18000,
                               22000,
                               '<details>
                                  <color>White</color>
                                  <accessory>
                                    <stereo make="Koss">CD</stereo>
                                    <GPS>
                                      <resolution>1FT</resolution>
                                      <memory>64MB</memory>
                                    </GPS>
                                  </accessory>
                                </details>'))) = 1;

--- 5.2 Indexing XPath expressions 

--- default index parameters associated with the attribute set 
BEGIN
  --- parameters for non-XMLType attributes
  dbms_expfil.default_index_parameters(
    attr_set  => 'Car4Sale',
    attr_list => exf$attribute_list (
      exf$attribute (attr_name => 'Model',   
                     attr_oper => exf$indexoper('='),
                     attr_indexed => 'TRUE'), 
      exf$attribute (attr_name => 'Price',
                     attr_oper => exf$indexoper('all'),
                     attr_indexed => 'FALSE')
    ));

  --- parameters for XMLType attributes 
  dbms_expfil.default_xpindex_parameters (
    attr_set  => 'Car4Sale',
    xmlt_attr => 'Details', --- XMLType Attribute
    xptag_list => --- Tag list
      exf$xpath_tags(
        exf$xpath_tag(tag_name => 'stereo@make',  --- XML Attribute
                      tag_indexed => 'TRUE',
                      tag_type => 'VARCHAR(15)'), --- value filter
        exf$xpath_tag(tag_name => 'stereo',       --- XML Element
                      tag_indexed => 'FALSE',
                      tag_type => null),          ---  positional filter
        exf$xpath_tag(tag_name => 'memory',       --- XML Element
                      tag_indexed => 'TRUE',
                      tag_type => 'VARCHAR(10)')  --- value filter
      ));
END;
/

select attribute_set_name, attribute, indexed, operator_list, xmltype_attr 
  from user_expfil_def_index_params;

--- create the index using defaults --
CREATE INDEX InterestIndex ON Consumer (Interest) 
  INDEXTYPE IS EXFSYS.EXPFILTER;

select index_name, subexpression, indexed, operator_list, xptag_type,
       xpfilter_type
from user_expfil_predtab_attributes;

DROP INDEX InterestIndex;

--- index paramters assigned to the expression column --
BEGIN
  -- Derive the index parameters from defaults --
  dbms_expfil.index_parameters (
    expr_tab => 'Consumer',
    expr_col => 'Interest',
    attr_list => null,
    operation => 'DEFAULT');

  -- fine-tune the XPath index parameters by adding another Tag --
  dbms_expfil.xpindex_parameters (
    expr_tab => 'Consumer',
    expr_col => 'Interest',
    xmlt_attr => 'Details',
    xptag_list =>
               exf$xpath_tags (
                 exf$xpath_tag(tag_name => 'GPS',
                               tag_indexed => 'TRUE',
                               tag_type => null)),
    operation => 'ADD');
END;
/

select  expset_table, expset_column, attribute, indexed, operator_list,
        xmltype_attr 
  from user_expfil_index_params;

--- create the index using the parameters associated with the expr col. 
CREATE INDEX InterestIndex ON Consumer (Interest) 
  INDEXTYPE IS EXFSYS.EXPFILTER;

select index_name, subexpression, indexed, operator_list, xptag_type,
       xpfilter_type
from user_expfil_predtab_attributes;

SELECT * FROM Consumer 
  WHERE  EVALUATE (Consumer.Interest,
                   'Model => ''Mustang'',
                    Year => 2000,
                    Price => 18000,
                    Mileage => 22000,
                    Details =>
                      sys.XMLType(''<details>
                                      <color>White</color>
                                      <accessory>
                                        <stereo make="Koss">CD</stereo>
                                        <GPS>
                                           <resolution>1FT</resolution>
                                           <memory>64MB</memory>
                                        </GPS>
                                      </accessory>
                                    </details>'')') = 1;

drop table Consumer;
exec dbms_expfil.drop_attribute_set('Car4Sale');
drop type Car4Sale;

--- 6. DML with EVALUATE Operator 

--- create the attribute set 
CREATE OR REPLACE TYPE ITTicket AS OBJECT (
                     Priority NUMBER,
                     Environment VARCHAR2(10),
                     Organization VARCHAR2(10));
/

BEGIN
  dbms_expfil.create_attribute_set(attr_set => 'ITTicket',
                                   from_type => 'Yes');
END;
/

--- create the table storing expressions 
CREATE TABLE ITResource (RId NUMBER,
                         Duties VARCHAR2(100));

BEGIN
dbms_expfil.assign_attribute_set(attr_set => 'ITTicket',
                                 expr_tab => 'ITResource',
                                 expr_col => 'Duties');
END;
/

INSERT INTO ITResource (RId, Duties) VALUES
  (1, 'Priority <= 2 and Environment = ''NT'' and Organization = ''Research''');

INSERT INTO ITResource (RId, Duties) VALUES
  (2, 'Priority = 1 and (Environment = ''UNIX'' or Environment = ''LINUX'')
       and Organization = ''APPS''');

--- create the table for data items 
CREATE TABLE ITProblem (PId NUMBER,
                        Description ITTicket,
                        AssignedTo NUMBER);

insert into ITProblem values (1, ITTicket(2,'NT','Research'), NULL);
insert into ITProblem values (2, ITTicket(1,'UNIX','APPS'), NULL);

--- Update the ITProblem to table to assign the tickets to appropriate 
--- ITResource 
UPDATE ITProblem p SET AssignedTo =
  (SELECT RId FROM ITResource r
   WHERE EVALUATE(r.Duties, p.Description.getVarchar()) = 1 and
         rownum < 2)
WHERE AssignedTo is NULL;

drop table itproblem;
drop table itresource;
exec dbms_expfil.drop_attribute_set('itticket');
drop type itticket;

