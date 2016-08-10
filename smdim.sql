rem
rem $Header: smdim.sql 13-feb-02.14:20:58 twtong Exp $
rem
rem smdim.sql
rem
rem Copyright (c) 1998, 1999, Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      smdim.sql - describe dimensions
rem
rem    DESCRIPTION
rem      The procedures here describe dimensions in a user's schema.
rem      PRINT_DIM shows information about a specified dimension;
rem      PRINT_ALLDIMS shows information about all the dimensions
rem      accessible to the user.
rem
rem    NOTES
rem      These routines are similar to the Server Manager builtin
rem      "describe" command which displays information about tables
rem      and packages. Hence they should be useful to DBA's using
rem      a command-line interface. For developers, these routines are
rem      useful because they illustrate techniques to navigate the
rem      various catalog views related to dimensions.
rem
rem
rem    MODIFIED   (MM/DD/YY)
rem    twtong      02/13/02 - add attribute_name to print_attributes
rem    jraitto     04/14/00 - fix bugs 1268813, 845372
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    sramakri    10/28/98 - make it JOIN KEY, not "JOINKEY"
rem    sramakri    10/28/98 - cleanup
rem    sramakri    10/28/98 - PL/SQL procedures to "describe" dimensions
rem    sramakri    10/28/98 - Created
rem

CREATE OR REPLACE PACKAGE demo_dim IS

 PROCEDURE print_dim(in_dim_name varchar2);
 PROCEDURE print_alldims;

END demo_dim;
/

CREATE OR REPLACE PACKAGE BODY demo_dim  AS


 PROCEDURE print_attributes(in_owner varchar2, in_dimension_name varchar2)
 IS
   CURSOR c_cur IS
     select attribute_name, level_name, column_name
     from all_dim_attributes
     where
       in_owner = owner and
       in_dimension_name = dimension_name
     order by level_name, column_name;

   child_table_owner varchar2(32);
   child_table_name varchar2(32);

 BEGIN
    FOR c_rec IN c_cur LOOP
      dbms_output.put('    ');
      dbms_output.put('ATTRIBUTE ' || c_rec.attribute_name || 
                      ' LEVEL ' || c_rec.level_name ||
                      ' DETERMINES ');

      select dl.detailobj_owner, dl.detailobj_name
      into child_table_owner, child_table_name
      from all_dim_levels dl
      where in_owner = dl.owner and
            in_dimension_name = dl.dimension_name and
            c_rec.level_name = dl.level_name;

      dbms_output.put_line(child_table_owner ||  '.' || child_table_name
                           || '.' || c_rec.column_name);
    END LOOP;
 END print_attributes;




 PROCEDURE get_joinkey(in_owner varchar2, in_dimension_name varchar2,
                       in_hierarchy_name varchar2, in_jkid integer,
                       jkchildcolms out varchar2,
                       num_jkchildcolms out integer, parent_level out varchar2)
 IS
   CURSOR c_cur IS
       select key_position, child_join_column, level_name
       from all_dim_join_key
       where in_jkid = dim_key_id and
             in_owner = owner and
             in_dimension_name = dimension_name and
             in_hierarchy_name = hierarchy_name
       order by key_position;
 BEGIN
    num_jkchildcolms := 0;
    FOR c_rec IN c_cur LOOP

      num_jkchildcolms := num_jkchildcolms + 1;
      parent_level := c_rec.level_name;

      if num_jkchildcolms > 1
      then
        jkchildcolms := jkchildcolms || ', ' || c_rec.child_join_column;
      else
        jkchildcolms := c_rec.child_join_column;
      end if;

    END LOOP;
 END get_joinkey;



 PROCEDURE print_children(in_owner varchar2, in_dimension_name varchar2,
                        in_hierarchy_name varchar2, children out varchar2,
                        num_children out integer)
 IS
   CURSOR c_cur IS
         select dco.position, dco.child_level_name, dco.parent_level_name
         from all_dim_child_of dco
         where in_owner = dco.owner and
               in_dimension_name = dco.dimension_name and
               in_hierarchy_name = dco.hierarchy_name
         order by dco.position;
 BEGIN
    num_children := 0;
    FOR c_rec IN c_cur LOOP

      num_children := num_children + 1;

      if num_children = 1
      then
        dbms_output.put_line('             ' || c_rec.child_level_name);
      end if;

      children :=  'CHILD OF ' || c_rec.parent_level_name;

      dbms_output.put_line('             ' || children);


    END LOOP;
 END print_children;




 PROCEDURE print_hierarchies(in_owner varchar2, in_dimension_name varchar2)
 IS
   CURSOR c_cur IS
     select hierarchy_name
     from all_dim_hierarchies
     where
       in_owner = owner and
       in_dimension_name = dimension_name
     order by hierarchy_name;

   num_children integer;
   children varchar2(2000);
   num_jkcolms integer;
   jkcolms varchar2(2000);
   in_hierarchy_name varchar2(32);
   child_table_owner varchar2(32);
   child_table_name varchar2(32);
   parent_level varchar2(32);
   CURSOR c_jkcur IS
     select dco.join_key_id, dco.child_level_name
     from all_dim_child_of dco
     where in_owner = dco.owner and
           in_dimension_name = dco.dimension_name and
           in_hierarchy_name = dco.hierarchy_name and
           dco.join_key_id is not null
     order by dco.position;
 BEGIN
    FOR c_rec IN c_cur LOOP

      dbms_output.put('    ');
      dbms_output.put_line('HIERARCHY ' || c_rec.hierarchy_name ||
                      ' ( ');
      print_children(in_owner, in_dimension_name, c_rec.hierarchy_name,
                     children, num_children);
      in_hierarchy_name := c_rec.hierarchy_name;

      for c_jkrec in c_jkcur loop

          select dl.detailobj_owner, dl.detailobj_name
          into child_table_owner, child_table_name
          from all_dim_levels dl
          where in_owner = dl.owner and
                in_dimension_name = dl.dimension_name and
                c_jkrec.child_level_name = dl.level_name;

          get_joinkey(in_owner, in_dimension_name, in_hierarchy_name,
                      c_jkrec.join_key_id,
                      jkcolms, num_jkcolms, parent_level);

         dbms_output.put('      ');
         dbms_output.put('JOIN KEY ');

         if num_jkcolms > 1
         then
             dbms_output.put('(');
         end if;

         dbms_output.put(child_table_owner || '.' || child_table_name
                         || '.' || jkcolms);

         if num_jkcolms > 1
         then
             dbms_output.put(')');
         end if;

         dbms_output.put_line(' REFERENCES ' || parent_level);

      end loop;

      dbms_output.put_line('    )');

    END LOOP;
 END print_hierarchies;





 PROCEDURE get_segments(in_owner varchar2, in_dimension_name varchar2,
                        in_level_name varchar2, segments out varchar2,
                        num_segments out integer)
 IS
   CURSOR c_cur IS
         select dlk.key_position, dlk.column_name
         from all_dim_level_key dlk
         where in_owner = dlk.owner and
               in_dimension_name = dlk.dimension_name and
               in_level_name = level_name
               order by dlk.key_position;
 BEGIN
    num_segments := 0;
    FOR c_rec IN c_cur LOOP

      num_segments := num_segments + 1;

      if num_segments > 1
      then
        segments := segments || ', ' || c_rec.column_name;
      else
        segments := c_rec.column_name;
      end if;

    END LOOP;
 END get_segments;




 PROCEDURE print_levels(in_owner varchar2, in_dimension_name varchar2)
 IS
   CURSOR c_cur IS
     select level_name, detailobj_owner, detailobj_name
     from all_dim_levels
     where
       in_owner = owner and
       in_dimension_name = dimension_name
       order by owner, dimension_name, level_name;
   num_segments integer;
   segments varchar2(2000);
 BEGIN
    FOR c_rec IN c_cur LOOP

      dbms_output.put('    ');
      dbms_output.put('LEVEL ' || c_rec.level_name ||
                      ' IS ');

      get_segments(in_owner, in_dimension_name, c_rec.level_name,
                   segments, num_segments);

      if num_segments > 1
      then
          dbms_output.put('(');
      end if;

      dbms_output.put(c_rec.detailobj_owner || '.' || c_rec.detailobj_name
                         || '.' || segments);

      if num_segments > 1
      then
          dbms_output.put(')');
      end if;

      dbms_output.put_line(' ');

    END LOOP;
 END print_levels;



 PROCEDURE print_dim(in_dim_name varchar2)
 IS
   CURSOR c_cur IS
          select owner, dimension_name
          from all_dimensions
          where owner = user 
            and decode(substr(trim(in_dim_name),1,1),'"',
                       trim('"' from trim(in_dim_name)), 
                       trim(upper(in_dim_name))) 
              = dimension_name
          order by owner, dimension_name;
   counter integer;
 BEGIN
    counter := 0;
    FOR c_rec IN c_cur LOOP

      counter := counter + 1;
      dbms_output.put('  ');
      dbms_output.put_line('DIMENSION ' || c_rec.owner || '.'
                                        || c_rec.dimension_name );

      print_levels( c_rec.owner,  c_rec.dimension_name );
      dbms_output.put_line(' ');
      print_hierarchies( c_rec.owner,  c_rec.dimension_name );
      dbms_output.put_line(' ');
      print_attributes( c_rec.owner,  c_rec.dimension_name );
      dbms_output.put_line(' ');

    END LOOP;

    if counter = 0
    then
       dbms_output.put_line('No such dimension present in schema');
    end if;

 END print_dim;


 PROCEDURE print_alldims
 IS
   CURSOR c_cur IS
          select owner, dimension_name
          from all_dimensions
          where invalid = 'N'
          order by owner, dimension_name;
 BEGIN
    FOR c_rec IN c_cur LOOP

      dbms_output.put('  ');
      dbms_output.put_line('DIMENSION ' || c_rec.owner || '.'
                                        || c_rec.dimension_name );

      print_levels( c_rec.owner,  c_rec.dimension_name );
      dbms_output.put_line(' ');
      print_hierarchies( c_rec.owner,  c_rec.dimension_name );
      dbms_output.put_line(' ');
      print_attributes( c_rec.owner,  c_rec.dimension_name );
      dbms_output.put_line(' ');

    END LOOP;
 END print_alldims;



END demo_dim;

/
