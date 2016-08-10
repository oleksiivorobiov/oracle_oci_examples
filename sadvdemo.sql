rem
rem $Header: sadvdemo.sql 05-sep-00.17:17:16 sramakri Exp $
rem
rem sadvdemo.sql
rem
rem Copyright (c) 1998, 1999,, 2000 Oracle Corporation.  All rights reserved.
rem
rem     NAME
rem      sadvdemo.sql - Demo package used to pretty-print recommendations
rem                     of the Summary Advisor.
rem
rem
rem     DESCRIPTION
rem        The Summary Advisor is a component of the DBMS_OLAP package and
rem        contains the RECOMMEND_MV and RECOMMEND_MV_W function which
rem        recommend materialized views (summaries). The recommendations are
rem        written to a table MVIEW$_RECOMMENDATIONS in the user's schema.
rem        The function PRETTYPRINT_RECOMMENDATIONS in this package presents
rem        the information from that table in a more readable format.
rem
rem
rem    PACKAGE INSTALL NOTES
rem
rem      o Install/load this package in the Oracle USER where you wish
rem        to run the Summary Advisor
rem
rem
rem    USAGE NOTES
rem
rem      o Run RECOMMEND_MV or RECOMMEND_MV_W as appropriate to compute the
rem        the recommendations
rem
rem      o To enable outpiut from the demo package, use the SET SERVEROUTPUT
rem        command as follows:
rem
rem             SET SERVEROUTPUT ON SIZE 1000000
rem
rem     o Execute procedure DEMO_SUMADV.PRETTYPRINT_RECOMMENDATIONS as:
rem
rem           execute DEMO_SUMADV.PRETTYPRINT_RECOMMENDATIONS;
rem
rem
rem    MODIFIED   (MM/DD/YY)
rem    sramakri    09/05/00 - format numbers
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    sramakri    10/15/98 - Summary Advisor pretty-printer script
rem    sramakri    10/15/98 - Created
rem

rem  create tables in user schema; if tables already exist
rem these create's will fail; that is okay

create table mview$_recommendations (
              recommendation_number integer primary key, 
              recommended_action varchar2(6), 
              summary_owner varchar(30), 
              summary_name varchar(30), 
              group_by_columns   varchar2(2000),
              where_clause varchar2(2000), 
              from_clause varchar2(2000), 
              measures_list varchar2(2000),
              storage_in_bytes number,
              pct_performance_gain number, 
              benefit_to_cost_ratio number                 
              );             
       
create table mview$_evaluations (
              summary_owner varchar(30), 
              summary_name varchar(30), 
              rank integer, 
              storage_in_bytes number, 
              frequency number, 
              cumulative_benefit number, 
              benefit_to_cost_ratio number 
              );           


CREATE OR REPLACE PACKAGE demo_sumadv IS

  PROCEDURE prettyprint_recommendations;
  PROCEDURE prettyprint_evaluations;

END demo_sumadv;
/

CREATE OR REPLACE PACKAGE BODY demo_sumadv  AS


  PROCEDURE printbuff(buff in varchar2)
  IS
    para demo_sumadv_wrap.paragraph_tabletype;
    loc_lines integer;
    loc_line_length integer := 76;
    j BINARY_INTEGER;
  BEGIN
    demo_sumadv_wrap.to_paragraph(buff, loc_line_length, para, loc_lines);
    dbms_output.put_line(para(1));
    for j in 2 .. loc_lines loop
      dbms_output.put_line('     ' || para(j));
    end loop;
  END printbuff;


  PROCEDURE print_new_sum_rec(group_by_columns in varchar2,
                             measures_list in varchar2,
                             from_clause in varchar2, where_clause in varchar2)
  IS
    buff VARCHAR2(5000);
  BEGIN
    buff := 'SELECT ' || group_by_columns || ', ' || measures_list;
    printbuff(buff);
    buff := 'FROM ' || from_clause;
    printbuff(buff);
    if where_clause is not null then
        buff := 'WHERE ' || where_clause;
        printbuff(buff);
    end if;
    buff := 'GROUP BY ' || group_by_columns;
    printbuff(buff);
  END print_new_sum_rec;

  PROCEDURE prettyprint_recommendations
  IS
    CURSOR c_cur IS
      SELECT  recommendation_number, recommended_action,
              summary_owner, summary_name, group_by_columns,
              measures_list, from_clause, where_clause,
              storage_in_bytes, pct_performance_gain, benefit_to_cost_ratio
      FROM MVIEW$_RECOMMENDATIONS
      ORDER BY recommendation_number;
  BEGIN
    FOR c_rec IN c_cur LOOP

      dbms_output.put_line(' ');
      dbms_output.put_line(
        'Recommendation Number = ' || c_rec.recommendation_number);

      IF c_rec.summary_name is null then
         dbms_output.put_line('Recommended Action is CREATE new summary:');
      ELSE
         dbms_output.put_line('Recommended Action is ' ||
                            c_rec.recommended_action || ' existing summary ' ||
                            c_rec.summary_owner || '.' || c_rec.summary_name);
      END IF;

      IF c_rec.summary_name is null then
         print_new_sum_rec(c_rec.group_by_columns,  c_rec.measures_list,
                           c_rec.from_clause, c_rec.where_clause);
      END IF;

      IF c_rec.storage_in_bytes is null then
         dbms_output.put_line('Storage in bytes is null');
      ELSE
         dbms_output.put_line(
           'Storage in bytes is ' || to_char(c_rec.storage_in_bytes, '9.99EEEE'));
      END IF;

      IF c_rec.pct_performance_gain is null then
         dbms_output.put_line('Percent performance gain is null');
      ELSE
         dbms_output.put_line(
           'Percent performance gain is ' || 
            to_char(c_rec.pct_performance_gain, '9.99EEEE'));
      END IF;

      IF c_rec.benefit_to_cost_ratio is null then
         dbms_output.put_line('Benefit-to-cost ratio is null');
      ELSE
         dbms_output.put_line(
           'Benefit-to-cost ratio is ' || 
             to_char(c_rec.benefit_to_cost_ratio, '9.99EEEE'));
      END IF;


    END LOOP;
 END prettyprint_recommendations;

 PROCEDURE prettyprint_evaluations
 IS
    CURSOR c_cur IS
      SELECT  summary_owner, summary_name, rank,
              storage_in_bytes, frequency, cumulative_benefit, benefit_to_cost_ratio
      FROM MVIEW$_EVALUATIONS
      ORDER BY rank;
  BEGIN
    FOR c_rec IN c_cur LOOP

      dbms_output.put_line(' ');
      dbms_output.put_line(
        'Rank = ' || c_rec.rank || ' Summmary = ' ||
         c_rec.summary_owner || '.' || c_rec.summary_name);

      dbms_output.put_line(
           'Frequency is ' || c_rec.frequency);

      dbms_output.put_line('Storage_in_bytes = ' ||
            to_char(c_rec.storage_in_bytes, '9.99EEEE'));
      dbms_output.put_line('Cumulative_benefit = ' || 
            to_char(c_rec.cumulative_benefit, '9.99EEEE'));
      dbms_output.put_line('Benefit-to-cost ratio is ' || 
            to_char(c_rec.benefit_to_cost_ratio, '9.99EEEE'));

      dbms_output.put_line(' ');


    END LOOP;
 END prettyprint_evaluations;

END demo_sumadv;

/

CREATE OR REPLACE PACKAGE demo_sumadv_wrap
IS
        TYPE paragraph_tabletype IS TABLE OF VARCHAR2 (80)
                INDEX BY BINARY_INTEGER;

        PROCEDURE to_paragraph
                (text_in IN VARCHAR2,
                 line_length IN INTEGER,
                 paragraph_out IN OUT paragraph_tabletype,
                 num_lines_out IN OUT INTEGER,
                 word_break_at_input IN VARCHAR2 := ' ',
                 line_break_at_in IN VARCHAR2 := NULL);
END demo_sumadv_wrap;
/

create or replace PACKAGE BODY demo_sumadv_wrap
IS
  replace_string VARCHAR2(100) := NULL;

  word_break_at_in constant char := ' ';


  PROCEDURE set_replace_string IS
  BEGIN
    replace_string := RPAD ('@', LENGTH (word_break_at_in), '@');
  END;

  PROCEDURE find_last_delim_loc (line_in IN VARCHAR2, loc_out OUT INTEGER)
  IS
    v_line VARCHAR2(1000) := line_in;
  BEGIN
    IF word_break_at_in IS NOT NULL THEN
      v_line := TRANSLATE (line_in, word_break_at_in, replace_string);
    END IF;
    loc_out := INSTR (v_line, '@', -1);
  END;

  PROCEDURE to_paragraph
        (text_in IN VARCHAR2,
         line_length IN INTEGER,
         paragraph_out IN OUT paragraph_tabletype,
         num_lines_out IN OUT INTEGER,
         word_break_at_input IN VARCHAR2 := ' ',
         line_break_at_in IN VARCHAR2 := NULL)
  IS
        len_text INTEGER := LENGTH (text_in);
        line_start_loc INTEGER := 1;
        line_end_loc INTEGER := 1;
        last_space_loc INTEGER;
        curr_line VARCHAR2(80);
  BEGIN
    set_replace_string;

    IF len_text IS NULL THEN
      num_lines_out := 0;
    ELSE
      num_lines_out := 1;
      LOOP
        EXIT WHEN line_end_loc > len_text;
        line_end_loc := LEAST (line_end_loc + line_length, len_text + 1);

        /* get the next possible line of text */
        curr_line := SUBSTR (text_in || ' ',
                             line_start_loc,  line_length + 1);

        /* find the last space in this section of the line */
        find_last_delim_loc (curr_line, last_space_loc);

        /* When NO spaces exist, use the full current line*/
        /* otherwise, cut the line at the space.       */
        IF last_space_loc > 0 THEN
          line_end_loc := line_start_loc + last_space_loc;
        END IF;

        /* Add this line to the paragraph */
        paragraph_out (num_lines_out) :=
                       substr (text_in,
                               line_start_loc,
                               line_end_loc - line_start_loc);

        num_lines_out := num_lines_out + 1;
        line_start_loc := line_end_loc;
      END LOOP;
      num_lines_out := num_lines_out - 1;
    END IF;
  END to_paragraph;
END demo_sumadv_wrap;
/

