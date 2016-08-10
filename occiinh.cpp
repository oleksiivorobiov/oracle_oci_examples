/* Copyright (c) 2001, 2006, Oracle. All rights reserved.  */
/*
   NAME
     occiinh.cpp - OCCI Object Inheritance demo

   DESCRIPTION
     This demo performs all DML operations using OCCI interface
     on the objects
     Object Hierarchy
     people_typ <---- student <----- parttime_stud <----- foreign_student 

   MODIFIED   (MM/DD/YY)
   sudsrini   10/22/06 - Username/Password lower case
   sudsrini   07/23/04 - Copyright Info
   idcqe      03/05/01 - Creation

*/

#include <iostream>
#include <occi.h>
using namespace oracle::occi;
using namespace std;
 
#include "occiinhm.h"

/* Add on your methods in this class*/ 
class foreign_student_obj : public foreign_student
{
   /* New methods can be added here */
};

class occiinh
{
  private:

  Environment *env;
  Connection *con;

  // This method will return the Ref  
  RefAny getRefObj(string sqlString)
  {
    Statement *stmt = con->createStatement (sqlString);
    ResultSet *rs;
    try
    {
       rs = stmt->executeQuery ();
       if ( rs->next() )
       {
          RefAny ref1 = rs->getRef (1);
          stmt->closeResultSet (rs);
          con->terminateStatement (stmt);
          return ref1;
       }
    }
    catch(SQLException ex)
    {
      cout << "Error in fetching ref" << endl;
    }
    stmt->closeResultSet (rs);
    con->terminateStatement (stmt);
    return RefAny();
  }


  public:

  occiinh (string user, string passwd, string db)
   throw (SQLException)
  {
    env = Environment::createEnvironment (Environment::OBJECT);
    occiinhm(env);
    con = env->createConnection (user, passwd, db);
  }// end of constructor occiinh (string, string, string)

  ~occiinh ()
   throw (SQLException)
  {
    env->terminateConnection (con);
    Environment::terminateEnvironment (env);
  }// end of destructor

  /**
   * Insertion of a row 
   */
  void insertRow ()
   throw (SQLException)
  {
    cout << "Inserting a record (joe)" << endl;
    string sqlStmt = 
    "INSERT INTO foreign_student_tab VALUES(:a)";
    Statement *stmt = con->createStatement (sqlStmt);
    string fs_name = "joe";
    Number fs_ssn (4);
    Date fs_dob(env, 2000, 5, 11, 16, 05, 0);
    Number fs_stud_id (400);
    Ref< people_typ > fs_teammate = getRefObj(
       "SELECT REF(a) FROM people_tab a where name='john'");
    Number fs_course_id(4000);
    Ref< student > fs_partner = getRefObj(
      "SELECT REF(a) FROM student_tab a"); 
    string fs_country = "india"; 
    Ref< parttime_stud > fs_leader = getRefObj(
      "SELECT REF(a) FROM parttime_stud_tab a");
    foreign_student_obj *fs_obj = new foreign_student_obj ();
    fs_obj->setname(fs_name);
    fs_obj->setssn(fs_ssn);
    fs_obj->setdob(fs_dob);
    fs_obj->setstud_id(fs_stud_id);
    fs_obj->setteammate(fs_teammate);
    fs_obj->setcourse_id(fs_course_id);
    fs_obj->setpartner(fs_partner);
    fs_obj->setcountry(fs_country);
    fs_obj->setleader(fs_leader);
    stmt->setObject(1, fs_obj);
    stmt->executeUpdate();

    con->terminateStatement (stmt);
    delete fs_obj;
    cout << "Insertion Successful" << endl;
  }// end of insertRow ();


  /**
   * updating a row
   */
  void updateRow ()
   throw (SQLException)
  {
    cout << "Upadating record (Changing name,teammate and course_id)" << endl;
    string sqlStmt = 
     "UPDATE foreign_student_tab SET name=:x, teammate=:y, course_id=:z";
    Statement *stmt = con->createStatement (sqlStmt);
    string fs_name = "jeffree";
    Ref< people_typ > fs_teammate = getRefObj(
       "SELECT REF(a) FROM people_tab a where name='jill'");
    Number fs_course_id(5000);
    stmt->setString(1, fs_name);
    stmt->setRef(2,fs_teammate);
    stmt->setInt(3, fs_course_id);
    stmt->executeUpdate ();
    con->commit();
    con->terminateStatement (stmt);
    cout << "Updation Successful" << endl;
  }// end of updateRow (int, string);


  /**
   * deletion of a row
   */
  void deleteRow ()
   throw (SQLException)
  {
    cout << "Deletion of jeffree record " << endl;
    string sqlStmt = "DELETE FROM foreign_student_tab where name=:x";
    Statement *stmt = con->createStatement (sqlStmt);
    string fs_name = "jeffree";
    stmt->setString(1,fs_name);
    stmt->executeUpdate();
    con->commit();
    con->terminateStatement (stmt);
    cout << "Deletion Successful" << endl;
  }// end of deleteRow (int, string);

  /**
   * displaying all the rows in the table
   */
  void displayAllRows ()
   throw (SQLException)
  {
    int count=0;
    string sqlStmt = "SELECT REF(a) FROM foreign_student_tab a";
    Statement *stmt = con->createStatement (sqlStmt);
    ResultSet *resultSet = stmt->executeQuery ();

    while (resultSet->next ())
    {
       count++;
       RefAny fs_refany = resultSet->getRef(1);
       Ref <foreign_student_obj> fs_ref(fs_refany);
       fs_ref.setPrefetch(4);
       string fmt = "DD-MON-YYYY";
       string nlsParam = "NLS_DATE_LANGUAGE = American";
       Date fs_dob = fs_ref->getdob();
       string date1 = fs_dob.toText (fmt, nlsParam);
       cout << "Foreign Student Information" << endl;
       cout << "Name      : " << fs_ref->getname();
       cout << "  SSN       : " << (int)fs_ref->getssn();
       cout << "  DOB       : " << date1 << endl;
       cout << "Stud id   : " << (int)fs_ref->getstud_id() ;
       cout << "  Course id : " << (int)fs_ref->getcourse_id();
       cout << "  Country   : " << fs_ref->getcountry() <<endl;
       Ref <people_typ> fs_teammate = (Ref <people_typ>)
         fs_ref->getteammate();
       cout << "Teammate's Information " << endl;
       cout << "Name      : " << fs_teammate->getname();
       cout << "  SSN       : " << (int)fs_teammate->getssn();
       fs_dob = fs_teammate->getdob();
       date1 = fs_dob.toText(fmt, nlsParam);
       cout << "  DOB       : " << date1 << endl << endl; 
       /* Leader */
       Ref< parttime_stud > fs_leader = (Ref < parttime_stud >)
          fs_ref->getleader();
       /* Leader's Partner */ 
       Ref < student > fs_partner = (Ref <student> )
          fs_leader->getpartner();
      /* Leader's Partenr's teammate */ 
        fs_teammate = (Ref <people_typ>) fs_partner->getteammate();

       cout << "Leader Information " << endl;
       cout << "Name      : " << fs_leader->getname();  
       cout << "  SSN       : " << (int)fs_leader->getssn();
       fs_dob = fs_leader->getdob();
       date1 = fs_dob.toText(fmt, nlsParam);
       cout << "  DOB       : " << date1 << endl;
       cout << "Stud id   : " << (int)fs_leader->getstud_id();
       cout << "  Course id : " << (int)fs_leader->getcourse_id() << endl;

       cout << "Leader's Partner's Information " << endl;
       cout << "Name      : " << fs_partner->getname() ;  
       cout << "  SSN       : " << (int)fs_partner->getssn();
       fs_dob = fs_partner->getdob();
       date1 = fs_dob.toText(fmt, nlsParam);
       cout << "  DOB       : " << date1 ;
       cout << "  Stud id   : " << (int)fs_partner->getstud_id() << endl;


       cout << "Leader's Partner's Teammate's Information " << endl;
       cout << "Name      : " << fs_teammate->getname();  
       cout << "  SSN       : " << (int)fs_teammate->getssn();
       fs_dob = fs_teammate->getdob();
       date1 = fs_dob.toText(fmt, nlsParam);
       cout << "  DOB       : " << date1 << endl << endl;

    }//end of while (resultSet->next ());
    if (count <=0)
       cout << "No record found " << endl;
    stmt->closeResultSet (resultSet);
    con->terminateStatement (stmt);
  }// end of updateRow (string);

}; // end of class occiinh


int main (void)
{
  string user = "hr";
  string passwd = "hr";
  string db = "";

  try
  {
    cout << "occiinh - Exhibiting simple insert, delete & update operations" 
     " on Oracle objects" << endl;
    occiinh *demo = new occiinh (user, passwd, db);

    cout << "displaying all rows before operations" << endl;
    demo->displayAllRows ();
  
    demo->insertRow ();
    cout << "displaying all rows after insertions" << endl;
    demo->displayAllRows ();

    demo->updateRow ();
    cout << "displaying all rows after updations" << endl;
    demo->displayAllRows ();
  
    demo->deleteRow ();
    cout << "displaying all rows after deletions" << endl;
    demo->displayAllRows ();
  
  
    delete (demo);
    cout << "occiinh - done" << endl;
  }catch (SQLException ea)
  {
    cerr << "Error running the demo: " << ea.getMessage () << endl;
  }
}// end of int main (void);
