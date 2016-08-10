/* Copyright (c) 2001, 2006, Oracle. All rights reserved.  */
/*
   NAME
     occipobj.cpp - OCCI Objects demo using Navigational Access

   DESCRIPTION
     This demo uses OCCI navigational access for manipulating persistent 
     objects demonstrating pinning, unpinning, marking for flush and delete


   MODIFIED   (MM/DD/YY)
   sudsrini    10/22/06 - Username/Password lower case
   sudsrini    07/23/04 - Copyright Info
   idcqe       03/05/01 - Creation

*/

#include <iostream>
#include "occipobjm.h"

using namespace oracle::occi;
using namespace std;

class occipobj
{
  private:

  Environment *env;
  Connection *conn;
  Statement *stmt;
  string tableName;
  string typeName;

  public:

  occipobj (string user, string passwd, string db)
  { 
    env = Environment::createEnvironment (Environment::OBJECT);
    occipobjm (env);
    conn = env->createConnection (user, passwd, db);
  }

  ~occipobj ()
  {
    env->terminateConnection (conn);
    Environment::terminateEnvironment (env);
  }

  void setTableName (string s)
  {
    tableName = s;
  }

  /**
   * Insertion of a row 
   */
  void insertRow (int a1, string a2)
  {
    cout << "Inserting row ADDRESS (" << a1 << ", " << a2 << ")" << endl;
    Number n1(a1);
    address *o = new (conn, tableName) address();
    o->setStreet_no(n1);
    o->setCity(a2);
    conn->commit ();
    cout << "Insertion - Successful" << endl;
  }

  /**
   * updating a row
   */
  void updateRow (int b1, int a1, string a2)
  {
    cout << "Updating a row with attribute a1 = " << b1 << endl;
    stmt = conn->createStatement
      ("SELECT REF(a) FROM publ_address_tab a WHERE street_no = :x FOR UPDATE");
    stmt->setInt (1, b1);
    ResultSet *rs = stmt->executeQuery ();
    try{
    if ( rs->next() )
    {
      RefAny rany = rs->getRef (1);
      Ref <address > r1(rany);
      address *o = r1.ptr();
      o->markModified ();
      o->setStreet_no (Number (a1));
      o->setCity (a2);
      o->flush ();
    }
    }catch(SQLException ex)
    {
     cout<<"Exception thrown updateRow"<<endl;
     cout<<"Error number: "<<  ex.getErrorCode() << endl;
     cout<<ex.getMessage() << endl;
    }

    conn->commit ();
    stmt->closeResultSet(rs);
    conn->terminateStatement (stmt);
    cout << "Updation - Successful" << endl;
  }


  /**
   * deletion of a row
   */
  void deleteRow (int a1, string a2)
  {
    cout << "Deleting a row with object ADDRESS (" << a1 << ", " << a2 
     << ")" << endl;
    stmt = conn->createStatement
    ("SELECT REF(a) FROM publ_address_tab a WHERE street_no = :x AND city = :y FOR UPDATE");
    stmt->setInt (1, a1);
    stmt->setString (2, a2);
    ResultSet *rs = stmt->executeQuery ();
    try{
    if ( rs->next() )
    {
      RefAny rany = rs->getRef (1);
      Ref<address > r1(rany);
      address *o = r1.ptr();
      o->markDelete ();
    }
    }catch(SQLException ex)
    {
     cout<<"Exception thrown for deleteRow"<<endl;
     cout<<"Error number: "<<  ex.getErrorCode() << endl;
     cout<<ex.getMessage() << endl;
    }

    conn->commit ();
    stmt->closeResultSet(rs);
    conn->terminateStatement (stmt);
    cout << "Deletion - Successful" << endl;
  }

  /**
   * displaying all the rows in the table
   */
  void displayAllRows ()
  {
    string sqlStmt = "SELECT REF (a) FROM publ_address_tab a \
                      ORDER BY street_no";
    stmt = conn->createStatement (sqlStmt);
    ResultSet *rset = stmt->executeQuery ();
    try{
    while (rset->next ())
    {
      RefAny rany = rset->getRef (1); 
      Ref<address > r1(rany);
      address *o = r1.ptr();
      cout << "ADDRESS(" << (int)o->getStreet_no () << ", " << o->getCity () << ")" << endl;
    }
    }catch(SQLException ex)
    {
     cout<<"Exception thrown for displayAllRows"<<endl;
     cout<<"Error number: "<<  ex.getErrorCode() << endl;
     cout<<ex.getMessage() << endl;
    }

    stmt->closeResultSet (rset);
    conn->terminateStatement (stmt);
  }

}; // end of class occipobj


int main (void)
{
  string user = "hr";
  string passwd = "hr";
  string db = "";

  try
  {
    cout << "occipobj - Exhibiting simple insert, delete & update operations" 
     " on persistent objects" << endl;
    occipobj *demo = new occipobj (user, passwd, db);

    cout << "Displaying all rows before the opeations" << endl;
    demo->displayAllRows ();

    demo->setTableName ("PUBL_ADDRESS_TAB");
 
    demo->insertRow (21, "KRISHNA");

    demo->deleteRow (22, "BOSTON");
  
    demo->updateRow (33, 123, "BHUMI");
  
    cout << "Displaying all rows after all the operations" << endl;
    demo->displayAllRows ();

    delete (demo);
    cout << "occipobj - done" << endl;
  }catch (SQLException ea)
  {
    cerr << "Error running the demo: " << ea.getMessage () << endl;
  }
}
