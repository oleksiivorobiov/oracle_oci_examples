/* Copyright (c) 2001, 2008, Oracle. All rights reserved.  */
/*
   NAME
     occipool.cpp - OCCI Connection Pool Interface demo

   DESCRIPTION :
     This program demonstates creating and using of connection pool

     Make sure setup file, occidemo.sql is run prior to running this program.

   MODIFIED   (MM/DD/YY)
   mvasudev    05/23/08 - Add try/catch blocks
   sudsrini    10/22/06 - Username/Password lower case
   sudsrini    07/23/04 - Copyright Info
   idcqe       03/05/01 - Creation

*/

#include <iostream>
#include <occi.h>
using namespace oracle::occi;
using namespace std;

class occipool 
{
  private:

  Environment *env;
  Connection *con;
  Statement *stmt;
  public :
  /**
   * Constructor for the occipool test case.
   */
  occipool () 
  {
    env = Environment::createEnvironment (Environment::DEFAULT);
  }// end of constructor occipool ()
  
  /**
   * Destructor for the occipool test case.
   */
  ~occipool () 
  {
    Environment::terminateEnvironment (env);
  }  // end of ~occipool ()

  /**
   * The testing logic of the test case.
   */
  dvoid select ()
  {
    cout << "occipool - Selecting records using ConnectionPool interface" <<
    endl;
    const string poolUserName = "hr";
    const string poolPassword = "hr";
    const string connectString = "";
    const string username = "hr";
    const string passWord = "hr";
    unsigned int maxConn = 5;
    unsigned int minConn = 3;
    unsigned int incrConn = 2;
    ConnectionPool *connPool;
    try{
    connPool = env->createConnectionPool
      (poolUserName, poolPassword, connectString, minConn, maxConn, incrConn);
    if (connPool)
      cout << "SUCCESS - createConnectionPool" << endl;
    else
      cout << "FAILURE - createConnectionPool" << endl;
    con = connPool->createConnection (username, passWord);
    if (con)
      cout << "SUCCESS - createConnection" << endl;
    else
      cout << "FAILURE - createConnection" << endl;
    }catch(SQLException ex)
    {
     cout<<"Exception thrown for createConnectionPool"<<endl;
     cout<<"Error number: "<<  ex.getErrorCode() << endl;
     cout<<ex.getMessage() << endl;
     return;
    }

    cout << "retrieving the data" << endl;

    try{
      stmt = con->createStatement 
        ("SELECT author_id, author_name FROM author_tab");
      ResultSet *rset = stmt->executeQuery();
      while (rset->next())
      {
        cout << "author_id:" << rset->getInt (1) << endl;
        cout << "author_name:" << rset->getString (2) << endl;
      }
      stmt->closeResultSet (rset);
      con->terminateStatement (stmt);
      connPool->terminateConnection (con);
      env->terminateConnectionPool (connPool);
    }catch(SQLException ex)
    {
      cout<<"Exception thrown for retrieving data"<<endl;
      cout<<"Error number: "<<  ex.getErrorCode() << endl;
      cout<<ex.getMessage() << endl;
    }

    cout << "occipool - done" << endl;
  } // end of test (Connection *)

}; // end of class occipool

int main (void)
{
  string user = "hr";
  string passwd = "hr";
  string db = "";

  cout << "occipool - Exhibiting interoperability of  and OCI" << endl;
  try{
  occipool *demo = new occipool ();

  demo->select();
  delete demo;
  }
  catch(SQLException ex)
  {
      cout<<ex.getMessage() << endl;
  }

}// end of main ()
