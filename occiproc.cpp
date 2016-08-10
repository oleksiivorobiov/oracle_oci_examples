/* Copyright (c) 2001, 2008, Oracle. All rights reserved.  */
/*
   NAME
     occiproc.cpp - PL/SQL function or procedure call from OCCI

   DESCRIPTION
     This program demonstrates invoking a PL/SQL function and procedure
     having IN, IN/OUT and OUT parameters using OCCI interface

   MODIFIED   (MM/DD/YY)
   mvasudev   05/22/08 - Add try/catch blocks
   sudsrini   10/22/06 - Username/Password lower case
   sudsrini   07/23/04 - Copyright Info
   idcqe      03/05/01 - Creation

*/

#include <iostream>
#include <occi.h>
using namespace oracle::occi;
using namespace std;

class occiproc 
{
  private:

  Environment *env;
  Connection *con;

  public :
  /**
   * Constructor for the occiproc demo program.
   */
  occiproc (string user, string passwd, string db) throw (SQLException)
  {
    env = Environment::createEnvironment (Environment::DEFAULT);
    con = env->createConnection (user, passwd, db);
  }// end of constructor occiproc (string, string, string )
  
  /**
   * Destructor for the occiproc demo program.
   */
  ~occiproc () throw (SQLException)
  {
    env->terminateConnection (con);
    Environment::terminateEnvironment (env);
  }  // end of ~occiproc ()

  // Function to call a PL/SQL procedure
  void callproc ()
  {
    cout << "callproc - invoking a PL/SQL procedure having IN, OUT and IN/OUT ";
    cout << "parameters" << endl;
    Statement *stmt = con->createStatement 
      ("BEGIN demo_proc(:v1, :v2, :v3); END;");
    try{
    cout << "Executing the block :" << stmt->getSQL() << endl;
    stmt->setInt (1, 10);
    stmt->setMaxParamSize (2, 30);
    stmt->setString (2, "IN");
    stmt->registerOutParam (3, OCCISTRING, 30, "");
    int updateCount = stmt->executeUpdate ();
    cout << "Update Count:" << updateCount << endl;
    string c1 = stmt->getString (2);
    string c2 = stmt->getString (3);
    cout << "Printing the INOUT & OUT parameters:" << endl;
    cout << "Col2: " << c1 << endl;
    cout << "Col3: " << c2 << endl;
    con->terminateStatement (stmt);
    cout << "occiproc - done" << endl;
    }
    catch (SQLException ex){
         cout << ex.getMessage() << endl;
    }
  } // end of callproc ()

  // Function to call a PL/SQL function
  void callfun ()
  { cout << "callfun - invoking a PL/SQL function having IN, OUT and IN/OUT ";
    cout << "parameters" << endl;
    Statement *stmt = con->createStatement 
      ("BEGIN :a := demo_fun(:v1, :v2, :v3); END;");
    try{
       cout << "Executing the block :" << stmt->getSQL() << endl;
       stmt->setInt (2, 10);
       stmt->setMaxParamSize (3, 30);
       stmt->setString (3, "IN");
       stmt->registerOutParam (1, OCCISTRING, 30, "");
       stmt->registerOutParam (4, OCCISTRING, 30, "");
       int updateCount = stmt->executeUpdate ();
    cout << "Update Count : " << updateCount << endl;

    string c1 = stmt->getString (1);
    string c2 = stmt->getString (3);
    string c3 = stmt->getString (4);

    cout << "Printing the INOUT & OUT parameters :" << endl;
    cout << "Col2: " << c2 << endl;
    cout << "Col3: " << c3 << endl;
    cout << "Printing the return value of the function: ";
    cout << c1 << endl;

    con->terminateStatement (stmt);
    cout << "occifun - done" << endl;
    }
    catch (SQLException ex){
        cout << ex.getMessage() << endl;
    }
  } // end of callfun ()
}; // end of class occiproc

int main (void)
{
  string user = "hr";
  string passwd = "hr";
  string db = "";

  cout << "occiproc - invoking a PL/SQL function and procedure having ";
  cout << "parameters" << endl;
  try{ 
      occiproc *demo = new occiproc (user, passwd, db); 
  demo->callproc();
  demo->callfun();
  delete demo;
  }
  catch (SQLException ex){
      cout << ex.getMessage() << endl;
   }
 
}// end of main ()
