/* Copyright (c) 2001, 2008, Oracle. All rights reserved.  */
/*
   NAME
     occistre.cpp - OCCI Streams Interface demo

   DESCRIPTION
     This demo program selects data from VARCHAR2 datatype using 
     OCCI streams interface

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

class occistrm
{
  private:

  Environment *env;
  Connection *conn;

  public:

  occistrm (string user, string passwd, string db)
   throw (SQLException)
  {
    env = Environment::createEnvironment (Environment::DEFAULT);
    conn = env->createConnection (user, passwd, db);
  }// end of constructor occistrm (string, string, string)

  ~occistrm ()
   throw (SQLException)
  {
    env->terminateConnection (conn);
    Environment::terminateEnvironment (env);
  }// end of destructor

  /**
   * displaying all the rows in the table
   */
  void displayAllRows ()
  {
    Statement *stmt = conn->createStatement (
      "SELECT summary FROM book WHERE bookid = 11");
    stmt->execute ();
    ResultSet *rs = stmt->getResultSet ();
    rs->setCharacterStreamMode(1, 4000);
    char buffer[500];
    int length = 0; 
    unsigned int size = 500;
    try{
    	while (rs->next ())
    	{
      	Stream *stream = rs->getStream (1);
      	while( (length=stream->readBuffer(buffer, size))!=-1)
      	{
          cout << "Read " << length << " bytes from stream" << endl;
      	}
      	rs->closeStream(stream);
    	}
      }
    catch (SQLException ex){
       cout << ex.getMessage() << endl;
    }
    stmt->closeResultSet (rs);
    conn->terminateStatement (stmt);
  }// end of updateRow (string);

}; // end of class occistrm


int main (void)
{
  string user = "hr";
  string passwd = "hr";
  string db = "";
 try{
     cout << "occistrm - Exhibiting usage of streams for VARCHAR2 data" 
     << endl;
     occistrm *demo = new occistrm (user, passwd, db);

  demo->displayAllRows ();

  delete (demo);
  }
  catch (SQLException ex){
      cout << ex.getMessage() << endl;
   }
  cout << "occistrm - done" << endl;
}// end of int main (void);
