/* Copyright (c) 2003, 2009, Oracle and/or its affiliates. 
All rights reserved. */
/*
  NAME
    occixa.cpp: OCCI XA interface demo

  DESCRIPTION
    This program creates an XA Environment, gets an XA Connection from the
    created environment, uses the XA connection to do DMLs on a table.

    The key APIs that are used are:
    - getXAEnviroment
    - getXAConnection
    - getXAErrorCode
    - releaseXAConnection
    - releaseXAEnvironment

    For more more information, please refer Oracle documentaion.

   MODIFIED   (MM/DD/YY)
   sudsrini    07/08/09 - Include string.h, iostream not including it on sles11
   sudsrini    10/22/06 - Username/Password lower case
   sudsrini    07/23/04 - Copyright Info
   shiyer      03/05/03 - Creation

*/

#include <iostream>
#include <string.h>
#include <occi.h>
#include <xa.h>       // Should be included for all OCCI XA interface

using namespace oracle::occi;
using namespace std;

#ifdef WIN32COMMON
  extern "C" __declspec(dllimport)  struct xa_switch_t xaosw;
  extern "C" __declspec(dllimport)  struct xa_switch_t xaoswd;
#else
  extern struct xa_switch_t xaosw;
  extern struct xa_switch_t xaoswd;
#endif

static  XID xidx = { 0x1e0a0a1e, 12, 8, "KeesGTID01Branch0001" };
static  XID *xid = &xidx;

class occixa 
{

private :
  struct xa_switch_t *xafunc ;
  Environment *xaenvrnmnt;
  Connection *xaConnection;
  
public :

occixa ()
{
}  

~occixa ()
{
}  

void test (Connection *conn)
{
  Statement *stmt;

  showln("Invoking createStatement using XA Connection");
  stmt = conn->createStatement
    ("INSERT INTO EMPLOYEES (employee_id, last_name, email,hire_date,job_id) \
    values (998, 'Mike', 'x1@y.com','12-Mar-1970','AC_ACCOUNT')");

  showln("Inserting Records into Local Database");
  stmt->executeUpdate();

  showln("Selecting Records From Local Database using XA Connection");
  displayQueryResult ( conn,
  "SELECT Employee_id,last_name FROM EMPLOYEES where employee_id \
   between 998 and 999 ORDER BY 1,2" );

  showln("Deleting the inserted  Records from Database");
  stmt = conn->createStatement
    ("DELETE EMPLOYEES where employee_id between 998 and 999");

  stmt->executeUpdate();

  showln("Terminating the Statements using XA Connection ");
  conn->terminateStatement (stmt);

}

// This function establishes an XA  connection to the resource manager
void startXATest (string uname, string pwd, string db , XID *xid )
{
  
  int xarslt = 0;     // xa result code 
  int rmid = 1;
  char *xaoinfo;
  this->xafunc   = &xaoswd;

  //Forming the xaoinfo1 string which has to be sent as a parameter for
  //xa_open_entry function call.

  string xaoinfo1 = "oracle_xa+ACC=P/";
  xaoinfo1 += uname;
  xaoinfo1 +="/";
  xaoinfo1 += pwd;
  
  //For an Object Test this parameter OBJECTS=T should also be given to the 
  //xaoinfo string ( a parameter of the xa_open_entry function call)

  #ifdef OBJECTS 
    xaoinfo1 +="+OBJECTS=T";
  #endif

  //To open an entry for XA with a remote database, DB="database_name"  
  //should also be given to the xaoinfo string ( a parameter of the 
  //xa_open_entry function call)

  if (!db.empty())
    xaoinfo1 += "+SESTM=50+logdir=.+DB="+db;
  else
    xaoinfo1 += "+SESTM=50+logdir=.";
  xaoinfo=(char *)xaoinfo1.c_str();
  
  // Connects to the resource manager 
  xarslt = xafunc->xa_open_entry(xaoinfo, rmid, TMNOFLAGS);
  
  if ( xarslt )
    cout << "xaoopen = " << xarslt << endl;
  
  XID *t = xid;
  int i;
  for (i=0;i<64;i++)
    t->data[i] = '\0';
  t->formatID = 1;
  t->gtrid_length = 8;
  for (i=0;i<t->gtrid_length;i++)
    t->data[i] = 1;
  t->bqual_length = 4;
  for (i=0;i<t->bqual_length;i++)
    t->data[i+t->gtrid_length] = 1;
  xid=t;
  
  // Starts a new transaction and associates it with the given transaction ID (XID)
  xarslt = xafunc->xa_start_entry(xid, rmid, TMNOFLAGS);

  if ( xarslt )
    cout << "xaostart = " << xarslt << endl;
  
  try 
  {
    // This method returns a pointer to OCCI Environment Object that corresponds 
    // to the one opened by the XA Library
    // db represents the database name in which the XA Environment is created

    xaenvrnmnt = Environment::getXAEnvironment(db);
  }
  catch(SQLException &ex)
  {
    cout << ex.what() << endl;

    //This method returns an XA error code when a XA exception is encountered.
    //It returns a XA_OK if the error is not due to XA.It takes db as paramter.

    int error = ex.getXAErrorCode(db);
    if(error==XA_OK)
      cout << "Error in SQL processing" << endl;
    else
      cout << "XA Error Code:" << error << endl;
  }

  try
  {
    // This method returns a pointer to an OCCI Connection object that corresponds 
    // to the one opened by the XA Library.
    //db represents the database name to which the XA Connection is got

    xaConnection = xaenvrnmnt->getXAConnection(db);
  }
  catch(SQLException &ex)
  {
    cout << ex.what() << endl;
    int error = ex.getXAErrorCode(db);
    if(error==XA_OK)
      cout << "Error in SQL processing" << endl;
    else
      cout << "XA Error Code:" << error << endl;
  }

  try
  {
    test ( xaConnection );
  }
  catch (SQLException ea)
  {
    showError (ea);
  } 
  
  try
  {
    //This method releases/de-allocates the neccessary handles allocated by the 
    //getXAConnection call. It takes XA Connection as the parameter

    xaenvrnmnt->releaseXAConnection (xaConnection);
  }
  catch(SQLException &ex)
  {
    cout << ex.what() << endl;
    int error = ex.getXAErrorCode(db);
    if(error==XA_OK)
      cout << "Error in SQL processing" << endl;
    else
      cout << "XA Error Code:" << error << endl;
  }
  
  // Disassociates the process from the given XID
  xarslt = xafunc->xa_end_entry(xid, rmid, TMSUCCESS);
  if ( xarslt )
    cout << "xaoend = " << xarslt << endl;
 
  // Prepares the transaction associated with the given XID. First phase of
  // two-phase commit protocol.
  xarslt = xafunc->xa_prepare_entry(xid, rmid, TMNOFLAGS);
  if ( xarslt )
    cout << "xaoprepare = " << xarslt << endl;

  // Commits the transaction associated with the given XID. Second phase of
  // two-phase commit protocol.
  xarslt = xafunc->xa_commit_entry(xid, rmid, TMNOFLAGS);
  if ( xarslt )
    cout << "xaocommit = " << xarslt << endl;
  
  try
  {
    //This call releases the neccessary handles allocated by the 
    //getXAEnvironment call. It takes XA Environment as the parameter

    Environment::releaseXAEnvironment (xaenvrnmnt);
  }
  catch(SQLException &ex)
  {
    cout << ex.what() << endl;
    int error = ex.getXAErrorCode(db);
    if(error==XA_OK)
      cout << "Error in SQL processing" << endl;
    else
      cout << "XA Error Code:" << error << endl;
  }
  
  // Disconnects from the resource manager.
  xarslt = xafunc->xa_close_entry(xaoinfo, rmid, TMNOFLAGS);
  if ( xarslt )
  {
    cout << "xaoclose = " << xarslt << endl;
  }
  
} 

dvoid displayQueryResult (Connection* conn, string sqlQuery)
{
  Statement *stmt = conn->createStatement (sqlQuery);
  int i=0;
  try
  {
    ResultSet *rs = stmt->executeQuery ();
    while (rs->next ())
    {
    int a = rs->getInt (1);
    cout  << "Col 1" << ": " << a << endl;
    string ch = rs->getString (2);
    cout  << "Col 2" << ": " << ch << endl;
  
    }
    stmt->closeResultSet (rs);
  }
  catch ( SQLException e )
  {
    showError ( e );
    conn->terminateStatement (stmt);
    throw;
  }

  conn->terminateStatement (stmt);
}

dvoid showError (SQLException ex)
{
  showln ("Error number: ") <<  ex.getErrorCode() << endl;
  showln (ex.getMessage()) << endl;
}


ostream& showln (string str)
{
  return cout << str << endl;
}
};

int main ( int argc, char* argv[])
{
  string userName  = "hr";
  string password  = "hr";
  string database  ="";

  occixa* h = new occixa();
   
  // Gets an XA Connection to the default database, uses the XA Connection
  // to Insert/Select from a table with/without database link

  try 
  {
    h->startXATest (userName, password, database,xid);
  }
  catch (SQLException ea) 
  {
    h->showError (ea);
  }

}
