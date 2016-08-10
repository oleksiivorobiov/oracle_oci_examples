/* Copyright (c) 2003, 2006, Oracle. All rights reserved.  */
/*
   NAME
     occiscp.cpp - OCCI StatelessConnectionPool with Statement Cache

   DESCRIPTION :
     This program demonstates the creating and using of 
     StatelessConnectionPool in the database with statement caching enabled.
 
     * Create a  StatelessConnectionPool.
     * Caching can be enabled for all the connections in the pool 
       by calling setStmtCacheSize(size) on the pool object with 
       a non-zero value of size.
     * Get a connection from the pool.
     * If necessary, override the cache size for the connection 
       with setStmtCacheSize(size) on the connection.
     * Create a statement with a call to createStatement(sql, tag) 
       or createStatement(sql). If the statement is in the cache, 
       it is returned; if not, a new statement with NULL tag is created 
       for the user and returned.
     * Use the statement to execute SQL commands and obtain results.
     * Return the statement back to the cache by calling 
       terminateStatement(stmt, tag) or terminateStatement(stmt). 
       Alternately, if the user does not want the statement to be cached, 
       he can call setDisableStmtCaching(TRUE) on the statement and 
       then terminateStatement(stmt). This will delete the statement 
       instead of caching it.
     * Release the connection back to the pool.

   NOTE :
     Stateless Connection Pool & Statement Cache are two different 
     features of OCCI and can be used exclusively.
     This program makes use of both the features for demonstration purpose only

   MODIFIED   (MM/DD/YY)
   sudsrini   10/22/06 - Username/Password lower case
   sudsrini   07/23/04 - Copyright Info
   idcqe      03/05/03 - Creation

*/

#include <iostream>
#include <occi.h>
using namespace oracle::occi;
using namespace std;

Environment *env;
int main (void)
{
  env = Environment::createEnvironment (Environment::DEFAULT);
  const string poolUserName = "hr";
  const string poolPassword = "hr";
  const string connectString = "";
  unsigned int maxConn = 5;
  unsigned int minConn = 3;
  unsigned int incrConn = 2;

  cout<<"Demo for StatelessConnectionPool with statement cache"<<endl;
  StatelessConnectionPool *scPool;
  Connection *con1, *con2, *con3;
  Statement *stmt1, *stmt2, *stmt3;
  char *sqlquery1=(char *)
  "SELECT employee_id FROM  hr.employees WHERE employee_id=100";
  char *sqlquery2=(char *)
  "SELECT employee_id FROM  hr.employees WHERE employee_id=101";
  const string tag = "tagA";
  try
  {
    // Create a homogenous connection pool
    scPool = env->createStatelessConnectionPool
             (poolUserName, poolPassword, connectString, maxConn, 
             minConn, incrConn, StatelessConnectionPool::HOMOGENEOUS);

    if (scPool)
      cout<<"SUCCESS - createStatelessConnectionPool"<<endl;
    else
      cout<<"FAILURE - createStatelessConnectionPool"<<endl;

    // Get the connection pool parameters
    cout<<"BusyConn="<<scPool->getBusyConnections()<<endl;
    cout<<"OpenConn="<<scPool->getOpenConnections()<<endl;
    cout<<"MaxConn="<<scPool->getMaxConnections()<<endl;
    cout<<"MinConn="<<scPool->getMinConnections()<<endl;
    cout<<"IncrConn="<<scPool->getIncrConnections()<<endl;

    //Enabling the cache on the pool
    scPool->setStmtCacheSize(5);
    unsigned size = scPool->getStmtCacheSize();
    cout<<"The size of cache for pool is "<< size<<endl;

    // Create a connection from the pool
    con1 = scPool->getConnection();
    if (con1)
      cout<<"SUCCESS - getConnection"<<endl;
    else
      cout<<"FAILURE - getConnection"<<endl;

    //Enabling the cache on the connection
    con1->setStmtCacheSize(2);
    unsigned size1 = con1->getStmtCacheSize();
    cout<<"The size of cache for the connection is "<<size1<<endl;
    
    // Release the connection back to the pool with the specified tag.
    scPool->releaseConnection (con1, tag);

    // Get the connection with tag = "tagA" from the pool.
    con1 = scPool->getConnection (tag);
    if (con1)
      cout<<"SUCCESS - getConnection"<<endl;
    else
      cout<<"FAILURE - getConnection"<< endl;
    stmt1 = con1->createStatement(sqlquery1);
    ResultSet *rset1 = stmt1->executeQuery ();
    cout<<"Retrieving the data"<<endl;
    while (rset1->next())
    {
      cout<<"Emp id: "<<rset1->getInt(1)<<endl;
    }
    stmt1->closeResultSet ( rset1 );
    //Put the statement into the cache
    con1->terminateStatement ( stmt1);
    //Check if the statement is in the cache.
    bool cached = con1->isCached(sqlquery1);
    if (cached)
      cout<<"The statement is in the cache"<<endl;
    else
      cout<<"The statement is not in the cache"<<endl;

    stmt2=con1->createStatement(sqlquery2);
    ResultSet *rset2 = stmt2->executeQuery ();
    cout<<"Retrieving the data"<<endl;
    while (rset2->next())
    {
      cout<<"Emp id: "<<rset2->getInt(1)<<endl;
    }
    stmt2->closeResultSet ( rset2 );
    
   //Put the statement into the cache by tagging it
    con1->terminateStatement(stmt2, tag);
    
    stmt3=con1->createStatement(sqlquery2,tag);
    //Remove the statement from the cache
    cout<<"Now disable caching on the statement"<<endl;
    stmt3->disableCaching();
    con1->terminateStatement (stmt3 );
    
    //Check if the statement is in the cache.
    cached = con1->isCached(sqlquery2, tag);
    if (cached)
      cout<<"The statement is in the cache"<<endl;
    else
      cout<<"The statement is not in the cache"<<endl;
    
    //Release the connection
    scPool->releaseConnection(con1);
   
    //Get one tagged connection from the pool
    con2 = scPool->getAnyTaggedConnection(tag);
    if (con2)
      cout<<"SUCCESS - getAnyTaggedConnection"<<endl;
    else
      cout<<"FAILURE - getAnyTaggedConnection"<<endl;
    
    //Destroy the connection. Do not release it back to the pool
    scPool->terminateConnection (con2);
    
    //Get one more connection from the pool
    con3 = scPool->getConnection();
    if (con3)
      cout<<"SUCCESS - getConnection"<<endl;
    else
      cout<<"FAILURE - getConnection"<<endl;
 
    // Get the connection pool parameters
    cout<<"BusyConn="<<scPool->getBusyConnections()<<endl;
    cout<<"OpenConn="<<scPool->getOpenConnections()<<endl;

    //Release the connection
    scPool->releaseConnection(con3);
 
    // Destroy the connection pool
    env->terminateStatelessConnectionPool (scPool);
   }
   catch(SQLException ex)
   {
     cout<<"Exception thrown "<<endl;
     cout<<"Error number: "<<  ex.getErrorCode()<<endl;
     cout<<ex.getMessage() << endl;
   }
   cout<<"StatelessConnectionPoolDemo with statement cache-done"<<endl;
   Environment::terminateEnvironment (env);

} // end of main ()
