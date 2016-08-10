/* Copyright (c) 2001, 2009, Oracle and/or its affiliates. 
All rights reserved. */
/*
   NAME
     occiclob.cpp - OCCI CLOB Interface demo

   DESCRIPTION
     This demo program explains the way of using CLOB interface.   
     This program inserts an empty clob into the table,selects the 
     clob from table, modifies it and stores it back into the table.
     The Clob content will be printed finally.
     This demo uses write , writeChunk and writeBuffer methods to 
     modify the clob contents, and also uses read and readBuffer
     methods for reading.

   MODIFIED   (MM/DD/YY)
   sudsrini    07/08/09 - Include string.h, iostream not including it on sles11
   sudsrini    10/22/06 - Username/Password lower case
   sudsrini    07/22/04 - Copyright Info
   idcqe       03/05/01 - Creation

*/

#include <iostream> 
#include <fstream> 
#include <string.h> 
#include <occi.h> 
using namespace oracle::occi;
using namespace std;

/**
 * The demo sample has starts from startDemo method.  This method is called 
 * by main.  startDemo calls other methods, the supporting methods for 
 * startDemo are,
 * insertRows    - insert the rows into the table
 * dataRollBack  - deletes the inserted rows
 * populateClob  - populates a given clob
 * dumpClob      - prints the clob as an integer stream
 */
// These will tell the type of read/write we use
#define USE_NORM 1
#define USE_CHUN 2
#define USE_BUFF 3

/* Buffer Size */
#define BUFSIZE 200;

class  occiClob
{
  public:  
  string username;
  string password;
  string url;

  void insertRows (Connection *conn)
   throw (SQLException)
  {
    Statement *stmt = conn->createStatement ("INSERT INTO electronic_media(product_id,ad_id,ad_composite,ad_sourcetext) VALUES (:v1,:v2,:v3,:v4)");
    Blob blob(conn);
    blob.setEmpty();
    Clob clob(conn);
    clob.setEmpty();
    stmt->setInt(1,6666);
    stmt->setInt(2,11001);
    stmt->setBlob(3,blob);
    stmt->setClob(4,clob);
    stmt->executeUpdate();
    stmt->setSQL ("INSERT INTO electronic_media(product_id,ad_id,ad_composite,ad_sourcetext) VALUES (:v1, :v2, :v3, :v4)");
    stmt->setInt(1,7777);
    stmt->setInt(2,11001);
    stmt->setBlob(3,blob);
    stmt->setClob(4,clob);
    stmt->executeUpdate();
    stmt->setSQL ("INSERT INTO electronic_media(product_id,ad_id,ad_composite,ad_sourcetext) VALUES (:v1, :v2, :v3, :v4)");
    stmt->setInt(1,8888);
    stmt->setInt(2,11001);
    stmt->setBlob(3,blob);
    stmt->setClob(4,clob);
    stmt->executeUpdate();
    conn->commit();
    conn->terminateStatement (stmt);

  }

  void dataRollBack (Connection *conn)
   throw (SQLException)
  {
    Statement *stmt = conn->createStatement ("DELETE FROM electronic_media WHERE product_id=6666 AND ad_id=11001");
    stmt->executeUpdate();
    stmt->setSQL("DELETE FROM electronic_media WHERE product_id=7777 AND ad_id=11001");
    stmt->executeUpdate();
    stmt->setSQL("DELETE FROM electronic_media WHERE product_id=8888 AND ad_id=11001");
    stmt->executeUpdate();
    conn->commit();
    conn->terminateStatement (stmt);

  }

  /**
   * populating the clob uses write method;
   */
  void populateClob (Clob &clob,unsigned int way)
   throw (SQLException)
  {
    unsigned int bufsize=BUFSIZE;
    if (way == USE_NORM)
    {
       cout << "Populating the Clob using write method" << endl;
       unsigned int offset=1;
       unsigned char *buffer = new unsigned char[bufsize];
       strcpy((char *)buffer,
         "Just for source text content(added using write method)");
       unsigned int size=strlen((char *)buffer);
       unsigned int bytesWritten=clob.write (size,buffer, size,offset);
       //cout <<"Bytes Written : " << bytesWritten << endl;
       delete[] buffer;
    }
    else if(way==USE_CHUN)
    { 
       cout << "Populating the Clob using writeChunk method" << endl;
       unsigned int offset=1;
       unsigned int pieceCount = 4;
       unsigned char *buffer = new unsigned char[bufsize*pieceCount];
       strcpy((char *)buffer,
         "Just for source text content(added using writeChunk method)");
       unsigned int size=strlen((char *)buffer);
       // Open the clob for writeChunk
       clob.open(OCCI_LOB_READWRITE);
       for (int i = 0; i < pieceCount; ++i,offset+=size)
         clob.writeChunk(size,buffer,size,offset);
       cout << "Clob Size " << clob.length() << endl;
       delete[] buffer;
       clob.close();
    }
    else if(way==USE_BUFF)
    {
       // Uses stream here
       cout << "Populating the Clob using writeBuffer(Stream) method" << endl;
       char *file = (char *)"clobdemo.dat";
       char *buffer = new char[bufsize + 1];
       ifstream inFile;
       inFile.open(file,ios::in);
       if (!inFile)
       {
          cout << "clobdemo.dat file not found\n";
          delete[] buffer;
          return;
       }
       unsigned int size;
       Stream *strm=clob.getStream();
       while(inFile)
       {
          memset (buffer, NULL, bufsize + 1);
          inFile.read(buffer,bufsize);
          strm->writeBuffer(buffer,strlen(buffer));
       }
       strcpy(buffer,"This piece for writeLastBuffer");
       size=strlen(buffer);
       strm->writeLastBuffer(buffer,size);
       clob.closeStream(strm);
       inFile.close();
       delete[] buffer;
    }
    cout << "Populating the Clob - Success" << endl;
  }

  /**
   * printing the clob data as integer stream
   */
  void dumpClob (Clob &clob,unsigned int way)
   throw (SQLException)
  {
    unsigned int size=BUFSIZE;
    unsigned int offset = 1;
  
    if (clob.isNull())
    {
       cout << "Clob is Null\n";
       return;
    }
    unsigned int cloblen = clob.length();
    cout << "Length of Clob : "<< cloblen << endl;
    if (cloblen == 0)
       return;
    unsigned char *buffer= new unsigned char[size]; 
    memset (buffer, NULL, size);
    if (way==USE_NORM)
    {
       cout << "Dumping clob (using read ): ";
       int bytesRead=clob.read(size,buffer,size,offset);
       for (int i = 0; i < bytesRead; ++i)
          cout << buffer[i];
       cout << endl;
    }
    else if(way==USE_BUFF)
    {
       Stream *inStream = clob.getStream (1,0);
       cout << "Dumping clob(using stream): ";
       int bytesRead=(inStream->readBuffer((char *)buffer, size));
       while (bytesRead > 0)
       {
          for (int i = 0; i < bytesRead; ++i) 
          {
              cout << buffer[i];
          }
          bytesRead=(inStream->readBuffer((char *)buffer, size));
       }
       cout << endl;
       clob.closeStream (inStream);
    }
    delete []buffer;
  }

  occiClob ()
  {
    /**
     * default values of username & password
     */
     username = "hr";
     password = "hr";
     url = "";
  }

  void setUsername (string u)
  {
    username = u;
  }

  void setPassword (string p)
  {
    password = p;
  }

  void setUrl (string u)
  {
    url = u;
  }

  void runSample ()
   throw (SQLException)
  {
    Environment *env = Environment::createEnvironment (
    Environment::DEFAULT);
    Connection *conn = env->createConnection (username, password, url);
    insertRows (conn);

    // Selecting and modifying the clob column of the table
    string sqlQuery = 
     "SELECT  product_id,ad_id,ad_sourcetext FROM electronic_media FOR UPDATE";
    Statement *stmt1 = conn->createStatement (sqlQuery);
    ResultSet *rset1 = stmt1->executeQuery ();
    cout << "Query :" << sqlQuery << endl;
    unsigned int way=USE_NORM; 
    while (rset1->next ())
    {
      cout << "Product_id : " << (int)rset1->getInt(1) << endl;
      cout << "Ad_id      : " << (int)rset1->getInt(2) << endl;
      Clob clob = rset1->getClob (3);
      dumpClob (clob, USE_NORM);
      if (way==USE_NORM)
      {
         populateClob(clob,USE_NORM);      
         way=USE_CHUN;
      }
      else if(way==USE_CHUN)
      {
         populateClob(clob,USE_CHUN);      
         way=USE_BUFF;
      }
      else if(way==USE_BUFF)
      {
         populateClob(clob,USE_BUFF);      
         way=USE_NORM;
      } 
    }
    stmt1->executeUpdate();
    stmt1->closeResultSet (rset1);

    // Printing after updating the clob content.
    way = USE_BUFF;
    sqlQuery = "SELECT product_id, ad_id, ad_sourcetext FROM electronic_media \
ORDER BY product_id";
    Statement *stmt2 = conn->createStatement (sqlQuery);
    ResultSet *rset2 = stmt2->executeQuery ();
    cout << "Query :" << sqlQuery << endl;
    while (rset2->next ())
    {
      cout << "Product_id : " << (int)rset2->getInt(1) << endl;
      cout << "Ad_id      : " << (int)rset2->getInt(2) << endl;
      Clob clob = rset2->getClob (3);
      if (way==USE_NORM)
      {
         dumpClob (clob, USE_NORM);
         way=USE_BUFF;
      }
      else if(way==USE_BUFF)
      {
         dumpClob (clob, USE_BUFF);
         way=USE_NORM;
      }
    }
    stmt2->closeResultSet (rset2);
    dataRollBack(conn);
    conn->terminateStatement (stmt1);
    conn->terminateStatement (stmt2);
    env->terminateConnection (conn);
    Environment::terminateEnvironment (env);
  }

};//end of class  occiClob

int main (void)
{
  try
  {
    occiClob *b = new occiClob ();
    b->setUsername ("hr");
    b->setPassword ("hr");
    b->runSample ();
  }
  catch (exception &e)
  {
    cout << e.what();
  }
}

