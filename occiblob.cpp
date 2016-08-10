/* Copyright (c) 2001, 2009, Oracle and/or its affiliates. 
All rights reserved. */
/*
   NAME
     occiblob.cpp - OCCI BLOB Interface demo

   DESCRIPTION
     This demo program explains usage of BLOB interface.
     This program inserts an empty blob into the table,selects the 
     blob from table, modifies it and stores it back into the table.
     The Blob content will be printed finally.
     This demo uses write , writeChunk and writeBuffer methods to 
     modify the blob contents, and also uses read and readBuffer
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
 * populateBlob  - populates a given blob
 * dumpBlob      - prints the blob as an integer stream
 */
// These will tell the type of read/write we use
#define USE_NORM 1
#define USE_CHUN 2
#define USE_BUFF 3

/* Buffer Size */
#define BUFSIZE 200;

class  occiBlob
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
   * populating the blob uses write method;
   */
  void populateBlob (Blob &blob,unsigned int way,unsigned int size=0)
   throw (SQLException)
  {
    if (way == USE_NORM)
    {
       cout << "Populating the Blob using write method" << endl;
       unsigned int offset=1;
       unsigned char *buffer = new unsigned char[size];
       memset (buffer, (char)10, size);
       unsigned int bytesWritten=blob.write (size,buffer, size,offset);
       //cout <<"Bytes Written : " << bytesWritten << endl;
       delete[] buffer;
    }
    else if(way==USE_CHUN)
    { 
       cout << "Populating the Blob using writeChunk method" << endl;
       unsigned int offset=1;
       unsigned int pieceCount = 4;
       unsigned char *buffer = new unsigned char[size*pieceCount];
       memset (buffer, (char)10, size*pieceCount);
       // Open the blob for writeChunk
       blob.open(OCCI_LOB_READWRITE);
       for (int i = 0; i < pieceCount; ++i,offset+=size)
         blob.writeChunk(size,buffer,size,offset);
       cout << "Blob Size " << blob.length() << endl; 
       delete[] buffer;
       blob.close();
    }
    else if(way==USE_BUFF)
    {
       // Uses stream here
       unsigned int size;
       unsigned int bufsize=BUFSIZE;
       cout << "Populating the Blob using writeBuffer(Stream) method" << endl;
       char *file = (char *)"blobdemo.dat";
       char *buffer = new char[bufsize];
       ifstream inFile;
       inFile.open(file,ios::in);
       if (!inFile)
       {
          cout << "blobdemo.dat file not found\n";
          delete[] buffer;
          return;
       }
       Stream *strm=blob.getStream();
       while(inFile)
       {
          memset (buffer, NULL, bufsize);
          inFile.read(buffer,bufsize);
          strm->writeBuffer(buffer,strlen(buffer));
       }
       strcpy(buffer,"This piece for writeLastBuffer");
       size=strlen(buffer);
       strm->writeLastBuffer(buffer,size);
       blob.closeStream(strm);
       inFile.close();
       delete[] buffer;
    }
    cout << "Populating the Blob - Success" << endl;
  }

  /**
   * printing the blob data as integer stream
   */
  void dumpBlob (Blob &blob,unsigned int way)
   throw (SQLException)
  {
    unsigned int size=BUFSIZE;
    if (blob.isNull())
    {
       cout << "Blob is Null\n";
       return;
    }
    unsigned int offset= 1,bloblen = blob.length();
    cout << "Length of Blob : "<< bloblen << endl;
    if (bloblen == 0)
       return;
    unsigned char *buffer= new unsigned char[size]; 
    memset (buffer, NULL, size);
    if (way==USE_NORM)
    {
       cout << "Dumping blob (using read ): ";
       int bytesRead=blob.read(bloblen,buffer,bloblen,offset);
       for (int i = 0; i < bytesRead; ++i)
          cout << (int) buffer[i];
       cout << endl;
    }
    else if(way==USE_BUFF)
    {
       Stream *inStream = blob.getStream (1,0);
       cout << "Dumping blob(using stream): ";
       int bytesRead=(inStream->readBuffer((char *)buffer, size));
       while (bytesRead > 0)
       {
          for (int i = 0; i < bytesRead; ++i) 
          {
              cout << (int) buffer[i];
          }
          bytesRead=(inStream->readBuffer((char *)buffer, size));
       }
       cout << endl;
       blob.closeStream (inStream);
    }
    delete []buffer;
  }

  occiBlob ()
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

    // Selecting and modifying the blob column of the table
    string sqlQuery = 
     "SELECT  product_id,ad_id,ad_composite FROM electronic_media FOR UPDATE";
    Statement *stmt1 = conn->createStatement (sqlQuery);
    ResultSet *rset1 = stmt1->executeQuery ();
    cout << "Query :" << sqlQuery << endl;
    unsigned int way=USE_NORM; 
    while (rset1->next ())
    {
      cout << "Product_id : " << (int)rset1->getInt(1) << endl;
      cout << "Ad_id      : " << (int)rset1->getInt(2) << endl;
      Blob blob = rset1->getBlob (3);
      dumpBlob (blob, USE_NORM);
      if (way==USE_NORM)
      {
         populateBlob(blob,USE_NORM,20);      
         way=USE_CHUN;
      }
      else if(way==USE_CHUN)
      {
         populateBlob(blob,USE_CHUN,20);      
         way=USE_BUFF;
      }
      else if(way==USE_BUFF)
      {
         populateBlob(blob,USE_BUFF);      
         way=USE_NORM;
      } 
    }
    stmt1->executeUpdate();
    stmt1->closeResultSet (rset1);

    // Printing after updating the blob content.
    way = USE_BUFF;
    sqlQuery = "SELECT product_id, ad_id, ad_composite FROM electronic_media \
ORDER BY product_id";
    Statement *stmt2 = conn->createStatement (sqlQuery);
    ResultSet *rset2 = stmt2->executeQuery ();
    cout << "Query :" << sqlQuery << endl;
    while (rset2->next ())
    {
      cout << "Product_id : " << (int)rset2->getInt(1) << endl;
      cout << "Ad_id      : " << (int)rset2->getInt(2) << endl;
      Blob blob = rset2->getBlob (3);
      if (way==USE_NORM)
      {
         dumpBlob (blob, USE_NORM);
         way=USE_BUFF;
      }
      else if(way==USE_BUFF)
      {
         dumpBlob (blob, USE_BUFF);
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

};//end of class  occiBlob

int main (void)
{
  try 
  {
    occiBlob *b = new occiBlob ();
    b->setUsername ("hr");
    b->setPassword ("hr");
    b->runSample ();
    delete(b);
  }
  catch (exception &e)
  {
    cout << e.what();
  }
}

