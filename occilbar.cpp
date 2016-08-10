/* Copyright (c) 2004, 2006, Oracle. All rights reserved.  */
/*
   NAME
     occilbar.cpp - OCCI Support for reading/writing arrays of LOB's.

  DESCRIPTION
     To exhibit OCCI Support for reading/writing arrays of LOB's.
     Create a table with BLOB, CLOB column.
     Populate the values in the table by using new LOB Array Write method
     Retrieve the values from the table by using the LOB Array Read method
     
     1. readVectorOfBlobs()
     2. readVectorOfClobs()
     3. writeVectorOfBlobs()
     4. writeVectorOfClobs()
  
   MODIFIED   (MM/DD/YY)
   sudsrini    10/22/06  - Username/Password lower case
   kukannan    09/21/04  - Include iostream 
   sudsrini    07/22/04  - Copyright Info
   debanerj    06/04/04  - debanerj_13064_lob_array_read
   kukannan    05/26/04  - Creation
*/

#include <stdlib.h>
#include <occi.h>
#include <iostream>
using namespace oracle::occi;
using namespace std;

#define NO_OF_LOBS 10

class lobarray 
{
  private:

  Environment *env;
  Connection *conn;

  public:

  lobarray (string user, string passwd, string db)
  {
    env = Environment::createEnvironment (Environment::DEFAULT);
    conn = env->createConnection (user, passwd, db);
  }

  /**
   * Destructor for the lobarray test case.
   */
  ~lobarray ()
  {
    env->terminateConnection (conn);
    Environment::terminateEnvironment (env);
  }  // end of ~lobarray ()

  /**
   * Creating schema objects.
   */
  dvoid createTables ()
  {
    string sqlstr1 = "CREATE TABLE lobarray_tab";
    sqlstr1 += " (c1 NUMBER,  c2 BLOB, c3 CLOB)";
    Statement *stmt = conn->createStatement(sqlstr1);
    stmt->execute();
  }  // end of createTables ()

  /**
   * Removing schema objects created.
   */
  dvoid dropTable ()
  {
    Statement *stmt= conn->createStatement("DROP TABLE lobarray_tab");
    stmt->execute();
  }  // end of cleanup ()

  /**
  * The testing logic of the test case.
  */
  dvoid test ()
  {
    cout << "lobarray - Testing the OCCI Array LOB Api's"  << endl;
    createTables ();

    Statement *stmt = conn->createStatement("");

    char stmt_str[500];
    for (int i=0; i<NO_OF_LOBS; i++)
    {
      sprintf(stmt_str, "INSERT INTO lobarray_tab VALUES \
      (%d, empty_blob(), empty_clob())", i);

      stmt->execute ((const char *)stmt_str);
    }
    conn->terminateStatement (stmt);
 
    writeArrayBlob ();
    readArrayBlob ();

    cout << "Writing into clob by mentioning character amts " << endl; 
    writeArrayClob (1);
    cout << "Reading from clob by mentioning character amts " << endl; 
    readArrayClob (1);

    cout << "Writing into clob by mentioning byte amts " << endl; 
    writeArrayClob (2);
    cout << "Reading from clob by mentioning byte amts " << endl; 
    readArrayClob (2);
  
    dropTable ();
    cout << "lobarray - done" << endl;
  } // end of test()

        /************************************************************/
        /*                  writeArrayBlob()                        */
        /************************************************************/
        /*Select all the Lob Locators from Blob col from the table. */
        /*Push these Lob Locators in a Vector of Blobs.             */
        /*Call populate_buffer function which would populate the    */
        /*amount of data to be written, sets the offsets, populates */
        /*the buffer.                                               */
        /*Call writeVectorOfBlobs with the populate buffer,amount   */
        /*and offset value. This would write the buffered values    */
        /*into respective lobs. Do and connection commit and release*/
        /*the allocated memory.                                     */
        /************************************************************/

  void writeArrayBlob ()
  {
    cout << "Writing Array Lob using writeVectorofBlobs" << endl;
    char stmt_str[500]; 

    vector<Blob> lob_vec;
    sprintf(stmt_str,"Select c2 from lobarray_tab for update");
    Statement *stmt = conn->createStatement((const char *)stmt_str);
    ResultSet *rs = stmt->executeQuery();
  
    while (rs->next())
        lob_vec.push_back(rs->getBlob(1));
  
    stmt->closeResultSet(rs);
    conn->terminateStatement(stmt);

    oraub8 byte_amts[NO_OF_LOBS];
    oraub8 offsets[NO_OF_LOBS];
    unsigned char *buffers[NO_OF_LOBS];
    oraub8 buffer_lens[NO_OF_LOBS];

    populate_buffer(byte_amts,offsets, buffers,buffer_lens,1);
    writeVectorOfBlobs(conn,lob_vec,byte_amts,offsets,buffers,buffer_lens);
    conn->commit();
    release_memory(buffers);
    cout << "Writing - Done" << endl;
  }
        /************************************************************/
        /*                  writeArrayClob()                        */
        /************************************************************/
        /*Select all the Lob Locators from Clob col from the table. */
        /*Push these Lob Locators in a Vector of Clobs.             */
        /*Call populate_buffer function which would populate the    */
        /*amount of data to be written, sets the offsets, populates */
        /*the buffer.                                               */
        /*Call writeVectorOfClobs with the populated buffer,amount  */
        /*and offset value. This would write the buffered values    */
        /*into respective lobs. Do and connection commit and release*/
        /*the allocated memory.                                     */
        /*The type variable differentiates the attributes based on  */
        /*which the value gets inserted into the Clob.              */
        /*A value of 1 for the TYPE variable means that the <amts>  */
        /*of CHARACTER gets inserted into the Lobs.                 */
        /*A value of 2 for the TYPE variable means that the <amts>  */
        /*of BYTES gets inserted into the Lobs.                     */
        /************************************************************/

  void writeArrayClob (int type)
  {
    char stmt_str[500]; 

    vector<Clob> lob_vec;
    sprintf(stmt_str,"Select c3 from lobarray_tab for update");
    Statement *stmt = conn->createStatement((const char *)stmt_str);
    ResultSet *rs = stmt->executeQuery();
  
    // Got all the loblocator and pushed in to the vector 
    while (rs->next())
        lob_vec.push_back(rs->getClob(1));
  
    stmt->closeResultSet(rs);
    conn->terminateStatement(stmt);

    oraub8 amts[NO_OF_LOBS];
    oraub8 offsets[NO_OF_LOBS];
    unsigned char *buffers[NO_OF_LOBS];
    oraub8 buffer_lens[NO_OF_LOBS];
    populate_buffer(amts,offsets, buffers,buffer_lens,1);
    
        /**************************************************************/
        /*Populating the Lob by passing Character amounts if type = 1 */
        /*Populating the Lob by passing Byte amounts if type = 2      */
        /**************************************************************/

    if (type==1)
    {
      writeVectorOfClobs(conn,lob_vec,NULL,amts,offsets,\
      buffers,buffer_lens);
    }
    else
    {
      writeVectorOfClobs(conn,lob_vec,amts,NULL,offsets,\
      buffers,buffer_lens);
   }
    conn->commit();
    release_memory(buffers);
    cout << "Writing - Done" << endl;
  }
        /************************************************************/
        /*                  readArrayBlob()                         */
        /************************************************************/
        /*Select all the Lob Locators from Blob col from the table. */
        /*Push these Lob Locators in a Vector of Blobs.             */
        /*Call populate_buffer function which would assign the      */
        /*the offsets, allocate memory for the buffer for           */
        /*readVectorOfBlobs to read the value into.                 */
        /*Call readVectorOfBlobs with the populated amount and      */
        /*offset value. This would read the LOB data into the       */
        /*allocated buffers.  Print the buffers by calling          */
        /*print_buffers() and release the allocated memory.         */ 
        /************************************************************/


  void readArrayBlob ()
  {
    cout << "Reading Array Lob using readVectorofBlob" << endl;
    char stmt_str[500]; 

    vector<Blob> lob_vec;
    sprintf(stmt_str,"Select c2 from lobarray_tab order by c1");
    Statement *stmt = conn->createStatement((const char *)stmt_str);
    ResultSet *rs = stmt->executeQuery();
  
    while (rs->next())
        lob_vec.push_back(rs->getBlob(1));
  
    stmt->closeResultSet(rs);
    conn->terminateStatement(stmt);

    oraub8 read_amts[NO_OF_LOBS];
    oraub8 offsets[NO_OF_LOBS];
    unsigned char *buffers[NO_OF_LOBS];
    oraub8 buffer_lens[NO_OF_LOBS];
    populate_buffer(read_amts,offsets, buffers,buffer_lens,0);
    
    readVectorOfBlobs(conn,lob_vec,read_amts,offsets,buffers,buffer_lens);
    
    print_buffer(buffers,read_amts);
    release_memory(buffers);
    cout << "Reading - Done " << endl;
  }
        /************************************************************/
        /*                  readArrayClob()                         */
        /************************************************************/
        /*Select all the Lob Locators from Clob col from the table. */
        /*Push these Lob Locators in a Vector of Clobs.             */
        /*Call populate_buffer function which would assign the      */
        /*the offsets, allocate memory for the buffer for           */
        /*readVectorOfClobs to read the value into.                 */
        /*Call readVectorOfClobs with the populated amount and      */
        /*offset value. This would read the LOB data into the       */
        /*allocated buffers.Print the buffers by calling            */
        /*print_buffers() and release the allocated memory.         */ 
        /*A value of 1 for the TYPE variable means that the <amts>  */
        /*of CHARACTER gets inserted into the Lobs.                 */
        /*A value of 2 for the TYPE variable means that the <amts>  */
        /*of BYTES gets inserted into the Lobs.                     */
        /************************************************************/


  void readArrayClob (int type)
  {
    char stmt_str[500]; 

    vector<Clob> lob_vec;
    sprintf(stmt_str,"Select c3 from lobarray_tab order by c1");
    Statement *stmt = conn->createStatement((const char *)stmt_str);
    ResultSet *rs = stmt->executeQuery();
  
    while (rs->next())
        lob_vec.push_back(rs->getClob(1));
  
    stmt->closeResultSet(rs);
    conn->terminateStatement(stmt);

    oraub8 read_amts[NO_OF_LOBS];
    oraub8 offsets[NO_OF_LOBS];
    unsigned char *buffers[NO_OF_LOBS];
    oraub8 buffer_lens[NO_OF_LOBS];
    populate_buffer(read_amts,offsets,buffers,buffer_lens,0);
    
    if (type==1)
      readVectorOfClobs(conn,lob_vec,NULL,read_amts,offsets,\
      buffers,buffer_lens);
    else if (type==2)
      readVectorOfClobs(conn,lob_vec,read_amts,NULL,offsets,\
      buffers,buffer_lens);

    print_buffer(buffers,read_amts);
    release_memory(buffers);
    cout << "Reading - Done " << endl;
  }

        /************************************************************/
        /*                  populate_buffer()                       */
        /************************************************************/
        /*This function assign the offsets,buffer_lens,buffer_amts  */
        /*for each Lob.Allocates memory for each buffer and populate*/
        /*buffer for each Lob. The significance of Idx is to        */
        /*differentiate the calls made while reading or writing the */
        /*Lob Data. Reading/Writing Lobs require offset,amts and    */
        /*buffer_lens for each Lob.                                 */
        /*Idx=1 means the function is called prior to writing into  */
        /*the LOB and populates the buffer                          */
        /*Idx=1 means the function is called prior to reading from  */
        /*the LOB and donot populate the buffer                     */
        /************************************************************/

  void populate_buffer(oraub8 *amts,oraub8 *offsets,unsigned char **buffers,\
  oraub8 *buffer_lens,int idx)
  {
      for (int i=0; i < NO_OF_LOBS ; i++)
      {
        offsets[i]=1; 
        buffer_lens[i]=i+1; //Writing upto multiples of 5 
        amts[i]=buffer_lens[i];
        buffers[i]=new unsigned char[buffer_lens[i]];

        if (idx!=0)
        {
           for (int j=0; j<buffer_lens[i]; j++)
           {
              buffers[i][j] = (unsigned char)(65);
           }
        }
      }
  }
        /************************************************************/
        /*                  print_buffer()                          */
        /************************************************************/
        /*This function prints character by character (byte_amts)   */
        /*of the buffer which is passed                             */
        /************************************************************/

  void print_buffer(unsigned  char **buffers, oraub8 *byte_amts)
  {
    for (int i=0; i<NO_OF_LOBS;i++)
    {
      cout << "Number of Bytes read " << (ub4) byte_amts[i] << endl;
      cout << "[" ;
      for (int j=0; j< byte_amts[i];j++)
        cout <<  buffers[i][j]  ;
      cout <<"]" << endl;
    }
  }
        /************************************************************/
        /*                  release_memory()                        */
        /************************************************************/
        /*This function releases the memory of the buffers which get*/
        /*allocated by the populate_buffer function calls           */
        /************************************************************/


  void release_memory(unsigned char **buffer)
  {
    for (int i=0;i<NO_OF_LOBS;i++)
      delete [] buffer[i];
  }
}; // end of class lobarray 

/**
 * The main function where the test case is instantiated
 */
int main ( int argc, char* argv[])
{
  string userName  = "hr";
  string password  = "hr";
  string database  = "";

  lobarray* h = new lobarray (userName,password,database);
  try 
  {
    h->test ();
  }
  catch (SQLException ea) 
  {
     cout<<"Exception thrown by test Function" <<endl;
     cout<<"Error number: "<<  ea.getErrorCode() << endl;
     cout<<ea.getMessage() << endl;
  }
  delete (h);
  return 0;
}// end of main (int, char*)
