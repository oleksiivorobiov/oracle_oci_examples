/* Copyright (c) 2003, 2006, Oracle. All rights reserved.  */
/*
  NAME
    occiuni2.cpp - OCCI Globalization Support demo (Unicode, Windows specific)

  DESCRIPTION
    This is a Microsoft Windows specific demo.
    Except for the use of Windows API's and wstring datatypes, other concepts
    demonstrated apply to all platforms. 
    The data file used is also Windows specific.

     Demonstrates the following capabilities :-
     1. Equivalance of OCCI's UString & Windows wstring datatype.
        A wstring can be passed to OCCI Unicode interfaces expecting a 
        UString datatype. A wchar literal (L prefix) can also be passed
        for a UString

     2. Initialize OCCI Environment in Unicode(UTF16) 

     3. Logon & executing statements using the UString interfaces

     4. Creating & updating persistent objects containing NCLOB 
        attributes

     5. Reading & writing Unicode data from/into NCLOBs. Shows use of
        Clob::writeChunk & Clob::read methods for wchar data. The Unicode
        data is stored in text files.

     6. OTT generated class definition with UString attribute.

     Schema :-
     create type DocObjType as object
     (
      DocName     varchar2(100),
      DocText     nclob
     )

     create table DocumentsTab of DocObjType;

     OTT generated C++ class : DocObjType

     INSTRUCTIONS
     ------------
     1.Run SQL script - occiuni2.sql before running this program.
     2.This demo is Microsoft Windows only.

   MODIFIED   (MM/DD/YY)
   sudsrini    10/22/06 - Username/Password lower case
   kukannan    03/12/06 - Fix bug 5074203 
   sudsrini    07/23/04 - Copyright Info
   shiyer      03/05/03 - Creation

*/

#include <windows.h>
#include <iostream>
#include <fstream>

#include <occi.h>

//mapping function include
#include "occiuni2m.h"

using namespace std;
using namespace oracle::occi;

int main()
{
  cout << "***OCCI Globalization Support demo program.***" << endl;
  cout << "***This program loads Unicode data files into a NClob attribute***" << endl;

  char *unicodefiles[4] = {"occiuni2_hindi.txt", "occiuni2_russian.txt", "occiuni2_korean.txt",
                            "occiuni2_japanese.txt"};
  try
  {

     cout << "Initializing OCCI environment in Unicode mode" << endl;
     //initialize in Unicode(UTF16) mode
     Environment *utf16env = Environment::createEnvironment( "OCCIUTF16",
                        "OCCIUTF16", Environment::OBJECT );
     occiuni2m(utf16env);

     //"L" prefix will create a widechar i.e Unicode literal which is equivalent to
     // OCCI's UString datatype
     Connection *conn = utf16env->createConnection( L"hr",L"hr",L"" );

     //load the 4 sample Unicode files
     for (int i = 0; i < 4;i++)
     {
      //convert the filename argument to Unicode. We will be saving the filename
      //as one of the attributes of the object.
      const char *asciifilename = unicodefiles[i];
      wchar_t wcfilename[100];
      MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, (LPCSTR)asciifilename, 
                           strlen(asciifilename)+1, (LPWSTR)wcfilename, 100 );
      wstring docname(wcfilename);

      cout << "Loading " << asciifilename << endl;


      //Create a persistent object, set the NClob to empty and save
      //the object. Use the overloaded new operator that takes UString
      //arugments
      DocObjType *newdoc = new (conn, L"DOCUMENTSTAB",L"DOCOBJTYPE",
                                L"HR",L"HR") DocObjType();

      newdoc->setDocname(docname);

      //first insert a empty clob
      Clob doccontents(conn);
      doccontents.setEmpty();
      newdoc->setDoctext(doccontents);//empty 

      conn->commit();

      //Now, we will select the object again and add the document text
      //to the NClob attribute. 
      Statement *stmt = conn->createStatement(
      L"select ref(a) from documentstab a where docname = :1 for update");

      stmt->setUString(1, docname);//bind wstring
      ResultSet *rs = stmt->executeQuery();

      rs->next();//this will actually fetch the Ref

      Ref<DocObjType> docobjref = rs->getRef(1);
      DocObjType *docobj = docobjref.ptr();//pin the object

      doccontents = docobj->getDoctext();//get the Clob
      doccontents.open();

      doccontents.setCharSetId("OCCIUTF16");
      doccontents.setCharSetForm(OCCI_SQLCS_NCHAR);

      ifstream in( asciifilename, ios::binary );
      int bytesread=0, totalbytesread=0, wcharsread=0;
      wchar_t wbuffer1[50];//50 chars = 100 bytes at a time

      //we have stored the data in a Unicode text file. The first
      //character (2 bytes) is a Unicode Byte-Order-Mark and 
      //indicates whether the data in the file is little-endian or
      //big-endian
      in.read((char *)wbuffer1,2);//read & skip BOM
      int offset = 1;

      while (in.read( (char *)wbuffer1,sizeof(wbuffer1) ))
      {
        bytesread = in.gcount();
        wcharsread = bytesread/2;

        //write to the NClob
        doccontents.writeChunk( wcharsread, (utext *)wbuffer1, sizeof(wbuffer1), offset );

        offset += wcharsread;//offset is in terms of characters
        totalbytesread += bytesread;
      }
      //last chunk
      bytesread = in.gcount();
      wcharsread = bytesread/2;      
      totalbytesread += bytesread;

      doccontents.writeChunk( wcharsread, (utext *)wbuffer1, bytesread, offset );
      doccontents.close();

      //update the object and flush to database
      docobj->setDoctext(doccontents);
      docobj->markModified();
      docobj->flush();
      conn->commit();


      cout << totalbytesread/2 << " characters saved" << endl;

      //Statement & ResultSet objects will be created again
      stmt->closeResultSet(rs);
      conn->terminateStatement(stmt);
     }//for (i = 0; i < 4)


     cout << "Now reading the NClob data back and saving to new files" << endl;

     Statement *selectstmt = conn->createStatement(
     L"select ref(a) from DocumentsTab a" );
     ResultSet *queryrs = selectstmt->executeQuery();
     wstring wfilehdrtxt = L"If you cannot see the text properly, try setting your font to a Unicode font like : - Arial Unicode MS, Lucinda Sans Unicode etc...";
     while (queryrs->next())
     {
        Ref<DocObjType> docobjref = queryrs->getRef(1);

        wstring docname = docobjref->getDocname();

        //create the output file, prepend "fetch_" to the original filename
        docname = L"fetch_" + docname;
        char asciifilenamebuf[100];

        int ret = WideCharToMultiByte(CP_ACP, 0, (LPCWSTR)docname.c_str(),
                  docname.length()+1, (LPSTR)asciifilenamebuf, 100, NULL, NULL);

        cout << "Creating Unicode textfile " << asciifilenamebuf << endl;
        ofstream outdoc(asciifilenamebuf, ios_base::binary | ios_base::trunc);

        //first write the BOM
        wchar_t bom = 0xFEFF;//Windows is little-endian
        outdoc.write((char *)&bom, 2);

        outdoc.write((char *)wfilehdrtxt.c_str(), wfilehdrtxt.length()*2);

        Clob doccontents = docobjref->getDoctext();
        doccontents.setCharSetId("OCCIUTF16");
        doccontents.setCharSetForm(OCCI_SQLCS_NCHAR);
        int offset = 1;
        int clobcharsread=0;
        //now read the NClob and write to file
        wchar_t wbuffer2[100];//100 chars at a time
        while ( (clobcharsread = 
                 doccontents.read(100, wbuffer2, sizeof(wbuffer2), offset)) !=0 )
        {
            offset = offset+clobcharsread;
            outdoc.write((char *)wbuffer2,clobcharsread*2);//write takes number of bytes
        }
        outdoc.close();

      }//while (queryrs->next())

      //done
      cout << "You can view the created files in Notepad(or any other editor that displays Unicode)" << endl;
      selectstmt->closeResultSet(queryrs);
      conn->terminateStatement(selectstmt);

      //delete the rows from the table
      //if you want to retain the rows, comment out 
      cout << "Cleaning up table" << endl;
      Statement *deletestmt = conn->createStatement(
      L"delete from DocumentsTab");
      deletestmt->executeUpdate();

      conn->commit();

      utf16env->terminateConnection(conn);
      Environment::terminateEnvironment(utf16env);

      cout << "Done" << endl;

  }
  catch (SQLException &e)
  {
         std::cout << e.getErrorCode() << std::endl;
  }
  return 0;
}
