/* Copyright (c) 2003, 2006, Oracle. All rights reserved.  */
/*
   NAME
     occiuni1.cpp - OCCI Globalization Support demo (Unicode)

   DESCRIPTION
     Demonstrates OCCI globalization support capabilities :-

     1.createEnvironment : specifying client characterset & 
       national characterset of the application

     2.setUString : binding Unicode data for a NVARCHAR2 
       column using OCCI's UString datatype

     3.setCharSet : specifying charactersets different from the
       Environment's for Statement bind and ResultSet fetch

     4.getString : fetching Unicode data from a NVARCHAR2 
       column in UTF8 charset

     Demo schema : -
     table Countries_Tab
     (
     CID   Number(10), Country Id
     CENGLISHNAME Varchar2(100), Country name in English
     CNATIONALNAME NVarchar2(100), Country name in national language
     )

     Oracle's NVARCHAR2 datatype is capable of storing Unicode data.

     This demo inserts some country names in their national languages(
     e.g Japan in japanese) in Unicode(UTF16) and then fetches them in 
     UTF8 characterset.

     INSTRUCTIONS
     ------------

     1.Run SQL script - occiuni1.sql before running this program. The
     database characterset can be any characterset, the database
     national characterset will be one of the Unicode 
     charactersets : AL16UTF16 or UTF8.

     2.Run this program and direct output to a file and view the output
     file from a browser :-
     $ ./occiuni1 > occiuni1.html   

   MODIFIED   (MM/DD/YY)
   sudsrini    10/22/06 - Username/Password lower case
   kukannan    03/12/06 - Fix bug 5074203 
   sudsrini    07/23/04 - Copyright Info
   shiyer      03/05/03 - Creation

*/

#include <iostream>
#include <occi.h>

using namespace std;
using namespace oracle::occi;

//ascii2utf16 - simpile utility function to convert an
//ASCII string to Unicode string
UString ascii2utf16(string asciistr)
{
   UString ustr;
   ustr.resize (asciistr.length());
   for (int i = 0; i < asciistr.length(); i++)
      ustr[i] = (unsigned short)asciistr[i];

   return ustr;
}

int main()
{

  try
  {

//initialize environment with client characterset = US7ASCII,
//national characterset = UTF16
    Environment *env = Environment::createEnvironment("US7ASCII","OCCIUTF16");

    Connection *conn = env->createConnection("hr","hr");

    cout << "Inserting Unicode data in UTF16 characterset <BR>" << endl;

    Statement *stmt = conn->createStatement("insert into Countries_Tab values (:1,:2,:3)");

//If the database column type is NCHAR/NVARCHAR2 then need to call
//setDatabaseNCHARParam with true to indicate this.
    stmt->setDatabaseNCHARParam(3, true);

//-------------------1, U.S.A----------------------------------------
    stmt->setInt(1, 1);
    stmt->setString(2, "U.S.A"); //data in ASCII
    UString uUSA = ascii2utf16("U.S.A");//U.S.A in Unicode
    stmt->setUString(3, uUSA);//setUString for binding Unicode
    stmt->executeUpdate();

//-------------------2, Russia----------------------------------------
    stmt->setInt(1, 2);
    stmt->setString(2, "Russia");
//the 6 Cyrillic Unicode characters for 'Russia'.
    unsigned short russiaunicodechars[] = {0x0420,0x043E,0x0441,
                                           0x0441,0x0438,0x044F};
    UString uRussia(russiaunicodechars,6);
    stmt->setUString(3,uRussia);

    stmt->executeUpdate();

//-------------------3, Japan----------------------------------------
    stmt->setInt(1, 3);
    stmt->setString(2, "Japan");

//the 2 Katakana Unicode characters for 'Japan' in japanese
    unsigned short japanunicodechars[] = {0x65E5,0x672C};
    UString uJapan(japanunicodechars, 2);
    stmt->setUString(3,uJapan);

    stmt->executeUpdate();

//-------------------4, Austria----------------------------------------
    stmt->setInt(1, 4);
    stmt->setString(2, "Austria");

// the client charset & client national charset environment settings 
// can be over-ridden for specific columns by calling the setCharSet
// method on Statement/ResultSet
    stmt->setCharSet(3, "WE8DEC");//override UTF16 for this column

//the character Ö (WE8DEC codepoint D6) is for Latin capital letter O with
//Diaresis. Use setString instead of setUString, since not in UTF16
    string aAustria("Österreich");//data in WE8DEC
    stmt->setString(3, aAustria);

    stmt->executeUpdate();

//commit the inserts. Users can view the inserted data from SQL*Plus or
//ISQL*Plus
    conn->commit();

    conn->terminateStatement(stmt);

//Now fetch the data. We will fetch the Unicode data (i.e country
//names in national language) in UTF8 charset
//added some HTML <BR> output for formatting in browser

    cout << "Retrieving data in UTF8 characterset <BR>" << endl;
    cout << 
"The Unicode data will not display correctly in terminal. \
To properly view, pipe the output of this program to a text file. \
Open this text file from a Web browser and set \
Encoding/Characterset to UTF8 <BR> " << endl;
    cout << "========================================= <BR> " << endl;

    stmt = conn->createStatement("select Cid, CEnglishName, CNationalName from Countries_Tab");
    ResultSet *rs = stmt->executeQuery();

    rs->setDatabaseNCHARParam(3, true);//NVARCHAR2 column
    rs->setCharSet(3, "UTF8"); //explicitly specify characterset

    while (rs->next())
    {
          cout << rs->getInt(1) << "," << rs->getString(2) << ",";
          cout << rs->getString(3); //country name in UTF8

          cout << "  <BR>" << endl; //HTML formatting tag
    }

    cout << "========================================= <BR> " << endl;

    stmt->closeResultSet(rs);
    conn->terminateStatement(stmt);

    //terminate connection & environment
    env->terminateConnection(conn);
    Environment::terminateEnvironment(env);
  }
  catch (SQLException e)
  {
         cout << "Error : ";
         cout << e.getMessage() << endl;
  }

  return 0;
}
