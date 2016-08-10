/* Copyright (c) 2003, 2006, Oracle. All rights reserved.  */
/*
   NAME
     occimb1.cpp - OCCI Globalization Support demo

   DESCRIPTION

     Demonstrates OCCI globalization support capabilities :-

     1.Multiple clients with different charactersets connecting 
       and interacting with a single UTF8 database 

     2.createEnvironment() with multibyte charactersets.

     3.using Statement::setString to bind multibyte data

     4.Storing Chinese(ZHT16BIG5) & Greek(EL8ISO8859P7) data
       into a VARCHAR2 column. of a UTF8 database.

     5.Retrieving the multi-lingual data in UTF8 characterset

     Demo schema : -
     table GlobalInfo_Tab
     (
      G_LANG   VARCHAR2(10), 
      G_INFODESC VARCHAR2(50),
      G_INFO VARCHAR2(100), Chinese/Greek information
     )

     This demo inserts some Chinese & Greek data in ZHT16BIG5 and
     EL8ISO8859P7 charactersets and then fetches them in UTF8 characterset.

     INSTRUCTIONS
     ------------

     1.Run SQL script - occimb1.sql before running this program.
     THE DATABASE CHARACTERSET MUST BE UTF8 FOR THE SQL AND DEMO PROGRAM TO RUN.
     TO CONVERT AN EXISTING DATABASE NON-UTF8 CHARACTER SET TO UTF8,
     USE "ALTER DATABASE CHARACTER SET" COMMAND 
     FOR MORE INFO, PLEASE REFER "SQL REFERENCE" MANUAL

     2.Run this program and direct output to a file and view the output
       file from a browser :-
     $ ./occimb1 > occimb1.html

   MODIFIED   (MM/DD/YY)
   sudsrini    03/07/08 - order by for deterministic output
   sudsrini    10/22/06 - Username/Password lower case
   kukannan    03/12/06 - Fix bug 5074203 
   sudsrini    07/23/04 - Copyright Info
   shiyer      03/05/03 - Creation

*/

#include <iostream>
#include <occi.h>

using namespace std;
using namespace oracle::occi;


int main()
{
   //part1 - use ZHT16BIG5 characterset
   try
   {
        cout << "Creating environment with ZHT16BIG5(Chinese) characterset <BR>" << endl;
        Environment *big5env = Environment::createEnvironment("ZHT16BIG5","ZHT16BIG5");

        Connection *conn = big5env->createConnection("hr", "hr");

        cout << "Inserting some information in Chinese - Periodic table elements <BR>" << endl;

        Statement *stmt = 
        conn->createStatement("INSERT INTO GLOBALINFO_TAB VALUES (:1, :2 ,:3)");

        stmt->setString(1,"Chinese");
        stmt->setString(2,"Periodic Table Elements");

        //the code-points in ZHT16BIG5 for the symbols
        const char siliconsym[] = {0xD6,0xBA,0x00};
        const char goldsym[] = {0xAA,0xF7,0x00};
        const char calciumsym[] = {0xE0,0xB3,0x00};

        string info;
        info += "Silicon = ";
        info += siliconsym;
        info += " Gold = ";
        info += goldsym;
        info += " Calcium = ";
        info += calciumsym;
        stmt->setString(3,info);

        stmt->executeUpdate();
        conn->commit();

        conn->terminateStatement(stmt);
        big5env->terminateConnection(conn);
        Environment::terminateEnvironment(big5env);
   }
   catch (SQLException &e)
   {
        cout << "Error : ";
        cout << e.getMessage() << endl;
   }

   //part2 - use EL8ISO8859P7(Greek) characterset
   try
   {
        cout << "Now creating environment with EL8ISO8859P7(Greek) characterset <BR>" << endl;
        Environment *greekenv= Environment::createEnvironment("EL8ISO8859P7","EL8ISO8859P7");

        Connection *conn = greekenv->createConnection("hr", "hr");

        cout << "Inserting some information in Greek - Mathematical symbols <BR>" << endl;

        Statement *stmt = 
        conn->createStatement("INSERT INTO GLOBALINFO_TAB VALUES (:1, :2 ,:3)");

        stmt->setString(1,"Greek");
        stmt->setString(2,"Mathematical symbols");

        string info;
        info += "alpha = ";
        info += "á";//codepoint - 0xE1 in EL8ISO8859P7
        info += " beta = ";
        info += "â";//codepoint - 0xE2
        info += " gamma = ";
        info += "ã";//codepoint - 0xE3
        info += " delta = ";
        info += "ä";//codepoint - 0xE4
        info += " epsilon = ";
        info += "å";//codepoint - 0xE5
        stmt->setString(3,info);
        stmt->executeUpdate();
        conn->commit();
    }
    catch (SQLException &e)
    {
        cout << "Error : ";
        cout << e.getMessage() << endl;
    }

  //part3 - use UTF8 characterset and retrieve the data we have inserted
  try
  {
        cout << "Now creating environment with UTF8 characterset <BR>" << endl;
        Environment *utf8env= Environment::createEnvironment("UTF8","UTF8");

        Connection *conn = utf8env->createConnection("hr","hr");

        cout << "Retrieveing the Chinese & Greek information inserted <BR>" << endl;

        Statement *stmt = 
        conn->createStatement("SELECT  G_LANG, G_INFODESC, G_INFO FROM GLOBALINFO_TAB ORDER BY G_LANG");
        ResultSet *rs = stmt->executeQuery();

        cout <<
"The Unicode data will not display correctly in terminal. \
To properly view, pipe the output of this program to a text file. \
Open this text file from a Web browser and set \
Encoding/Characterset to UTF8 <BR> " << endl;

        cout << "========================================= <BR> " << endl;

        while (rs->next())
        {
          cout << rs->getString(1) << "," << rs->getString(2) << ",";
          cout << rs->getString(3); //Chinese/Greek Info

          cout << "  <BR>" << endl; //HTML formatting tag
        }

        cout << "========================================= <BR> " << endl;

        stmt->closeResultSet(rs);
        conn->terminateStatement(stmt);

        utf8env->terminateConnection(conn);
        Environment::terminateEnvironment(utf8env);

   }
   catch (SQLException &e)
   {
        cout << "Error : ";
        cout << e.getMessage() << endl;
   }

  return 0;
}
