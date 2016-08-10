/* Copyright (c) 2001, 2008, Oracle. All rights reserved.  */
/*
   NAME
     occidesc.cpp - Describing the various objects of the database

   DESCRIPTION
     This program describes the database objects like table, object and 
     procedure to get the metadata

   MODIFIED   (MM/DD/YY)
   mvasudev    05/22/08 - Add try / catch blocks
   sudsrini    10/22/06 - Username/Password lower case
   sudsrini    07/22/04 - Copyright Info
   idcqe       03/05/01 - Creation

*/

#include <iostream>
#include <occi.h>
using namespace oracle::occi;
using namespace std;

class occidesc 
{
  private:

  Environment *env;
  Connection *conn;
  public :
  /**
   * Constructor for the occidesc demo program.
   */
  occidesc (string user, string passwd, string db) throw (SQLException)
  {
    env = Environment::createEnvironment (Environment::OBJECT);
    conn = env->createConnection (user, passwd, db);
  }// end of constructor occidesc (string, string, string )
  
  /**
   * Destructor for the occidesc demo program.
   */
  ~occidesc () throw (SQLException)
  {
    env->terminateConnection (conn);
    Environment::terminateEnvironment (env);
  }  // end of ~occidesc ()

  // Describing a subtype
  void describe_type()
  {
    cout << "Describing the object - PEOPLE_OBJ";
    MetaData metaData = conn->getMetaData ((char *)"PEOPLE_OBJ");
    int mdTyp = metaData.getInt(MetaData::ATTR_PTYPE);
    if (mdTyp == MetaData::PTYPE_TYPE)
    {
      cout << "PEOPLE_OBJis a type" << endl;
    }
    int typcode = metaData.getInt(MetaData::ATTR_TYPECODE);
    if (typcode == OCCI_TYPECODE_OBJECT)
       cout << "PERSON is an object type" << endl;
    else
       cout << "PERSON is not an object type" << endl;
    int numtypeattrs = metaData.getInt(MetaData::ATTR_NUM_TYPE_ATTRS);
    cout << "Object has " << numtypeattrs << " attributes" << endl;
    try
    {
      cout << "Object id: " << metaData.getUInt (MetaData::ATTR_OBJ_ID)
        << endl;
    }
    catch (SQLException ex)
    {
      cout << ex.getMessage() << endl;
    }
    cout << "Object Name: " <<
      metaData.getString (MetaData::ATTR_OBJ_NAME) << endl;
    cout << "Schema Name: " <<
      (metaData.getString(MetaData::ATTR_OBJ_SCHEMA)) << endl;
    cout << "Attribute version: " <<
      (metaData.getString(MetaData::ATTR_VERSION)) << endl;
    if (metaData.getBoolean(MetaData::ATTR_IS_INCOMPLETE_TYPE))
      cout << "Incomplete type" << endl;
    else
      cout << "Not Incomplete type" << endl;
    if (metaData.getBoolean(MetaData::ATTR_IS_SYSTEM_TYPE))
      cout << "System type" << endl;
    else
      cout << "Not System type" << endl;
    if (metaData.getBoolean(MetaData::ATTR_IS_PREDEFINED_TYPE))
      cout << "Predefined Type" << endl;
    else
      cout << "Not Predefined Type" << endl;
    if (metaData.getBoolean(MetaData::ATTR_IS_TRANSIENT_TYPE))
      cout << "Transient Type" << endl;
    else
      cout << "Not Transient Type" << endl;
    if (metaData.getBoolean(MetaData::ATTR_IS_SYSTEM_GENERATED_TYPE))
      cout << "System-generated type" << endl;
    else
      cout << "Not System-generated type" << endl;
    if (metaData.getBoolean(MetaData::ATTR_HAS_NESTED_TABLE))
      cout << "Has nested table" << endl;
    else
      cout << "Does not have nested table" << endl;
    if (metaData.getBoolean(MetaData::ATTR_HAS_LOB))
      cout << "Has LOB" << endl;
    else
      cout << "Does not have LOB" << endl;
    if (metaData.getBoolean(MetaData::ATTR_HAS_FILE))
      cout << "Has BFILE" << endl;
    else
      cout << "Does not have BFILE" << endl;
    if (metaData.getBoolean(MetaData::ATTR_IS_INVOKER_RIGHTS))
      cout << "Object is Invoker rights" << endl;
    else
      cout << "Object is Not Invoker rights" << endl;
    RefAny ref = metaData.getRef (MetaData::ATTR_REF_TDO);
    MetaData md1 = conn->getMetaData (ref);
    vector<MetaData> v1 =
      md1.getVector (MetaData::ATTR_LIST_TYPE_ATTRS);

    for (int i = 0; i < v1.size (); ++i)
    {
      MetaData md2 = (MetaData)v1[i];
      cout << "Column Name :" <<
        (md2.getString(MetaData::ATTR_NAME)) << endl;
      cout << " Data Type :" <<
        (printType (md2.getInt(MetaData::ATTR_DATA_TYPE))) << endl;
      cout << " Size :" << md2.getInt(MetaData::ATTR_DATA_SIZE) << endl;
      cout << " Precision :" << md2.getInt(MetaData::ATTR_PRECISION) << endl;
      cout << " Scale :" << md2.getInt(MetaData::ATTR_SCALE) << endl << endl;
    }

    cout << "describe_type - done" << endl;
  } // end of describe_type()

  // Describing a table
  void describe_table ()
  {
    cout << "Describing the table - _TAB1" << endl;
    vector<MetaData> v1;
    MetaData metaData = conn->getMetaData("PUBLISHER_TAB");
    cout << "Object name:" <<
      (metaData.getString(MetaData::ATTR_OBJ_NAME)) << endl;
    cout << "Schema:" <<
      (metaData.getString(MetaData::ATTR_OBJ_SCHEMA)) << endl;
    if (metaData.getInt(MetaData::ATTR_PTYPE) ==
      MetaData::PTYPE_TABLE)
    {
      cout << "_TAB1 is a table" << endl;
    }
    else
      cout << "_TAB1 is not a table" << endl;
    if (metaData.getBoolean(MetaData::ATTR_PARTITIONED))
      cout << "Table is partitioned" << endl;
    else
      cout << "Table is not partitioned" << endl;
    if (metaData.getBoolean(MetaData::ATTR_IS_TEMPORARY))
      cout << "Table is temporary" << endl;
    else
      cout << "Table is not temporary" << endl;
    if (metaData.getBoolean(MetaData::ATTR_IS_TYPED))
      cout << "Table is typed" << endl;
    else
      cout << "Table is not typed" << endl;
    if (metaData.getBoolean(MetaData::ATTR_CLUSTERED))
      cout << "Table is clustered" << endl;
    else
      cout << "Table is not clustered" << endl;
    if (metaData.getBoolean(MetaData::ATTR_INDEX_ONLY))
      cout << "Table is Index-only" << endl;
    else
      cout << "Table is not Index-only" << endl;
    cout << "Duration:";
    switch (metaData.getInt(MetaData::ATTR_DURATION))
    {
      case MetaData::DURATION_SESSION : cout << "Connection" << endl;
                                                 break;
      case MetaData::DURATION_TRANS   : cout << "Transaction" << endl;
                                                 break;
      case MetaData::DURATION_NULL  : cout << "Table not temporary" << endl;
                                                 break;
    }
    try
    {
      cout << "Data Block Address:" <<
        metaData.getUInt (MetaData::ATTR_RDBA) << endl;
    }
    catch (SQLException ex)
    {
      cout << ex.getMessage() << endl;
    }
    try
    {
      cout << "Tablespace:" <<
        metaData.getInt (MetaData::ATTR_TABLESPACE) << endl;
    }
    catch (SQLException ex)
    {
      cout << ex.getMessage() << endl;
    }
    try
    {
      cout << "Object Id:" <<
        metaData.getUInt(MetaData::ATTR_OBJID) << endl;
    }
    catch (SQLException ex)
    {
      cout << ex.getMessage() << endl;
    }

    int columnCount = metaData.getInt(MetaData::ATTR_NUM_COLS);
    cout << "Number of Columns : " << columnCount << endl;

    v1 =  metaData.getVector(MetaData::ATTR_LIST_COLUMNS);
    for(int i=0; i < v1.size(); i++)
    {
      MetaData md = v1[i];
      cout << " Column Name :" <<
        (md.getString(MetaData::ATTR_NAME)) << endl;
      cout << " Data Type :" <<
        (printType (md.getInt(MetaData::ATTR_DATA_TYPE))) << endl;
      cout << " Size :" << md.getInt(MetaData::ATTR_DATA_SIZE) << endl;
      cout << " Precision :" << md.getInt(MetaData::ATTR_PRECISION) << endl;
      cout << " Scale :" << md.getInt(MetaData::ATTR_SCALE) << endl;
      bool isnull = md.getBoolean(MetaData::ATTR_IS_NULL);
      if (isnull)
        cout << " Allows null" << endl;
      else
        cout << " Does not allow null" << endl;
    }
    cout << "describe_table - done" << endl;
  }  // end of describe_table ()

  // Describing a procedure
  void describe_proc ()
  {
    cout << "Describing the procedure - DEMO_PROC" << endl;
    MetaData metaData = conn->getMetaData("DEMO_PROC");
    vector<MetaData> v1 =
      metaData.getVector ( MetaData::ATTR_LIST_ARGUMENTS );
    cout << "The number of arguments are:" << v1.size() << endl;
    cout << "Object Name :" <<
      (metaData.getString(MetaData::ATTR_OBJ_NAME)) << endl;
    cout << "Schema Name :" <<
      (metaData.getString(MetaData::ATTR_OBJ_SCHEMA)) << endl;
    if (metaData.getInt(MetaData::ATTR_PTYPE) == MetaData::
      PTYPE_PROC)
    {
      cout << "DEMO_PROC is a procedure" << endl;
    }
    else
    {
      if (metaData.getInt(MetaData::ATTR_PTYPE) == MetaData::
        PTYPE_FUNC)
      {
        cout << "HIKE is a function" << endl;
      }
    }
    try
    {
    cout << "Object Id:" <<
      metaData.getUInt(MetaData::ATTR_OBJ_ID) << endl;
    }
    catch (SQLException ex)
    {
      cout << ex.getMessage() << endl;
    }
    try
    {
      cout << "Name :" <<
        (metaData.getString(MetaData::ATTR_NAME)) << endl;
    }
    catch (SQLException ex)
    {
      cout << ex.getMessage() << endl;
    }

    if (metaData.getBoolean(MetaData::ATTR_IS_INVOKER_RIGHTS))
      cout << "It is Invoker-rights" << endl;
    else
      cout << "It is not Invoker-rights" << endl;
    cout << "Overload Id:" <<
      metaData.getInt(MetaData::ATTR_OVERLOAD_ID) << endl;

    for(int i=0; i < v1.size(); i++)
    {
      MetaData md = v1[i];
      cout << "Column Name :" <<
        (md.getString(MetaData::ATTR_NAME)) << endl;
      cout << "DataType :" <<
        (printType (md.getInt(MetaData::ATTR_DATA_TYPE)))
        << endl;
      cout << "Argument Mode:";
      int mode = md.getInt (MetaData::ATTR_IOMODE);
      if (mode == 0)
        cout << "IN" << endl;
      if (mode == 1)
        cout << "OUT" << endl;
      if (mode == 2)
        cout << "IN/OUT" << endl;
      cout << "Size :" <<
        md.getInt(MetaData::ATTR_DATA_SIZE) << endl;
      cout << "Precision :" <<
        md.getInt(MetaData::ATTR_PRECISION) << endl;
      cout << "Scale :" <<
        md.getInt(MetaData::ATTR_SCALE) << endl;
      int isNull = md.getInt ( MetaData::ATTR_IS_NULL);
      if (isNull != 0)
        cout << "Allows null," << endl;
      else
        cout << "Does not allow null," << endl;

      int hasDef = md.getInt ( MetaData::ATTR_HAS_DEFAULT);
      if (hasDef != 0)
        cout << "Has Default" << endl;
      else
        cout << "Does not have Default" << endl;
    }
    cout << "test1 - done" << endl;
  }

  // Method which prints the data type
  string printType (int type)
  {
    switch (type)
    {
      case OCCI_SQLT_CHR : return "VARCHAR2";
                           break;
      case OCCI_SQLT_NUM : return "NUMBER";
                           break;
      case OCCIINT : return "INTEGER";
                           break;
      case OCCIFLOAT : return "FLOAT";
                           break;
      case OCCI_SQLT_STR : return "STRING";
                           break;
      case OCCI_SQLT_VNU : return "VARNUM";
                           break;
      case OCCI_SQLT_LNG : return "LONG";
                           break;
      case OCCI_SQLT_VCS : return "VARCHAR";
                           break;
      case OCCI_SQLT_RID : return "ROWID";
                           break;
      case OCCI_SQLT_DAT : return "DATE";
                           break;
      case OCCI_SQLT_VBI : return "VARRAW";
                           break;
      case OCCI_SQLT_BIN : return "RAW";
                           break;
      case OCCI_SQLT_LBI : return "LONG RAW";
                           break;
      case OCCIUNSIGNED_INT : return "UNSIGNED INT";
                           break;
      case OCCI_SQLT_LVC : return "LONG VARCHAR";
                           break;
      case OCCI_SQLT_LVB : return "LONG VARRAW";
                           break;
      case OCCI_SQLT_AFC : return "CHAR";
                           break;
      case OCCI_SQLT_AVC : return "CHARZ";
                           break;
      case OCCI_SQLT_RDD : return "ROWID";
                           break;
      case OCCI_SQLT_NTY : return "NAMED DATA TYPE";
                           break;
      case OCCI_SQLT_REF : return "REF";
                           break;
      case OCCI_SQLT_CLOB: return "CLOB";
                           break;
      case OCCI_SQLT_BLOB: return "BLOB";
                           break;
      case OCCI_SQLT_FILE: return "BFILE";
                           break;
      default: return "";
    }
  } // End of printType (int)

  
}; // end of class occidesc

int main (void)
{
  string user = "hr";
  string passwd = "hr";
  string db = "";

  cout << "occidesc - Describing the various objects of the database" << endl;
  try{
     occidesc *demo = new occidesc (user, passwd, db); 
  demo->describe_table();
  demo->describe_type();
  demo->describe_proc();
  delete demo;
  }
  catch (SQLException ex){
      cout << ex.getMessage() << endl;
  }
 
}// end of main ()
