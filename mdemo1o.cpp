#ifndef OTT_USERCODE_START
# define OTT_USERCODE_START
#endif

#ifndef OTT_USERCODE_END
# define OTT_USERCODE_END
#endif

#ifndef MDEMO1_ORACLE
# include "mdemo1.h"
#endif


/*****************************************************************/
//  generated method implementations for the FULL_NAME_O object type.
/*****************************************************************/

void *FULL_NAME_O::operator new(size_t size)
{
  return oracle::occi::PObject::operator new(size);
}

void *FULL_NAME_O::operator new(size_t size, const oracle::occi::Connection * sess,
  const OCCI_STD_NAMESPACE::string& table)
{
  return oracle::occi::PObject::operator new(size, sess, table, 
            (char *) "SCOTT.FULL_NAME_O");
}

void *FULL_NAME_O::operator new(size_t size, void *ctxOCCI_)
{
 return oracle::occi::PObject::operator new(size, ctxOCCI_);
}

void *FULL_NAME_O::operator new(size_t size,
    const oracle::occi::Connection *sess,
    const OCCI_STD_NAMESPACE::string &tableName, 
    const OCCI_STD_NAMESPACE::string &typeName,
    const OCCI_STD_NAMESPACE::string &tableSchema, 
    const OCCI_STD_NAMESPACE::string &typeSchema)
{
  return oracle::occi::PObject::operator new(size, sess, tableName,
        typeName, tableSchema, typeSchema);
}

OCCI_STD_NAMESPACE::string FULL_NAME_O::getSQLTypeName() const
{
  return OCCI_STD_NAMESPACE::string("SCOTT.FULL_NAME_O");
}

void FULL_NAME_O::getSQLTypeName(oracle::occi::Environment *env, void **schemaName,
    unsigned int &schemaNameLen, void **typeName, unsigned int &typeNameLen) const
{
  PObject::getSQLTypeName(env, &FULL_NAME_O::readSQL, schemaName,
        schemaNameLen, typeName, typeNameLen);
}

FULL_NAME_O::FULL_NAME_O()
{
}

OTT_USERCODE_START

FullName::FullName(void *ctxOCCI_): FULL_NAME_O(ctxOCCI_)
{
}

OTT_USERCODE_END

void *FULL_NAME_O::readSQL(void *ctxOCCI_)
{
  FullName *objOCCI_ = new(ctxOCCI_) FullName(ctxOCCI_);
  oracle::occi::AnyData streamOCCI_(ctxOCCI_);

  try
  {
    if (streamOCCI_.isNull())
      objOCCI_->setNull();
    else
      objOCCI_->readSQL(streamOCCI_);
  }
  catch (oracle::occi::SQLException& excep)
  {
    delete objOCCI_;
    excep.setErrorCtx(ctxOCCI_);
    return (void *)NULL;
  }
  return (void *)objOCCI_;
}

void FULL_NAME_O::readSQL(oracle::occi::AnyData& streamOCCI_)
{
   first_name = streamOCCI_.getString();
   last_name = streamOCCI_.getString();
}

void FULL_NAME_O::writeSQL(void *objectOCCI_, void *ctxOCCI_)
{
  FULL_NAME_O *objOCCI_ = (FULL_NAME_O *) objectOCCI_;
  oracle::occi::AnyData streamOCCI_(ctxOCCI_);

  try
  {
    if (objOCCI_->isNull())
      streamOCCI_.setNull();
    else
      objOCCI_->writeSQL(streamOCCI_);
  }
  catch (oracle::occi::SQLException& excep)
  {
    excep.setErrorCtx(ctxOCCI_);
  }
  return;
}

void FULL_NAME_O::writeSQL(oracle::occi::AnyData& streamOCCI_)
{
   streamOCCI_.setString(first_name);
   streamOCCI_.setString(last_name);
}

FULL_NAME_O::~FULL_NAME_O()
{
  int i;
}

OTT_USERCODE_START

// initialize FullName
FullName::FullName(string FirstName, string LastName)
{
  first_name = FirstName;
  last_name = LastName;
}

// display all the information in FullName
void FullName::displayInfo()
{
  cout << "FIRST NAME is " << first_name << endl;
  cout << "LAST NAME is " << last_name << endl;
}

FullName::~FullName()
{
}

OTT_USERCODE_END

/*****************************************************************/
//  generated method implementations for the ADDRESS_O object type.
/*****************************************************************/

void *ADDRESS_O::operator new(size_t size)
{
  return oracle::occi::PObject::operator new(size);
}

void *ADDRESS_O::operator new(size_t size, const oracle::occi::Connection * sess,
  const OCCI_STD_NAMESPACE::string& table)
{
  return oracle::occi::PObject::operator new(size, sess, table, 
            (char *) "SCOTT.ADDRESS_O");
}

void *ADDRESS_O::operator new(size_t size, void *ctxOCCI_)
{
 return oracle::occi::PObject::operator new(size, ctxOCCI_);
}

void *ADDRESS_O::operator new(size_t size,
    const oracle::occi::Connection *sess,
    const OCCI_STD_NAMESPACE::string &tableName, 
    const OCCI_STD_NAMESPACE::string &typeName,
    const OCCI_STD_NAMESPACE::string &tableSchema, 
    const OCCI_STD_NAMESPACE::string &typeSchema)
{
  return oracle::occi::PObject::operator new(size, sess, tableName,
        typeName, tableSchema, typeSchema);
}

OCCI_STD_NAMESPACE::string ADDRESS_O::getSQLTypeName() const
{
  return OCCI_STD_NAMESPACE::string("SCOTT.ADDRESS_O");
}

void ADDRESS_O::getSQLTypeName(oracle::occi::Environment *env, void **schemaName,
    unsigned int &schemaNameLen, void **typeName, unsigned int &typeNameLen) const
{
  PObject::getSQLTypeName(env, &ADDRESS_O::readSQL, schemaName,
        schemaNameLen, typeName, typeNameLen);
}

ADDRESS_O::ADDRESS_O()
{
}

void *ADDRESS_O::readSQL(void *ctxOCCI_)
{
  ADDRESS_O *objOCCI_ = new(ctxOCCI_) ADDRESS_O(ctxOCCI_);
  oracle::occi::AnyData streamOCCI_(ctxOCCI_);

  try
  {
    if (streamOCCI_.isNull())
      objOCCI_->setNull();
    else
      objOCCI_->readSQL(streamOCCI_);
  }
  catch (oracle::occi::SQLException& excep)
  {
    delete objOCCI_;
    excep.setErrorCtx(ctxOCCI_);
    return (void *)NULL;
  }
  return (void *)objOCCI_;
}

void ADDRESS_O::readSQL(oracle::occi::AnyData& streamOCCI_)
{
   state = streamOCCI_.getString();
   zip = streamOCCI_.getString();
}

void ADDRESS_O::writeSQL(void *objectOCCI_, void *ctxOCCI_)
{
  ADDRESS_O *objOCCI_ = (ADDRESS_O *) objectOCCI_;
  oracle::occi::AnyData streamOCCI_(ctxOCCI_);

  try
  {
    if (objOCCI_->isNull())
      streamOCCI_.setNull();
    else
      objOCCI_->writeSQL(streamOCCI_);
  }
  catch (oracle::occi::SQLException& excep)
  {
    excep.setErrorCtx(ctxOCCI_);
  }
  return;
}

void ADDRESS_O::writeSQL(oracle::occi::AnyData& streamOCCI_)
{
   streamOCCI_.setString(state);
   streamOCCI_.setString(zip);
}

ADDRESS_O::~ADDRESS_O()
{
  int i;
}

OTT_USERCODE_START

// initialize ADDRESS_O
ADDRESS_O::ADDRESS_O(string state_i, string zip_i)
{
 state = state_i;
 zip = zip_i;
}

// display all the information in ADDRESS_O
void ADDRESS_O::displayInfo()
{
  cout << "STATE is " << state << endl;
  cout << "ZIP is " << zip << endl;
}

OTT_USERCODE_END

/*****************************************************************/
//  generated method implementations for the PERSON_O object type.
/*****************************************************************/

void *PERSON_O::operator new(size_t size)
{
  return oracle::occi::PObject::operator new(size);
}

void *PERSON_O::operator new(size_t size, const oracle::occi::Connection * sess,
  const OCCI_STD_NAMESPACE::string& table)
{
  return oracle::occi::PObject::operator new(size, sess, table, 
            (char *) "SCOTT.PERSON_O");
}

void *PERSON_O::operator new(size_t size, void *ctxOCCI_)
{
 return oracle::occi::PObject::operator new(size, ctxOCCI_);
}

void *PERSON_O::operator new(size_t size,
    const oracle::occi::Connection *sess,
    const OCCI_STD_NAMESPACE::string &tableName, 
    const OCCI_STD_NAMESPACE::string &typeName,
    const OCCI_STD_NAMESPACE::string &tableSchema, 
    const OCCI_STD_NAMESPACE::string &typeSchema)
{
  return oracle::occi::PObject::operator new(size, sess, tableName,
        typeName, tableSchema, typeSchema);
}

OCCI_STD_NAMESPACE::string PERSON_O::getSQLTypeName() const
{
  return OCCI_STD_NAMESPACE::string("SCOTT.PERSON_O");
}

void PERSON_O::getSQLTypeName(oracle::occi::Environment *env, void **schemaName,
    unsigned int &schemaNameLen, void **typeName, unsigned int &typeNameLen) const
{
  PObject::getSQLTypeName(env, &PERSON_O::readSQL, schemaName,
        schemaNameLen, typeName, typeNameLen);
}

PERSON_O::PERSON_O()
{
   name = (FullName *) 0;
}

OTT_USERCODE_START

Person::Person(void *ctxOCCI_): PERSON_O(ctxOCCI_)
{
}

OTT_USERCODE_END

void *PERSON_O::readSQL(void *ctxOCCI_)
{
  Person *objOCCI_ = new(ctxOCCI_) Person(ctxOCCI_);
  oracle::occi::AnyData streamOCCI_(ctxOCCI_);

  try
  {
    if (streamOCCI_.isNull())
      objOCCI_->setNull();
    else
      objOCCI_->readSQL(streamOCCI_);
  }
  catch (oracle::occi::SQLException& excep)
  {
    delete objOCCI_;
    excep.setErrorCtx(ctxOCCI_);
    return (void *)NULL;
  }
  return (void *)objOCCI_;
}

void PERSON_O::readSQL(oracle::occi::AnyData& streamOCCI_)
{
   id = streamOCCI_.getNumber();
   name = (FullName *) streamOCCI_.getObject(&FullName::readSQL);
   addr = streamOCCI_.getRef();
}

void PERSON_O::writeSQL(void *objectOCCI_, void *ctxOCCI_)
{
  PERSON_O *objOCCI_ = (PERSON_O *) objectOCCI_;
  oracle::occi::AnyData streamOCCI_(ctxOCCI_);

  try
  {
    if (objOCCI_->isNull())
      streamOCCI_.setNull();
    else
      objOCCI_->writeSQL(streamOCCI_);
  }
  catch (oracle::occi::SQLException& excep)
  {
    excep.setErrorCtx(ctxOCCI_);
  }
  return;
}

void PERSON_O::writeSQL(oracle::occi::AnyData& streamOCCI_)
{
   streamOCCI_.setNumber(id);
   streamOCCI_.setObject(name);
   streamOCCI_.setRef(addr);
}

PERSON_O::~PERSON_O()
{
  int i;
  delete name;
}

OTT_USERCODE_START

// initialize Person
Person::Person(int id_i,
               FullName *name_i,
               Ref<ADDRESS_O>& addr_i)
{
  id = id_i;
  name = name_i;
  addr =addr_i ;
}

// Move Person from curr_addr to new_addr
void Person::move(const Ref<ADDRESS_O>& new_addr)
{
  addr = new_addr;
  this->markModified();   // mark the object as dirty
}

// Display all the information of Person
void Person::displayInfo() {
  cout << "ID is " << (int)id << endl;
  name->displayInfo();

  // de-referencing the Ref attribute using -> operator
  addr->displayInfo();

}

Person::~Person()
{
}

OTT_USERCODE_END
