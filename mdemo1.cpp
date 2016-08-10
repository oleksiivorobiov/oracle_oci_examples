#include "mdemo1.h"
#include "mdemo1m.h"

// global Oracle variables

Environment     *env;
Connection      *conn;
Statement       *stmt;
ResultSet       *rs;

void initialize() {

  // Create environment
  env = Environment::createEnvironment(Environment::OBJECT);

  // Call the OTT generated function to register the mappings
  mdemo1m(env);

  // Create Connection
  conn = env->createConnection( USERNAME, PASSWORD );

  // Create a statement
  stmt = conn->createStatement();
}

void terminateit() {

  //Terminate statement
  conn->terminateStatement(stmt);

  //Terminate connection
  env->terminateConnection(conn);

  //Terminate environment
  Environment::terminateEnvironment(env);
}

/* Do the work.
   The environment is set up.
   A single new entry is created in the Address table.
   Then it is committed.
*/
void dowrite() {

  // Create an Address
  ADDRESS_O *addr1 = new(conn, "ADDR_TAB") ADDRESS_O("GE", "1211");
  Ref<ADDRESS_O> addr1_ref = (Ref<ADDRESS_O>) addr1->getRef();

  // Create joe black
  FullName *name1= new FullName("Joe", "Black");

  Person *person1 =
    new(conn, "PERSON_TAB") Person(1,name1,addr1_ref);
  Ref<Person> person1_ref = (Ref<Person>) person1->getRef();

  // Display, using reference
  cout<<"-------------------"<<endl;
  person1_ref->displayInfo();
  cout<<"-------------------"<<endl;

  // Commit the changes
  conn->commit();

}

void doread()
{
  // Retrieve joe black
  string sel_joe = "SELECT REF(p) from person_tab p where \"id\" = 1";
  
  rs =stmt->executeQuery (sel_joe);
  
  // Get reference
  rs->next();
  Ref<Person> joe_ref = (Ref<Person>) rs->getRef(1);

  // Display, using reference
  cout<<"-------------------"<<endl;
  joe_ref->displayInfo();
  cout<<"-------------------"<<endl;
  stmt->closeResultSet(rs);

}

int main() {

  try {
    initialize();
    dowrite();
    doread();
    terminateit();
  }
  catch (SQLException &e) {
    cout << "SQL exception :" << e.getMessage() << endl;
  }

  return 0;
}
