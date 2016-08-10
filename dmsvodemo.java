// Copyright (c) 2004, 2005, Oracle. All rights reserved.  
// File: dmsvodemo.java
 import java.math.BigDecimal;

 import java.sql.PreparedStatement;
 import java.sql.ResultSet;
 import java.sql.ResultSetMetaData;
 import java.sql.SQLException;
 import java.sql.Statement;

 import java.text.DecimalFormat;
 import java.text.MessageFormat;

 import java.util.Collection;
 import java.util.Iterator;

 import java.util.Map;
 import javax.datamining.ExecutionHandle;
 import javax.datamining.ExecutionState;
 import javax.datamining.ExecutionStatus;
 import javax.datamining.JDMException;
 import javax.datamining.MiningAlgorithm;
 import javax.datamining.MiningFunction;
 import javax.datamining.NamedObject;
 import javax.datamining.algorithm.svm.KernelFunction;
 import javax.datamining.algorithm.svm.classification.SVMClassificationSettings;
 import javax.datamining.algorithm.svm.classification.SVMClassificationSettingsFactory;
 import javax.datamining.base.AlgorithmSettings;
 import javax.datamining.base.Model;
 import javax.datamining.base.Task;
 import javax.datamining.data.AttributeDataType;
 import javax.datamining.data.CategorySet;
 import javax.datamining.data.ModelSignature;
 import javax.datamining.data.PhysicalAttribute;
 import javax.datamining.data.PhysicalAttributeFactory;
 import javax.datamining.data.PhysicalAttributeRole;
 import javax.datamining.data.PhysicalDataSet;
 import javax.datamining.data.PhysicalDataSetFactory;
 import javax.datamining.data.SignatureAttribute;
 import javax.datamining.modeldetail.svm.SVMClassificationModelDetail;
 import javax.datamining.resource.ConnectionSpec;
 import javax.datamining.supervised.classification.ClassificationApplySettings;
 import javax.datamining.supervised.classification.ClassificationApplySettingsFactory;
 import javax.datamining.supervised.classification.ClassificationModel;
 import javax.datamining.supervised.classification.ClassificationSettings;
 import javax.datamining.supervised.classification.ClassificationSettingsFactory;
 import javax.datamining.task.BuildTask;
 import javax.datamining.task.BuildTaskFactory;
 import javax.datamining.task.apply.DataSetApplyTask;
 import javax.datamining.task.apply.DataSetApplyTaskFactory;

 import oracle.dmt.jdm.algorithm.svm.classification.OraSVMClassificationSettings;
 import oracle.dmt.jdm.base.OraBuildSettings;
 import oracle.dmt.jdm.modeldetail.svm.OraSVMClassificationModelDetail;
 import oracle.dmt.jdm.resource.OraConnection;
 import oracle.dmt.jdm.resource.OraConnectionFactory;
 import oracle.dmt.jdm.supervised.classification.OraClassificationModel;
 import oracle.dmt.jdm.transform.normalize.OraNormalizeTransformImpl;
/**
*   This demo program describes how to use the Oracle Data Mining (ODM) Java API
* to solve a classification problem using Support Vector Machines (SVM) 
* algorithm.
* ------------------------------------------------------------------------------
*                             PROBLEM DEFINITION
* ------------------------------------------------------------------------------
* Given demographic about a set of customers that are known to have 
* an affinity card, 1) find the most atypical members of this group 
* (outlier identification), 2) discover the common demographic 
* characteristics of the most typical customers with affinity  card, 
* and 3) compute how typical a given new/hypothetical customer is.
* ------------------------------------------------------------------------------
*                             DATA DESCRIPTION
* ------------------------------------------------------------------------------
*   Data for this demo is composed from base tables in the Sales History (SH) 
* Schema. The SH schema is an Oracle Database Sample Schema that has the customer
* demographics, purchasing, and response details for the previous affinity card 
* programs. Data exploration and preparing the data is a common step before 
* doing data mining. Here in this demo, the following views are created in the user 
* schema using CUSTOMERS, COUNTRIES and SUPPLIMENTARY_DEMOGRAPHICS tables.
* 
* MINING_DATA_ONE_CLASS_V:
*   This view collects the previous customers' demographics, purchasing, and affinity 
*   card response details for building the model.
*
* ------------------------------------------------------------------------------
*                             DATA MINING PROCESS
* ------------------------------------------------------------------------------
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*     illustrates how to build a classification one-class model using the SVM 
*     algorithm.
*   
*   Test Model:
*     Classification model performance can be evaluated by computing test 
*     metrics like accuracy, confusion matrix, lift and ROC. The testModel() or 
*     computeTestMetrics() method illustrates how to perform the test operation to 
*     compute various metrics.  We skip the test model in this demo.
*   
*   Apply Model:
*     Predicting the target attribute values is the prime function of 
*     classification one-class models.  Depending on the business case, the 
*     model can be scored against the build data or against new, previously
*     unseen data. New apply data needs to undergo the same transformations as 
*     the build data.
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide 
*   for guidelines for executing this demo program.
*/


public class dmsvodemo extends Object {
  // Connection related data members
  private static javax.datamining.resource.Connection m_dmeConn ;
  private static javax.datamining.resource.ConnectionFactory m_dmeConnFactory;
  // Object factories used in this demo program
  private static PhysicalDataSetFactory m_pdsFactory;
  private static PhysicalAttributeFactory m_paFactory;
  private static ClassificationSettingsFactory m_clasFactory;
  private static SVMClassificationSettingsFactory m_svmoFactory;
  private static BuildTaskFactory m_buildFactory;
  private static DataSetApplyTaskFactory m_dsApplyFactory;
  private static ClassificationApplySettingsFactory m_applySettingsFactory;
  // Global constants 
  private static DecimalFormat m_df = new DecimalFormat("##.####");
  // Global data members 
  private static OraNormalizeTransformImpl m_normalizeDataXform;
  
  public static void main( String args[] ) { 
    try { 
        if (args.length != 3 ) {
          System.out.println("Usage: java dmsvodemo <Host name>:<Port>:<SID> <User Name> <Password>");
          return;
        }
        String uri =  args[0];
        String name =  args[1]; 
        String password =  args[2]; 
        // 1. Login to the Data Mining Engine
        m_dmeConnFactory = new OraConnectionFactory();
        ConnectionSpec connSpec = m_dmeConnFactory.getConnectionSpec();
        connSpec.setURI("jdbc:oracle:thin:@"+uri); 
        connSpec.setName(name); 
        connSpec.setPassword(password);
        m_dmeConn = m_dmeConnFactory.getConnection(connSpec); 
        clean();
        // 3. Initialize factories for mining objects
        initFactories();
        // 4. Build model - Please see dmnbdemo.java to see how to build a model 
        //                  with supplied prior probability.
        buildModel();
        // 5. Apply the model
        applyModel();
    } catch(Exception anyExp) {
      anyExp.printStackTrace(System.out);
    } finally {
      try {
        // 7. Logout from the Data Mining Engine
        m_dmeConn.close();
      } catch(Exception anyExp1) { } // Ignore
    }
  }

  /**
   * Initialize all object factories used in the demo program.
   * 
   * @exception JDMException if factory initalization failed
   */ 
  public static void initFactories() throws JDMException
  {
    m_pdsFactory = (PhysicalDataSetFactory)m_dmeConn.getFactory(
      "javax.datamining.data.PhysicalDataSet");
    m_paFactory = (PhysicalAttributeFactory)m_dmeConn.getFactory(
      "javax.datamining.data.PhysicalAttribute");
    m_clasFactory = (ClassificationSettingsFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.classification.ClassificationSettings");
    m_svmoFactory = (SVMClassificationSettingsFactory)m_dmeConn.getFactory(
      "javax.datamining.algorithm.svm.classification.SVMClassificationSettings");
    m_buildFactory = (BuildTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.task.BuildTask");
    m_dsApplyFactory = (DataSetApplyTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.task.apply.DataSetApplyTask");
    m_applySettingsFactory = (ClassificationApplySettingsFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.classification.ClassificationApplySettings");
  }

  /**
   *   This method illustrates how to build a mining model using the
   * SVMO_NORM_DATA_BUILD_JDM dataset and classification settings with 
   * SVM algorithm. 
   * 
   *    By default, the SVM algorithm chooses a kernel type automatically. This 
   * choice can be overriden by the user. Linear kernel is preferred for high 
   * dimensional data, and Gaussian kernel for low dimensional data. Here we 
   * will let the data mining server choose the kernel type automatically, and
   * define no target since this is a one-class classification demo.
   * 
   * @exception JDMException if model build failed
   */
  public static void buildModel() throws JDMException 
  {
      System.out.println("---------------------------------------------------");    
      System.out.println("--- Build Model                                 ---");
      System.out.println("---------------------------------------------------");
     // 1. Create & save PhysicalDataSpecification 
      PhysicalDataSet buildData = 
                        m_pdsFactory.create("MINING_DATA_BUILD_V", false);
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      buildData.addAttribute(pa);
      m_dmeConn.saveObject("svmoBuildData_jdm", buildData, true);
     // 2. Create & save Mining Function Settings
      // Create SVMC algorithm settings
      SVMClassificationSettings svmoAlgo = m_svmoFactory.create();
      // Examples settings are:
      // - Select a different rate of outliers in the data (default 0.1)
      // svmoAlgo.setOutlierRate(0.05f); 
      // - Select a kernel type (default kernel: selected by the algorithm)
      // svmoAlgo.setKernelFunction(KernelFunction.kLinear);
      // svmoAlgo.setKernelFunction(KernelFunction.kGaussian);
      // - Turn off active learning (enabled by default)
      // svmoAlgo.setActiveLearning(false);
      //
      //Create ClassificationSettings
      ClassificationSettings buildSettings = m_clasFactory.create();
      buildSettings.setAlgorithmSettings(svmoAlgo);
      //Set auto data preparation on
      ((OraBuildSettings)buildSettings).useAutomatedDataPreparations(true);
      m_dmeConn.saveObject("svmoBuildSettings_jdm", buildSettings, true);
     // 3. Create, save & execute Build Task      
      BuildTask buildTask = m_buildFactory.create(
                     "svmoBuildData_jdm", //Build data specification
                     "svmoBuildSettings_jdm", //Mining function settings name
                     "svmoModel_jdm" //Mining model name
                     );                          
      buildTask.setDescription("svmoBuildTask_jdm");
      executeTask(buildTask, "svmoBuildTask_jdm");
     //4. Restore the model from the DME and explore the details of the model
      ClassificationModel model = 
        (ClassificationModel)m_dmeConn.retrieveObject(
          "svmoModel_jdm", NamedObject.model);
      // Display model build settings
      ClassificationSettings retrievedBuildSettings = 
                               (ClassificationSettings)model.getBuildSettings();
      if(buildSettings == null) 
          System.out.println("Failure to restore build settings.");
      else 
          displayBuildSettings(retrievedBuildSettings, "svmoBuildSettings_jdm");
      // Display model signature    
      displayModelSignature((Model)model);
      // Display model details
      displaySVMOModelDetails((Model)model);
  }
  
  /**
   *   This method illustrates how to apply the mining model on the
   * SVMO_NORM_DATA_BUILD_JDM dataset to predict customer
   * response. After completion of the task, the apply output table with the
   * predicted results is created at the user specified location. 
   * 
   * @exception JDMException if model apply failed
   */ 
  public static void applyModel() throws JDMException
  {
      System.out.println("---------------------------------------------------");    
      System.out.println("--- Apply Model                                 ---");
      System.out.println("---------------------------------------------------");
    // 1. Create & save PhysicalDataSpecification
      PhysicalDataSet applyData = m_pdsFactory.create(
                                            "MINING_DATA_BUILD_V", false );
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      applyData.addAttribute( pa );
      m_dmeConn.saveObject( "svmoApplyData_jdm", applyData, true );
    // 2. Create & save ClassificationApplySettings
      ClassificationApplySettings clasAS = m_applySettingsFactory.create();
      m_dmeConn.saveObject( "svmoApplySettings_jdm", clasAS, true);           
    // 3. Create, store & execute apply Task
     DataSetApplyTask applyTask = m_dsApplyFactory.create(
                              "svmoApplyData_jdm", "svmoModel_jdm", 
                              "svmoApplySettings_jdm", "SVMO_APPLY_OUTPUT_JDM");
     executeTask(applyTask, "svmoApplyTask_jdm");
     // 4. Display apply result 
     displayTable("SVMO_APPLY_OUTPUT_JDM",       // apply output table name
                  
                  "where PREDICTION = 0",        // 0 means the record does not 
                                                 // belong to the population 
                                                 
                  "order by PROBABILITY DESC",   // List from top probability to 
                                                 // bottom
                                                 
                  10);                           // List the first 10 rows
  }  
  
  /**
   * This method stores the given task with the specified name in the DMS 
   * and submits the task for asynchronous execution in the DMS. After 
   * completing the task successfully it returns true. If there is a task 
   * failure, then it prints error description and returns false.
   * 
   * @param taskObj task object
   * @param taskName name of the task
   * 
   * @return boolean returns true when the task is successful
   * @exception JDMException if task execution failed
   */
  public static boolean executeTask(Task taskObj, String taskName)
    throws JDMException {
    boolean isTaskSuccess = false;
    m_dmeConn.saveObject(taskName, taskObj, true);
    ExecutionHandle execHandle = m_dmeConn.execute(taskName);
    System.out.print(taskName + " is started, please wait. ");
    //Wait for completion of the task
    ExecutionStatus status = execHandle.waitForCompletion(Integer.MAX_VALUE);    
    //Check the status of the task after completion
    isTaskSuccess = status.getState().equals(ExecutionState.success);
    if( isTaskSuccess ) {
      //Task completed successfully
      System.out.println(taskName + " is successful.");
    } else {//Task failed
      System.out.println(taskName + " is failed.\nFailure Description: " + 
        status.getDescription() );
    }
    return isTaskSuccess;
  }

  private static void displayBuildSettings(
    ClassificationSettings clasSettings, String buildSettingsName) 
  {
    // Display build settings table
    System.out.println("BuildSettings Details from the " 
      + buildSettingsName + " table:");
      displayTable(buildSettingsName, "", "order by SETTING_NAME");
    // Display build settings object obtained from the model
    System.out.println("BuildSettings Details from the " 
      + buildSettingsName + " object:");    
    String objName = clasSettings.getName();
    if(objName != null)
      System.out.println("Name = " + objName);
    String objDescription = clasSettings.getDescription();
    if(objDescription != null)
      System.out.println("Description = " + objDescription);
    java.util.Date creationDate = clasSettings.getCreationDate();
    String creator = clasSettings.getCreatorInfo();     
    String targetAttrName = clasSettings.getTargetAttributeName();
    System.out.println("Target attribute name = " + targetAttrName);
    AlgorithmSettings algoSettings = clasSettings.getAlgorithmSettings();
    if(algoSettings == null)
      System.out.println("Failure: clasSettings.getAlgorithmSettings() returns null");
    MiningAlgorithm algo = algoSettings.getMiningAlgorithm();
    if(algo == null) System.out.println("Failure: algoSettings.getMiningAlgorithm() returns null");
    System.out.println("Algorithm Name: " + algo.name());
    MiningFunction function = clasSettings.getMiningFunction();
    if(function == null) System.out.println("Failure: clasSettings.getMiningFunction() returns null");
    System.out.println("Function Name: " + function.name());
    // List of SVM algorithm settings availanle in one class model
    KernelFunction kernelFunction = ((OraSVMClassificationSettings)algoSettings).getKernelFunction();
    System.out.println("Kernel Function: " + kernelFunction.name());
    double doubleValue = ((OraSVMClassificationSettings)algoSettings).getTolerance();
    System.out.println("Tolerance: " + m_df.format(doubleValue));
    boolean enable = ((OraSVMClassificationSettings)algoSettings).getActiveLearning();
    System.out.println("Is Active Learning enabled? " + enable);
    doubleValue = ((OraSVMClassificationSettings)algoSettings).getOutlierRate();
    System.out.println("Outlier Rate: " + m_df.format(doubleValue));
    doubleValue = ((OraSVMClassificationSettings)algoSettings).getStandardDeviation();
    System.out.println("Standard Deviation: " + m_df.format(doubleValue));
    int intValue = ((OraSVMClassificationSettings)algoSettings).getKernelCacheSize();
    System.out.println("Kernel Cache Size: " + intValue);
  }

  /**
   * This method displayes SVMO model signature.
   * 
   *  @param model model object
   *  @exception JDMException if failed to retrieve model signature
   */
  public static void displayModelSignature(Model model) throws JDMException
  {
    String modelName = model.getName();
    System.out.println("Model Name: " + modelName);
    ModelSignature modelSignature = model.getSignature();
    System.out.println("Model Signature: ( Attribute Name, Attribute Type )");
    MessageFormat mfSign = new MessageFormat("                        ( {0}, {1} )");
    String[] vals = new String[2]; 
    Collection sortedSet = modelSignature.getAttributes();
    Iterator attrIterator = sortedSet.iterator();
    while(attrIterator.hasNext()) 
    {
      SignatureAttribute attr = (SignatureAttribute)attrIterator.next();
      vals[0] = attr.getName();
      vals[1] = attr.getDataType().name();
      System.out.println( mfSign.format(vals) );
    }
  }
  
  /**
   * This method displayes SVMO model details.  The coefficient indicates the 
   * relative influence of a given (attribute, value) pair on the target value. 
   * A negative coefficient value indicates a negative influence.
   * 
   * @param svmcModelDetails svm classification model details object
   * @exception JDMException if failed to retrieve model details
   */ 
  public static void displaySVMOModelDetails(
    Model model) throws JDMException
  {
    // Obtains model details
    SVMClassificationModelDetail svmcModelDetails = 
      (SVMClassificationModelDetail)model.getModelDetail();
    // Is linear model? 
    System.out.println("Is linear model? " + svmcModelDetails.isLinearSVMModel() );
    // Available targets
    CategorySet targetSet = ((OraClassificationModel)model).getTargetCategorySet();
    Object[] targets = targetSet.getValues();
    // Attribute coefficient value will be available if it is a linear model
    if (svmcModelDetails.isLinearSVMModel())
    {
      System.out.println("Model Deatils: ( Target Value, Attribute Name, Attribute Value, Coefficient)");
      MessageFormat mfDetails = new MessageFormat("               ( {0}, {1}, {2}, {3} )");
      String[] vals = new String[4]; 
      for (int row =0; row < targets.length; row++)
      {
        // Print bias of the current target value by invoking getBias method.
        vals[0] =(String) targets[row].toString();
        vals[1] = "";
        vals[2] = "";
        vals[3] = m_df.format(svmcModelDetails.getBias(targets[row])) + " (Bias)"; 
        System.out.println( mfDetails.format(vals) );
        //Get the signature attributes
        ModelSignature modelSignature = model.getSignature();      
        Collection sortedSet = modelSignature.getAttributes();
        Iterator attrIterator = sortedSet.iterator();
        
        while(attrIterator.hasNext()) 
        {
          SignatureAttribute attr = (SignatureAttribute)attrIterator.next();
          // Print all attribute coefficients -- The FIRST row in the SVM model 
          // details output shows the value for SVM bias under the COEFFICIENT 
          // column.
          Map attrCoefficientMap = ((OraSVMClassificationModelDetail)svmcModelDetails).getCoefficients(targets[row],attr.getName());
          Object[] attrVals = attrCoefficientMap.keySet().toArray();
          if(attrVals != null) {
            for(int iAttr=0; iAttr < attrVals.length; iAttr++) 
            {            
              vals[1] = attr.getName();
              vals[2] = "";
              if(attrVals[iAttr] != null)
                vals[2] = attrVals[iAttr].toString();
              vals[3] = "";
                Number coefficient = (Number)attrCoefficientMap.get(attrVals[iAttr]);
                if(coefficient != null)
                  vals[3] = m_df.format(coefficient);
              System.out.println( mfDetails.format(vals) );
            }
          }
        }
      }
    }
  }

  private static void displayTable(String tableName, String whereCause, String orderByColumn) 
  {
      displayTable(tableName, whereCause, orderByColumn, 0); // display all rows
  }
  
  private static void displayTable(String tableName, String whereCause, String orderByColumn, int maxRow) 
  {
    StringBuffer emptyCol = new StringBuffer("                    ");
    java.sql.Connection dbConn = 
      ((OraConnection)m_dmeConn).getDatabaseConnection();
    PreparedStatement pStmt = null;
    ResultSet rs = null;
    try 
    {
      pStmt = dbConn.prepareStatement("SELECT * FROM " + tableName + " " + whereCause + " " + orderByColumn);
      rs = pStmt.executeQuery();      
      ResultSetMetaData rsMeta = rs.getMetaData();
      int colCount = rsMeta.getColumnCount();
      StringBuffer header = new StringBuffer();
      System.out.println("Table : " + tableName);
      // Build table header
      for(int iCol=1; iCol<=colCount; iCol++) 
      {
        String colName = rsMeta.getColumnName(iCol);
        header.append(emptyCol.replace(0, colName.length(), colName));
        emptyCol = new StringBuffer("                    ");
      }
      System.out.println(header.toString());
      // Write table data
      int row = 0;
      while(rs.next()) 
      {
        row = row + 1;
        if (row > maxRow & maxRow > 0)
          break;
          
        StringBuffer rowContent = new StringBuffer();
        for(int iCol=1; iCol<=colCount; iCol++) 
        {
          int sqlType = rsMeta.getColumnType(iCol);          
          Object obj = rs.getObject(iCol);
          String colContent = null;
          
          if(obj instanceof java.lang.Number) 
          {
            try 
            {
              BigDecimal bd = (BigDecimal)obj;
              if(bd.scale() > 5) 
              {
                colContent = m_df.format(obj);
              } else 
              {
                colContent = bd.toString();
              }
            } catch(Exception anyExp) {
              colContent = m_df.format(obj);
            }
          } else 
          {
            if(obj == null)
              colContent = "NULL";
            else
              colContent = obj.toString();
          }
          rowContent.append(" "+emptyCol.replace(0, colContent.length(), colContent));
          emptyCol = new StringBuffer("                    ");
        }
        System.out.println(rowContent.toString());
      }
    } catch(Exception anySqlExp) {
      anySqlExp.printStackTrace();
    } // Ignore
  }
  
  private static void clean() 
  {
    java.sql.Connection dbConn = 
      ((OraConnection)m_dmeConn).getDatabaseConnection();
    Statement stmt = null;
   //Drop prepared views
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP VIEW SVMO_NORM_DATA_BUILD_JDM");
    } catch(SQLException anySqlExp) {} // Ignore
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    }
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP VIEW SVMO_NORM_DATA_APPLY_JDM");
    } catch(SQLException anySqlExp) {} // Ignore
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    }
    //Drop apply output table
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP TABLE SVMO_APPLY_OUTPUT_JDM");
    } catch(SQLException anySqlExp) {} // Ignore
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    }
    //Drop the model
    try {
      m_dmeConn.removeObject("svmoModel_jdm", NamedObject.model);
    } catch(JDMException jdmExp) {}
  }
}
