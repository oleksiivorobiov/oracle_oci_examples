// Copyright (c) 2004, 2006, Oracle. All rights reserved.  
// File: dmsvrdemo.java
/**
*   This demo program describes how to use the Oracle Data Mining (ODM) Java API
* to solve a regression problem using Support Vector Machines (SVM) algorithm.
* ------------------------------------------------------------------------------
*                             PROBLEM DEFINITION
* ------------------------------------------------------------------------------
*   Given demographic, purchase, and affinity card membership data for a set of 
*   customers, predict customer's age. Since age is a continuous variable, this 
*   is a regression problem.
* ------------------------------------------------------------------------------
*                             DATA DESCRIPTION
* ------------------------------------------------------------------------------
*   Data for this demo is composed from base tables in the Sales History (SH) 
* Schema. The SH schema is an Oracle Database Sample Schema that has the customer
* demographics, purchasing, and response details for the 
* previous affinity card programs. Data exploration and preparing the data is a 
* common step before doing data mining. Here in this demo, the following views are 
* created in the user schema using CUSTOMERS, COUNTRIES, and 
* SUPPLIMENTARY_DEMOGRAPHICS tables.
* 
* MINING_DATA_BUILD_V:
*   This view collects the previous customers' demographics, purchasing, and affinity 
*   card response details for building the model.
* 
* MINING_DATA_TEST_V:
*   This view collects the previous customers' demographics, purchasing, and affinity 
*   card response details for testing the model. 
* 
* MINING_DATA_APPLY_V:
*   This view collects the prospective customers' demographics, purchasing, 
*   and affinity card response details for predicting customer's age.
*
* ------------------------------------------------------------------------------
*                             DATA MINING PROCESS
* ------------------------------------------------------------------------------
*         
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*     illustrates how to build a regression model using SVM algorithm.
*     
*   Test Model:
*     Regression models performance can be evaluated by computing different types
*     of error rates. The testModel() or computeTestMetrics() method illustrates how 
*     to perform the test operation to compute various error rates.
*     
*   Apply Model:
*     Predicting the target attribute values is the prime function of regression 
*     models. The applyModel() method illustrates how to predict the customer's age.
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide 
*   for guidelines for executing this demo program.
*/
// Generic Java api imports
import java.math.BigDecimal;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;

import java.text.DecimalFormat;
import java.text.MessageFormat;

import java.util.Collection;
import java.util.HashMap;
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
import javax.datamining.algorithm.svm.regression.SVMRegressionSettings;
import javax.datamining.algorithm.svm.regression.SVMRegressionSettingsFactory;
import javax.datamining.base.AlgorithmSettings;
import javax.datamining.base.Model;
import javax.datamining.base.Task;
import javax.datamining.data.AttributeDataType;
import javax.datamining.data.ModelSignature;
import javax.datamining.data.PhysicalAttribute;
import javax.datamining.data.PhysicalAttributeFactory;
import javax.datamining.data.PhysicalAttributeRole;
import javax.datamining.data.PhysicalDataSet;
import javax.datamining.data.PhysicalDataSetFactory;
import javax.datamining.data.SignatureAttribute;
import javax.datamining.modeldetail.svm.SVMRegressionModelDetail;
import javax.datamining.resource.ConnectionSpec;
import javax.datamining.supervised.classification.ClassificationModel;
import javax.datamining.supervised.classification.ClassificationSettings;
import javax.datamining.supervised.classification.ClassificationTestMetrics;
import javax.datamining.supervised.regression.RegressionApplySettings;
import javax.datamining.supervised.regression.RegressionApplySettingsFactory;
import javax.datamining.supervised.regression.RegressionModel;
import javax.datamining.supervised.regression.RegressionSettings;
import javax.datamining.supervised.regression.RegressionSettingsFactory;
import javax.datamining.supervised.regression.RegressionTestMetrics;
import javax.datamining.supervised.regression.RegressionTestMetricsTask;
import javax.datamining.supervised.regression.RegressionTestMetricsTaskFactory;
import javax.datamining.supervised.regression.RegressionTestTask;
import javax.datamining.supervised.regression.RegressionTestTaskFactory;
import javax.datamining.task.BuildTask;
import javax.datamining.task.BuildTaskFactory;
import javax.datamining.task.apply.DataSetApplyTask;
import javax.datamining.task.apply.DataSetApplyTaskFactory;

import oracle.dmt.jdm.algorithm.svm.regression.OraSVMRegressionSettings;
import oracle.dmt.jdm.base.OraBuildSettings;
import oracle.dmt.jdm.base.OraTask;
import oracle.dmt.jdm.modeldetail.svm.OraSVMRegressionModelDetail;
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;
import oracle.dmt.jdm.supervised.regression.OraRegressionApplySettings;
import oracle.dmt.jdm.supervised.regression.OraRegressionTestMetrics;
import oracle.dmt.jdm.task.OraTransformationTask;
import oracle.dmt.jdm.task.OraTransformationTaskFactory;
import oracle.dmt.jdm.transform.normalize.OraNormalizeTransformFactory;
import oracle.dmt.jdm.transform.normalize.OraNormalizeTransformImpl;
import oracle.dmt.jdm.transform.normalize.OraNormalizeType;
// Java Data Mining (JDM) standard api imports
// Oracle Java Data Mining (JDM) implemented api imports

public class dmsvrdemo extends Object {  
  // Connection related data members
  private static javax.datamining.resource.Connection m_dmeConn;
  private static javax.datamining.resource.ConnectionFactory m_dmeConnFactory;
  // Object factories used in this demo program
  private static PhysicalDataSetFactory m_pdsFactory;
  private static PhysicalAttributeFactory m_paFactory;
  private static RegressionSettingsFactory m_regrFactory;
  private static SVMRegressionSettingsFactory m_svmrFactory;
  private static BuildTaskFactory m_buildFactory;
  private static DataSetApplyTaskFactory m_dsApplyFactory;
  private static RegressionTestTaskFactory m_testFactory;
  private static RegressionApplySettingsFactory m_applySettingsFactory;
  private static RegressionTestMetricsTaskFactory m_testMetricsTaskFactory;
  // Global constants 
  private static DecimalFormat m_df = new DecimalFormat("##.####");
  // Global data members 
  private static OraNormalizeTransformImpl m_buildDataXform;
  
  public static void main( String args[] ) { 
    try {
        if ( args.length != 3 ) {
          System.out.println("Usage: java dmsvrdemo <Host name>:<Port>:<SID> <User Name> <Password>");
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
        // 2. Clean up all previuosly created demo objects
        clean();
        // 3. Initialize factories for mining objects
        initFactories();        
        // 4. Build a model
        buildModel();
        // 5a. Test model - To compute different type of error rates for the 
        //                  model from a test input data.
        testModel();
        // 5b. Test model - To compute different type of error rates for the 
        //                  model from an apply output data.
        computeTestMetrics();
        // 6. Apply the model
        applyModel();
        // 7. Start execution of the first task in the sequnece
        m_dmeConn.execute("svmrBuildTask_jdm");
        // 8. Monitor and display results (if any)
        monitorTaskExecutionProcess();
    } catch(Exception anyExp) {
      anyExp.printStackTrace(System.out);
    } finally {
      try {
        // 8. Logout from the Data Mining Engine
        m_dmeConn.close();
      } catch(Exception anyExp1) { }//Ignore
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
    m_regrFactory = (RegressionSettingsFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.regression.RegressionSettings");
    m_svmrFactory = (SVMRegressionSettingsFactory)m_dmeConn.getFactory(
      "javax.datamining.algorithm.svm.regression.SVMRegressionSettings");
    m_buildFactory = (BuildTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.task.BuildTask");
    m_dsApplyFactory = (DataSetApplyTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.task.apply.DataSetApplyTask");
    m_testFactory = (RegressionTestTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.regression.RegressionTestTask");
    m_applySettingsFactory = (RegressionApplySettingsFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.regression.RegressionApplySettings");
    m_testMetricsTaskFactory = (RegressionTestMetricsTaskFactory) m_dmeConn.getFactory(
      "javax.datamining.supervised.regression.RegressionTestMetricsTask");
  }
  
  /**
   *   This method illustrates how to build mining model using 
   * MINING_DATA_BUILD_V dataset and regression settings with 
   * SVM algorithm. 
   * 
   *    By default SVM algorithm chooses a kernel type automatically. This 
   * choice can be overriden by the user. Linear kernel is preferred for high 
   * dimensional data, and Gaussian kernel for low dimensional data. Here we use 
   * Gaussian kernel in this demo.
   * 
   * @exception JDMException if model build failed
   */
  public static void buildModel() throws JDMException 
  {
     // 1. Create & save PhysicalDataSpecification    
      PhysicalDataSet buildData = m_pdsFactory.create("MINING_DATA_BUILD_V", false);
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      buildData.addAttribute(pa);
      m_dmeConn.saveObject("svmrBuildData_jdm", buildData, true);
     // 2. Create & save Mining Function Settings
      // Create SVMR algorithm settings
      SVMRegressionSettings svmrAlgo = m_svmrFactory.create();
      svmrAlgo.setKernelFunction(KernelFunction.kGaussian);
      // Examples settings are:
      // svmrAlgo.setKernelFunction(KernelFunction.kLinear);
      // svmrAlgo.setEpsilon(0.1f);
      // svmrAlgo.setTolerance(0.01f);
      // svmrAlgo.setKernelCacheSize(50000000);
      //
      // Create RegressionSettings
      RegressionSettings buildSettings = m_regrFactory.create();
      buildSettings.setAlgorithmSettings(svmrAlgo);
      buildSettings.setTargetAttributeName("AGE");
      //Set auto data preparation on
      ((OraBuildSettings)buildSettings).useAutomatedDataPreparations(true);
      m_dmeConn.saveObject("svmrBuildSettings_jdm", buildSettings, true);
     // 3. Create, save & execute Build Task      
      BuildTask buildTask = m_buildFactory.create(
                     "svmrBuildData_jdm", //Build data specification
                     "svmrBuildSettings_jdm", //Mining function settings name
                     "svmrModel_jdm" //Mining model name
                     );                          
      buildTask.setDescription("svmrBuildTask_jdm");
      saveTask(buildTask, "svmrBuildTask_jdm", null);
  }

  /**
   *   This method illustrates how to test the mining model with the
   * MINING_DATA_TEST_V dataset using regression test task. After
   * completion of the task, a regression test metrics object is created
   * in the DMS. Regression test metrics object encapsulates 
   * different types of error rates. 
   * 
   * @exception JDMException if model test failed
   */
  public static void testModel() 
    throws JDMException 
  {
      
    // 1. Create & save PhysicalDataSpecification      
      PhysicalDataSet testData = m_pdsFactory.create(
        "MINING_DATA_TEST_V", false );
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      testData.addAttribute( pa );
      m_dmeConn.saveObject( "svmrTestData_jdm", testData, true );
    // 2. Create, store & execute Test Task
      RegressionTestTask testTask = m_testFactory.create( 
        "svmrTestData_jdm", "svmrModel_jdm", "svrTestMetrics_jdm" );
      // 3. Store  the task
      saveTask(testTask, "svmrTestTask_jdm", "svmrBuildTask_jdm");       
  }

  /**
   *   This method illustrates how to compute test metrics using 
   * an apply output table that has actual and predicted target values. Here the
   * apply operation is done on the MINING_DATA_TEST_V dataset. It creates
   * an apply output table with actual and predicted target values. Using 
   * RegressionTestMetricsTask test metrics are computed. This produces
   * the same test metrics results as RegressionTestTask.
   * 
   * @exception JDMException if model test failed
   */
  public static void computeTestMetrics() throws JDMException
  { 
    
    // 1. Do the apply on test data to create an apply output table
    // Create & save PhysicalDataSpecification      
    PhysicalDataSet applyData = 
                    m_pdsFactory.create( "MINING_DATA_TEST_V", false );
    PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
                  AttributeDataType.integerType, PhysicalAttributeRole.caseId );
    applyData.addAttribute( pa );
    m_dmeConn.saveObject( "svmrTestApplyData_jdm", applyData, true );
    // 2. Create & save RegressionApplySettings
    RegressionApplySettings regreAS = m_applySettingsFactory.create();    
    HashMap sourceAttrMap = new HashMap();
    sourceAttrMap.put( "AGE", "AGE" );
    regreAS.setSourceDestinationMap( sourceAttrMap );
    m_dmeConn.saveObject( "svmrTestApplySettings_jdm", regreAS, true);
    // 3. Create & store apply Task
    DataSetApplyTask applyTask = m_dsApplyFactory.create(
        "svmrTestApplyData_jdm", "svmrModel_jdm", "svmrTestApplySettings_jdm", 
        "SVMR_TEST_APPLY_OUTPUT_JDM");
    saveTask(applyTask, "svmrTestApplyTask_jdm", "svmrBuildTask_jdm");
      // 4. Generate test metrics on the above new created apply output table    
      // Create & save PhysicalDataSpecification
      PhysicalDataSet applyOutputData = m_pdsFactory.create(
        "SVMR_TEST_APPLY_OUTPUT_JDM", false );
      applyOutputData.addAttribute( pa );
      m_dmeConn.saveObject( "svmrTestApplyOutput_jdm", applyOutputData, true );
      // 5. Create a RegressionTestMetricsTask
      RegressionTestMetricsTask testMetricsTask =
        m_testMetricsTaskFactory.create( "svmrTestApplyOutput_jdm", // apply output data used as input
                                       "AGE", // actual target column
                                       "PREDICTION", // predicted target column
                                       "svrComputeTestMetrics_jdm" // test metrics result name
                                      );
      // 6. Store & execute the task
      saveTask(testMetricsTask, "svmrTestMetricsTask_jdm", "svmrTestApplyTask_jdm");      
  } 

  /**
   *   This method illustrates how to apply mining model on 
   * MINING_DATA_APPLY_V dataset to predict customer's age.
   * After completion of the task apply output table with the
   * predicted results is created at the user specified location. 
   *
   * @exception JDMException if model apply failed
   */ 
  public static void applyModel() throws JDMException
  {
    // 1. Create & save PhysicalDataSpecification    
      PhysicalDataSet applyData = 
                        m_pdsFactory.create("MINING_DATA_APPLY_V", false );
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      applyData.addAttribute( pa );
      m_dmeConn.saveObject( "svmrApplyData_jdm", applyData, true );
    // 2. Create & save RegressionApplySettings
      RegressionApplySettings regrAS = m_applySettingsFactory.create();      
      m_dmeConn.saveObject( "svmrApplySettings_jdm", regrAS, true);
    // 3. Create, store & execute apply Task
     DataSetApplyTask applyTask = m_dsApplyFactory.create(
        "svmrApplyData_jdm", "svmrModel_jdm", "svmrApplySettings_jdm", 
        "SVMR_APPLY_OUTPUT_JDM");
     saveTask(applyTask, "svmrApplyTask_jdm", "svmrBuildTask_jdm");
  } 
  
  /**
   * This method saves the given task with the specified task name and task
   * dependency (parent task) in the DME.
   *
   * @param taskObj task object
   * @param taskName name of the task
   * @param parentTaskName name of the parent task
   *
   * @return boolean returns true when the task is successful
   * @exception JDMException if task execution failed
   */
  public static void saveTask(Task taskObj, String taskName,
                              String parentTaskName)
    throws JDMException
  {
    if (parentTaskName != null)
      ((OraTask) taskObj).addDependency(parentTaskName); //Since OJDM 11.1
    ((OraTask) taskObj).overwriteOutput(true); //Since OJDM 11.1
    m_dmeConn.saveObject(taskName, taskObj, true);
  }
  
  /**
       * This method monitor task execution initiated by the first task
       * in the sequence of dependent tasks. In addition, this method displays
       * task output results (if any).
       * @throws JDMException
       */
  public static void monitorTaskExecutionProcess()
   throws JDMException
  {
     //BuildTask
     System.out.println("---------------------------------------------------");
     System.out.println("--- Build Model                                 ---");
     System.out.println("---------------------------------------------------");
     //1. Wait for the completion of the task
     System.out.print("Waiting for the completion of svmrBuildTask_jdm. ");
     ExecutionStatus buildTaskCompletionStatus =
       m_dmeConn.getLastExecutionHandle("svmrBuildTask_jdm").waitForCompletion(Integer.MAX_VALUE);
     //2. If successful
     if (ExecutionState.success.equals(buildTaskCompletionStatus.getState()))
     {
       System.out.println("It is successful. ");
       // 3. Restore the model from the data mining server
       RegressionModel model = 
         (RegressionModel)m_dmeConn.retrieveObject(
           "svmrModel_jdm", NamedObject.model);
       // 4. Explore the details of the restored model
       // Display model build settings
       RegressionSettings retrievedBuildSettings = 
                                (RegressionSettings)model.getBuildSettings();
       if(retrievedBuildSettings == null) 
           System.out.println("Failure to restore build settings.");
       else 
           displayBuildSettings(retrievedBuildSettings, "svmrBuildSettings_jdm");
       // Display model signature    
       displayModelSignature((Model)model);
       // Display model details      
       displaySVMRModelDetails(model);

       System.out.println("---------------------------------------------------");
       System.out.println("--- Test Model                                  ---");
       System.out.println("---------------------------------------------------");

       //If model build is successful, then do testData ApplyTask
       //1. Wait for the completion of the task
       System.out.print("Waiting for the completion of svmcTestTask_jdm. ");
       ExecutionStatus testTaskCompletionStatus =
         m_dmeConn.getLastExecutionHandle("svmrTestTask_jdm").waitForCompletion(Integer.MAX_VALUE);
       //2. If successful
       if (ExecutionState.success.equals(testTaskCompletionStatus.getState()))
       {
         System.out.println("It is successful. ");          
         // Restore & display test metrics
         RegressionTestMetrics testMetrics = 
           (RegressionTestMetrics)m_dmeConn.retrieveObject( 
             "svrTestMetrics_jdm", 
             NamedObject.testMetrics );
         // Display regression test metrics                     
            displayTestMetricDetails(testMetrics);
       }
       else
       {
         System.out.println("It is at state:" +
                            testTaskCompletionStatus.getState().name() +
                            ((testTaskCompletionStatus.getDescription() ==
                              null)? "":
                             "State Description:" + testTaskCompletionStatus.getDescription()));
       }
       System.out.println("---------------------------------------------------");
       System.out.println("--- Test Model - using apply output table       ---");
       System.out.println("---------------------------------------------------");

       //If model build is successful, then do testData ApplyTask
       //1. Wait for the completion of the task
       System.out.print("Waiting for the completion of svmrTestMetricsTask_jdm. ");
       ExecutionStatus testMetricsTaskCompletionStatus =
         m_dmeConn.getLastExecutionHandle("svmrTestMetricsTask_jdm").waitForCompletion(Integer.MAX_VALUE);
       //2. If successful
       if (ExecutionState.success.equals(testMetricsTaskCompletionStatus.getState()))
       {
         System.out.println("It is successful. ");          
         // Restore & display test metrics
         RegressionTestMetrics testMetrics = 
           (RegressionTestMetrics)m_dmeConn.retrieveObject( 
             "svrComputeTestMetrics_jdm", 
             NamedObject.testMetrics );
         // Display regression test metrics                     
            displayTestMetricDetails(testMetrics);
       }
       else
       {
         System.out.println("It is at state:" +
                            testMetricsTaskCompletionStatus.getState().name() +
                            ((testMetricsTaskCompletionStatus.getDescription() ==
                              null)? "":
                             "State Description:" + testMetricsTaskCompletionStatus.getDescription()));
       }
       
       //ApplyTask(s)
       //Waiting for apply task to complete
       System.out.println("---------------------------------------------------");
       System.out.println("--- Apply Model                                 ---");
       System.out.println("---------------------------------------------------");

       ExecutionStatus applyTaskStatus =
         m_dmeConn.getLastExecutionHandle("svmrApplyTask_jdm").waitForCompletion(Integer.MAX_VALUE);
       if (ExecutionState.success.equals(applyTaskStatus.getState()))
       {
         // Display apply result 
         displayTable("SVMR_APPLY_OUTPUT_JDM", "where ROWNUM < 11",
                      "order by CUST_ID");
       }
       else
       {
         System.out.println("svmcApplyTask_jdm is at state:" +
                            applyTaskStatus.getState().name() +
                            ((applyTaskStatus.getDescription() == null)?
                             "":
                             "State Description:" + applyTaskStatus.getDescription()));
       }
     }
     else
     {
       System.out.println("It is at state:" +
                          buildTaskCompletionStatus.getState().name() +
                          ((buildTaskCompletionStatus.getDescription() ==
                            null)? "":
                           "State Description:" + buildTaskCompletionStatus.getDescription()));
     }
   
  }

  private static void displayBuildSettings(
    RegressionSettings regrSettings, String buildSettingsName) 
  {
    // Display build settings table
    System.out.println("BuildSettings Details from the " 
      + buildSettingsName + " table:");
      displayTable(buildSettingsName, "", "order by SETTING_NAME");
    // Display build settings object obtained from the model
    System.out.println("BuildSettings Details from the " 
      + buildSettingsName + " object:");    
    String objName = regrSettings.getName();
    if(objName != null)
      System.out.println("Name = " + objName);
    String objDescription = regrSettings.getDescription();
    if(objDescription != null)
      System.out.println("Description = " + objDescription);
    java.util.Date creationDate = regrSettings.getCreationDate();
    String creator = regrSettings.getCreatorInfo();     
    String targetAttrName = regrSettings.getTargetAttributeName();
    System.out.println("Target attribute name = " + targetAttrName);
    AlgorithmSettings algoSettings = regrSettings.getAlgorithmSettings();
    if(algoSettings == null)
      System.out.println("Failure: regrSettings.getAlgorithmSettings() returns null");
    MiningAlgorithm algo = algoSettings.getMiningAlgorithm();
    if(algo == null) System.out.println("Failure: algoSettings.getMiningAlgorithm() returns null");
    System.out.println("Algorithm Name: " + algo.name());
    MiningFunction function = regrSettings.getMiningFunction();
    if(function == null) System.out.println("Failure: regrSettings.getMiningFunction() returns null");
    System.out.println("Function Name: " + function.name());
    // List of SVM algorithm settings availabe in linear kernel
    KernelFunction kernelFunction = ((OraSVMRegressionSettings)algoSettings).getKernelFunction();
    System.out.println("Kernel Function: " + kernelFunction.name());
    double doubleValue = ((OraSVMRegressionSettings)algoSettings).getComplexityFactor();
    System.out.println("Complexity Factor: " + m_df.format(doubleValue));
    doubleValue = ((OraSVMRegressionSettings)algoSettings).getTolerance();
    System.out.println("Tolerance: " + m_df.format(doubleValue));
    boolean enable = ((OraSVMRegressionSettings)algoSettings).getActiveLearning();
    System.out.println("Is Active Learning enabled? " + enable);
    doubleValue = ((OraSVMRegressionSettings)algoSettings).getEpsilon();
    System.out.println("Epsilon: " + m_df.format(doubleValue));
  }

  /**
   * This method displayes SVMR model signature.
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
   * This method displays SVMR model details.  The coefficient indicates the 
   * relative influence of a given (attribute, value). A negative coefficient 
   * value indicates a negative influence.
   * 
   * @param svmrModelDetails svm regression model details object
   *  @exception JDMException if failed to retrieve model details
   */ 
  public static void displaySVMRModelDetails(Model model)
    throws JDMException
  {    
    SVMRegressionModelDetail svmrModelDetails = 
        (SVMRegressionModelDetail)model.getModelDetail();
    // Is linear model? 
    System.out.println("Is linear model? " + svmrModelDetails.isLinearSVMModel() );
    // Attribute coefficient value will be available if it is a linear model
    if (svmrModelDetails.isLinearSVMModel())
    {
      System.out.println("Model Deatils: ( Attribute Name, Attribute Value, Coefficient)");
      MessageFormat mfDetails = new MessageFormat("               ( {0}, {1}, {2}, {3} )");
      String[] vals = new String[4]; 
      // Print bias by invoking getBias method.
      vals[0] = "";
      vals[1] = "";
      vals[2] = m_df.format(svmrModelDetails.getBias()) + " (Bias)"; 
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
          Map attrCoefficientMap = ((OraSVMRegressionModelDetail)svmrModelDetails).getCoefficients(attr.getName());
          Object[] attrVals = attrCoefficientMap.keySet().toArray();
          if(attrVals != null) {
            for(int iAttr=0; iAttr < attrVals.length; iAttr++) 
            {            
              vals[0] = attr.getName();
              vals[1] = "";
              if(attrVals[iAttr] != null)
                vals[1] = attrVals[iAttr].toString();
              vals[2] = "";
                Number coefficient = (Number)attrCoefficientMap.get(attrVals[iAttr]);
                if(coefficient != null)
                  vals[2] = m_df.format(coefficient);
              System.out.println( mfDetails.format(vals) );
            }
          }
        }
    }
  }

  /**
   * Display regression test metrics object
   * 
   * @param testMetrics classification test metrics object
   * @exception JDMException if failed to retrieve test metric details
   */
  public static void displayTestMetricDetails(
    RegressionTestMetrics testMetrics) throws JDMException
  {
    //Retrieve Oracle SVMR model test metrics deatils extensions 
    // Name
    System.out.println("Test Metrics Name = " + testMetrics.getName());  
    // Model Name
    System.out.println("Model Name = " + testMetrics.getModelName());
    // Test Data Name
    System.out.println("Test Data Name = " + testMetrics.getTestDataName());
    // Mean Absolute Error
    System.out.println("Mean Absolute Error = " + m_df.format(testMetrics.getMeanAbsoluteError().doubleValue()));
    // Mean Actual Value
    System.out.println("Mean Actual Value = " + m_df.format(testMetrics.getMeanActualValue().doubleValue()));
    // Mean Predicted Value
    System.out.println("Mean Predicted Value = " + m_df.format(testMetrics.getMeanPredictedValue().doubleValue()));
    // Root Mean Squared Error 
    System.out.println("Root Mean Squared Error = " + m_df.format(testMetrics.getRMSError().doubleValue()));
  }
  
  private static void displayTable(String tableName, String whereCause, String orderByColumn) 
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
      while(rs.next()) 
      {
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
    //Drop the model
    try {
      m_dmeConn.removeObject("svmrModel_jdm", NamedObject.model);
    } catch(JDMException jdmExp) {}    
  }
}
