// Copyright (c) 2004, 2006, Oracle. All rights reserved.  
// File: dmglmrdemo.java
/**
*   This demo program describes how to use the Oracle Data Mining (ODM) Java API
* to solve a regression problem using Support Vector Machines (GLM) algorithm.
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
*   Prepare Data:
*   1. Missing Value treatment for Predictors in Build, Test, and Apply data
*   See dmsvcdemo.java for a definition of missing values, and the steps to be 
*   taken for missing value imputation.  GLM interprets all NULL values for a 
*   given attribute as "sparse".  We skip missing values treatment in this demo.
*
*   2. Outlier/Clipping Treatment for Predictors for Build data
*   See dmsvcdemo.java for discussion on outlier treatment.  We skip outlier 
*   treatment in this demo.
*
*   3. Normalize Predictors in Build, Test, and Apply data. 
*   See dmsvcdemo.java for discussion on normalization treatment.  The target is 
*   also normalized for GLM regression to achieve convergence of model builds 
*   with better performance.
*   
*   NOTE: AGE is the only predictor with continuous values in this dataset.
*   
*   The PrepareData() method in this demo program illustrates the preparation of the 
*   build, test, and apply data.
*         
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*     illustrates how to build a regression model using GLM algorithm.
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
import javax.datamining.resource.ConnectionSpec;
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


import oracle.dmt.jdm.algorithm.glm.OraGLMClassificationSettings;
import oracle.dmt.jdm.algorithm.glm.OraGLMRegressionSettings;
import oracle.dmt.jdm.algorithm.glm.OraGLMSettingsFactory;
import oracle.dmt.jdm.base.OraBuildSettings;
import oracle.dmt.jdm.base.OraTask;
import oracle.dmt.jdm.modeldetail.glm.OraGLMModelDetail;
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;
import oracle.dmt.jdm.supervised.regression.OraRegressionApplySettings;
import oracle.dmt.jdm.supervised.regression.OraRegressionTestMetrics;
import oracle.dmt.jdm.task.OraBuildTask;
import oracle.dmt.jdm.transform.OraExpressionTransform;
import oracle.dmt.jdm.transform.OraTransformationFactory;
import oracle.dmt.jdm.transform.OraTransformationSequence;

/**
*   This demo program describes how to use the Oracle Data Mining (ODM) Java API
* to solve a regression problem using Support Vector Machines (GLM) algorithm.
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
*   Prepare Data:
*   1. Missing Value treatment for Predictors in Build, Test, and Apply data
*   See dmsvcdemo.java for a definition of missing values, and the steps to be
*   taken for missing value imputation.  GLM interprets all NULL values for a
*   given attribute as "sparse".  We skip missing values treatment in this demo.
*
*   2. Outlier/Clipping Treatment for Predictors for Build data
*   See dmsvcdemo.java for discussion on outlier treatment.  We skip outlier
*   treatment in this demo.
*
*   3. Normalize Predictors in Build, Test, and Apply data.
*   See dmsvcdemo.java for discussion on normalization treatment.  The target is
*   also normalized for GLM regression to achieve convergence of model builds
*   with better performance.
*
*   NOTE: AGE is the only predictor with continuous values in this dataset.
*
*   The PrepareData() method in this demo program illustrates the preparation of the
*   build, test, and apply data.
*
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*     illustrates how to build a regression model using GLM algorithm.
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
public class dmglrdemo extends Object {  
// Java Data Mining (JDM) standard api imports
// Oracle Java Data Mining (JDM) implemented api imports


  // Connection related data members
  private static javax.datamining.resource.Connection m_dmeConn;
  private static javax.datamining.resource.ConnectionFactory m_dmeConnFactory;
  // Object factories used in this demo program
  private static PhysicalDataSetFactory m_pdsFactory;
  private static PhysicalAttributeFactory m_paFactory;
  private static RegressionSettingsFactory m_regrFactory;
  private static OraGLMSettingsFactory m_glmrFactory;
  private static BuildTaskFactory m_buildFactory;
  private static DataSetApplyTaskFactory m_dsApplyFactory;
  private static RegressionTestTaskFactory m_testFactory;
  private static RegressionApplySettingsFactory m_applySettingsFactory;
  private static RegressionTestMetricsTaskFactory m_testMetricsTaskFactory;
  private static OraTransformationFactory m_xformFactory = null;
  // Global constants 
  private static DecimalFormat m_df = new DecimalFormat("##.##");
  // Global data members 
  private static boolean m_displayModelDetails = false;
  
  public static void main( String args[] ) { 
    try {
        if ( args.length != 3 ) {
          System.out.println("Usage: java dmglrdemo <Host name>:<Port>:<SID> <User Name> <Password>");
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
        //computeTestMetrics();
        // 6. Apply the model
        applyModel();
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
      m_glmrFactory = (OraGLMSettingsFactory)m_dmeConn.getFactory(
        "oracle.dmt.jdm.algorithm.glm.OraGLMSettings");
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
    m_xformFactory = (OraTransformationFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.transform.OraTransformation" );
  }
  
  /**
   *   This method illustrates how to build mining model using 
   * GLMR_NORM_DATA_BUILD_JDM dataset and regression settings with 
   * GLM algorithm. 
   * 
   *    By default GLM algorithm chooses a kernel type automatically. This 
   * choice can be overriden by the user. Linear kernel is preferred for high 
   * dimensional data, and Gaussian kernel for low dimensional data. Here we use 
   * Gaussian kernel in this demo.
   * 
   * @exception JDMException if model build failed
   */
  public static void buildModel() throws JDMException 
  {
      System.out.println("---------------------------------------------------");    
      System.out.println("--- Build Model                                 ---");
      System.out.println("---------------------------------------------------");
     // 1. Create & save PhysicalDataSpecification    
      PhysicalDataSet buildData = m_pdsFactory.create("MINING_DATA_BUILD_V", false);
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      buildData.addAttribute(pa);
      m_dmeConn.saveObject("glmrBuildData_jdm", buildData, true);
     // 2. Create & save Mining Function Settings
       // 2. Create & save Mining Function Settings
        // Create GLMC algorithm settings
        OraGLMRegressionSettings glmrAlgo = m_glmrFactory.createGLMRegressionSettings();
        glmrAlgo.setDiagnosticsTableName("GLMR_DIAGNOSTICS_TABLE_JDM");
        
        //Create TransformationSequence
         OraExpressionTransform exprXform = m_xformFactory.createExpressionTransform();
         //Exclude PRINTER_SUPPLIES, CUST_INCOME_LEVEL and BOOKKEEPING_APPLICATION 
         //to avoid singularities in the data that lead to invalid covariance matrix.
         //(When expression is specified as null those attributes will be excluded)
         exprXform.addAttributeExpression("PRINTER_SUPPLIES", null, null);
         exprXform.addAttributeExpression("CUST_INCOME_LEVEL", null, null);
         exprXform.addAttributeExpression("BOOKKEEPING_APPLICATION", null, null);
         //Output view can be specified a null in this case, because we are 
         //not intended to create a view but embed the expression transformations 
         //with the model
         OraTransformationSequence xformSeq = m_xformFactory.createTransformationSequence(
          "MINING_DATA_BUILD_V", exprXform, null ); 
         m_dmeConn.saveObject("glmrTSExcludeAttrs_jdm", xformSeq, true);
         
      // Create RegressionSettings
      RegressionSettings buildSettings = m_regrFactory.create();
      buildSettings.setAlgorithmSettings(glmrAlgo);
      buildSettings.setTargetAttributeName("AGE");
      ((OraBuildSettings)buildSettings).useAutomatedDataPreparations(true);
      m_dmeConn.saveObject("glmrBuildSettings_jdm", buildSettings, true);
     // 3. Create, save & execute Build Task      
      BuildTask buildTask = m_buildFactory.create(
                     "glmrBuildData_jdm", //Build data specification
                     "glmrBuildSettings_jdm", //Mining function settings name
                     "glmrModel_jdm" //Mining model name
                     );                          
      buildTask.setDescription("glmrBuildTask_jdm");
      //Specify expression transformations that are defined to exclude attributes 
      //as an embedded transformation to the model build
      ((OraBuildTask)buildTask).setTransformationSequenceName("glmrTSExcludeAttrs_jdm");
      executeTask(buildTask, "glmrBuildTask_jdm");
     // 4. Restore the model from the DME and explore the details of the model
      RegressionModel model = 
        (RegressionModel)m_dmeConn.retrieveObject(
          "glmrModel_jdm", NamedObject.model);
      // Display model build settings
      RegressionSettings retrievedBuildSettings = 
                               (RegressionSettings)model.getBuildSettings();
      if(buildSettings == null) 
          System.out.println("Failure to restore build settings.");
      else 
          displayBuildSettings(retrievedBuildSettings, "glmrBuildSettings_jdm");
      // Display model signature    
      displayModelSignature((Model)model);
      // To display model details enable explicitely by setting m_displayModelDetails to true
      displayGLMRModelDetails(model);
  }

  /**
   *   This method illustrates how to test the mining model with the
   * GLMR_NORM_DATA_TEST_JDM dataset using regression test task. After
   * completion of the task, a regression test metrics object is created
   * in the DMS. Regression test metrics object encapsulates 
   * different types of error rates. 
   * 
   * @exception JDMException if model test failed
   */
  public static void testModel() 
    throws JDMException 
  {
      System.out.println("---------------------------------------------------");    
      System.out.println("--- Test Model - using test input table         ---");
      System.out.println("---------------------------------------------------");
    // 1. Create & save PhysicalDataSpecification      
      PhysicalDataSet testData = m_pdsFactory.create(
        "MINING_DATA_TEST_V", false );
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      testData.addAttribute( pa );
      m_dmeConn.saveObject( "glmrTestData_jdm", testData, true );
    // 2. Create, store & execute Test Task
      RegressionTestTask testTask = m_testFactory.create( 
        "glmrTestData_jdm", "glmrModel_jdm", "glmrTestMetrics_jdm" );
      // 3. Store & execute the task
      boolean isTaskSuccess = executeTask(testTask, "glmrTestTask_jdm"); 
      if( isTaskSuccess ) {
        // 4. Restore & display test metrics
        RegressionTestMetrics testMetrics = 
          (RegressionTestMetrics)m_dmeConn.retrieveObject( 
            "glmrTestMetrics_jdm", 
            NamedObject.testMetrics );
        // 5. Display regression test metrics        
        displayTestMetricDetails(testMetrics);
      }
  }

  /**
   *   This method illustrates how to compute test metrics using 
   * an apply output table that has actual and predicted target values. Here the
   * apply operation is done on the GLMR_NORM_DATA_TEST_JDM dataset. It creates
   * an apply output table with actual and predicted target values. Using 
   * RegressionTestMetricsTask test metrics are computed. This produces
   * the same test metrics results as RegressionTestTask.
   * 
   * @exception JDMException if model test failed
   */
  public static void computeTestMetrics() throws JDMException
  { 
    System.out.println("---------------------------------------------------");    
    System.out.println("--- Test Model - using apply output table       ---");
    System.out.println("---------------------------------------------------");
    // 1. Do the apply on test data to create an apply output table
    // Create & save PhysicalDataSpecification      
    PhysicalDataSet applyData = 
                    m_pdsFactory.create( "MINING_DATA_TEST_V", false );
    PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
                  AttributeDataType.integerType, PhysicalAttributeRole.caseId );
    applyData.addAttribute( pa );
    m_dmeConn.saveObject( "glmrTestApplyData_jdm", applyData, true );
    // 2. Create & save RegressionApplySettings
    RegressionApplySettings regreAS = m_applySettingsFactory.create();    
    HashMap sourceAttrMap = new HashMap();
    sourceAttrMap.put( "AGE", "AGE" );
    regreAS.setSourceDestinationMap( sourceAttrMap );
    m_dmeConn.saveObject( "glmrTestApplySettings_jdm", regreAS, true);
    // 3. Create, store & execute apply Task
    DataSetApplyTask applyTask = m_dsApplyFactory.create(
        "glmrTestApplyData_jdm", "glmrModel_jdm", "glmrTestApplySettings_jdm", 
        "GLMR_TEST_APPLY_OUTPUT_JDM");
    if(executeTask(applyTask, "glmrTestApplyTask_jdm")) {
      // 4. Generate test metrics on the above new created apply output table    
      // Create & save PhysicalDataSpecification
      PhysicalDataSet applyOutputData = m_pdsFactory.create(
        "GLMR_TEST_APPLY_OUTPUT_JDM", false );
      applyOutputData.addAttribute( pa );
      m_dmeConn.saveObject( "glmrTestApplyOutput_jdm", applyOutputData, true );
      // 5. Create a RegressionTestMetricsTask
      RegressionTestMetricsTask testMetricsTask =
        m_testMetricsTaskFactory.create( "glmrTestApplyOutput_jdm", // apply output data used as input
                                       "AGE", // actual target column
                                       "PREDICTION", // predicted target column
                                       "glrComputeTestMetrics_jdm" // test metrics result name
                                      );
      // 6. Store & execute the task
      boolean isTaskSuccess = executeTask(testMetricsTask, "glmrTestMetricsTask_jdm");
      if( isTaskSuccess ) {
        // 7. Restore & display test metrics
        RegressionTestMetrics testMetrics = 
          (RegressionTestMetrics)m_dmeConn.retrieveObject( 
            "glrComputeTestMetrics_jdm", 
            NamedObject.testMetrics );  
        // 8. Display regression test metrics        
        displayTestMetricDetails(testMetrics);
      }
    }
  } 

  /**
   *   This method illustrates how to apply mining model on 
   * GLMR_NORM_DATA_APPLY_JDM dataset to predict customer's age.
   * After completion of the task apply output table with the
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
      PhysicalDataSet applyData = 
                        m_pdsFactory.create("MINING_DATA_APPLY_V", false );
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      applyData.addAttribute( pa );
      m_dmeConn.saveObject( "glmrApplyData_jdm", applyData, true );
    // 2. Create & save RegressionApplySettings
      RegressionApplySettings regrAS = m_applySettingsFactory.create();      
      m_dmeConn.saveObject( "glmrApplySettings_jdm", regrAS, true);
    // 3. Create, store & execute apply Task
     DataSetApplyTask applyTask = m_dsApplyFactory.create(
        "glmrApplyData_jdm", "glmrModel_jdm", "glmrApplySettings_jdm", 
        "GLMR_APPLY_OUTPUT_JDM");
     executeTask(applyTask, "glmrApplyTask_jdm");
     // 4. Display apply result
     displayTable("GLMR_APPLY_OUTPUT_JDM", 
                  "where ROWNUM < 11", 
                  "order by CUST_ID");
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
    ((OraTask)taskObj).overwriteOutput(true);
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
    // List of GLM algorithm settings availabe
     
     String diagnsticsTableName = ((OraGLMRegressionSettings)algoSettings).getDiagnosticsTableName();
     System.out.println("Diagnostics Table Name: " + diagnsticsTableName);
     
     boolean useRidge = ((OraGLMRegressionSettings)algoSettings).useRidgeRegression();
     System.out.println("Use Ridge Regression: " + useRidge);
          
  }

  /**
   * This method displayes GLMR model signature.
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
   * This method displays GLMR model details.  The coefficient indicates the 
   * relative influence of a given (attribute, value). A negative coefficient 
   * value indicates a negative influence.
   * 
   * @param model glm regression model object
   *  @exception JDMException if failed to retrieve model details
   */ 
   public static void displayGLMRModelDetails(
     Model model) throws JDMException
   {
      // Obtains model details
      OraGLMModelDetail glmrModelDetails = 
        (OraGLMModelDetail)model.getModelDetail();
      //Get computer high-level statitics of the model
      Map glblDetails = glmrModelDetails.getGlobalDetails();
      if(m_displayModelDetails) {
        System.out.println("Global Model details:");
        System.out.println(glblDetails.toString());
      }
      //Get all attribute coefficients without any filters
      ResultSet rsGLM = glmrModelDetails.getAttributeCoefficients(null, null, null, null);
      //Display first coeficcient record
       try {
         rsGLM.next();
         if(m_displayModelDetails) {
           System.out.println("Target value " + rsGLM.getString("CLASS") );
           System.out.println("Attribute name " +  rsGLM.getString("ATTRIBUTE_NAME"));
           System.out.println("Attribute value " +  rsGLM.getString("ATTRIBUTE_VALUE"));
           System.out.println("Coefficient " +  rsGLM.getDouble("COEFFICIENT")); 
           System.out.println("Standard Error " +  rsGLM.getDouble("STD_ERROR"));
           System.out.println("Test static " +  rsGLM.getDouble("TEST_STATISTIC"));
           System.out.println("P Value " +  rsGLM.getDouble("P_VALUE"));
           System.out.println("Variance Inflation Factor " +  rsGLM.getDouble("VIF"));
           System.out.println("Std. Coefficient " +  rsGLM.getDouble("STD_COEFFICIENT"));
           System.out.println("Lower Coefficient Limit " +  rsGLM.getDouble("LOWER_COEFF_LIMIT"));
           System.out.println("Upper Coefficient Limit " +  rsGLM.getDouble("UPPER_COEFF_LIMIT"));
           System.out.println("Exp Coefficient " +  rsGLM.getDouble("EXP_COEFFICIENT"));
           System.out.println("Exp Lower Coefficient " +  rsGLM.getDouble("EXP_LOWER_COEFF_LIMIT")); 
           System.out.println("Exp Upper Coefficient " +  rsGLM.getDouble("EXP_UPPER_COEFF_LIMIT"));
         }
        } catch(SQLException anyExp) {
            anyExp.printStackTrace();
        }
      //After using the details must close the statement associated with the resultset and the resultset
      Statement stmtGLM = null;
      try {
          stmtGLM = rsGLM.getStatement();
          rsGLM.close();
          stmtGLM.close();
      } catch(SQLException anyExp) {
          
      } finally {
          try { if(rsGLM != null) rsGLM.close(); } catch(SQLException sqlExp) {}
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
    //Retrieve Oracle GLMR model test metrics deatils extensions 
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
    //Drop prepared views
    java.sql.Connection dbConn = 
      ((OraConnection)m_dmeConn).getDatabaseConnection();
    Statement stmt = null;
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP TABLE GLMR_DIAGNOSTICS_TABLE_JDM purge");   
    } catch(SQLException anySqlExp) {}//Ignore
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    }
    //Drop the model
    try {
      m_dmeConn.removeObject("glmrModel_jdm", NamedObject.model);
    } catch(JDMException jdmExp) {}
  }
}
