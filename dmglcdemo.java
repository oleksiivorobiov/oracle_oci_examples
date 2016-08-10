// Copyright (c) 2004, 2006, Oracle. All rights reserved.
// File: dmglcdemo.java
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
import javax.datamining.supervised.classification.ClassificationApplySettings;
import javax.datamining.supervised.classification.ClassificationApplySettingsFactory;
import javax.datamining.supervised.classification.ClassificationModel;
import javax.datamining.supervised.classification.ClassificationSettings;
import javax.datamining.supervised.classification.ClassificationSettingsFactory;
import javax.datamining.supervised.classification.ClassificationTestMetricOption;
import javax.datamining.supervised.classification.ClassificationTestMetrics;
import javax.datamining.supervised.classification.ClassificationTestMetricsTask;
import javax.datamining.supervised.classification.ClassificationTestMetricsTaskFactory;
import javax.datamining.supervised.classification.ClassificationTestTaskFactory;
import javax.datamining.supervised.classification.ConfusionMatrix;
import javax.datamining.supervised.classification.Lift;
import javax.datamining.supervised.classification.ReceiverOperatingCharacterics;
import javax.datamining.task.BuildTask;
import javax.datamining.task.BuildTaskFactory;
import javax.datamining.task.apply.DataSetApplyTask;

import javax.datamining.task.apply.DataSetApplyTaskFactory;

import oracle.dmt.jdm.algorithm.glm.OraGLMClassificationSettings;
import oracle.dmt.jdm.algorithm.glm.OraGLMSettingsFactory;
import oracle.dmt.jdm.base.OraBuildSettings;
import oracle.dmt.jdm.base.OraTask;
import oracle.dmt.jdm.modeldetail.glm.OraGLMModelDetail;
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;
import oracle.dmt.jdm.supervised.classification.OraClassificationSettings;
import oracle.dmt.jdm.supervised.classification.OraLift;
import oracle.dmt.jdm.task.OraBuildTask;
import oracle.dmt.jdm.task.OraTransformationTaskFactory;
import oracle.dmt.jdm.transform.OraExpressionTransform;
import oracle.dmt.jdm.transform.OraTransformationFactory;
import oracle.dmt.jdm.transform.OraTransformationSequence;
import oracle.dmt.jdm.transform.normalize.OraNormalizeTransformImpl;
// Copyright (c) 2004, 2006, Oracle. All rights reserved.  
// File: dmsvcdemo.java
/**
*   This demo program describes how to use the Oracle Data Mining (ODM) Java API
* to solve a classification problem using Support Vector Machines (SVM) 
* algorithm.
* ------------------------------------------------------------------------------
*                             PROBLEM DEFINITION
* ------------------------------------------------------------------------------
*   How to predict whether a customer responds or not to the new affinity card 
* program using a classifier based on SVM algorithm?
* ------------------------------------------------------------------------------
*                             DATA DESCRIPTION
* ------------------------------------------------------------------------------
*   Data for this demo is composed from base tables in the Sales History (SH) 
* Schema. The SH schema is an Oracle Database Sample Schema that has the customer
* demographics, purchasing, and response details for the previous affinity card 
* programs. Data exploration and preparing the data is a common step before 
* doing data mining. Here in this demo, the following views are created in the user 
* schema using CUSTOMERS, COUNTRIES, and SUPPLIMENTARY_DEMOGRAPHICS tables.
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
*   This view collects the prospective customers' demographics and purchasing 
*   details for predicting response for the new affinity card program.
*
* ------------------------------------------------------------------------------
*                             DATA MINING PROCESS
* ------------------------------------------------------------------------------
*   Prepare Data:
*   1. Missing Value treatment for Predictors in Build, Test, and Apply data
*
*    "Missing Value" here implies "Missing at Random" values, that is, 
*    there is no pattern to the missing data, a NULL value does not have a
*    special meaning, and there is no correlation either direct or indirect
*    with the target or other predictor values.
*
*    Missing value imputation is recommended when the fraction of missing 
*    at random values is high compared to the overall attribute value set.
*
*    Contrast this with a "sparse" dataset - where a NULL value for
*    an attribute encodes a zero/non-present. Typical examples of domains
*    with sparse data are market basket and text mining. Missing value
*    imputation should not be used on sparse data. By default, SVM
*    interprets all NULL values for a given attribute as "sparse".
*
*    Based on the business problem, one should designate the NULL value
*    for a given attribute to be "missing at random" or "sparse". For
*    example, a NULL value for AGE in demographic data can be missing
*    at random if omitting this information is a random occurrence.
*    In contrast, a shopping cart record with NULL values for certain
*    items is usually considered sparse - because a customer cannot
*    buy the entire inventory of items in a store.
*
*    Here we are showing a simple approach to missing value treatment:
*
*    1.1. Compute the mean for numerical attributes, and mode for
*         categoricals. Store them away in a table for use with
*         Test and Apply data.
*
*    1.2. Replace the NULLs in the build, and test and apply, data
*         with the computed mean (for numerical), mode (for categorical).
*
*   2. Outlier/Clipping Treatment for Predictors for Build data
*
*    For SVM, we recommended that the data be normalized (individual 
*    attributes need to be on a similar scale) to achieve faster
*    convergence of model builds and more accurate models. Large outliers 
*    in individual attributes can result in sub-optimal normalization
*    output and sub-optimal models. Note that outlier/clipping treatment would 
*    be also beneficial for algorithms that use equi-width-binning 
*    (e.g., O-Cluster) since outliers in the data would produce sub-optimal bin 
*    boundaries.
*
*    Here we winsorize the tail elements of the data as a preparatory step to 
*    normalization.
*    
*    We recommend the use of winsorizing to reduce the influence of outliers on
*    the normalization computation. The normalization parameters are computed 
*    on the winsorized data. Once the normalization parameters are computed,
*    they can be used directly on the original data. Winsorizing is not needed
*    at test and apply time - it only affects the specification of the
*    normalization parameters.
*
*   3. Normalize Predictors in Build, Test, and Apply data, unlike Regression 
*   using SVM, the target attribute is NOT normalized.
*   
*   The PrepareData() method in this demo program illustrates the preparation of the 
*   build, test, and apply data.
*         
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*     illustrates how to build a classification model using SVM algorithm.
*     
*   Test Model:
*     Classification models' performance can be evaluated by computing test 
*     metrics like accuracy, confusion matrix, lift and ROC. The testModel() or 
*     computeTestMetrics() method illustrates how to perform a test operation to 
*     compute various metrics.
*     
*   Apply Model:
*     Predicting the target attribute values is the prime function of 
*     classification models. The applyModel() method illustrates how to 
*     predict the customer response for an affinity card program.
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide 
*   for guidelines for executing this demo program.
*/
// Generic Java api imports


/**
*   This demo program describes how to use the Oracle Data Mining (ODM) Java API
* to solve a classification problem using Support Vector Machines (SVM)
* algorithm.
* ------------------------------------------------------------------------------
*                             PROBLEM DEFINITION
* ------------------------------------------------------------------------------
*   How to predict whether a customer responds or not to the new affinity card
* program using a classifier based on SVM algorithm?
* ------------------------------------------------------------------------------
*                             DATA DESCRIPTION
* ------------------------------------------------------------------------------
*   Data for this demo is composed from base tables in the Sales History (SH)
* Schema. The SH schema is an Oracle Database Sample Schema that has the customer
* demographics, purchasing, and response details for the previous affinity card
* programs. Data exploration and preparing the data is a common step before
* doing data mining. Here in this demo, the following views are created in the user
* schema using CUSTOMERS, COUNTRIES, and SUPPLIMENTARY_DEMOGRAPHICS tables.
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
*   This view collects the prospective customers' demographics and purchasing
*   details for predicting response for the new affinity card program.
*
* ------------------------------------------------------------------------------
*                             DATA MINING PROCESS
* ------------------------------------------------------------------------------
*   Prepare Data:
*   1. Missing Value treatment for Predictors in Build, Test, and Apply data
*
*    "Missing Value" here implies "Missing at Random" values, that is,
*    there is no pattern to the missing data, a NULL value does not have a
*    special meaning, and there is no correlation either direct or indirect
*    with the target or other predictor values.
*
*    Missing value imputation is recommended when the fraction of missing
*    at random values is high compared to the overall attribute value set.
*
*    Contrast this with a "sparse" dataset - where a NULL value for
*    an attribute encodes a zero/non-present. Typical examples of domains
*    with sparse data are market basket and text mining. Missing value
*    imputation should not be used on sparse data. By default, SVM
*    interprets all NULL values for a given attribute as "sparse".
*
*    Based on the business problem, one should designate the NULL value
*    for a given attribute to be "missing at random" or "sparse". For
*    example, a NULL value for AGE in demographic data can be missing
*    at random if omitting this information is a random occurrence.
*    In contrast, a shopping cart record with NULL values for certain
*    items is usually considered sparse - because a customer cannot
*    buy the entire inventory of items in a store.
*
*    Here we are showing a simple approach to missing value treatment:
*
*    1.1. Compute the mean for numerical attributes, and mode for
*         categoricals. Store them away in a table for use with
*         Test and Apply data.
*
*    1.2. Replace the NULLs in the build, and test and apply, data
*         with the computed mean (for numerical), mode (for categorical).
*
*   2. Outlier/Clipping Treatment for Predictors for Build data
*
*    For SVM, we recommended that the data be normalized (individual
*    attributes need to be on a similar scale) to achieve faster
*    convergence of model builds and more accurate models. Large outliers
*    in individual attributes can result in sub-optimal normalization
*    output and sub-optimal models. Note that outlier/clipping treatment would
*    be also beneficial for algorithms that use equi-width-binning
*    (e.g., O-Cluster) since outliers in the data would produce sub-optimal bin
*    boundaries.
*
*    Here we winsorize the tail elements of the data as a preparatory step to
*    normalization.
*
*    We recommend the use of winsorizing to reduce the influence of outliers on
*    the normalization computation. The normalization parameters are computed
*    on the winsorized data. Once the normalization parameters are computed,
*    they can be used directly on the original data. Winsorizing is not needed
*    at test and apply time - it only affects the specification of the
*    normalization parameters.
*
*   3. Normalize Predictors in Build, Test, and Apply data, unlike Regression
*   using SVM, the target attribute is NOT normalized.
*
*   The PrepareData() method in this demo program illustrates the preparation of the
*   build, test, and apply data.
*
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*     illustrates how to build a classification model using SVM algorithm.
*
*   Test Model:
*     Classification models' performance can be evaluated by computing test
*     metrics like accuracy, confusion matrix, lift and ROC. The testModel() or
*     computeTestMetrics() method illustrates how to perform a test operation to
*     compute various metrics.
*
*   Apply Model:
*     Predicting the target attribute values is the prime function of
*     classification models. The applyModel() method illustrates how to
*     predict the customer response for an affinity card program.
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide
*   for guidelines for executing this demo program.
*/
// Java Data Mining (JDM) standard api imports
// Oracle Java Data Mining (JDM) implemented api imports

public class dmglcdemo extends Object {
  // Connection related data members
  private static javax.datamining.resource.Connection m_dmeConn;
  private static javax.datamining.resource.ConnectionFactory m_dmeConnFactory;
  // Object factories used in this demo program
  private static PhysicalDataSetFactory m_pdsFactory;
  private static PhysicalAttributeFactory m_paFactory;
  private static ClassificationSettingsFactory m_clasFactory;
  private static OraGLMSettingsFactory m_glmcFactory;
  private static BuildTaskFactory m_buildFactory;
  private static DataSetApplyTaskFactory m_dsApplyFactory;
  private static ClassificationTestTaskFactory m_testFactory;
  private static ClassificationApplySettingsFactory m_applySettingsFactory;
  private static ClassificationTestMetricsTaskFactory m_testMetricsTaskFactory;
  private static OraTransformationFactory m_xformFactory;
  // Global constants 
  private static DecimalFormat m_df = new DecimalFormat("##.##");
  // Global data members 
  private static boolean m_displayModelDetails = false;
  private static boolean m_displayTestMetricsDetails = false;
  
  public static void main( String args[] ) { 
    try { 
        if (( args.length != 0 ) & ( args.length != 3 )) {
          System.out.println("Usage: java dmglcdemo ");
          System.out.println("   or: java dmglcdemo <Host name>:<Port>:<SID> <User Name> <Password>");
          return;
        }
        String uri = args[0];
        String name =  args[1];
        String password = args[2]; 
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
        // 4. Build a model with supplied prior probability
        buildModel();
        // 6. Test model - To compute accuracy and confusion matrix, lift result
        //                 and ROC for the model from an apply output data.
        //                 Please see dnnbdemo.java to see how to test the model 
        //                 with a test input data and cost matrix.  In SVM, we 
        //                 recommend that any known costs be provided to the 
        //                 algorithm during build via the prior mechanism.
        computeTestMetrics();
        // 7. Apply the model
        applyModel();
    } catch(Exception anyExp) {
      anyExp.printStackTrace(System.out);
    } finally {
      try {
        // 8. Logout from the Data Mining Engine
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
    m_glmcFactory = (OraGLMSettingsFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.algorithm.glm.OraGLMSettings");
    m_buildFactory = (BuildTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.task.BuildTask");
    m_testFactory = (ClassificationTestTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.classification.ClassificationTestTask");
    m_dsApplyFactory = (DataSetApplyTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.task.apply.DataSetApplyTask");
    m_applySettingsFactory = (ClassificationApplySettingsFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.classification.ClassificationApplySettings");
    m_testMetricsTaskFactory = (ClassificationTestMetricsTaskFactory) m_dmeConn.getFactory(
      "javax.datamining.supervised.classification.ClassificationTestMetricsTask");
    m_xformFactory = (OraTransformationFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.transform.OraTransformation" );
  }

  /**
   *   This method illustrates how to build mining model using 
   * MINING_DATA_BUILD_V dataset and classification settings with 
   * SVM algorithm. 
   * 
   *    By default SVM algorithm chooses an kernel type automatically. This 
   * choice can be overriden by the user. Linear kernel is preferred high 
   * dimensional data, and Gaussian kernel for low dimensional data. Here we use 
   * linear kernel to demonstrate the getModelDetail() API, which applies only 
   * for models.  
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
      m_dmeConn.saveObject("glmcBuildData_jdm", buildData, true);
     // 2. Create & save Mining Function Settings
      //Create GLMC algorithm settings
      OraGLMClassificationSettings glmcAlgo = m_glmcFactory.createGLMClassificationSettings();
      glmcAlgo.setDiagnosticsTableName("GLMC_DIAGNOSTICS_TABLE_JDM");
      glmcAlgo.setReferenceCategory(new Integer(1));
      
      //Create ClassificationSettings
      ClassificationSettings buildSettings = m_clasFactory.create();
      buildSettings.setAlgorithmSettings(glmcAlgo);
      buildSettings.setTargetAttributeName("AFFINITY_CARD");      
      ((OraBuildSettings)buildSettings).useAutomatedDataPreparations(true);
      m_dmeConn.saveObject("glmcBuildSettings_jdm", buildSettings, true);
      
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
       m_dmeConn.saveObject("glmcTSExcludeAttrs_jdm", xformSeq, true);
      
     // 3. Create, save & execute Build Task      
      BuildTask buildTask = m_buildFactory.create(
                     "glmcBuildData_jdm", //Build data specification
                     "glmcBuildSettings_jdm", //Mining function settings name
                     "glmcModel_jdm" //Mining model name
                     );                          
      buildTask.setDescription("glmcBuildTask_jdm");
      //Specify expression transformations that are defined to exclude attributes 
      //as an embedded transformation to the model build
      ((OraBuildTask)buildTask).setTransformationSequenceName("glmcTSExcludeAttrs_jdm");
      executeTask(buildTask, "glmcBuildTask_jdm");
     //4. Restore the model from the DME and explore the details of the model
      ClassificationModel model = 
        (ClassificationModel)m_dmeConn.retrieveObject(
          "glmcModel_jdm", NamedObject.model);
      // Display model build settings
      ClassificationSettings retrievedBuildSettings = 
                               (ClassificationSettings)model.getBuildSettings();
      if(buildSettings == null) 
          System.out.println("Failure to restore build settings.");
      else 
          displayBuildSettings(retrievedBuildSettings, "glmcBuildSettings_jdm");
      // Display model signature    
      displayModelSignature((Model)model);
      displayGLMCModelDetails((Model)model); 
  }


  /**
   *   This method illustrates how to compute test metrics using 
   * an apply output table that has actual and predicted target values. Here 
   * apply operation is done on MINING_DATA_TEST_V dataset which creates
   * an apply output table with actual and predicted target values. Using 
   * ClassificationTestMetricsTask, test metrics are computed. This produces
   * the same test metrics results as ClassificationTestTask.
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
        PhysicalDataSet applyData = m_pdsFactory.create(
          "MINING_DATA_TEST_V", false );
        PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
          AttributeDataType.integerType, PhysicalAttributeRole.caseId );
        applyData.addAttribute( pa );
        m_dmeConn.saveObject( "glmcTestApplyData_jdm", applyData, true );
      // 2. Create & save ClassificationApplySettings
        ClassificationApplySettings clasAS = m_applySettingsFactory.create();
        HashMap srcDestMap = new HashMap();
        srcDestMap.put("AFFINITY_CARD", "AFFINITY_CARD");
        clasAS.setSourceDestinationMap( srcDestMap );
        m_dmeConn.saveObject( "glmcTestApplySettings_jdm", clasAS, true);    
      // 3. Create, store & execute apply Task
       DataSetApplyTask applyTask = m_dsApplyFactory.create(
          "glmcTestApplyData_jdm", "glmcModel_jdm", 
          "glmcTestApplySettings_jdm", 
          "GLMC_TEST_APPLY_OUTPUT_JDM");
       if(executeTask(applyTask, "glmcTestApplyTask_jdm")) {
        // 4. Generate test metrics on the above new created apply output table    
        // Create & save PhysicalDataSpecification
          PhysicalDataSet applyOutputData = m_pdsFactory.create(
            "GLMC_TEST_APPLY_OUTPUT_JDM", false );
          applyOutputData.addAttribute( pa );
          m_dmeConn.saveObject( "glmcTestApplyOutput_jdm", applyOutputData, true );        
        // 5. Create a ClassificationTestMetricsTask
        ClassificationTestMetricsTask testMetricsTask =
          m_testMetricsTaskFactory.create( "glmcTestApplyOutput_jdm", // apply output data used as input
                                           "AFFINITY_CARD", // actual target column
                                           "PREDICTION", // predicted target column
                                           "svcComputeTestMetrics_jdm" // test metrics result name
                                          );
        testMetricsTask.computeMetric(   // enable confusion matrix computation
                ClassificationTestMetricOption.confusionMatrix, true );
        testMetricsTask.computeMetric(   // enable lift computation
                ClassificationTestMetricOption.lift, true );
        testMetricsTask.computeMetric(   // enable ROC computation
                ClassificationTestMetricOption.receiverOperatingCharacteristics, true );
        testMetricsTask.setPositiveTargetValue( new Integer(1) );
        testMetricsTask.setNumberOfLiftQuantiles( 10 );
        testMetricsTask.setPredictionRankingAttrName( "PROBABILITY" );
        // 6. Store & execute the task
        boolean isTaskSuccess = executeTask(testMetricsTask, "glmcTestMetricsTask_jdm");     
        if( isTaskSuccess ) {
          // 7. Restore & display test metrics
          ClassificationTestMetrics testMetrics = 
            (ClassificationTestMetrics)m_dmeConn.retrieveObject( 
              "svcComputeTestMetrics_jdm", 
              NamedObject.testMetrics );
          // Display classification test metrics
          displayTestMetricDetails(testMetrics);
        }
       }
  } 
  
  /**
   *   This method illustrates how to apply the mining model on the
   * GLMC_NORM_DATA_APPLY_JDM dataset to predict customer
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
                                            "MINING_DATA_APPLY_V", false );
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      applyData.addAttribute( pa );
      m_dmeConn.saveObject( "glmcApplyData_jdm", applyData, true );
    // 2. Create & save ClassificationApplySettings
      ClassificationApplySettings clasAS = m_applySettingsFactory.create();
      m_dmeConn.saveObject( "glmcApplySettings_jdm", clasAS, true);           
    // 3. Create, store & execute apply Task
     DataSetApplyTask applyTask = m_dsApplyFactory.create(
                              "glmcApplyData_jdm", "glmcModel_jdm", 
                              "glmcApplySettings_jdm", "GLMC_APPLY_OUTPUT_JDM");
     executeTask(applyTask, "glmcApplyTask_jdm");
     // 4. Display apply result -- Note that APPLY results do not need to be 
     //    reverse transformed, as done in the case of model details. This is 
     //    because class values of a classification target were not (required to 
     //    be) binned or normalized.
     displayTable("GLMC_APPLY_OUTPUT_JDM", 
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
    ClassificationSettings clasSettings, String buildSettingsName) 
  {
    // Display build settings table
    System.out.println("BuildSettings Details from the " 
      + buildSettingsName + " table:");
      displayTable(buildSettingsName, "", "order by SETTING_NAME");
    // Display build settings object obtained from the model
    System.out.println("BuildSettings Details from the " 
      + buildSettingsName + " model build settings object:");    
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
    
    try {
      Map map = ((OraClassificationSettings)clasSettings).getTargetWeightsMap();
      if(map != null)    System.out.println("Target Weights Map: " + map.toString());
    } catch(Exception jdmExp) 
    {
      System.out.println("Failure: clasSettings.getTargetWeightsMap()throws exception");
      jdmExp.printStackTrace();
    }  
    
    // List of GLM algorithm settings availabe in linear kernel
    String diagnsticsTableName = ((OraGLMClassificationSettings)algoSettings).getDiagnosticsTableName();
    System.out.println("Diagnostics Table Name: " + diagnsticsTableName);
    
    Object refCategory = ((OraGLMClassificationSettings)algoSettings).getReferenceCategory();
    System.out.println("Reference category: " + refCategory);
    
    boolean useRidge = ((OraGLMClassificationSettings)algoSettings).useRidgeRegression();
    System.out.println("Use Ridge Regression: " + useRidge);
    
  }

  /**
   * This method displayes GLMC model signature.
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
   * This method displayes GLMC model details.  The coefficient indicates the 
   * relative influence of a given (attribute, value) pair on the target value. 
   * A negative coefficient value indicates a negative influence.
   * 
   * @param glmcModelDetails svm classification model details object
   * @exception JDMException if failed to retrieve model details
   */ 
  public static void displayGLMCModelDetails(
    Model model) throws JDMException
  {
    // Obtains model details
    OraGLMModelDetail glmcModelDetails = 
      (OraGLMModelDetail)model.getModelDetail();
    //Get computer high-level statitics of the model
    Map glblDetails = glmcModelDetails.getGlobalDetails();
    if(m_displayModelDetails) {
      System.out.println("Global Model details:");
      System.out.println(glblDetails.toString());
    }
    //Get all attribute coefficients without any filters
    ResultSet rsGLM = glmcModelDetails.getAttributeCoefficients(null, null, null, null);
    //Display first coeficcient record
     try {
       rsGLM.next();
       //To display model details enable explicitely by setting m_displayModelDetails to true
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
   * Display classification test metrics object
   * 
   * @param testMetrics classification test metrics object
   * @exception JDMException if failed to retrieve test metric details
   */
  public static void displayTestMetricDetails(
    ClassificationTestMetrics testMetrics) throws JDMException
  {
    // Retrieve Oracle SVMC model test metrics deatils extensions
    // Test Metrics Name
    System.out.println("Test Metrics Name = " + testMetrics.getName());  
    // Model Name
    System.out.println("Model Name = " + testMetrics.getModelName());
    // Test Data Name
    System.out.println("Test Data Name = " + testMetrics.getTestDataName());
    // Accuracy
    System.out.println("Accuracy = " + m_df.format(testMetrics.getAccuracy().doubleValue()));
    // Confusion Matrix
    ConfusionMatrix confusionMatrix = testMetrics.getConfusionMatrix();
    Collection categories = confusionMatrix.getCategories(); 
    Iterator xIterator = categories.iterator();
    System.out.println("Confusion Matrix: Accuracy = " + m_df.format(confusionMatrix.getAccuracy()));
    System.out.println("Confusion Matrix: Error = " + m_df.format(confusionMatrix.getError()));
    if(m_displayTestMetricsDetails) 
      System.out.println("Confusion Matrix:( Actual, Prection, Value )");
    MessageFormat mf = new MessageFormat("                 ( {0}, {1}, {2} )");
    String[] vals = new String[3];    
    while(xIterator.hasNext()) 
    {
      Object actual = xIterator.next();
      vals[0] = actual.toString();
      Iterator yIterator = categories.iterator();
      while(yIterator.hasNext()) 
      {
        Object predicted = yIterator.next();
        vals[1] = predicted.toString();
        long number = confusionMatrix.getNumberOfPredictions(actual, predicted);
        vals[2] = Long.toString(number);
        if(m_displayTestMetricsDetails) 
          System.out.println(mf.format(vals));
      }
    }
    // Lift
    Lift lift = testMetrics.getLift();
    String targetAttrName = lift.getTargetAttributeName();
    Object positiveTargetVal = lift.getPositiveTargetValue();
    long totalCases = lift.getTotalCases();
    long totalPositiveCases = lift.getTotalPositiveCases();
    int numberOfQuantiles = lift.getNumberOfQuantiles();
    if(m_displayTestMetricsDetails) {
      System.out.println("Lift Details:");
      System.out.println("Lift: Target Attribute Name = " + targetAttrName);
      System.out.println("Lift: Positive Target Value = " + positiveTargetVal);
      System.out.println("Lift: Total Cases  = " + totalCases);
      System.out.println("Lift: Total Positive Cases = " + totalPositiveCases);      
      System.out.println("Lift: Number Of Quantiles = " + numberOfQuantiles);
      System.out.println("Lift: ( QUANTILE_NUMBER, TOTAL_CASES, QUANTILE_PERCENT_SIZE, POSITIVE_CASES, NEGATIVE_CASES, PROBABILITY_THRESHOLD, LIFT, CUMULATIVE_LIFT, GAIN, CUMULATIVE_GAIN, TARGET_DENSITY,  CUMULATIVE_TARGET_DENSITY)");
    }
    MessageFormat mfLift =
     new MessageFormat("      ( {0}, {1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10} )");
  
    double[] totalCasesPerQuantile = ((OraLift)lift).getCases();//Total cases per quantile
    double[] quantilePercentageSize = ((OraLift)lift).getPercentageSize();//Quantile pecentage size
    double[] positiveCasesPerQuantile = ((OraLift)lift).getNumberOfPositiveCases();//Positive target cases per quantile
    double[] negativeCasesPerQuantile = ((OraLift)lift).getNumberOfNegativeCases();//Negative target cases per quantile
    double[] probabilityOrCostThresholdPerQuantile = ((OraLift)lift).getProbabilityOrCostThreshold();//Probability or cost threshold per quantile
    double[] liftPerQuantile = ((OraLift)lift).getLift();//Lift values for all quantiles    
    double[] targetDensityPerQuantile = ((OraLift)lift).getTargetDensity();//Target densities per quantile    
    
    double[] cumulativeLift = ((OraLift)lift).getCumulativeLift();
    double[] cumulativeGain = ((OraLift)lift).getCumulativeGain();
    double[] cumulativeTargetDensity = ((OraLift)lift).getCumulativeTargetDensity();
    
    String[] messageParams = new String[11];
    
    for (int iQuantile = 0; iQuantile < numberOfQuantiles; iQuantile++)
    {
      //Assign message parameters
      messageParams[0] = Integer.toString(iQuantile+1); //QUANTILE_NUMBER
      messageParams[1] = m_df.format(totalCasesPerQuantile[iQuantile]); //TOTAL_CASES
      messageParams[2] = m_df.format(quantilePercentageSize[iQuantile]);//QUANTILE_PERCENT_SIZE
      messageParams[3] = m_df.format(positiveCasesPerQuantile[iQuantile]); //POSITIVE_CASES
      messageParams[4] = m_df.format(negativeCasesPerQuantile[iQuantile]); //NEGATIVE_CASES
      messageParams[5] = m_df.format(probabilityOrCostThresholdPerQuantile[iQuantile]); //PROBABILITY_THRESHOLD
      messageParams[6] = m_df.format(liftPerQuantile[iQuantile]); //LIFT
      messageParams[7] = m_df.format(cumulativeLift[iQuantile]); //CUMULATIVE_LIFT
      messageParams[8] = m_df.format(cumulativeGain[iQuantile]); //CUMULATIVE_GAIN
      messageParams[9] = m_df.format(targetDensityPerQuantile[iQuantile]); //TARGET_DENSITY
      messageParams[10] = m_df.format(cumulativeTargetDensity[iQuantile]); //CUMULATIVE_TARGET_DENSITY
      if(m_displayTestMetricsDetails) 
       System.out.println(mfLift.format(messageParams));
    }
    // ROC
    ReceiverOperatingCharacterics roc = testMetrics.getROC();
    double areaUnderCurve = roc.getAreaUnderCurve();
    int nROCThresh = roc.getNumberOfThresholdCandidates();
    if(m_displayTestMetricsDetails) {
      System.out.println("ROC Details:");
      System.out.println("ROC: Area Under Curve = " + m_df.format(areaUnderCurve));
      System.out.println("ROC: Number Of Threshold Candidates = " + nROCThresh);
      System.out.println("ROC: ( INDEX, PROBABILITY, TRUE_POSITIVES, FALSE_NEGATIVES, FALSE_POSITIVES, TRUE_NEGATIVES, TRUE_POSITIVE_FRACTION, FALSE_POSITIVE_FRACTION )");
    }
    MessageFormat mfROC = new MessageFormat("     ( {0}, {1}, {2}, {3}, {4}, {5}, {6}, {7} )");
    String[] rocVals = new String[8];
    for(int iROC=1; iROC <= nROCThresh; iROC++) 
    {
      rocVals[0] = Integer.toString(iROC); //INDEX
      rocVals[1] = m_df.format(roc.getProbabilityThreshold(iROC));//PROBABILITY
      rocVals[2] = Long.toString(roc.getPositives(iROC, true));//TRUE_POSITIVES
      rocVals[3] = Long.toString(roc.getNegatives(iROC, false));//FALSE_NEGATIVES
      rocVals[4] = Long.toString(roc.getPositives(iROC, false));//FALSE_POSITIVES
      rocVals[5] = Long.toString(roc.getNegatives(iROC, true));//TRUE_NEGATIVES
      rocVals[6] = m_df.format(roc.getHitRate(iROC));//TRUE_POSITIVE_FRACTION
      rocVals[7] = m_df.format(roc.getFalseAlarmRate(iROC));//FALSE_POSITIVE_FRACTION     
      if(m_displayTestMetricsDetails)
        System.out.println(mfROC.format(rocVals));
    }
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
    java.sql.Connection dbConn = 
      ((OraConnection)m_dmeConn).getDatabaseConnection();
    Statement stmt = null;
    //Drop prepared tables
    try
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP TABLE GLMC_DIAGNOSTICS_TABLE_JDM PURGE");
    } catch(SQLException anySqlExp) {} // Ignore
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    }
    //Drop the model
    try {
      m_dmeConn.removeObject("glmcModel_jdm", NamedObject.model);
    } catch(JDMException jdmExp) {}
  }
}
